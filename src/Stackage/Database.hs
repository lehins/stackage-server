{-# LANGUAGE NoImplicitPrelude #-}
module Stackage.Database
    ( PostgresConf (..)
    , HasStorage(..)
    , SnapName (..)
    , SnapshotId ()
    , Snapshot(..)
    , StackageDatabase
    , GetStackageDatabase(..)
    , closeStackageDatabase
    , newestSnapshot
    , newestLTS
    , newestLTSMajor
    , newestNightly
    , ltsMajorVersions
    , snapshotBefore
    , lookupSnapshot
    , snapshotTitle
    , PackageListingInfo (..)
    , getAllPackages
    , getPackages
    , getPackageVersionBySnapshot
    , createStackageDatabase
    , openStackageDatabase
    , ModuleListingInfo (..)
    , getSnapshotModules
    , getPackageModules
    , SnapshotPackage (..)
    , lookupSnapshotPackage
    , SnapshotHackagePackage(..)
    , getDeprecated
    , LatestInfo (..)
    , getLatests
    , getDeps
    , getRevDeps
    , getDepsCount
    , Package (..)
    , getPackage
    , prettyName
    , prettyNameShort
    , getSnapshotsForPackage
    , getSnapshots
    , countSnapshots
    , currentSchema
    , last5Lts5Nightly
    , lastXLts5Nightly
    , snapshotsJSON
    , getPackageCount
    , getLatestLtsByGhc
    ) where

import RIO
import RIO.Time
import RIO.Process
import qualified RIO.Text as T
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified RIO.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8
import RIO.FilePath
import RIO.Directory (removeFile, getAppUserDataDirectory, doesDirectoryExist, createDirectoryIfMissing)
import Conduit
import qualified Data.Conduit.List as CL
import Web.PathPieces (toPathPiece)
import qualified Codec.Archive.Tar as Tar
import Database.Esqueleto.Internal.Language (From)
import CMarkGFM
import Stackage.Database.Haddock
import System.FilePath (takeBaseName, takeExtension)
import Text.Blaze.Html (Html, toHtml, preEscapedToHtml)
import Yesod.Form.Fields (Textarea (..))
import Stackage.Database.Types
import Stackage.Types
import Stackage.Metadata
import Stackage.PackageIndex.Conduit
import Web.PathPieces (fromPathPiece)
import Data.Yaml (decodeFileEither, decodeEither', decodeThrow)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Logger (runNoLoggingT)
import qualified Database.Esqueleto as E
import qualified Data.Aeson as A
import Types (SnapshotBranch(..))
import Data.Pool (destroyAllResources)
import Data.List as L (stripPrefix, isPrefixOf)
import Pantry.Types (Storage(Storage), HasStorage(storageG), FileSize(..))
import Pantry.SHA256 (SHA256(..))
import Pantry.Storage hiding (migrateAll)
import qualified Pantry.Storage as Pantry (migrateAll)
import Pantry.Hackage (updateHackageIndex, DidUpdateOccur(..))
import Control.Monad.Trans.Class (lift)

currentSchema :: Int
currentSchema = 2 -- TODO: change to 2

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Schema
    val Int
    deriving Show
Imported
    name SnapName
    type Text
    UniqueImported name type

Snapshot
    name SnapName
    ghc Text
    created Day
    updatedOn UTCTime
    UniqueSnapshot name
Lts
    snap SnapshotId
    major Int
    minor Int
    UniqueLts major minor
Nightly
    snap SnapshotId
    day Day
    UniqueNightly day
Package
    name Text
    latest Text
    synopsis Text
    homepage Text
    author Text
    maintainer Text
    licenseName Text
    description Html
    changelog Html
    UniquePackage name
SnapshotPackage
    snapshot SnapshotId
    package PackageId
    isCore Bool
    version Text
    UniqueSnapshotPackage snapshot package
SnapshotHackagePackage
    snapshot SnapshotId
    cabal HackageCabalId
    isHidden Bool default=False
    pantryTreeSha256 SHA256
    pantryTreeSize FileSize
    UniqueSnapshotHackagePackage snapshot cabal
Module
    package SnapshotPackageId
    name Text
    UniqueModule package name
Dep
    user PackageId
    uses Text -- avoid circular dependency issue when loading database
    range Text
    UniqueDep user uses
Deprecated
    package PackageId
    inFavorOf [PackageId]
    UniqueDeprecated package
|]



instance A.ToJSON Snapshot where
  toJSON Snapshot{..} =
    A.object [ "name"    A..= snapshotName
             , "ghc"     A..= snapshotGhc
             , "created" A..= formatTime defaultTimeLocale "%F" snapshotCreated
             ]

_hideUnusedWarnings
    :: ( SnapshotPackageId
       , SnapshotHackagePackageId
       , SchemaId
       , ImportedId
       , LtsId
       , NightlyId
       , ModuleId
       , DepId
       , DeprecatedId
       ) -> ()
_hideUnusedWarnings _ = ()


closeStackageDatabase :: HasStorage env => RIO env ()
closeStackageDatabase = do
    Storage pool <- view storageG
    liftIO $ destroyAllResources pool

type StackageDatabase = Storage

class (HasStorage env, MonadIO m) => GetStackageDatabase env m | m -> env where
    getStackageDatabase :: m Storage
instance (HasStorage env, MonadIO m) => GetStackageDatabase env (ReaderT env m) where
    getStackageDatabase = view storageG
instance HasStorage env => GetStackageDatabase env (RIO env) where
    getStackageDatabase = view storageG

sourceSnapshots
  :: ConduitT a (SnapshotFile, Day, UTCTime) (ResourceT (RIO StackageCron)) ()
sourceSnapshots = do
    rootDir <- scStackageRoot <$> lift ask
    gitDir <- lift $ cloneOrUpdate rootDir "commercialhaskell" "stackage-snapshots"
    sourceDirectoryDeep False (gitDir </> "lts") .| concatMapMC (parseLts gitDir)
    sourceDirectoryDeep False (gitDir </> "nightly") .| concatMapMC (parseNightly gitDir)
  where
    parseSnapshot gitDir fp snapName = do
        esnap <- liftIO $ decodeFileEither fp
        case esnap of
            Right snap
                | snapName == sfName snap -> do
                    let gitLog args =
                            lift $
                            withWorkingDir gitDir $
                            proc "git" ("log" : (args ++ [fp])) readProcessStdout_
                    lastCommitTimestamps <- gitLog ["-1", "--format=%cD"]
                    authorDates <- gitLog ["--reverse", "--date=short", "--format=%ad"]
                    let parseGitDate fmt dates = do
                            date <- listToMaybe $ LBS8.lines dates
                            parseTimeM False defaultTimeLocale fmt (LBS8.unpack date)
                    let snapAndDates = do
                            createdOn <- parseGitDate "%F" authorDates
                            updatedOn <- parseGitDate rfc822DateFormat lastCommitTimestamps
                            return (snap, createdOn, updatedOn)
                    when (isNothing snapAndDates) $
                        (lift $
                         logError $
                         "Issue parsing the git log timestamps: " <>
                         displayShow (lastCommitTimestamps, authorDates))
                    return snapAndDates
            Right snap -> do
                logError $
                    "Snapshot name mismath. Received: " <> displayShow (sfName snap) <>
                    " in the file: " <>
                    display (T.pack fp)
                return Nothing
            Left exc -> do
                lift $ logError $ display $ T.pack $ displayException exc
                return Nothing
    parseLts gitDir fp = do
        case mapM (BS8.readInt . BS8.pack) $ take 2 $ reverse (splitPath fp) of
            Just [(minor, ".yaml"), (major, "/")] -> parseSnapshot gitDir fp $ SNLts major minor
            _ -> do
                lift $
                    logError
                        ("Couldn't parse the filepath into an LTS version: " <> display (T.pack fp))
                return Nothing
    parseNightly gitDir fp = do
        case mapM (BS8.readInt . BS8.pack) $ take 3 $ reverse (splitPath fp) of
            Just [(day, ".yaml"), (month, "/"), (year, "/")]
                | Just date <- fromGregorianValid (fromIntegral year) month day ->
                    parseSnapshot gitDir fp $ SNNightly date
            _ -> do
                lift $
                    logError
                        ("Couldn't parse the filepath into an Nightly date: " <> display (T.pack fp))
                return Nothing



updateSnapshot :: (HasStorage env, HasLogFunc env) => (SnapshotFile, Day, UTCTime) -> RIO env ()
updateSnapshot (SnapshotFile {..}, createdOn, updatedOn) = do
    let runUpdate = do
            mSnapshotKey <- insertUnique (Snapshot sfName sfCompiler createdOn updatedOn)
            case mSnapshotKey of
                Just snapshotKey -> do
                    case sfName of
                        SNLts major minor -> insert_ $ Lts snapshotKey major minor
                        SNNightly day -> insert_ $ Nightly snapshotKey day
                    msgs <-
                        forMaybeM sfPackages $ \(PantryHackagePackage phc PantryTree {..}) -> do
                            let packageIdentifierRevision =
                                    pantryHackageCabalToPackageIdentifierRevision phc
                            mHackageCabalKey <- getHackageCabalKey packageIdentifierRevision
                            case mHackageCabalKey of
                                Nothing ->
                                    let msg =
                                            "Couldn't find hackage cabal file " <>
                                            "in pantry corresponding to: " <>
                                            display packageIdentifierRevision
                                     in return $ Just (LevelError, msg)
                                Just hackageCabalKey -> do
                                    let PantryHackageCabal packageName _ _ _ = phc
                                        isHidden = fromMaybe False $ Map.lookup packageName sfHidden
                                    insert_ $
                                        SnapshotHackagePackage
                                            snapshotKey
                                            hackageCabalKey
                                            isHidden
                                            ptSHA256
                                            ptFileSize
                                    return Nothing
                    return $
                        if null msgs
                            then [ ( LevelInfo
                                   , "Added snapshot '" <> display sfName <> "' successfully")
                                 ]
                            else [ ( LevelError
                                   , "There were errors while adding snapshot '" <> display sfName <>
                                     "'")
                                 ] ++
                                 msgs
                Nothing ->
                    return
                        [ ( LevelInfo
                          , "Snapshot with name: " <> display sfName <> " already exists.")
                        ]
    msgs <- withStorage runUpdate
    mapM_ (uncurry $ logGeneric "") msgs

-- sourceBuildPlans :: MonadResource m => FilePath -> ConduitT i (SnapName, FilePath, Either (IO BuildPlan) (IO DocMap)) m ()
-- sourceBuildPlans root = do
--     forM_ ["lts-haskell", "stackage-nightly"] $ \repoName -> do
--         dir <- cloneOrUpdate root "fpco" repoName
--         sourceDirectory dir .| concatMapMC (go Left . fromString)
--         let docdir = dir </> "docs"
--         whenM (liftIO $ doesDirectoryExist docdir) $
--             sourceDirectory docdir .| concatMapMC (go Right . fromString)
--   where
--     go wrapper fp | Just name <- nameFromFP fp = liftIO $ do
--         let bp = decodeFileEither fp >>= either throwIO return
--         return $ Just (name, fp, wrapper bp)
--     go _ _ = return Nothing

--     nameFromFP fp = do
--         base <- T.stripSuffix ".yaml" $ T.pack $ takeFileName fp
--         fromPathPiece base

cloneOrUpdate ::
       (MonadReader env m, HasLogFunc env, HasProcessContext env, MonadIO m)
    => FilePath
    -> String
    -> String
    -> m FilePath
cloneOrUpdate root org name = do
    exists <- doesDirectoryExist dest
    if exists
        then withWorkingDir dest $ do
            proc "git" ["fetch"] runProcess_
            proc "git" ["reset", "--hard", "origin/master"] runProcess_
        else withWorkingDir root $ do
            proc "git" ["clone", url, name] runProcess_
    return dest
  where
    url = "https://github.com/" <> org <> "/" <> name <> ".git"
    dest = root </> name


openStackageDatabase :: MonadIO m => PostgresConf -> m Storage
openStackageDatabase pg = liftIO $ do
    fmap Storage $ runNoLoggingT $ createPostgresqlPool
      (pgConnStr pg)
      (pgPoolSize pg)

getSchema :: (HasLogFunc env, HasStorage env) => RIO env (Maybe Int)
getSchema = do
    eres <- tryAny $ withStorage (selectList [] [])
    logInfo $ "getSchema result: " <> displayShow eres
    case eres of
        Right [Entity _ (Schema v)] -> return $ Just v
        _ -> return Nothing

createStackageDatabase :: RIO StackageCron ()
createStackageDatabase = do
    logInfo "Entering createStackageDatabase"
    actualSchema <- getSchema
    let schemaMatch = actualSchema == Just currentSchema
    unless schemaMatch $ do
        logWarn $
            "Current schema does not match actual schema: " <>
            displayShow (actualSchema, currentSchema)
    withStorage $ do
        runMigration Pantry.migrateAll
        runMigration migrateAll
        unless schemaMatch $ insert_ $ Schema currentSchema
    didUpdate <- updateHackageIndex True (Just "Stackage Server cron job")
    case didUpdate of
        UpdateOccurred -> logInfo "Updated hackage index"
        NoUpdateOccurred -> logInfo "No new packages in hackage index"
    logWarn "FIXME: update stackage snapshots repos"
    runConduitRes $
        sourceSnapshots .|
        mapM_C (lift . updateSnapshot)
            -- (\(snap, _createdOn, _updatedOn) ->
            --      lift $
            --      logInfo $
            --      "Parsed: " <> displayShow (sfName snap) <> " with: " <>
            --      displayShow (length (sfPackages snap)) <>
            --      " number of packages")
        -- sourceBuildPlans rootDir .|
        -- mapM_C
        --     (\(sname, fp', eval) ->
        --          flip runSqlPool pool $ do
        --              let (typ, action) =
        --                      case eval of
        --                          Left bp -> ("build-plan", liftIO bp >>= addPlan sname fp')
        --                          Right dm -> ("doc-map", liftIO dm >>= addDocMap sname)
        --              let i = Imported sname typ
        --              eres <- insertBy i
        --              case eres of
        --                  Left _ -> lift $ logInfo $ "Skipping: " <> displayShow fp'
        --                  Right _ -> action)
    withStorage $ mapM_ (flip rawExecute []) ["COMMIT", "VACUUM", "BEGIN"]

getDeprecated' :: [Deprecation] -> Tar.Entry -> [Deprecation]
getDeprecated' orig e =
    case (Tar.entryPath e, Tar.entryContent e) of
        ("deprecated.yaml", Tar.NormalFile lbs _) ->
            case decodeThrow $ LBS.toStrict lbs of
                Just x -> x
                Nothing -> orig
        _ -> orig

addDeprecated :: Deprecation -> SqlPersistT (ResourceT IO) ()
addDeprecated (Deprecation name others) = do
    name' <- getPackageId name
    others' <- mapM getPackageId $ Set.toList others
    insert_ $ Deprecated name' others'

getPackageId :: MonadIO m => Text -> ReaderT SqlBackend m (Key Package)
getPackageId x = do
    keys' <- selectKeysList [PackageName ==. x] [LimitTo 1]
    case keys' of
        k:_ -> return k
        [] -> insert Package
            { packageName = x
            , packageLatest = "unknown"
            , packageSynopsis = "Metadata not found"
            , packageDescription = "Metadata not found"
            , packageChangelog = mempty
            , packageAuthor = ""
            , packageMaintainer = ""
            , packageHomepage = ""
            , packageLicenseName = ""
            }

addPackage :: HasLogFunc env => Tar.Entry -> SqlPersistT (RIO env) ()
addPackage e =
    case ("packages/" `L.isPrefixOf` fp && takeExtension fp == ".yaml", Tar.entryContent e) of
        (True, Tar.NormalFile lbs _) ->
          case decodeEither' $ LBS.toStrict lbs of
            Left err ->
              lift $ logInfo $ "ERROR: Could not parse " <>
                  displayShow fp <> ": " <> displayShow err
            Right pi -> onParse pi
        _ -> return ()
  where
    onParse pi = do
            let p = Package
                    { packageName = T.pack base
                    , packageLatest = dtDisplay $ piLatest pi
                    , packageSynopsis = piSynopsis pi
                    , packageDescription = renderContent (piDescription pi) (piDescriptionType pi)
                    , packageChangelog = renderContent (piChangeLog pi) (piChangeLogType pi)
                    , packageAuthor = piAuthor pi
                    , packageMaintainer = piMaintainer pi
                    , packageHomepage = piHomepage pi
                    , packageLicenseName = piLicenseName pi
                    }

            mp <- getBy $ UniquePackage $ packageName p
            pid <- case mp of
                Just (Entity pid _) -> do
                    replace pid p
                    return pid
                Nothing -> insert p
            deleteWhere [DepUser ==. pid]
            forM_ (Map.toList $ piBasicDeps pi) $ \(uses, range) -> insert_ Dep
                { depUser = pid
                , depUses = dtDisplay uses
                , depRange = dtDisplay range
                }

    fp = Tar.entryPath e
    base = takeBaseName fp

    renderContent txt "markdown" = preEscapedToHtml $ commonmarkToHtml
                                    [optSmart, optSafe]
                                    [extTable, extAutolink]
                                    txt
    renderContent txt "haddock" = renderHaddock txt
    renderContent txt _ = toHtml $ Textarea txt

-- addPlan
--   :: (MonadReader env m, MonadThrow m, MonadIO m, HasLogFunc env) =>
--      SnapName -> FilePath -> BuildPlan -> ReaderT SqlBackend m ()
-- addPlan name fp bp = do
--     lift $ logInfo $ "Adding build plan: " <> display (toPathPiece name)
--     created <-
--         case name of
--             SNNightly d -> return d
--             SNLts _ _ -> do
--                 let cp' = proc "git"
--                         [ "log"
--                         , "-1"
--                         , "--format=%ad"
--                         , "--date=short"
--                         , takeFileName fp
--                         ]
--                     cp = cp' { cwd = Just $ takeDirectory fp }
--                 t <- withCheckedProcess cp $ \ClosedStream out ClosedStream ->
--                     runConduit $ out .| decodeUtf8C .| foldC
--                 case readMaybe $ T.unpack $ T.concat $ take 1 $ T.words t of
--                     Just created -> return created
--                     Nothing -> do
--                         lift $ logInfo $ "Warning: unknown git log output: " <> displayShow t
--                         return $ fromGregorian 1970 1 1
--     sid <- insert Snapshot
--         { snapshotName = name
--         , snapshotGhc = dtDisplay $ siGhcVersion $ bpSystemInfo bp
--         , snapshotCreated = created
--         }
--     forM_ allPackages $ \(dtDisplay -> pname, (dtDisplay -> version, isCore)) -> do
--         pid <- getPackageId pname
--         insert_ SnapshotPackage
--             { snapshotPackageSnapshot = sid
--             , snapshotPackagePackage = pid
--             , snapshotPackageIsCore = isCore
--             , snapshotPackageVersion = version
--             }
--     case name of
--         SNLts x y -> insert_ Lts
--             { ltsSnap = sid
--             , ltsMajor = x
--             , ltsMinor = y
--             }
--         SNNightly d -> insert_ Nightly
--             { nightlySnap = sid
--             , nightlyDay = d
--             }
--   where
--     allPackages =
--         Map.toList
--             $ fmap (, True) (siCorePackages $ bpSystemInfo bp)
--            <> fmap ((, False) . ppVersion) (bpPackages bp)

-- addDocMap
--   :: (MonadReader env m, MonadIO m, HasLogFunc env) =>
--      SnapName -> Map Text PackageDocs -> ReaderT SqlBackend m ()
-- addDocMap name dm = do
--     sids <- selectKeysList [SnapshotName ==. name] []
--     case sids of
--       [] -> lift $ logError $ "Couldn't find a snapshot by the name: " <> displayShow name
--       [sid] -> do
--          lift $ logInfo $ "Adding doc map: " <> display (toPathPiece name)
--          forM_ (Map.toList dm) $ \(pkg, pd) -> do
--              pids <- selectKeysList [PackageName ==. pkg] []
--              pid <-
--                case pids of
--                  [pid] -> return pid
--                  _ -> error $ "addDocMap (1): " <> show (name, pkg, pids)
--              spids <- selectKeysList [SnapshotPackageSnapshot ==. sid, SnapshotPackagePackage ==. pid] []
--              case spids of
--                [spid] ->
--                  forM_ (Map.toList $ pdModules pd) $ \(mname, _paths) ->
--                      insert_ Module
--                          { modulePackage = spid
--                          , moduleName = mname
--                          }
--                -- FIXME figure out why this happens for the ghc package with GHC 8.2.1
--                _ -> lift $ logError $ "addDocMap (2): " <> displayShow (name, pkg, pid, spids)
--       xs -> lift $ logError $ "Impossible happened: unique key constraint failure: " <> displayShow xs


run :: GetStackageDatabase env m => SqlPersistT IO a -> m a
run inner = do
    Storage pool <- getStackageDatabase
    liftIO $ runSqlPool inner pool


newestSnapshot :: GetStackageDatabase env m => SnapshotBranch -> m (Maybe SnapName)
newestSnapshot LtsBranch = fmap (uncurry SNLts) <$> newestLTS
newestSnapshot NightlyBranch = fmap SNNightly <$> newestNightly
newestSnapshot (LtsMajorBranch x) = fmap (SNLts x) <$> newestLTSMajor x

newestLTS :: GetStackageDatabase env m => m (Maybe (Int, Int))
newestLTS =
    run $ liftM (fmap go) $ selectFirst [] [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

newestLTSMajor :: GetStackageDatabase env m => Int -> m (Maybe Int)
newestLTSMajor x =
    run $ liftM (fmap $ ltsMinor . entityVal) $ selectFirst [LtsMajor ==. x] [Desc LtsMinor]

ltsMajorVersions :: GetStackageDatabase env m => m [(Int, Int)]
ltsMajorVersions =
    run $ liftM (dropOldMinors . map (toPair . entityVal))
        $ selectList [] [Desc LtsMajor, Desc LtsMinor]
  where
    toPair (Lts _ x y) = (x, y)

    dropOldMinors [] = []
    dropOldMinors (l@(x, _):rest) =
        l : dropOldMinors (dropWhile sameMinor rest)
      where
        sameMinor (y, _) = x == y

newestNightly :: GetStackageDatabase env m => m (Maybe Day)
newestNightly =
    run $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [Desc NightlyDay]

-- | Get the snapshot which precedes the given one with respect to it's branch (nightly/lts)
snapshotBefore :: GetStackageDatabase env m => SnapName -> m (Maybe (SnapshotId, SnapName))
snapshotBefore (SNLts x y)     = ltsBefore x y
snapshotBefore (SNNightly day) = nightlyBefore day

nightlyBefore :: GetStackageDatabase env m => Day -> m (Maybe (SnapshotId, SnapName))
nightlyBefore day = do
    run $ liftM (fmap go) $ selectFirst [NightlyDay <. day] [Desc NightlyDay]
  where
    go (Entity _ nightly) = (nightlySnap nightly, SNNightly $ nightlyDay nightly)

ltsBefore :: GetStackageDatabase env m => Int -> Int -> m (Maybe (SnapshotId, SnapName))
ltsBefore x y = do
    run $ liftM (fmap go) $ selectFirst
        ( [LtsMajor <=. x, LtsMinor <. y] ||.
          [LtsMajor <. x]
        )
        [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsSnap lts, SNLts (ltsMajor lts) (ltsMinor lts))

lookupSnapshot :: GetStackageDatabase env m => SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot name = run $ getBy $ UniqueSnapshot name

snapshotTitle :: Snapshot -> Text
snapshotTitle s = prettyName (snapshotName s) (snapshotGhc s)

prettyName :: SnapName -> Text -> Text
prettyName name ghc = T.concat [prettyNameShort name, " (ghc-", ghc, ")"]

prettyNameShort :: SnapName -> Text
prettyNameShort name =
    case name of
        SNLts x y -> T.concat ["LTS Haskell ", T.pack (show x), ".", T.pack (show y)]
        SNNightly d -> "Stackage Nightly " <> T.pack (show d)

getAllPackages :: GetStackageDatabase env m => m [(Text, Text, Text)] -- FIXME add information on whether included in LTS and Nightly
getAllPackages = liftM (map toPair) $ run $ do
    E.select $ E.from $ \p -> do
        E.orderBy [E.asc $ E.lower_ $ p E.^. PackageName]
        return
            ( p E.^. PackageName
            , p E.^. PackageLatest
            , p E.^. PackageSynopsis
            )
  where
    toPair (E.Value x, E.Value y, E.Value z) = (x, y, z)

getPackages :: GetStackageDatabase env m => SnapshotId -> m [PackageListingInfo]
getPackages sid = liftM (map toPLI) $ run $ do
    E.select $ E.from $ \(p,sp) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid)
        E.orderBy [E.asc $ E.lower_ $ p E.^. PackageName]
        return
            ( p E.^. PackageName
            , p E.^. PackageSynopsis
            , sp E.^. SnapshotPackageVersion
            , sp E.^. SnapshotPackageIsCore
            )
  where
    toPLI (E.Value name, E.Value synopsis, E.Value version, E.Value isCore) = PackageListingInfo
        { pliName = name
        , pliVersion = version
        , pliSynopsis = synopsis
        , pliIsCore = isCore
        }

getPackageVersionBySnapshot
  :: GetStackageDatabase env m
  => SnapshotId -> Text -> m (Maybe Text)
getPackageVersionBySnapshot sid name = liftM (listToMaybe . map toPLI) $ run $ do
    E.select $ E.from $ \(p,sp) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid) E.&&.
            (E.lower_ (p E.^. PackageName) E.==. E.lower_ (E.val name))
        E.orderBy [E.asc $ E.lower_ $ p E.^. PackageName]
        return
            ( sp E.^. SnapshotPackageVersion
            )
  where
    toPLI (E.Value version) = version


getSnapshotModules
    :: GetStackageDatabase env m
    => SnapshotId
    -> m [ModuleListingInfo]
getSnapshotModules sid = liftM (map toMLI) $ run $ do
    E.select $ E.from $ \(p,sp,m) -> do
        E.where_ $
            (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage) E.&&.
            (sp E.^. SnapshotPackageSnapshot E.==. E.val sid) E.&&.
            (m E.^. ModulePackage E.==. sp E.^. SnapshotPackageId)
        E.orderBy
            [ E.asc $ m E.^. ModuleName
            , E.asc $ E.lower_ $ p E.^. PackageName
            ]
        return
            ( m E.^. ModuleName
            , p E.^. PackageName
            , sp E.^. SnapshotPackageVersion
            )
  where
    toMLI (E.Value name, E.Value pkg, E.Value version) = ModuleListingInfo
        { mliName = name
        , mliPackageVersion = T.concat [pkg, "-", version]
        }

getPackageModules
    :: GetStackageDatabase env m
    => SnapName
    -> Text
    -> m [Text]
getPackageModules sname pname = run $ do
    sids <- selectKeysList [SnapshotName ==. sname] []
    pids <- selectKeysList [PackageName ==. pname] []
    case (,) <$> listToMaybe sids <*> listToMaybe pids of
        Nothing -> return []
        Just (sid, pid) -> do
            spids <- selectKeysList
                [ SnapshotPackageSnapshot ==. sid
                , SnapshotPackagePackage ==. pid
                ] []
            case spids of
                spid:_ -> map (moduleName . entityVal)
                      <$> selectList [ModulePackage ==. spid] [Asc ModuleName]
                [] -> return []

lookupSnapshotPackage
    :: GetStackageDatabase env m
    => SnapshotId
    -> Text
    -> m (Maybe (Entity SnapshotPackage))
lookupSnapshotPackage sid pname = run $ do
    mp <- getBy $ UniquePackage pname
    case mp of
        Nothing -> return Nothing
        Just (Entity pid _) -> getBy $ UniqueSnapshotPackage sid pid

getDeprecated :: GetStackageDatabase env m => Text -> m (Bool, [Text])
getDeprecated name = run $ do
    pids <- selectKeysList [PackageName ==. name] []
    case pids of
        [pid] -> do
            mdep <- getBy $ UniqueDeprecated pid
            case mdep of
                Nothing -> return defRes
                Just (Entity _ (Deprecated _ favors)) -> do
                    names <- mapM getName favors
                    return (True, catMaybes names)
        _ -> return defRes
  where
    defRes = (False, [])

    getName = fmap (fmap packageName) . get


getLatests :: GetStackageDatabase env m
           => Text -- ^ package name
           -> m [LatestInfo]
getLatests pname = run $ fmap (nubOrd . concat) $ forM [True, False] $ \requireDocs -> do
    mlts <- latestHelper pname requireDocs
        (\s ln -> s E.^. SnapshotId E.==. ln E.^. LtsSnap)
        (\_ ln ->
            [ E.desc $ ln E.^. LtsMajor
            , E.desc $ ln E.^. LtsMinor
            ])
    mnightly <- latestHelper pname requireDocs
        (\s ln -> s E.^. SnapshotId E.==. ln E.^. NightlySnap)
        (\s _ln -> [E.desc $ s E.^. SnapshotCreated])
    return $ concat [mlts, mnightly]

latestHelper
    :: (From E.SqlQuery E.SqlExpr SqlBackend t, MonadIO m)
    => Text -- ^ package name
    -> Bool -- ^ require docs?
    -> (E.SqlExpr (Entity Snapshot) -> t -> E.SqlExpr (E.Value Bool))
    -> (E.SqlExpr (Entity Snapshot) -> t -> [E.SqlExpr E.OrderBy])
    -> ReaderT SqlBackend m [LatestInfo]
latestHelper pname requireDocs clause order = do
  results <- E.select $ E.from $ \(s,ln,p,sp) -> do
    E.where_ $
        clause s ln E.&&.
        (s E.^. SnapshotId E.==. sp E.^. SnapshotPackageSnapshot) E.&&.
        (p E.^. PackageName E.==. E.val pname) E.&&.
        (p E.^. PackageId E.==. sp E.^. SnapshotPackagePackage)
    E.orderBy $ order s ln
    E.limit 1
    return
        ( s E.^. SnapshotName
        , s E.^. SnapshotGhc
        , sp E.^. SnapshotPackageVersion
        , sp E.^. SnapshotPackageId
        )
  if requireDocs
    then
      case results of
        tuple@(_, _, _, E.Value spid):_ -> do
          x <- count [ModulePackage ==. spid]
          return $ if x > 0 then [toLatest tuple] else []
        [] -> return []
    else return $ map toLatest results
  where
    toLatest (E.Value sname, E.Value ghc, E.Value version, _) = LatestInfo
        { liSnapName = sname
        , liVersion = version
        , liGhc = ghc
        }

getDeps :: GetStackageDatabase env m => Text -> Maybe Int -> m [(Text, Text)]
getDeps pname mcount = run $ do
    mp <- getBy $ UniquePackage pname
    case mp of
        Nothing -> return []
        Just (Entity pid _) -> fmap (map toPair) $ E.select $ E.from $ \d -> do
            E.where_ $
                (d E.^. DepUser E.==. E.val pid)
            E.orderBy [E.asc $ d E.^. DepUses]
            forM_ mcount $ E.limit . fromIntegral
            return (d E.^. DepUses, d E.^. DepRange)
  where
    toPair (E.Value x, E.Value y) = (x, y)

getRevDeps :: GetStackageDatabase env m => Text -> Maybe Int -> m [(Text, Text)]
getRevDeps pname mcount = run $ do
    fmap (map toPair) $ E.select $ E.from $ \(d,p) -> do
        E.where_ $
            (d E.^. DepUses E.==. E.val pname) E.&&.
            (d E.^. DepUser E.==. p E.^. PackageId)
        E.orderBy [E.asc $ p E.^. PackageName]
        forM_ mcount $ E.limit . fromIntegral
        return (p E.^. PackageName, d E.^. DepRange)
  where
    toPair (E.Value x, E.Value y) = (x, y)

getDepsCount :: GetStackageDatabase env m => Text -> m (Int, Int)
getDepsCount pname = run $ (,)
  <$> (do
          mp <- getBy $ UniquePackage pname
          case mp of
            Nothing -> return 0
            Just (Entity pid _) -> count [DepUser ==. pid]
      )
  <*> count [DepUses ==. pname]

getPackage :: GetStackageDatabase env m => Text -> m (Maybe (Entity Package))
getPackage = run . getBy . UniquePackage

getSnapshotsForPackage
    :: GetStackageDatabase env m
    => Text
    -> m [(Snapshot, Text)] -- version
getSnapshotsForPackage pname = run $ do
    pid <- getPackageId pname
    fmap (map go) $ E.select $ E.from $ \(s, sp) -> do
      E.where_ $ s E.^. SnapshotId E.==. sp E.^. SnapshotPackageSnapshot
          E.&&. sp E.^. SnapshotPackagePackage E.==. E.val pid
      E.orderBy [E.desc $ s E.^. SnapshotCreated]
      return (s, sp E.^. SnapshotPackageVersion)
  where
    go (Entity _ snapshot, E.Value version) = (snapshot, version)

-- | Count snapshots that belong to a specific SnapshotBranch
countSnapshots :: (GetStackageDatabase env m) => Maybe SnapshotBranch -> m Int
countSnapshots Nothing                   = run $ count ([] :: [Filter Snapshot])
countSnapshots (Just NightlyBranch)      = run $ count ([] :: [Filter Nightly])
countSnapshots (Just LtsBranch)          = run $ count ([] :: [Filter Lts])
countSnapshots (Just (LtsMajorBranch x)) = run $ count [LtsMajor ==. x]

-- | Get snapshots that belong to a specific SnapshotBranch
getSnapshots :: (GetStackageDatabase env m)
             => Maybe SnapshotBranch
             -> Int -- ^ limit
             -> Int -- ^ offset
             -> m [Entity Snapshot]
getSnapshots mBranch l o = run $ case mBranch of
    Nothing -> selectList [] [LimitTo l, OffsetBy o, Desc SnapshotCreated]
    Just NightlyBranch ->
        E.select $ E.from $ \(nightly `E.InnerJoin` snapshot) -> do
            E.on $ nightly E.^. NightlySnap E.==. snapshot E.^. SnapshotId
            E.orderBy [E.desc (nightly E.^. NightlyDay)]
            E.limit $ fromIntegral l
            E.offset $ fromIntegral o
            pure snapshot
    Just LtsBranch -> do
        E.select $ E.from $ \(lts `E.InnerJoin` snapshot) -> do
            E.on $ lts E.^. LtsSnap E.==. snapshot E.^. SnapshotId
            E.orderBy [ E.desc (lts E.^. LtsMajor)
                      , E.desc (lts E.^. LtsMinor) ]
            E.limit $ fromIntegral l
            E.offset $ fromIntegral o
            pure snapshot
    Just (LtsMajorBranch v) -> do
        E.select $ E.from $ \(lts `E.InnerJoin` snapshot) -> do
            E.on $ lts E.^. LtsSnap E.==. snapshot E.^. SnapshotId
            E.orderBy [E.desc (lts E.^. LtsMinor)]
            E.where_ ((lts E.^. LtsMajor) E.==. (E.val v))
            E.limit $ fromIntegral l
            E.offset $ fromIntegral o
            pure snapshot

last5Lts5Nightly :: GetStackageDatabase env m => m [SnapName]
last5Lts5Nightly = lastXLts5Nightly 5

lastXLts5Nightly :: GetStackageDatabase env m => Int -> m [SnapName]
lastXLts5Nightly ltsCount = run $ do
    ls <- selectList [] [Desc LtsMajor, Desc LtsMinor, LimitTo ltsCount]
    ns <- selectList [] [Desc NightlyDay, LimitTo 5]
    return $ map l ls <> map n ns
  where
    l (Entity _ x) = SNLts (ltsMajor x) (ltsMinor x)
    n (Entity _ x) = SNNightly (nightlyDay x)

snapshotsJSON :: GetStackageDatabase env m => m A.Value
snapshotsJSON = do
    mlatestNightly <- newestNightly
    ltses <- ltsMajorVersions
    let lts = case ltses of
            [] -> []
            majorVersions@(latest:_) ->
                   ("lts" A..= printLts latest)
                 : map toObj majorVersions
        nightly = case mlatestNightly of
            Nothing -> id
            Just n -> (("nightly" A..= printNightly n):)
    return $ A.object $ nightly lts
  where
    toObj lts@(major, _) =
        T.pack ("lts-" <> show major) A..= printLts lts
    printLts (major, minor) =
        "lts-" <> show major <> "." <> show minor

    printNightly day = "nightly-" <> T.pack (show day)

getPackageCount :: GetStackageDatabase env m
                => SnapshotId
                -> m Int
getPackageCount sid = run $ count [SnapshotPackageSnapshot ==. sid]

getLatestLtsByGhc :: GetStackageDatabase env m
                  => m [(Int, Int, Text, Day)]
getLatestLtsByGhc = run $ fmap (dedupe . map toTuple) $ do
    E.select $ E.from $ \(lts `E.InnerJoin` snapshot) -> do
        E.on $ lts E.^. LtsSnap E.==. snapshot E.^. SnapshotId
        E.orderBy [E.desc (lts E.^. LtsMajor), E.desc (lts E.^. LtsMinor)]
        E.groupBy (snapshot E.^. SnapshotGhc, lts E.^. LtsId, lts E.^. LtsMajor, lts E.^. LtsMinor, snapshot E.^. SnapshotId)
        return (lts, snapshot)
  where
    toTuple (Entity _ lts, Entity _ snapshot) =
        (ltsMajor lts, ltsMinor lts, snapshotGhc snapshot, snapshotCreated snapshot)

    dedupe [] = []
    dedupe (x:xs) = x : dedupe (dropWhile (\y -> thd x == thd y) xs)

    thd (_, _, x, _) = x
