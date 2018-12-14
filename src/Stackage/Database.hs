module Stackage.Database
    ( StackageDatabase
    , PostgresConf (..)
    , GetStackageDatabase (..)
    , SnapName (..)
    , SnapshotId ()
    , Snapshot (..)
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

import Web.PathPieces (toPathPiece)
import qualified Codec.Archive.Tar as Tar
import Database.Esqueleto.Internal.Language (From)
import CMarkGFM
import System.Directory (removeFile)
import Stackage.Database.Haddock
import System.FilePath (takeBaseName, takeExtension)
import ClassyPrelude.Conduit hiding (pi)
import Text.Blaze.Html (Html, toHtml, preEscapedToHtml)
import Yesod.Form.Fields (Textarea (..))
import Stackage.Database.Types
import System.Directory (getAppUserDataDirectory, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath (takeFileName, takeDirectory)
import Data.Conduit.Process
import Stackage.Types
import Stackage.Metadata
import Stackage.PackageIndex.Conduit
import Web.PathPieces (fromPathPiece)
import Data.Yaml (decodeFileEither, decodeEither', decodeThrow)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Logger
import System.IO.Temp
import qualified Database.Esqueleto as E
import qualified Data.Aeson as A
import Types (SnapshotBranch(..))
import Data.Pool (destroyAllResources)
import Data.List (nub)
import Pantry.Storage

currentSchema :: Int
currentSchema = 1

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
    package HackageTarballId
    UniqueSnapshotHackagePackage snapshot package
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

newtype StackageDatabase = StackageDatabase ConnectionPool

closeStackageDatabase :: StackageDatabase -> IO ()
closeStackageDatabase (StackageDatabase pool) = destroyAllResources pool

class MonadIO m => GetStackageDatabase m where
    getStackageDatabase :: m StackageDatabase
instance MonadIO m => GetStackageDatabase (ReaderT StackageDatabase m) where
    getStackageDatabase = ask

sourcePackages :: MonadResource m => FilePath -> ConduitT i Tar.Entry m ()
sourcePackages root = do
    dir <- liftIO $ cloneOrUpdate root "commercialhaskell" "all-cabal-metadata"
    bracketP
        (do
            (fp, h) <- openBinaryTempFile "/tmp" "all-cabal-metadata.tar"
            hClose h
            return fp)
        removeFile
        $ \fp -> do
            liftIO $ runIn dir "git" ["archive", "--output", fp, "--format", "tar", "master"]
            sourceTarFile False fp

sourceBuildPlans :: MonadResource m => FilePath -> ConduitT i (SnapName, FilePath, Either (IO BuildPlan) (IO DocMap)) m ()
sourceBuildPlans root = do
    forM_ ["lts-haskell", "stackage-nightly"] $ \repoName -> do
        dir <- liftIO $ cloneOrUpdate root "fpco" repoName
        sourceDirectory dir .| concatMapMC (go Left . fromString)
        let docdir = dir </> "docs"
        whenM (liftIO $ doesDirectoryExist docdir) $
            sourceDirectory docdir .| concatMapMC (go Right . fromString)
  where
    go wrapper fp | Just name <- nameFromFP fp = liftIO $ do
        let bp = decodeFileEither fp >>= either throwIO return
        return $ Just (name, fp, wrapper bp)
    go _ _ = return Nothing

    nameFromFP fp = do
        base <- stripSuffix ".yaml" $ pack $ takeFileName fp
        fromPathPiece base

cloneOrUpdate :: FilePath -> String -> String -> IO FilePath
cloneOrUpdate root org name = do
    exists <- doesDirectoryExist dest
    if exists
        then do
            let git = runIn dest "git"
            git ["fetch"]
            git ["reset", "--hard", "origin/master"]
        else runIn root "git" ["clone", url, name]
    return dest
  where
    url = "https://github.com/" ++ org ++ "/" ++ name ++ ".git"
    dest = root </> fromString name

runIn :: FilePath -> String -> [String] -> IO ()
runIn dir cmd args =
    withCheckedProcess cp $ \ClosedStream Inherited Inherited -> return ()
  where
    cp = (proc cmd args) { cwd = Just dir }

openStackageDatabase :: MonadIO m => PostgresConf -> m StackageDatabase
openStackageDatabase pg = liftIO $ do
    fmap StackageDatabase $ runNoLoggingT $ createPostgresqlPool
      (pgConnStr pg)
      (pgPoolSize pg)

getSchema :: PostgresConf -> IO (Maybe Int)
getSchema fp = do
    StackageDatabase pool <- openStackageDatabase fp
    eres <- tryAny $ runSqlPool (selectList [] []) pool
    putStrLn $ "getSchema result: " ++ tshow eres
    case eres of
        Right [Entity _ (Schema v)] -> return $ Just v
        _ -> return Nothing

createStackageDatabase :: MonadIO m => PostgresConf -> m ()
createStackageDatabase fp = liftIO $ do
    putStrLn "Entering createStackageDatabase"
    actualSchema <- getSchema fp
    let schemaMatch = actualSchema == Just currentSchema
    unless schemaMatch $ do
        putStrLn $ "Current schema does not match actual schema: " ++ tshow (actualSchema, currentSchema)

    StackageDatabase pool <- openStackageDatabase fp
    flip runSqlPool pool $ do
        runMigration migrateAll
        unless schemaMatch $ insert_ $ Schema currentSchema

    root <- liftIO $ (</> "database") <$> getAppUserDataDirectory "stackage"
    createDirectoryIfMissing True root
    runResourceT $ do
        putStrLn "Updating all-cabal-metadata repo"
        flip runSqlPool pool $ runConduit $ sourcePackages root .| getZipSink
            ( ZipSink (mapM_C addPackage)
           *> ZipSink (do
                deprs <- foldlC getDeprecated' []
                lift $ do
                    deleteWhere ([] :: [Filter Deprecated])
                    mapM_ addDeprecated deprs)
           *> ZipSink (
                let loop i =
                        await >>= maybe (return ()) (const $ go $ i + 1)
                    go i = do
                        when (i `mod` 500 == 0)
                            $ putStrLn $ concat
                                [ "Processed "
                                , tshow i
                                , " packages"
                                ]
                        loop i
                 in loop (0 :: Int))
            )
        runConduit $ sourceBuildPlans root .| mapM_C (\(sname, fp', eval) -> flip runSqlPool pool $ do
            let (typ, action) =
                    case eval of
                        Left bp -> ("build-plan", liftIO bp >>= addPlan sname fp')
                        Right dm -> ("doc-map", liftIO dm >>= addDocMap sname)
            let i = Imported sname typ
            eres <- insertBy i
            case eres of
                Left _ -> putStrLn $ "Skipping: " ++ tshow fp'
                Right _ -> action
            )
        flip runSqlPool pool $ mapM_ (flip rawExecute []) ["COMMIT", "VACUUM", "BEGIN"]

getDeprecated' :: [Deprecation] -> Tar.Entry -> [Deprecation]
getDeprecated' orig e =
    case (Tar.entryPath e, Tar.entryContent e) of
        ("deprecated.yaml", Tar.NormalFile lbs _) ->
            case decodeThrow $ toStrict lbs of
                Just x -> x
                Nothing -> orig
        _ -> orig

addDeprecated :: Deprecation -> SqlPersistT (ResourceT IO) ()
addDeprecated (Deprecation name others) = do
    name' <- getPackageId name
    others' <- mapM getPackageId $ setToList others
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

addPackage :: Tar.Entry -> SqlPersistT (ResourceT IO) ()
addPackage e =
    case ("packages/" `isPrefixOf` fp && takeExtension fp == ".yaml", Tar.entryContent e) of
        (True, Tar.NormalFile lbs _) ->
          case decodeEither' $ toStrict lbs of
            Left err -> putStrLn $ "ERROR: Could not parse " ++ tshow fp ++ ": " ++ tshow err
            Right pi -> onParse pi
        _ -> return ()
  where
    onParse pi = do
            let p = Package
                    { packageName = pack base
                    , packageLatest = display $ piLatest pi
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
            forM_ (mapToList $ piBasicDeps pi) $ \(uses, range) -> insert_ Dep
                { depUser = pid
                , depUses = display uses
                , depRange = display range
                }

    fp = Tar.entryPath e
    base = takeBaseName fp

    renderContent txt "markdown" = preEscapedToHtml $ commonmarkToHtml
                                    [optSmart, optSafe]
                                    [extTable, extAutolink]
                                    txt
    renderContent txt "haddock" = renderHaddock txt
    renderContent txt _ = toHtml $ Textarea txt

addPlan :: SnapName -> FilePath -> BuildPlan -> SqlPersistT (ResourceT IO) ()
addPlan name fp bp = do
    putStrLn $ "Adding build plan: " ++ toPathPiece name
    created <-
        case name of
            SNNightly d -> return d
            SNLts _ _ -> do
                let cp' = proc "git"
                        [ "log"
                        , "--format=%ad"
                        , "--date=short"
                        , takeFileName fp
                        ]
                    cp = cp' { cwd = Just $ takeDirectory fp }
                t <- withCheckedProcess cp $ \ClosedStream out ClosedStream ->
                    runConduit $ out .| decodeUtf8C .| foldC
                case readMay $ concat $ take 1 $ words t of
                    Just created -> return created
                    Nothing -> do
                        putStrLn $ "Warning: unknown git log output: " ++ tshow t
                        return $ fromGregorian 1970 1 1
    sid <- insert Snapshot
        { snapshotName = name
        , snapshotGhc = display $ siGhcVersion $ bpSystemInfo bp
        , snapshotCreated = created
        }
    forM_ allPackages $ \(display -> pname, (display -> version, isCore)) -> do
        pid <- getPackageId pname
        insert_ SnapshotPackage
            { snapshotPackageSnapshot = sid
            , snapshotPackagePackage = pid
            , snapshotPackageIsCore = isCore
            , snapshotPackageVersion = version
            }
    case name of
        SNLts x y -> insert_ Lts
            { ltsSnap = sid
            , ltsMajor = x
            , ltsMinor = y
            }
        SNNightly d -> insert_ Nightly
            { nightlySnap = sid
            , nightlyDay = d
            }
  where
    allPackages = mapToList
        $ fmap (, True) (siCorePackages $ bpSystemInfo bp)
       ++ fmap ((, False) . ppVersion) (bpPackages bp)

addDocMap :: SnapName -> DocMap -> SqlPersistT (ResourceT IO) ()
addDocMap name dm = do
    [sid] <- selectKeysList [SnapshotName ==. name] []
    putStrLn $ "Adding doc map: " ++ toPathPiece name
    forM_ (mapToList dm) $ \(pkg, pd) -> do
        pids <- selectKeysList [PackageName ==. pkg] []
        pid <-
          case pids of
            [pid] -> return pid
            _ -> error $ "addDocMap (1): " ++ show (name, pkg, pids)
        spids <- selectKeysList [SnapshotPackageSnapshot ==. sid, SnapshotPackagePackage ==. pid] []
        case spids of
          [spid] ->
            forM_ (mapToList $ pdModules pd) $ \(mname, _paths) ->
                insert_ Module
                    { modulePackage = spid
                    , moduleName = mname
                    }
          -- FIXME figure out why this happens for the ghc package with GHC 8.2.1
          _ -> sayErrString $ "addDocMap (2): " ++ show (name, pkg, pid, spids)

run :: GetStackageDatabase m => SqlPersistT IO a -> m a
run inner = do
    StackageDatabase pool <- getStackageDatabase
    liftIO $ runSqlPool inner pool

newestSnapshot :: GetStackageDatabase m => SnapshotBranch -> m (Maybe SnapName)
newestSnapshot LtsBranch = map (uncurry SNLts) <$> newestLTS
newestSnapshot NightlyBranch = map SNNightly <$> newestNightly
newestSnapshot (LtsMajorBranch x) = map (SNLts x) <$> newestLTSMajor x

newestLTS :: GetStackageDatabase m => m (Maybe (Int, Int))
newestLTS =
    run $ liftM (fmap go) $ selectFirst [] [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

newestLTSMajor :: GetStackageDatabase m => Int -> m (Maybe Int)
newestLTSMajor x =
    run $ liftM (fmap $ ltsMinor . entityVal) $ selectFirst [LtsMajor ==. x] [Desc LtsMinor]

ltsMajorVersions :: GetStackageDatabase m => m [(Int, Int)]
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

newestNightly :: GetStackageDatabase m => m (Maybe Day)
newestNightly =
    run $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [Desc NightlyDay]

-- | Get the snapshot which precedes the given one with respect to it's branch (nightly/lts)
snapshotBefore :: GetStackageDatabase m => SnapName -> m (Maybe (SnapshotId, SnapName))
snapshotBefore (SNLts x y)     = ltsBefore x y
snapshotBefore (SNNightly day) = nightlyBefore day

nightlyBefore :: GetStackageDatabase m => Day -> m (Maybe (SnapshotId, SnapName))
nightlyBefore day = do
    run $ liftM (fmap go) $ selectFirst [NightlyDay <. day] [Desc NightlyDay]
  where
    go (Entity _ nightly) = (nightlySnap nightly, SNNightly $ nightlyDay nightly)

ltsBefore :: GetStackageDatabase m => Int -> Int -> m (Maybe (SnapshotId, SnapName))
ltsBefore x y = do
    run $ liftM (fmap go) $ selectFirst
        ( [LtsMajor <=. x, LtsMinor <. y] ||.
          [LtsMajor <. x]
        )
        [Desc LtsMajor, Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsSnap lts, SNLts (ltsMajor lts) (ltsMinor lts))

lookupSnapshot :: GetStackageDatabase m => SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot name = run $ getBy $ UniqueSnapshot name

snapshotTitle :: Snapshot -> Text
snapshotTitle s = prettyName (snapshotName s) (snapshotGhc s)

prettyName :: SnapName -> Text -> Text
prettyName name ghc = concat [prettyNameShort name, " (ghc-", ghc, ")"]

prettyNameShort :: SnapName -> Text
prettyNameShort name =
    case name of
        SNLts x y -> concat ["LTS Haskell ", tshow x, ".", tshow y]
        SNNightly d -> "Stackage Nightly " ++ tshow d

getAllPackages :: GetStackageDatabase m => m [(Text, Text, Text)] -- FIXME add information on whether included in LTS and Nightly
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

data PackageListingInfo = PackageListingInfo
    { pliName :: !Text
    , pliVersion :: !Text
    , pliSynopsis :: !Text
    , pliIsCore :: !Bool
    }

instance A.ToJSON PackageListingInfo where
   toJSON PackageListingInfo{..} =
       A.object [ "name"     A..= pliName
                , "version"  A..= pliVersion
                , "synopsis" A..= pliSynopsis
                , "isCore"   A..= pliIsCore
                ]

getPackages :: GetStackageDatabase m => SnapshotId -> m [PackageListingInfo]
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
  :: GetStackageDatabase m
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

data ModuleListingInfo = ModuleListingInfo
    { mliName :: !Text
    , mliPackageVersion :: !Text
    }

getSnapshotModules
    :: GetStackageDatabase m
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
        , mliPackageVersion = concat [pkg, "-", version]
        }

getPackageModules
    :: GetStackageDatabase m
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
    :: GetStackageDatabase m
    => SnapshotId
    -> Text
    -> m (Maybe (Entity SnapshotPackage))
lookupSnapshotPackage sid pname = run $ do
    mp <- getBy $ UniquePackage pname
    case mp of
        Nothing -> return Nothing
        Just (Entity pid _) -> getBy $ UniqueSnapshotPackage sid pid

getDeprecated :: GetStackageDatabase m => Text -> m (Bool, [Text])
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

data LatestInfo = LatestInfo
    { liSnapName :: !SnapName
    , liVersion :: !Text
    , liGhc :: !Text
    }
    deriving (Show, Eq)

getLatests :: GetStackageDatabase m
           => Text -- ^ package name
           -> m [LatestInfo]
getLatests pname = run $ fmap (nub . concat) $ forM [True, False] $ \requireDocs -> do
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
    :: (From E.SqlQuery E.SqlExpr SqlBackend t, MonadIO m, Functor m)
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

getDeps :: GetStackageDatabase m => Text -> Maybe Int -> m [(Text, Text)]
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

getRevDeps :: GetStackageDatabase m => Text -> Maybe Int -> m [(Text, Text)]
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

getDepsCount :: GetStackageDatabase m => Text -> m (Int, Int)
getDepsCount pname = run $ (,)
  <$> (do
          mp <- getBy $ UniquePackage pname
          case mp of
            Nothing -> return 0
            Just (Entity pid _) -> count [DepUser ==. pid]
      )
  <*> count [DepUses ==. pname]

getPackage :: GetStackageDatabase m => Text -> m (Maybe (Entity Package))
getPackage = run . getBy . UniquePackage

getSnapshotsForPackage
    :: GetStackageDatabase m
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
countSnapshots :: (GetStackageDatabase m) => Maybe SnapshotBranch -> m Int
countSnapshots Nothing                   = run $ count ([] :: [Filter Snapshot])
countSnapshots (Just NightlyBranch)      = run $ count ([] :: [Filter Nightly])
countSnapshots (Just LtsBranch)          = run $ count ([] :: [Filter Lts])
countSnapshots (Just (LtsMajorBranch x)) = run $ count [LtsMajor ==. x]

-- | Get snapshots that belong to a specific SnapshotBranch
getSnapshots :: (GetStackageDatabase m)
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

last5Lts5Nightly :: GetStackageDatabase m => m [SnapName]
last5Lts5Nightly = lastXLts5Nightly 5

lastXLts5Nightly :: GetStackageDatabase m => Int -> m [SnapName]
lastXLts5Nightly ltsCount = run $ do
    ls <- selectList [] [Desc LtsMajor, Desc LtsMinor, LimitTo ltsCount]
    ns <- selectList [] [Desc NightlyDay, LimitTo 5]
    return $ map l ls ++ map n ns
  where
    l (Entity _ x) = SNLts (ltsMajor x) (ltsMinor x)
    n (Entity _ x) = SNNightly (nightlyDay x)

snapshotsJSON :: GetStackageDatabase m => m A.Value
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
        pack ("lts-" ++ show major) A..= printLts lts
    printLts (major, minor) =
        "lts-" ++ show major ++ "." ++ show minor

    printNightly day = "nightly-" ++ tshow day

getPackageCount :: GetStackageDatabase m
                => SnapshotId
                -> m Int
getPackageCount sid = run $ count [SnapshotPackageSnapshot ==. sid]

getLatestLtsByGhc :: GetStackageDatabase m
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
