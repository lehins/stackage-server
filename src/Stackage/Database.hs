{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
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
    , snapshotPrettyName
    , snapshotPrettyNameShort
    , getAllPackages
    , getPackages
    , getPackageVersionBySnapshot
    , createStackageDatabase
    , openStackageDatabase
    , getSnapshotModules
    , getPackageModules
    , SnapshotPackage (..)
    , lookupSnapshotPackage
    , SnapshotHackagePackage(..)
    , getDeprecated
    , getLatests
    , getHackageLatestVersion
    , getVersionForSnapshot
    , getSnapshotLatestVersion
    , getDeps
    , getRevDeps
    , getDepsCount
    , getPackageInfo
    , Package (..)
    , getPackage
    , getSnapshotsForPackage
    , getSnapshots
    , countSnapshots
    , currentSchema
    , last5Lts5Nightly
    , lastXLts5Nightly
    , snapshotsJSON
    , getPackageCount
    , getLatestLtsByGhc
    , cloneOrUpdate
    ) where

import RIO
import RIO.Time
import RIO.Process
import qualified RIO.Text as T
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8
import RIO.FilePath
import RIO.Directory (doesDirectoryExist)
import Conduit
import qualified Data.List  as L
import Database.Esqueleto.Internal.Language (From)
import Text.Blaze.Html (Html)
import Stackage.Database.Types
import Stackage.Database.PackageInfo
import Data.Yaml (decodeFileEither)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Logger (runNoLoggingT)
import qualified Database.Esqueleto as E
import qualified Data.Aeson as A
import Types (SnapshotBranch(..))
import Data.Pool (destroyAllResources)
import Pantry.Types
    ( HasStorage(storageG)
    , PackageNameP(..)
    , Storage(Storage)
    , VersionP(..)
    , Revision(..)
    )
import Pantry.Storage hiding (migrateAll)
import qualified Pantry.Storage as Pantry (migrateAll)
import Pantry.Hackage (updateHackageIndex, DidUpdateOccur(..))
import Control.Monad.Trans.Class (lift)
import Types
import Distribution.Types.VersionRange (VersionRange)

currentSchema :: Int
currentSchema = 2

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Schema
    val Int
    deriving Show

Snapshot
    name SnapName
    compiler Compiler
    created Day
    updatedOn UTCTime Maybe
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
    name PackageNameP
    latest VersionP
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
    version VersionP
    UniqueSnapshotPackage snapshot package
SnapshotHackagePackage
    snapshot SnapshotId
    cabal HackageCabalId
    isHidden Bool default=False
    UniqueSnapshotHackagePackage snapshot cabal
Module
    name ModuleNameP
    UniqueModule name
HackagePackageModule
    cabal HackageCabalId
    module ModuleId
    hasDocs Bool default=False
    UniqueSnapshotModule cabal module
HackageDep
    user HackageCabalId
    uses PackageNameId
    range Text
    UniqueHackageDep user uses
Dep
    user PackageId
    uses PackageNameP -- avoid circular dependency issue when loading database
    range Text
    UniqueDep user uses
Deprecated
    package PackageNameId
    inFavourOf [PackageNameId]
    UniqueDeprecated package
|]



instance A.ToJSON Snapshot where
  toJSON Snapshot{..} =
    A.object [ "name"     A..= snapshotName
             , "ghc"      A..= VersionP ghc -- TODO: deprecate
             , "compiler" A..= snapshotCompiler
             , "created"  A..= formatTime defaultTimeLocale "%F" snapshotCreated
             ]
    where CompilerGHC ghc = snapshotCompiler

_hideUnusedWarnings
    :: ( SnapshotPackageId
       , SnapshotHackagePackageId
       , SchemaId
       , LtsId
       , NightlyId
       , ModuleId
       , HackagePackageModuleId
       , DepId
       , HackageDepId
       , DeprecatedId
       ) -> ()
_hideUnusedWarnings _ = ()


closeStackageDatabase :: HasStorage env => RIO env ()
closeStackageDatabase = do
    Storage pool <- view storageG
    liftIO $ destroyAllResources pool

-- | Re-use Pantry's database connection pool
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
                        ("Couldn't parse the filepath into a Nightly date: " <> display (T.pack fp))
                return Nothing

createOrUpdateSnapshot ::
       Bool -> (SnapshotFile, Day, UTCTime) -> ReaderT SqlBackend (RIO StackageCron) ()
createOrUpdateSnapshot forceUpdate (SnapshotFile {..}, createdOn, updatedOn) = do
    eSnapshotKey <- insertBy (Snapshot sfName sfCompiler createdOn Nothing)
    -- Decide if creation or an update is necessary:
    mSnapKey <-
        lift $
        case eSnapshotKey of
            Left (Entity _key snap)
                | snapshotUpdatedOn snap == Just updatedOn && not forceUpdate -> do
                    logInfo $
                        "Snapshot with name: " <> display sfName <>
                        " already exists and is up to date."
                    return Nothing
            Left (Entity key snap)
                | Nothing <- snapshotUpdatedOn snap -> do
                    logWarn $
                        "Snapshot with name: " <> display sfName <>
                        " did not finish updating last time."
                    return $ Just key
            Left (Entity key _snap) -> do
                unless forceUpdate $
                    logWarn $
                    "Snapshot with name: " <> display sfName <> " was updated, applying new patch."
                return $ Just key
            Right key -> return $ Just key -- New snapshot go with full update
    forM_ mSnapKey $ \snapshotKey -> do
        case sfName of
            SNLts major minor -> void $ insertUnique $ Lts snapshotKey major minor
            SNNightly day -> void $ insertUnique $ Nightly snapshotKey day
        pantryUpdatesSucceeded <-
            forM sfPackages $ \(PantryHackagePackage phc PantryTree {..}) -> do
                let PantryHackageCabal packageName _ _ _ = phc
                mHackageCabal <- getHackageCabal phc
                case mHackageCabal of
                    Nothing -> do
                        lift $
                            logError $
                            "Couldn't find a valid hackage cabal file " <>
                            "in pantry corresponding to: " <>
                            displayShow phc
                        return False
                    Just (hackageCabalKey, cabalBlob) -> do
                        mCabalFile <- lift $ parseCabalBlobMaybe packageName cabalBlob
                        fmap (fromMaybe False) $ forM mCabalFile $ \gpd -> do
                            let isHidden = fromMaybe False $ Map.lookup packageName sfHidden
                            _ <- insertBy $ SnapshotHackagePackage snapshotKey hackageCabalKey isHidden
                            insertHackagePackageModules hackageCabalKey (getModuleNames gpd)
                            insertHackageDeps hackageCabalKey (extractDependencies gpd)
                            return True
        docUpdateSucceeded <- checkForDocs sfName
        if and pantryUpdatesSucceeded && docUpdateSucceeded
            then do
                update snapshotKey [SnapshotUpdatedOn =. Just updatedOn]
                lift $
                    logInfo $ "Created or updated snapshot '" <> display sfName <> "' successfully"
            else lift $
                 logError $ "There were errors while adding snapshot '" <> display sfName <> "'"



-- | Download a list of available .html files from S3 bucket for a particular resolver and record
-- in the database which modules have documentation available for them.
checkForDocs :: Monad m => SnapName -> m Bool
checkForDocs _sname = return True


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

createStackageDatabase :: RIO StackageCron [Deprecation] -> RIO StackageCron ()
createStackageDatabase getDeprecations = do
    logInfo "Entering createStackageDatabase"
    actualSchema <- getSchema
    let schemaMatch = actualSchema == Just currentSchema
    forceFullUpdate <- sfForceFullUpdate <$> ask
    unless schemaMatch $ do
        logWarn $
            "Current schema does not match actual schema: " <>
            displayShow (actualSchema, currentSchema)
    withStorage $ runMigration Pantry.migrateAll
    withStorage $ runMigration migrateAll
    unless schemaMatch $ withStorage $ insert_ $ Schema currentSchema
    didUpdate <- updateHackageIndex True (Just "Stackage Server cron job")
    case didUpdate of
        UpdateOccurred -> do
            logInfo "Updated hackage index. Getting deprecated info now"
            getDeprecations >>= withStorage . mapM_ addDeprecated
        NoUpdateOccurred -> do
            logInfo "No new packages in hackage index"
    runConduitRes $
        sourceSnapshots .| mapM_C (lift . withStorage . createOrUpdateSnapshot forceFullUpdate)
    withStorage $ mapM_ (flip rawExecute []) ["COMMIT", "VACUUM", "BEGIN"]

insertHackagePackageModules ::
       MonadIO m => HackageCabalId -> [ModuleNameP] -> ReaderT SqlBackend m ()
insertHackagePackageModules hackageCabalKey = mapM_ $ \ moduleName -> do
  eModuleId <- either entityKey id <$> insertBy (Module moduleName)
  void $ insertBy (HackagePackageModule hackageCabalKey eModuleId False)


insertHackageDeps ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => HackageCabalId
    -> Map PackageNameP VersionRange
    -> ReaderT SqlBackend m ()
insertHackageDeps hackageCabalKey dependencies =
    forM_ (Map.toList dependencies) $ \(dep, range) ->
        getBy (UniquePackageName dep) >>= \case
            Just pname -> do
                void $ insertBy (HackageDep hackageCabalKey (entityKey pname) (dtDisplay range))
            Nothing -> do
                lift $ logWarn $ "Couldn't find a dependency in Pantry with name: " <> display dep


getPackageNameId :: MonadIO m => PackageNameP -> ReaderT SqlBackend m (Maybe PackageNameId)
getPackageNameId pname = fmap entityKey <$> getBy (UniquePackageName pname)

addDeprecated ::
       (HasLogFunc env, MonadReader env m, MonadIO m) => Deprecation -> ReaderT SqlBackend m ()
addDeprecated (Deprecation pname inFavourOfNameSet) = do
    mPackageNameId <- getPackageNameId pname
    case mPackageNameId of
        Just packageNameId -> do
            let inFavourOfNames = Set.toList inFavourOfNameSet
            inFavourOfAllIds <- mapM getPackageNameId inFavourOfNames
            let (badNames, inFavourOfIds) =
                    partitionEithers $
                    L.zipWith
                        (\name mid -> maybe (Left name) Right mid)
                        inFavourOfNames
                        inFavourOfAllIds
            insert_ $ Deprecated packageNameId inFavourOfIds
            when (not (null badNames)) $
                lift $
                logError $
                mconcat
                    ("Couldn't find in Pantry names of packages in deprecation list: " :
                     L.intersperse ", " (map display badNames))
        Nothing ->
            lift $
            logError $
            "Package name: " <> display pname <> " from deprecation list was not found in Pantry."


getDeprecated :: GetStackageDatabase env m => PackageNameP -> m (Bool, [PackageNameP])
getDeprecated pname =
    run $ do
        getPackageNameId pname >>= \case
            Just pnid -> do
                getBy (UniqueDeprecated pnid) >>= \case
                    Just (Entity _ (Deprecated _ inFavourOfIds)) -> do
                        names <- mapM getPackageNameById inFavourOfIds
                        return (True, PackageNameP <$> catMaybes names)
                    Nothing -> return defRes
            Nothing -> return defRes
  where
    defRes = (False, [])


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
snapshotTitle s = snapshotPrettyName (snapshotName s) (snapshotCompiler s)

snapshotPrettyName :: SnapName -> Compiler -> Text
snapshotPrettyName sName sCompiler =
    T.concat [snapshotPrettyNameShort sName, " (", displayCompiler sCompiler, ")"]

snapshotPrettyNameShort :: SnapName -> Text
snapshotPrettyNameShort name =
    case name of
        SNLts x y -> T.concat ["LTS Haskell ", T.pack (show x), ".", T.pack (show y)]
        SNNightly d -> "Stackage Nightly " <> T.pack (show d)

getAllPackages :: GetStackageDatabase env m => m [(PackageNameP, VersionP, Text)] -- FIXME add information on whether included in LTS and Nightly
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
  => SnapshotId -> PackageNameP -> m (Maybe VersionP)
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

getSnapshotModules ::
    GetStackageDatabase env m => SnapshotId -> Bool -> m [ModuleListingInfo]
getSnapshotModules sid hasDoc =
    run $ do
        map toModuleListingInfo <$>
            rawSql
                "SELECT module.name, package_name.name, version.version \
            \FROM module, hackage_package_module, hackage_cabal, package_name, version, \
                  \snapshot, snapshot_hackage_package \
            \WHERE snapshot.id = ? \
            \AND snapshot_hackage_package.snapshot = snapshot.id \
            \AND snapshot_hackage_package.cabal = hackage_cabal.id \
            \AND hackage_package_module.cabal = hackage_cabal.id \
            \AND hackage_package_module.module = module.id \
            \AND hackage_cabal.name = package_name.id \
            \AND hackage_cabal.version = version.id \
            \AND hackage_package_module.has_docs = ? \
            \ORDER BY (module.name, package_name.name) ASC"
                [toPersistValue sid, toPersistValue (not hasDoc)] --TODO: remove `not` when doc is verified
  where
    toModuleListingInfo (Single moduleName, Single packageName, Single version) =
        ModuleListingInfo
            { mliModuleName = moduleName
            , mliPackageIdentifier = PackageIdentifierP packageName version
            }

getPackageModules
    :: MonadIO m
    => HackageCabalId
    -> Bool
    -> ReaderT SqlBackend m [ModuleNameP]
getPackageModules cabalId hasDoc =
    map unSingle <$>
    rawSql
        "SELECT module.name \
         \FROM module, hackage_package_module \
         \WHERE module.id = hackage_package_module.module \
         \AND hackage_package_module.cabal = ? \
         \AND hackage_package_module.has_docs = ? \
         \ORDER BY module.name ASC"
        [toPersistValue cabalId, toPersistValue (not hasDoc)] --TODO: remove `not` when doc is verified


lookupSnapshotPackage
    :: GetStackageDatabase env m
    => SnapshotId
    -> PackageNameP
    -> m (Maybe (Entity SnapshotPackage))
lookupSnapshotPackage sid pname = run $ do
    mp <- getBy $ UniquePackage pname
    case mp of
        Nothing -> return Nothing
        Just (Entity pid _) -> getBy $ UniqueSnapshotPackage sid pid


getHackageCabal
  :: MonadIO m
  => PantryHackageCabal
  -> ReaderT SqlBackend m (Maybe (HackageCabalId, ByteString))
getHackageCabal (PantryHackageCabal {..}) =
    fmap (\(Single cid, Single contents) -> (cid, contents)) . listToMaybe <$>
    rawSql
        "SELECT hackage_cabal.id, blob.contents \
         \FROM hackage_cabal, blob, package_name, version \
         \WHERE hackage_cabal.name=package_name.id AND package_name.name=? \
         \AND hackage_cabal.version=version.id AND version.version=? \
         \AND hackage_cabal.cabal=blob.id AND blob.sha=? \
         \AND blob.size=?"
        [ toPersistValue phcPackageName
        , toPersistValue phcPackageVersion
        , toPersistValue phcSHA256
        , toPersistValue phcFileSize
        ]

toHackageCabalInfo ::
       PackageNameP
    -> (Single HackageCabalId, Single BlobId, Single VersionP, Single Revision)
    -> HackageCabalInfo
toHackageCabalInfo pname (Single cid, Single bid, Single version, Single rev) =
    HackageCabalInfo
        { hciCabalId = cid
        , hciBlobId = bid
        , hciPackageName = pname
        , hciVersion = version
        , hciRevision = guard (rev /= Revision 0) >> Just rev
        }

getHackageLatestVersion ::
       GetStackageDatabase env m => PackageNameP -> m (Maybe HackageCabalInfo)
getHackageLatestVersion pname =
    fmap (toHackageCabalInfo pname) . listToMaybe <$>
    run (rawSql
             "SELECT hackage_cabal.id, hackage_cabal.cabal, version.version, hackage_cabal.revision \
              \FROM hackage_cabal, package_name, version \
              \WHERE package_name.name = ? \
              \AND hackage_cabal.name = package_name.id \
              \AND hackage_cabal.version = version.id \
              \ORDER BY (version.version, hackage_cabal.revision) DESC \
              \LIMIT 1"
             [toPersistValue pname])

getVersionForSnapshot ::
       GetStackageDatabase env m => SnapName -> PackageNameP -> m (Maybe HackageCabalInfo)
getVersionForSnapshot sname pname =
    fmap (toHackageCabalInfo pname) . listToMaybe <$>
    run (rawSql
             "SELECT hackage_cabal.id, hackage_cabal.cabal, version.version, hackage_cabal.revision \
             \FROM snapshot, snapshot_hackage_package, hackage_cabal, package_name, version \
             \WHERE snapshot_hackage_package.snapshot = snapshot.id \
             \AND snapshot_hackage_package.cabal = hackage_cabal.id \
             \AND snapshot.name = ? \
             \AND package_name.name = ? \
             \AND hackage_cabal.name = package_name.id \
             \AND hackage_cabal.version = version.id"
             [toPersistValue sname, toPersistValue pname])

getSnapshotLatestVersion ::
       GetStackageDatabase env m
    => PackageNameP
    -> m (Maybe (SnapName, HackageCabalInfo))
getSnapshotLatestVersion pname = do
    snaps <- getSnapshotsForPackage pname (Just 1)
    return $ listToMaybe [(s, hci) | (s, _, hci) <- snaps]

getPackageInfo :: GetStackageDatabase env m =>
  HackageCabalInfo -> Bool -> m (PackageInfo, [ModuleNameP])
getPackageInfo hci needModules =
    run $ do
        blob <- loadBlobById (hciBlobId hci)
        mods <-
            if needModules
                then getPackageModules (hciCabalId hci) True
                else return []
        -- In theory parseCabalBlob can throw an error, which can depend on a Cabal version
        return (toPackageInfo $ parseCabalBlob blob, mods)


getLatests :: GetStackageDatabase env m
           => PackageNameP -- ^ package name
           -> m [LatestInfo]
getLatests pname = run $ fmap (nubOrd . concat) $ do -- forM [True, False] $ \requireDocs ->
    mlts <- latestHelper pname True -- requireDocs
        (\s ln -> s E.^. SnapshotId E.==. ln E.^. LtsSnap)
        (\_ ln ->
            [ E.desc $ ln E.^. LtsMajor
            , E.desc $ ln E.^. LtsMinor
            ])
    mnightly <- latestHelper pname True --requireDocs
        (\s ln -> s E.^. SnapshotId E.==. ln E.^. NightlySnap)
        (\s _ln -> [E.desc $ s E.^. SnapshotCreated])
    return [mlts, mnightly]

latestHelper
    :: (From E.SqlQuery E.SqlExpr SqlBackend t, MonadIO m)
    => PackageNameP -- ^ package name
    -> Bool -- ^ require docs?
    -> (E.SqlExpr (Entity Snapshot) -> t -> E.SqlExpr (E.Value Bool))
    -> (E.SqlExpr (Entity Snapshot) -> t -> [E.SqlExpr E.OrderBy])
    -> ReaderT SqlBackend m [LatestInfo]
latestHelper pname _requireDocs clause order = do
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
        , s E.^. SnapshotCompiler
        , sp E.^. SnapshotPackageVersion
        , sp E.^. SnapshotPackageId
        )
  -- if requireDocs
  --   then
  --     case results of
  --       tuple@(_, _, _, E.Value spid):_ -> do
  --         x <- count [ModulePackage ==. spid]
  --         return $ if x > 0 then [toLatest tuple] else []
  --       [] -> return []
  --   else
  return $ map toLatest results
  where
    toLatest (E.Value sname, _, E.Value version, _) = LatestInfo
        { liSnapName = sname
        , liVersion = version
        }

getDeps :: GetStackageDatabase env m => PackageNameP -> Maybe Int -> m [(PackageNameP, Text)]
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

getRevDeps :: GetStackageDatabase env m => PackageNameP -> Maybe Int -> m [(PackageNameP, Text)]
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

getDepsCount :: GetStackageDatabase env m => PackageNameP -> m (Int, Int)
getDepsCount pname =
    run $ (,) <$>
    (getBy (UniquePackage pname) >>= \case
         Nothing             -> return 0
         Just (Entity pid _) -> count [DepUser ==. pid]) <*>
    count [DepUses ==. pname]

getPackage :: GetStackageDatabase env m => PackageNameP -> m (Maybe (Entity Package))
getPackage = run . getBy . UniquePackage

getSnapshotsForPackage
    :: GetStackageDatabase env m
    => PackageNameP
    -> Maybe Int
    -> m [(SnapName, Compiler, HackageCabalInfo)]
getSnapshotsForPackage pname mlimit =
    map (\(Single sn, Single c, scid, sbid, sv, srev) ->
             (sn, c, toHackageCabalInfo pname (scid, sbid, sv, srev))) <$>
    run (rawSql
             ("SELECT snapshot.name, \
                     \snapshot.compiler, \
                     \hackage_cabal.id, \
                     \hackage_cabal.cabal, \
                     \version.version, \
                     \hackage_cabal.revision \
             \FROM snapshot, snapshot_hackage_package, hackage_cabal, package_name, version \
             \WHERE snapshot_hackage_package.cabal = hackage_cabal.id \
             \AND snapshot_hackage_package.snapshot = snapshot.id \
             \AND package_name.name = ? \
             \AND hackage_cabal.name = package_name.id \
             \AND hackage_cabal.version = version.id \
             \ORDER BY snapshot.created DESC" <>
              mlimitQ)
             (toPersistValue pname : mlimitVal))
  where
    (mlimitQ, mlimitVal) = maybe ("", []) ((,) " LIMIT ?" . pure . toPersistValue) mlimit


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
getLatestLtsByGhc =
    run $
    fmap (dedupe . map toTuple) $ do
        E.select $
            E.from $ \(lts `E.InnerJoin` snapshot) -> do
                E.on $ lts E.^. LtsSnap E.==. snapshot E.^. SnapshotId
                E.orderBy [E.desc (lts E.^. LtsMajor), E.desc (lts E.^. LtsMinor)]
                E.groupBy
                    ( snapshot E.^. SnapshotCompiler
                    , lts E.^. LtsId
                    , lts E.^. LtsMajor
                    , lts E.^. LtsMinor
                    , snapshot E.^. SnapshotId)
                return (lts, snapshot)
  where
    toTuple (Entity _ lts, Entity _ snapshot) =
        ( ltsMajor lts
        , ltsMinor lts
        , displayCompiler (snapshotCompiler snapshot)
        , snapshotCreated snapshot)
    dedupe [] = []
    dedupe (x:xs) = x : dedupe (dropWhile (\y -> thd x == thd y) xs)
    thd (_, _, x, _) = x
