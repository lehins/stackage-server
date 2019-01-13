{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
module Stackage.Database
    ( -- * Database
      PostgresConf (..)
    , StackageDatabase
    , GetStackageDatabase(..)
    , openStackageDatabase
    , closeStackageDatabase
    , runStackageMigrations
    , Unique(..)
    -- * Snapshot
    , Snapshot(..)
    , SnapName (..)
    , SnapshotId ()
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
    , snapshotMarkUpdated
    , insertSnapshotName
    -- * Packages
    -- ** Addition
    , insertHackagePackageModules
    , insertHackagePackageDeps
    -- ** Retrieval
    , getHackageCabal
    , getPantryHackageCabal
    , getAllPackages
    , getPackagesForSnapshot
    , getPackageVersionBySnapshot
    , getSnapshotModules
    , getPackageModules
    , SnapshotPackage (..)
    , lookupSnapshotPackage
    , SnapshotHackagePackage(..)
    -- * Deprecations
    , addDeprecated
    , getDeprecated
    , getLatests
    , getHackageLatestVersion
    , getVersionForSnapshot
    , getSnapshotLatestVersion
    , getForwardDeps
    , getReverseDeps
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
    ) where

import RIO
import RIO.Time
import qualified RIO.Text as T
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified Data.List  as L
import Text.Blaze.Html (Html)
import Stackage.Database.Types
import Stackage.Database.PackageInfo
import Data.Bifunctor (bimap)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Logger (runNoLoggingT)
import qualified Database.Esqueleto as E
import qualified Data.Aeson as A
import Types (SnapshotBranch(..), PackageVersionRev(..), VersionRev(..))
import Data.Pool (destroyAllResources)
import Pantry.Types (BlobKey(..), HasPantryConfig(..), PantryConfig(pcStorage), Storage(Storage))
import Pantry.Storage hiding (migrateAll)
import qualified Pantry.Storage as Pantry (migrateAll)
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
    isCore Bool
    synopsis Text
    readme Text
    changelog Text
    isHidden Bool
    UniqueSnapshotHackagePackage snapshot cabal
Module
    name ModuleNameP
    UniqueModule name
HackagePackageModule
    cabal HackageCabalId
    module ModuleId
    hasDocs Bool
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
             , "ghc"      A..= VersionP ghc -- TODO: deprecate, since encapsulated in compiler?
             , "compiler" A..= snapshotCompiler
             , "created"  A..= formatTime defaultTimeLocale "%F" snapshotCreated
             ]
    where CompilerGHC ghc = snapshotCompiler

-- | Re-use Pantry's database connection pool
type StackageDatabase = Storage

class MonadIO m => GetStackageDatabase env m | m -> env where
    getStackageDatabase :: m StackageDatabase
-- instance (HasStorage env, MonadIO m) => GetStackageDatabase env (ReaderT env m) where
--     getStackageDatabase = view storageG
instance HasPantryConfig env => GetStackageDatabase env (RIO env) where
    getStackageDatabase = pcStorage <$> view pantryConfigL


_hideUnusedWarnings
    :: ( SnapshotPackageId
       , SnapshotHackagePackageId
       , SchemaId
       , LtsId
       , NightlyId
       , ModuleId
       , HackagePackageModuleId
       , PackageId
       , DepId
       , HackageDepId
       , DeprecatedId
       ) -> ()
_hideUnusedWarnings _ = ()


run :: GetStackageDatabase env m => SqlPersistT IO a -> m a
run inner = do
    Storage pool <- getStackageDatabase
    liftIO $ runSqlPool inner pool


openStackageDatabase :: MonadIO m => PostgresConf -> m Storage
openStackageDatabase pg = liftIO $ do
    fmap Storage $ runNoLoggingT $ createPostgresqlPool
      (pgConnStr pg)
      (pgPoolSize pg)

closeStackageDatabase :: GetStackageDatabase env m => m ()
closeStackageDatabase = do
    Storage pool <- getStackageDatabase
    liftIO $ destroyAllResources pool


getSchema :: ReaderT SqlBackend (RIO StackageCron) (Maybe Int)
getSchema = do
    eres <- tryAny (selectList [] [])
    lift $ logInfo $ "getSchema result: " <> displayShow eres
    case eres of
        Right [Entity _ (Schema v)] -> return $ Just v
        _ -> return Nothing

runStackageMigrations :: RIO StackageCron ()
runStackageMigrations =
    withStorage $ do
        actualSchema <- getSchema
        runMigration Pantry.migrateAll
        runMigration migrateAll
        unless (actualSchema == Just currentSchema) $ do
            lift $
                logWarn $
                "Current schema does not match actual schema: " <>
                displayShow (actualSchema, currentSchema)
            deleteWhere ([] :: [Filter Schema])
            insert_ $ Schema currentSchema



insertHackagePackageModules ::
       MonadIO m => HackageCabalId -> [ModuleNameP] -> ReaderT SqlBackend m ()
insertHackagePackageModules hackageCabalKey = mapM_ $ \ moduleName -> do
  eModuleId <- either entityKey id <$> insertBy (Module moduleName)
  void $ insertBy (HackagePackageModule hackageCabalKey eModuleId False)


insertHackagePackageDeps ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => PackageIdentifierP
    -> HackageCabalId
    -> Map PackageNameP VersionRange
    -> ReaderT SqlBackend m ()
insertHackagePackageDeps pid hackageCabalKey dependencies =
    forM_ (Map.toList dependencies) $ \(dep, range) ->
        getBy (UniquePackageName dep) >>= \case
            Just pname -> do
                void $ insertBy (HackageDep hackageCabalKey (entityKey pname) (dtDisplay range))
            Nothing ->
                lift $
                logWarn $
                "Couldn't find a dependency of " <> display pid <> " in Pantry with name: " <>
                display dep


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
            void $
                upsertBy
                    (UniqueDeprecated packageNameId)
                    (Deprecated packageNameId inFavourOfIds)
                    [DeprecatedInFavourOf =. inFavourOfIds]
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


snapshotMarkUpdated :: GetStackageDatabase env m => SnapshotId -> UTCTime -> m ()
snapshotMarkUpdated snapKey updatedOn = run $ update snapKey [SnapshotUpdatedOn =. Just updatedOn]

insertSnapshotName :: GetStackageDatabase env m => SnapshotId -> SnapName -> m ()
insertSnapshotName snapKey snapName =
    run $
    case snapName of
        SNLts major minor -> void $ insertUnique $ Lts snapKey major minor
        SNNightly day -> void $ insertUnique $ Nightly snapKey day


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

getPackagesForSnapshot :: GetStackageDatabase env m => SnapshotId -> m [PackageListingInfo]
getPackagesForSnapshot sid =
    liftM (map toPLI) $
    run $
    rawSql
        "SELECT package_name.name, version.version, \
               \snapshot_hackage_package.is_core, snapshot_hackage_package.synopsis \
        \FROM package_name, version, hackage_cabal, snapshot_hackage_package \
        \WHERE snapshot_hackage_package.snapshot = ? \
        \AND hackage_cabal.id = snapshot_hackage_package.cabal \
        \AND hackage_cabal.name = package_name.id \
        \AND hackage_cabal.version = version.id ORDER BY package_name.name ASC"
        [toPersistValue sid]
  where
    toPLI (Single name, Single version, Single isCore, Single synopsis) =
        PackageListingInfo
            {pliName = name, pliVersion = version, pliSynopsis = synopsis, pliIsCore = isCore}


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


getPantryHackageCabal
  :: MonadIO m
  => PantryCabal
  -> ReaderT SqlBackend m (Maybe (HackageCabalId, BlobId))
getPantryHackageCabal (PantryCabal {..}) =
    fmap (\(Single cid, Single bid) -> (cid, bid)) . listToMaybe <$>
    rawSql
        "SELECT hackage_cabal.id, hackage_cabal.cabal \
         \FROM hackage_cabal, blob, package_name, version \
         \WHERE hackage_cabal.name=package_name.id \
         \AND hackage_cabal.version=version.id \
         \AND hackage_cabal.cabal=blob.id \
         \AND package_name.name=? \
         \AND version.version=? \
         \AND blob.sha=? \
         \AND blob.size=?"
        [ toPersistValue pcPackageName
        , toPersistValue pcPackageVersion
        , toPersistValue sha256
        , toPersistValue size
        ]
  where (BlobKey sha256 size) = pcCabalKey

getHackageCabal ::
       MonadIO m
    => PackageNameP
    -> VersionP
    -> Maybe Revision
    -> ReaderT SqlBackend m (Maybe (HackageCabalId, BlobId))
getHackageCabal pname ver mrev =
    fmap (bimap unSingle unSingle) . listToMaybe <$>
    rawSql
        "SELECT hackage_cabal.id, hackage_cabal.cabal \
         \FROM hackage_cabal, package_name, version \
         \WHERE hackage_cabal.name = package_name.id \
         \AND hackage_cabal.version = version.id \
         \AND package_name.name = ? \
         \AND version.version = ? \
         \AND hackage_cabal.revision = ?"
        [toPersistValue pname, toPersistValue ver, toPersistValue $ fromMaybe (Revision 0) mrev]


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
        , hciRevision = toRevMaybe rev
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
             \ORDER BY (string_to_array(version.version, '.')::bigint[], hackage_cabal.revision) DESC \
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

getLatests :: GetStackageDatabase env m => PackageNameP -> m [LatestInfo]
getLatests pname = run $ do
  mLtsVer <- getLatestLts pname
  mNightlyVer <- getLatestNightly pname
  return $ catMaybes [mLtsVer, mNightlyVer]

getLatestHelper :: (MonadIO m) => Text -> PackageNameP -> ReaderT SqlBackend m (Maybe LatestInfo)
getLatestHelper sqlQuery pname =
    fmap
        (\(Single snapName, Single ver, Single rev) -> (LatestInfo snapName (toVersionRev ver rev))) .
    listToMaybe <$>
    rawSql sqlQuery [toPersistValue pname]


getLatestLts :: (MonadIO m) => PackageNameP -> ReaderT SqlBackend m (Maybe LatestInfo)
getLatestLts =
    getLatestHelper
        "SELECT snapshot.name, version.version, hackage_cabal.revision \
        \FROM lts, snapshot, snapshot_hackage_package, hackage_cabal, package_name, version \
        \WHERE snapshot.id = lts.snap \
        \AND snapshot.id = snapshot_hackage_package.snapshot \
        \AND hackage_cabal.id = snapshot_hackage_package.cabal \
        \AND hackage_cabal.name = package_name.id \
        \AND hackage_cabal.version = version.id \
        \AND package_name.name = ? \
        \ORDER BY (lts.major, lts.minor) DESC LIMIT 1"

getLatestNightly :: (MonadIO m) => PackageNameP -> ReaderT SqlBackend m (Maybe LatestInfo)
getLatestNightly =
    getLatestHelper
        "SELECT snapshot.name, version.version, hackage_cabal.revision \
        \FROM nightly, snapshot, snapshot_hackage_package, hackage_cabal, package_name, version \
        \WHERE snapshot.id = nightly.snap \
        \AND snapshot.id = snapshot_hackage_package.snapshot \
        \AND snapshot_hackage_package.cabal = hackage_cabal.id \
        \AND hackage_cabal.name = package_name.id \
        \AND hackage_cabal.version = version.id \
        \AND package_name.name = ? \
        \ORDER BY nightly.day DESC LIMIT 1"

mkLimitHelper :: Maybe Int -> (Text, [PersistValue])
mkLimitHelper = maybe ("", []) ((,) " LIMIT ?" . pure . toPersistValue)


getForwardDeps ::
       GetStackageDatabase env m
    => SnapName
    -> HackageCabalId
    -> Maybe Int
    -> m [(PackageVersionRev, Text)]
getForwardDeps snapName hackageCabalId mlimit =
    run $
    map (\(Single pname, Single ver, Single rev, Single range) ->
             (PackageVersionRev pname (toVersionRev ver rev), range)) <$>
    rawSql
        ("SELECT package_name.name, version.version, hc_uses.revision, hackage_dep.range \
         \FROM hackage_dep, package_name, version, hackage_cabal hc_uses, \
              \snapshot_hackage_package, snapshot \
         \WHERE hackage_dep.user = ? \
         \AND hackage_dep.uses = hc_uses.name \
         \AND hc_uses.version = version.id \
         \AND hc_uses.name = package_name.id \
         \AND snapshot_hackage_package.cabal = hc_uses.id \
         \AND snapshot_hackage_package.snapshot = snapshot.id \
         \AND snapshot.name=? \
         \ORDER BY package_name.name" <>
          mlimitSql)
        ([toPersistValue hackageCabalId, toPersistValue snapName] ++ mlimitValue)
  where
    (mlimitSql, mlimitValue) = mkLimitHelper mlimit

getForwardDepsCount :: MonadIO m => HackageCabalId -> ReaderT SqlBackend m Int
getForwardDepsCount hackageCabalId = count [HackageDepUser ==. hackageCabalId]

getDepsCount :: GetStackageDatabase env m => SnapName -> HackageCabalId -> m (Int, Int)
getDepsCount snapName hackageCabalId =
    run $
    (,) <$> getForwardDepsCount hackageCabalId <*> getReverseDepsCount snapName hackageCabalId

toRevMaybe :: Revision -> Maybe Revision
toRevMaybe rev = guard (rev /= Revision 0) >> Just rev

toVersionRev :: VersionP -> Revision -> VersionRev
toVersionRev v = VersionRev v . toRevMaybe

getReverseDeps ::
       GetStackageDatabase env m
    => SnapName
    -> HackageCabalId
    -> Maybe Int -- ^ Optionally limit number of dependencies
    -> m [(PackageVersionRev, Text)]
getReverseDeps snapName hackageCabalId mlimit =
    run $
    map (\(Single pname, Single ver, Single rev, Single range) ->
             (PackageVersionRev pname (toVersionRev ver rev), range)) <$>
    rawSql
        ("SELECT package_name.name, version.version, hc_user.revision, hackage_dep.range \
         \FROM hackage_dep, package_name, version, snapshot, snapshot_hackage_package, \
               \hackage_cabal hc_user, hackage_cabal hc_uses \
         \WHERE hc_uses.name = hackage_dep.uses \
         \AND hc_uses.id = ? \
         \AND hc_user.version = version.id \
         \AND hc_user.name = package_name.id \
         \AND hc_user.id = hackage_dep.user \
         \AND hc_user.id = snapshot_hackage_package.cabal \
         \AND snapshot_hackage_package.snapshot = snapshot.id \
         \AND snapshot.name = ? \
         \ORDER BY package_name.name" <>
         mlimitSql)
        ([toPersistValue hackageCabalId, toPersistValue snapName] ++ mlimitValue)
  where
    (mlimitSql, mlimitValue) = mkLimitHelper mlimit

getReverseDepsCount :: MonadIO m => SnapName -> HackageCabalId -> ReaderT SqlBackend m Int
getReverseDepsCount snapName hackageCabalId =
    maybe 0 unSingle . listToMaybe <$>
    rawSql
        "SELECT COUNT(hackage_dep.user) \
        \FROM hackage_dep, snapshot_hackage_package, snapshot, \
             \hackage_cabal hc_user, hackage_cabal hc_uses \
        \WHERE hc_uses.name = hackage_dep.uses \
        \AND hc_uses.id = ? \
        \AND hc_user.id = hackage_dep.user \
        \AND hc_user.id = snapshot_hackage_package.cabal \
        \AND snapshot_hackage_package.snapshot = snapshot.id \
        \AND snapshot.name = ?"
        [toPersistValue hackageCabalId, toPersistValue snapName]

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
