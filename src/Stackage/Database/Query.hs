{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Stackage.Database.Query
    (
      -- * Snapshot
      newestSnapshot
    , newestLTS
    , newestLTSMajor
    , newestNightly
    , getSnapshots
    , countSnapshots
    , ltsMajorVersions
    , snapshotBefore
    , lookupSnapshot
    , snapshotTitle
    , last5Lts5Nightly
    , lastXLts5Nightly
    , snapshotsJSON
    , getLatestLtsByGhc

    , getSnapshotModules
    , getSnapshotPackageModules

    -- * Package

    , getHackageCabal
    , getPantryHackageCabal
    , getAllPackages
    , getPackagesForSnapshot
    , getPackageVersionForSnapshot

    , getLatests
    , getHackageLatestVersion
    , getSnapshotPackageInfo
    , getSnapshotLatestVersion
    , getPackageInfo
    , getSnapshotsForPackage

    -- ** Dependencies

    , getForwardDeps
    , getReverseDeps
    , getDepsCount

    -- ** Deprecations

    , getDeprecated

    -- * Needed for Cron Job
    -- ** Re-exports from Pantry
    , loadBlobById
    , getTreeForKey
    , treeCabal
    -- ** Stackage server
    , addSnapshotPackage
    , getHackageCabalByRev0
    , getHackageCabalByKey
    , snapshotMarkUpdated
    , insertSnapshotName
    , addDeprecated
    , markModuleHasDocs
    , insertSnapshotPackageModules
    , insertDeps
    ) where

import qualified Data.Aeson                    as A
import qualified Data.List                     as L
import           Database.Esqueleto
import qualified Database.Persist              as P
import           Pantry.Storage                (EntityField (..), Unique (..),
                                                getPackageNameId, getPackageNameById,
                                                getTreeForKey,
                                                getVersionId, loadBlobById,
                                                treeCabal)
import           RIO                           hiding (on, (^.))
import qualified RIO.Map                       as Map
import qualified RIO.Set                       as Set
import qualified RIO.Text                      as T
import           RIO.Time                      (Day, UTCTime)
import           Stackage.Database.PackageInfo
import           Stackage.Database.Schema
import           Stackage.Database.Types



snapshotTitle :: Snapshot -> Text
snapshotTitle s = snapshotPrettyName (snapshotName s) (snapshotCompiler s)


lookupSnapshot :: GetStackageDatabase env m => SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot name = run $ getBy $ UniqueSnapshot name


newestSnapshot :: GetStackageDatabase env m => SnapshotBranch -> m (Maybe SnapName)
newestSnapshot LtsBranch          = fmap (uncurry SNLts) <$> newestLTS
newestSnapshot NightlyBranch      = fmap SNNightly <$> newestNightly
newestSnapshot (LtsMajorBranch x) = fmap (SNLts x) <$> newestLTSMajor x

newestLTS :: GetStackageDatabase env m => m (Maybe (Int, Int))
newestLTS =
    run $ liftM (fmap go) $ selectFirst [] [P.Desc LtsMajor, P.Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

newestLTSMajor :: GetStackageDatabase env m => Int -> m (Maybe Int)
newestLTSMajor x =
    run $ liftM (fmap $ ltsMinor . entityVal) $ P.selectFirst [LtsMajor P.==. x] [P.Desc LtsMinor]

ltsMajorVersions :: GetStackageDatabase env m => m [(Int, Int)]
ltsMajorVersions =
    run $ liftM (dropOldMinors . map (toPair . entityVal))
        $ P.selectList [] [P.Desc LtsMajor, P.Desc LtsMinor]
  where
    toPair (Lts _ x y) = (x, y)

    dropOldMinors [] = []
    dropOldMinors (l@(x, _):rest) =
        l : dropOldMinors (dropWhile sameMinor rest)
      where
        sameMinor (y, _) = x == y

newestNightly :: GetStackageDatabase env m => m (Maybe Day)
newestNightly =
    run $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [P.Desc NightlyDay]

-- | Get the snapshot which precedes the given one with respect to it's branch (nightly/lts)
snapshotBefore :: GetStackageDatabase env m => SnapName -> m (Maybe (SnapshotId, SnapName))
snapshotBefore (SNLts x y)     = ltsBefore x y
snapshotBefore (SNNightly day) = nightlyBefore day

nightlyBefore :: GetStackageDatabase env m => Day -> m (Maybe (SnapshotId, SnapName))
nightlyBefore day = do
    run $ liftM (fmap go) $ P.selectFirst [NightlyDay P.<. day] [P.Desc NightlyDay]
  where
    go (Entity _ nightly) = (nightlySnap nightly, SNNightly $ nightlyDay nightly)

ltsBefore :: GetStackageDatabase env m => Int -> Int -> m (Maybe (SnapshotId, SnapName))
ltsBefore x y = do
    run $ liftM (fmap go) $ selectFirst
        ( [LtsMajor P.<=. x, LtsMinor P.<. y] P.||.
          [LtsMajor P.<. x]
        )
        [P.Desc LtsMajor, P.Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsSnap lts, SNLts (ltsMajor lts) (ltsMinor lts))



last5Lts5Nightly :: GetStackageDatabase env m => m [SnapName]
last5Lts5Nightly = lastXLts5Nightly 5

lastXLts5Nightly :: GetStackageDatabase env m => Int -> m [SnapName]
lastXLts5Nightly ltsCount = run $ do
    ls <- P.selectList [] [P.Desc LtsMajor, P.Desc LtsMinor, P.LimitTo ltsCount]
    ns <- P.selectList [] [P.Desc NightlyDay, P.LimitTo 5]
    return $ map l ls <> map n ns
  where
    l (Entity _ x) = SNLts (ltsMajor x) (ltsMinor x)
    n (Entity _ x) = SNNightly (nightlyDay x)

snapshotsJSON :: GetStackageDatabase env m => m A.Value
snapshotsJSON = do
    mlatestNightly <- newestNightly
    ltses <- ltsMajorVersions
    let lts =
            case ltses of
                [] -> []
                majorVersions@(latest:_) -> ("lts" A..= printLts latest) : map toObj majorVersions
        nightly =
            case mlatestNightly of
                Nothing -> id
                Just n  -> (("nightly" A..= printNightly n) :)
    return $ A.object $ nightly lts
  where
    toObj lts@(major, _) = T.pack ("lts-" <> show major) A..= printLts lts
    printLts (major, minor) = "lts-" <> show major <> "." <> show minor
    printNightly day = "nightly-" <> T.pack (show day)


getLatestLtsByGhc :: GetStackageDatabase env m => m [(Int, Int, Text, Day)]
getLatestLtsByGhc =
    run $ fmap (dedupe . map toTuple) $ do
        select $
            from $ \(lts `InnerJoin` snapshot) -> do
                on $ lts ^. LtsSnap ==. snapshot ^. SnapshotId
                orderBy [desc (lts ^. LtsMajor), desc (lts ^. LtsMinor)]
                groupBy
                    ( snapshot ^. SnapshotCompiler
                    , lts ^. LtsId
                    , lts ^. LtsMajor
                    , lts ^. LtsMinor
                    , snapshot ^. SnapshotId)
                return (lts, snapshot)
  where
    toTuple (Entity _ lts, Entity _ snapshot) =
        ( ltsMajor lts
        , ltsMinor lts
        , textDisplay (snapshotCompiler snapshot)
        , snapshotCreated snapshot)
    dedupe []     = []
    dedupe (x:xs) = x : dedupe (dropWhile (\y -> thd x == thd y) xs)
    thd (_, _, x, _) = x


-- | Count snapshots that belong to a specific SnapshotBranch
countSnapshots :: (GetStackageDatabase env m) => Maybe SnapshotBranch -> m Int
countSnapshots Nothing                   = run $ P.count ([] :: [P.Filter Snapshot])
countSnapshots (Just NightlyBranch)      = run $ P.count ([] :: [P.Filter Nightly])
countSnapshots (Just LtsBranch)          = run $ P.count ([] :: [P.Filter Lts])
countSnapshots (Just (LtsMajorBranch x)) = run $ P.count [LtsMajor P.==. x]

-- | Get snapshots that belong to a specific SnapshotBranch
getSnapshots :: (GetStackageDatabase env m)
             => Maybe SnapshotBranch
             -> Int -- ^ limit
             -> Int -- ^ offset
             -> m [Entity Snapshot]
getSnapshots mBranch l o =
    run $
    case mBranch of
        Nothing -> P.selectList [] [P.LimitTo l, P.OffsetBy o, P.Desc SnapshotCreated]
        Just NightlyBranch ->
            select $
            from $ \(nightly `InnerJoin` snapshot) -> do
                on $ nightly ^. NightlySnap ==. snapshot ^. SnapshotId
                orderBy [desc (nightly ^. NightlyDay)]
                limit $ fromIntegral l
                offset $ fromIntegral o
                pure snapshot
        Just LtsBranch -> do
            select $
                from $ \(lts `InnerJoin` snapshot) -> do
                    on $ lts ^. LtsSnap ==. snapshot ^. SnapshotId
                    orderBy [desc (lts ^. LtsMajor), desc (lts ^. LtsMinor)]
                    limit $ fromIntegral l
                    offset $ fromIntegral o
                    pure snapshot
        Just (LtsMajorBranch v) -> do
            select $
                from $ \(lts `InnerJoin` snapshot) -> do
                    on $ lts ^. LtsSnap ==. snapshot ^. SnapshotId
                    orderBy [desc (lts ^. LtsMinor)]
                    where_ ((lts ^. LtsMajor) ==. (val v))
                    limit $ fromIntegral l
                    offset $ fromIntegral o
                    pure snapshot


getSnapshotModules ::
    GetStackageDatabase env m => SnapshotId -> Bool -> m [ModuleListingInfo]
getSnapshotModules sid hasDoc = undefined

getSnapshotPackageModules
    :: MonadIO m
    => SnapshotPackageId
    -> Bool
    -> ReaderT SqlBackend m [ModuleNameP]
getSnapshotPackageModules snapshotPackageId hasDocs = undefined


-- FIXME: list latest version with latest snapshot it is in
getAllPackages :: GetStackageDatabase env m => m [(PackageNameP, VersionP, Text)]
getAllPackages = undefined

getPackagesForSnapshot :: GetStackageDatabase env m => SnapshotId -> m [PackageListingInfo]
getPackagesForSnapshot sid = undefined


getPackageVersionForSnapshot
  :: GetStackageDatabase env m
  => SnapshotId -> PackageNameP -> m (Maybe VersionP)
getPackageVersionForSnapshot snapshotId pname = undefined

getLatests :: GetStackageDatabase env m => PackageNameP -> m [LatestInfo]
getLatests pname = undefined

getHackageLatestVersion ::
       GetStackageDatabase env m => PackageNameP -> m (Maybe HackageCabalInfo)
getHackageLatestVersion pname = undefined


getSnapshotPackageInfo ::
       GetStackageDatabase env m => SnapName -> PackageNameP -> m (Maybe getSnapshotPackageInfo)
getSnapshotPackageInfo sname pname = undefined


getSnapshotLatestVersion ::
       GetStackageDatabase env m
    => PackageNameP
    -> m (Maybe SnapshotPackageInfo)
getSnapshotLatestVersion pname = undefined
    -- snaps <- getSnapshotsForPackage pname (Just 1)
    -- return $ listToMaybe [(s, hci) | (s, _, hci) <- snaps]


getSnapshotsForPackage
    :: GetStackageDatabase env m
    => PackageNameP
    -> Maybe Int
    -> m [(CompilerP, SnapshotPackageInfo)]
getSnapshotsForPackage pname mlimit = undefined


getHackageCabal ::
       MonadIO m
    => PackageNameP
    -> VersionP
    -> Maybe Revision
    -> ReaderT SqlBackend m (Maybe (HackageCabalId, BlobId, Maybe TreeId))
getHackageCabal pname ver mrev = undefined


getPantryHackageCabal
  :: MonadIO m
  => PantryCabal
  -> ReaderT SqlBackend m (Maybe (HackageCabalId, BlobId))
getPantryHackageCabal (PantryCabal {..}) = undefined


getPackageInfo :: GetStackageDatabase env m => SnapshotPackageInfo -> m (PackageInfo, [ModuleNameP])
getPackageInfo spi = undefined


------ Dependencies

getForwardDeps ::
       GetStackageDatabase env m
    => SnapshotPackageInfo
    -> Maybe Int
    -> m [(PackageVersionRev, Text)]
getForwardDeps spi mlimit = undefined


getForwardDepsCount :: MonadIO m => SnapshotPackageInfo -> ReaderT SqlBackend m Int
getForwardDepsCount spi = P.count [DepUser P.==. spiSnapshotPackageId spi]

getReverseDepsCount :: MonadIO m => SnapshotPackageInfo -> ReaderT SqlBackend m Int
getReverseDepsCount spi = undefined

getDepsCount :: GetStackageDatabase env m => SnapshotPackageInfo -> m (Int, Int)
getDepsCount spi =
    run $
    (,) <$> getForwardDepsCount spi <*>
    getReverseDepsCount spi

getReverseDeps ::
       GetStackageDatabase env m
    => SnapshotPackageInfo
    -> Maybe Int -- ^ Optionally limit number of dependencies
    -> m [(PackageVersionRev, Text)]
getReverseDeps spi mlimit = undefined


----- Deprecated

-- | See if a package is deprecated on hackage and in favour of which packages.
getDeprecated :: GetStackageDatabase env m => PackageNameP -> m (Bool, [PackageNameP])
getDeprecated pname =
    run $ do
        lookupPackageNameId pname >>= \case
            Just pnid -> do
                P.getBy (UniqueDeprecated pnid) >>= \case
                    Just (Entity _ (Deprecated _ inFavourOfIds)) -> do
                        names <- mapM lookupPackageNameById inFavourOfIds
                        return (True, catMaybes names)
                    Nothing -> return defRes
            Nothing -> return defRes
  where
    defRes = (False, [])



--------------------------
-- Cron related queries --
--------------------------


snapshotMarkUpdated :: GetStackageDatabase env m => SnapshotId -> UTCTime -> m ()
snapshotMarkUpdated snapKey updatedOn =
    run $ P.update snapKey [SnapshotUpdatedOn P.=. Just updatedOn]

insertSnapshotName :: GetStackageDatabase env m => SnapshotId -> SnapName -> m ()
insertSnapshotName snapKey snapName =
    run $
    case snapName of
        SNLts major minor -> void $ insertUnique $ Lts snapKey major minor
        SNNightly day     -> void $ insertUnique $ Nightly snapKey day

-- | Add a map of all dependencies for the package together with version bounds. Returns a set of
-- all dependencies that could not be found in pantry
insertDeps ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => PackageIdentifierP -- ^ For error reporting only.
    -> SnapshotPackageId
    -> Map PackageNameP VersionRangeP
    -> ReaderT SqlBackend m (Set PackageNameP)
insertDeps pid snapshotPackageId dependencies =
    Map.keysSet <$> Map.traverseMaybeWithKey insertDep dependencies
  where
    insertDep dep range =
        lookupPackageNameId dep >>= \case
            Just packageNameId -> do
                void $ insertBy (Dep snapshotPackageId packageNameId range)
                return Nothing
            Nothing -> do
                lift $
                    logWarn $
                    "Couldn't find a dependency of " <> display pid <> " in Pantry with name: " <>
                    display dep
                return $ Just dep

-- TODO: Optimize, whenever package is already in one snapshot only create the modules and new
-- SnapshotPackage
addSnapshotPackage ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => SnapshotId
    -> CompilerP
    -> PackageOrigin
    -> Maybe (Entity Tree)
    -> Maybe HackageCabalId
    -> Bool
    -> Map FlagNameP Bool
    -> PackageIdentifierP
    -> GenericPackageDescription
    -> ReaderT SqlBackend m ()
addSnapshotPackage snapshotId compiler origin mTree mHackageCabalId isHidden flags pid gpd = do
    let PackageIdentifierP pname pver = pid
        keyInsertBy = fmap (either entityKey id) . P.insertBy
    packageNameId <-
        maybe (getPackageNameId (unPackageNameP pname)) (pure . treeName . entityVal) mTree
    versionId <- maybe (getVersionId (unVersionP pver)) (pure . treeVersion . entityVal) mTree
    let snapshotPackage =
            SnapshotPackage
                { snapshotPackageSnapshot = snapshotId
                , snapshotPackagePackageName = packageNameId
                , snapshotPackageVersion = versionId
                , snapshotPackageCabal = treeCabal . entityVal <$> mTree
                , snapshotPackageHackageCabal = mHackageCabalId
                , snapshotPackageOrigin = origin
                , snapshotPackageOriginUrl = "" -- TODO: add
                , snapshotPackageSynopsis = getSynopsis gpd
                , snapshotPackageReadme = Nothing -- TODO: find from Tree
                , snapshotPackageChangelog = Nothing -- TODO: find from Tree
                , snapshotPackageIsHidden = isHidden
                , snapshotPackageFlags = flags
                }
    snapshotPackageId <- keyInsertBy snapshotPackage
    -- TODO: collect all missing dependencies
    _ <- insertDeps pid snapshotPackageId (extractDependencies compiler flags gpd)
    insertSnapshotPackageModules snapshotPackageId (getModuleNames gpd)



lookupPackageNameId :: MonadIO m => PackageNameP -> ReaderT SqlBackend m (Maybe PackageNameId)
lookupPackageNameId pname = fmap entityKey <$> getBy (UniquePackageName pname)


lookupPackageNameById :: MonadIO m => PackageNameId -> ReaderT SqlBackend m (Maybe PackageNameP)
lookupPackageNameById pnid = fmap PackageNameP <$> getPackageNameById pnid

addDeprecated ::
       (HasLogFunc env, MonadReader env m, MonadIO m) => Deprecation -> ReaderT SqlBackend m ()
addDeprecated (Deprecation pname inFavourOfNameSet) = do
    mPackageNameId <- lookupPackageNameId pname
    case mPackageNameId of
        Just packageNameId -> do
            let inFavourOfNames = Set.toList inFavourOfNameSet
            inFavourOfAllIds <- mapM lookupPackageNameId inFavourOfNames
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
                    [DeprecatedInFavourOf P.=. inFavourOfIds]
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


getHackageCabalByRev0 ::
       MonadIO m
    => PackageIdentifierP
    -> ReaderT SqlBackend m (Maybe (HackageCabalId, BlobId, Maybe TreeId))
getHackageCabalByRev0 pid = getHackageCabalByRev pid Nothing

getHackageCabalByRev ::
       MonadIO m
    => PackageIdentifierP
    -> Maybe Revision
    -> ReaderT SqlBackend m (Maybe (HackageCabalId, BlobId, Maybe TreeId))
getHackageCabalByRev (PackageIdentifierP pname ver) mrev =
    fmap (\(Value hcid, Value bid, Value mtid) -> (hcid, bid, mtid)) . listToMaybe <$>
    select
        (from $ \(hc `InnerJoin` pn `InnerJoin` v) -> do
             on (hc ^. HackageCabalVersion ==. v ^. VersionId)
             on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
             where_
                 ((pn ^. PackageNameName ==. val pname) &&.
                  (v ^. VersionVersion ==. val ver) &&.
                  (hc ^. HackageCabalRevision ==. val (fromMaybe (Revision 0) mrev)))
             return (hc ^. HackageCabalId, hc ^. HackageCabalCabal, hc ^. HackageCabalTree))

-- | This query will return `Nothing` if the tarball for the hackage cabal file hasn't been loaded
-- yet.
getHackageCabalByKey ::
       MonadIO m
    => PackageIdentifierP
    -> BlobKey
    -> ReaderT SqlBackend m (Maybe (HackageCabalId, Maybe TreeId))
getHackageCabalByKey (PackageIdentifierP pname ver) (BlobKey sha size) =
    fmap (\(Value hcid, Value mtid) -> (hcid, mtid)) . listToMaybe <$>
    select
        (from $ \(hc `InnerJoin` pn `InnerJoin` v `InnerJoin` b) -> do
             on (hc ^. HackageCabalCabal ==. b ^. BlobId)
             on (hc ^. HackageCabalVersion ==. v ^. VersionId)
             on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
             where_
                 ((pn ^. PackageNameName ==. val pname) &&.
                  (v ^. VersionVersion ==. val ver) &&.
                  (b ^. BlobSha ==. val sha) &&.
                  (b ^. BlobSize ==. val size))
             return (hc ^. HackageCabalId, hc ^. HackageCabalTree))




getSnapshotPackageId ::
       MonadIO m
    => SnapshotId
    -> PackageIdentifierP
    -> ReaderT SqlBackend m (Maybe SnapshotPackageId)
getSnapshotPackageId snapshotId (PackageIdentifierP pname ver) =
    fmap unValue . listToMaybe <$>
    select
        (from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             where_
                 ((sp ^. SnapshotPackageSnapshot ==. val snapshotId) &&.
                  (pn ^. PackageNameName ==. val pname) &&.
                  (v ^. VersionVersion ==. val ver))
             return (sp ^. SnapshotPackageId))


-- | Add all modules available for the package in a particular snapshot. Initially they are marked
-- as without available documentation.
insertSnapshotPackageModules ::
       MonadIO m => SnapshotPackageId -> [ModuleNameP] -> ReaderT SqlBackend m ()
insertSnapshotPackageModules snapshotPackageId =
    mapM_ $ \modName -> do
        moduleId <- insertModuleSafe modName
        void $ P.insertBy (SnapshotPackageModule snapshotPackageId moduleId False)

-- | Idempotent and thread safe way of adding a new module.
insertModuleSafe :: MonadIO m => ModuleNameP -> ReaderT SqlBackend m ModuleId
insertModuleSafe modName = do
    rawExecute "INSERT INTO module(name) VALUES (?) ON CONFLICT DO NOTHING" [toPersistValue modName]
    mModId <-
        select
            (from $ \m -> do
                 where_ (m ^. ModuleName ==. val modName)
                 return (m ^. ModuleId))
    case mModId of
        [Value modId] -> return modId
        _ -> error $ "Module name: " ++ show modName ++ " should have been inserted by now"


markModuleHasDocs ::
       MonadIO m
    => SnapshotId
    -> PackageIdentifierP
    -> Maybe SnapshotPackageId
    -- ^ If we know ahead of time the SnapshotPackageId it will speed up a great deal if don't have
    -- to look it up in the database.
    -> ModuleNameP
    -> ReaderT SqlBackend m (Maybe SnapshotPackageId)
markModuleHasDocs snapshotId pid mSnapshotPackageId modName =
    maybe (getSnapshotPackageId snapshotId pid) (pure . Just) mSnapshotPackageId >>= \case
        Just snapshotPackageId -> do
            rawExecute
                    "UPDATE snapshot_package_module \
                    \SET has_docs = true \
                    \FROM module \
                    \WHERE module.id = snapshot_package_module.module \
                    \AND module.name = ? \
                    \AND snapshot_package_module.snapshot_package = ?"
                    [toPersistValue modName, toPersistValue snapshotPackageId]
            return $ Just snapshotPackageId
        Nothing -> return Nothing
