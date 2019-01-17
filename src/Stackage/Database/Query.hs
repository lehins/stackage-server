{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Stackage.Database.Query
    ( getLatestLtsByGhc
    , loadBlobById
    , getTreeForKey
    , treeCabal
    -- * Needed for Cron Job
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

import qualified Data.List                as L
import           Database.Esqueleto
import qualified Database.Persist         as P
import           Pantry.Storage           (EntityField (..), Unique (..),
                                           getPackageNameId, getTreeForKey,
                                           getVersionId, loadBlobById,
                                           treeCabal)
import           RIO                      hiding (on, (^.))
import qualified RIO.Map                  as Map
import qualified RIO.Set                  as Set
import           RIO.Time                 (Day, UTCTime)
import           Stackage.Database.Schema
import           Stackage.Database.Types
import           Stackage.Database.PackageInfo


snapshotMarkUpdated :: GetStackageDatabase env m => SnapshotId -> UTCTime -> m ()
snapshotMarkUpdated snapKey updatedOn =
    run $ P.update snapKey [SnapshotUpdatedOn P.=. Just updatedOn]

insertSnapshotName :: GetStackageDatabase env m => SnapshotId -> SnapName -> m ()
insertSnapshotName snapKey snapName =
    run $
    case snapName of
        SNLts major minor -> void $ insertUnique $ Lts snapKey major minor
        SNNightly day     -> void $ insertUnique $ Nightly snapKey day


getLatestLtsByGhc :: GetStackageDatabase env m
                  => m [(Int, Int, Text, Day)]
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

-- getHackageCabalId ::
--        MonadIO m
--     => PackageIdentifierP
--     -> BlobKey
--     -> ReaderT SqlBackend m (Maybe HackageCabalId)
-- getHackageCabalId (PackageIdentifierP pname ver) (BlobKey sha size) =
--     fmap unValue . listToMaybe <$>
--     select
--         (from $ \(hc `InnerJoin` pn `InnerJoin` v `InnerJoin` b) -> do
--              on (hc ^. HackageCabalCabal ==. b ^. BlobId)
--              on (hc ^. HackageCabalVersion ==. v ^. VersionId)
--              on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
--              where_
--                  ((pn ^. PackageNameName ==. val pname) &&.
--                   (v ^. VersionVersion ==. val ver) &&.
--                   (b ^. BlobSha ==. val sha) &&.
--                   (b ^. BlobSize ==. val size))
--              return (hc ^. HackageCabalId))




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
    -- eExc <- tryAny $ run $ P.insert_ $ Module modName
    -- mModId <-
    --     run
    --         (select
    --              (from $ \m -> do
    --                   where_ (m ^. ModuleName ==. val modName)
    --                   return (m ^. ModuleId)))
    -- case mModId of
    --     [Value modId] -> return modId
    --     _
    --         | Left exc <- eExc -> throwM exc
    --     _ -> error $ "Module name: " ++ show modName ++ " should have been inserted by now"


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
