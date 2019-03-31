{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

    , getAllPackages
    , getPackagesForSnapshot
    , getPackageVersionForSnapshot

    , getLatests
    , getHackageLatestVersion
    , getSnapshotPackageInfo
    , getSnapshotPackageLatestVersion
    , getSnapshotPackagePageInfo
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

import qualified Data.Aeson as A
import Data.Bifunctor (bimap)
import qualified Data.List as L
import Database.Esqueleto
import Database.Esqueleto.Internal.Language (FromPreprocess)
import Database.Esqueleto.Internal.Sql
import qualified Database.Persist as P
import Pantry.Storage (EntityField(..), PackageName, Unique(..), Version,
                       getPackageNameById, getPackageNameId, getTreeForKey,
                       getVersionId, loadBlobById, treeCabal)
import Pantry.Types (mkSafeFilePath)
import RIO hiding (on, (^.))
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T
import RIO.Time (Day, UTCTime)
import Stackage.Database.PackageInfo
import Stackage.Database.Schema
import Stackage.Database.Types


-- | Construct a pretty title for the snapshot
snapshotTitle :: Snapshot -> Text
snapshotTitle s = snapshotPrettyName (snapshotName s) (snapshotCompiler s)

-- | Get the snapshot from the database.
lookupSnapshot :: GetStackageDatabase env m => SnapName -> m (Maybe (Entity Snapshot))
lookupSnapshot name = run $ getBy $ UniqueSnapshot name

-- | A way to lookup a name of the newest snapshot per type: 'lts', 'lts-x' and 'nightly'. This is
-- used for resolving a snapshot
newestSnapshot :: GetStackageDatabase env m => SnapshotBranch -> m (Maybe SnapName)
newestSnapshot LtsBranch          = fmap (uncurry SNLts) <$> newestLTS
newestSnapshot NightlyBranch      = fmap SNNightly <$> newestNightly
newestSnapshot (LtsMajorBranch x) = fmap (SNLts x) <$> newestLTSMajor x

-- | Get the latest known LTS snapshot
newestLTS :: GetStackageDatabase env m => m (Maybe (Int, Int))
newestLTS =
    run $ liftM (fmap go) $ selectFirst [] [P.Desc LtsMajor, P.Desc LtsMinor]
  where
    go (Entity _ lts) = (ltsMajor lts, ltsMinor lts)

-- | Get the minor version 'y' of latest known LTS snapshot for the major version 'x' in 'lts-x.y'
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

-- | Look up the date 'in the newest nightly snapshot.
newestNightly :: GetStackageDatabase env m => m (Maybe Day)
newestNightly = run $ liftM (fmap $ nightlyDay . entityVal) $ selectFirst [] [P.Desc NightlyDay]

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


getSnapshotModules :: GetStackageDatabase env m => SnapshotId -> m [ModuleListingInfo]
getSnapshotModules sid =
    run $ do
        map toModuleListingInfo <$>
            select
                (from $ \(spm `InnerJoin` m `InnerJoin` sp `InnerJoin` pn `InnerJoin` v) -> do
                     on $ sp ^. SnapshotPackageVersion ==. v ^. VersionId
                     on $ sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId
                     on $ spm ^. SnapshotPackageModuleSnapshotPackage ==. sp ^. SnapshotPackageId
                     on $ spm ^. SnapshotPackageModuleModule ==. m ^. ModuleId
                     where_ $
                         (sp ^. SnapshotPackageSnapshot ==. val sid) &&.
                         (spm ^. SnapshotPackageModuleHasDocs ==. val True)
                     orderBy [asc (m ^. ModuleName), asc (pn ^. PackageNameName)]
                     pure (m ^. ModuleName, pn ^. PackageNameName, v ^. VersionVersion))
  where
    toModuleListingInfo (Value moduleName, Value packageName, Value version) =
        ModuleListingInfo
            { mliModuleName = moduleName
            , mliPackageIdentifier = PackageIdentifierP packageName version
            }

getSnapshotPackageModules
    :: MonadIO m
    => SnapshotPackageId
    -> Bool
    -> ReaderT SqlBackend m [ModuleNameP]
getSnapshotPackageModules snapshotPackageId hasDocs =
    map unValue <$>
    select
        (from $ \(spm `InnerJoin` m) -> do
             on $ spm ^. SnapshotPackageModuleModule ==. m ^. ModuleId
             where_ $
                 (spm ^. SnapshotPackageModuleSnapshotPackage ==. val snapshotPackageId) &&.
                 (spm ^. SnapshotPackageModuleHasDocs ==. val hasDocs)
             orderBy [asc (m ^. ModuleName)]
             pure (m ^. ModuleName))


getAllPackages :: GetStackageDatabase env m => m [(SnapName, PackageListingInfo)]
getAllPackages =
    run (map toPackageListingInfo <$>
         select
             (from $ \(sp `InnerJoin` snap `InnerJoin` pn `InnerJoin` v) ->
                  distinctOn [don (pn ^. PackageNameName)] $ do
                      on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
                      on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
                      on (sp ^. SnapshotPackageSnapshot ==. snap ^. SnapshotId)
                      orderBy
                          [ asc (pn ^. PackageNameName)
                          , desc (versionArray v)
                          , desc (sp ^. SnapshotPackageRevision)
                          , desc (snap ^. SnapshotCreated)
                          ]
                      pure
                          ( snap ^. SnapshotName
                          , pn ^. PackageNameName
                          , v ^. VersionVersion
                          , sp ^. SnapshotPackageSynopsis
                          , sp ^. SnapshotPackageOrigin)))
  where
    toPackageListingInfo (Value snapName, name, version, synopsis, origin) =
        ( snapName
        , PackageListingInfo
              { pliName = unValue name
              , pliVersion = unValue version
              , pliSynopsis = unValue synopsis
              , pliOrigin = unValue origin
              })

getPackagesForSnapshot :: GetStackageDatabase env m => SnapshotId -> m [PackageListingInfo]
getPackagesForSnapshot snapshotId =
    run (map toPackageListingInfo <$>
         select
             (from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
                  on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
                  on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
                  where_ (sp ^. SnapshotPackageSnapshot ==. val snapshotId)
                  orderBy [asc (pn ^. PackageNameName)]
                  pure
                      ( pn ^. PackageNameName
                      , v ^. VersionVersion
                      , sp ^. SnapshotPackageSynopsis
                      , sp ^. SnapshotPackageOrigin)))
  where
    toPackageListingInfo (Value pliName, Value pliVersion, Value pliSynopsis, Value pliOrigin) =
        PackageListingInfo {pliName, pliVersion, pliSynopsis, pliOrigin}


getPackageVersionForSnapshot
  :: GetStackageDatabase env m
  => SnapshotId -> PackageNameP -> m (Maybe VersionP)
getPackageVersionForSnapshot snapshotId pname =
    run $
    selectApplyMaybe
        unValue
        (from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             where_
                 ((sp ^. SnapshotPackageSnapshot ==. val snapshotId) &&.
                  (pn ^. PackageNameName ==. val pname))
             pure (v ^. VersionVersion))

getLatest ::
       (FromPreprocess SqlQuery SqlExpr SqlBackend t, MonadIO m)
    => PackageNameP
    -> (t -> SqlExpr (Value SnapshotId))
    -> (t -> SqlQuery ())
    -> ReaderT SqlBackend m (Maybe LatestInfo)
getLatest pname onWhich orderWhich =
    selectApplyMaybe
        toLatestInfo
        (from $ \(which `InnerJoin` snap `InnerJoin` sp `InnerJoin` pn `InnerJoin` v) -> do
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             on (sp ^. SnapshotPackageSnapshot ==. snap ^. SnapshotId)
             on (snap ^. SnapshotId ==. onWhich which)
             where_ (pn ^. PackageNameName ==. val pname)
             orderWhich which
             limit 1
             pure (snap ^. SnapshotName, v ^. VersionVersion, sp ^. SnapshotPackageRevision))
  where
    toLatestInfo (snapName, ver, mrev) =
        LatestInfo (unValue snapName) $ toVersionMRev (unValue ver) (unValue mrev)


getLatests :: MonadIO m => PackageNameP -> ReaderT SqlBackend m [LatestInfo]
getLatests pname = do
    mLts <-
        getLatest
            pname
            (^. LtsSnap)
            (\lts -> orderBy [desc (lts ^. LtsMajor), desc (lts ^. LtsMinor)])
    mNightly <-
        getLatest
            pname
            (^. NightlySnap)
            (\nightly -> orderBy [desc (nightly ^. NightlyDay)])
    pure $ catMaybes [mLts, mNightly]

-- | Looks up in pantry the latest information about the package on Hackage.
getHackageLatestVersion ::
       MonadIO m => PackageNameP -> ReaderT SqlBackend m (Maybe HackageCabalInfo)
getHackageLatestVersion pname =
    selectApplyMaybe toHackageCabalInfo
        (from $ \(hc `InnerJoin` pn `InnerJoin` v) -> do
             on (hc ^. HackageCabalVersion ==. v ^. VersionId)
             on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
             where_ (pn ^. PackageNameName ==. val pname)
             orderBy [desc (versionArray v), desc (hc ^. HackageCabalRevision)]
             limit 1
             pure $
                 ( hc ^. HackageCabalId
                 , hc ^. HackageCabalCabal
                 , v ^. VersionVersion
                 , hc ^. HackageCabalRevision))
  where
    toHackageCabalInfo (cid, cbid, v, rev) =
        HackageCabalInfo
            { hciCabalId = unValue cid
            , hciCabalBlobId = unValue cbid
            , hciPackageName = pname
            , hciVersionRev = toVersionRev (unValue v) (unValue rev)
            }


getSnapshotPackageInfo ::
       GetStackageDatabase env m => SnapName -> PackageNameP -> m (Maybe SnapshotPackageInfo)
getSnapshotPackageInfo snapName pname =
    fmap snd . listToMaybe <$>
    run (snapshotPackageInfoQuery $ \_sp s pn _v spiQ -> do
             where_ ((s ^. SnapshotName ==. val snapName) &&. (pn ^. PackageNameName ==. val pname))
             pure ((), spiQ))


getSnapshotPackagePageInfo ::
       GetStackageDatabase env m => SnapshotPackageInfo -> Int -> m SnapshotPackagePageInfo
getSnapshotPackagePageInfo spi maxDisplayedDeps =
    run $ do
        mhciLatest <-
            case spiOrigin spi of
                Hackage -> getHackageLatestVersion $ spiPackageName spi
                _       -> pure Nothing
        forwardDepsCount <- getForwardDepsCount spi
        reverseDepsCount <- getReverseDepsCount spi
        forwardDeps <-
            if forwardDepsCount > 0
                then getForwardDeps spi (Just maxDisplayedDeps)
                else pure []
        reverseDeps <-
            if reverseDepsCount > 0
                then getReverseDeps spi (Just maxDisplayedDeps)
                else pure []
        latestInfo <- getLatests (spiPackageName spi)
        moduleNames <- getModuleNames (spiSnapshotPackageId spi)
        pure
            SnapshotPackagePageInfo
                { sppiSnapshotPackageInfo = spi
                , sppiLatestHackageCabalInfo = mhciLatest
                , sppiForwardDeps = map (first dropVersionRev) forwardDeps
                , sppiForwardDepsCount = forwardDepsCount
                , sppiReverseDeps = map (first dropVersionRev) reverseDeps
                , sppiReverseDepsCount = reverseDepsCount
                , sppiLatestInfo = latestInfo
                , sppiModuleNames = moduleNames
                , sppiVersion =
                      listToMaybe
                          [ spiVersionRev spi
                          | VersionRev ver mrev <-
                                maybe [] (pure . hciVersionRev) mhciLatest ++
                                map liVersionRev latestInfo
                          , ver > curVer ||
                                (ver == curVer &&
                                 fromMaybe (Revision 0) mrev > fromMaybe (Revision 0) mcurRev)
                          ]
                }
  where
    VersionRev curVer mcurRev = spiVersionRev spi

type SqlExprSPI
     = ( SqlExpr (Value SnapshotPackageId)
       , SqlExpr (Value SnapshotId)
       , SqlExpr (Value SnapName)
       , SqlExpr (Value PackageNameP)
       , SqlExpr (Value (Maybe BlobId))
       , SqlExpr (Value VersionP)
       , SqlExpr (Value (Maybe Revision))
       , SqlExpr (Value Origin)
       , SqlExpr (Value (Maybe TreeEntryId))
       , SqlExpr (Value (Maybe TreeEntryId))
       )

snapshotPackageInfoQuery ::
       (MonadIO m, SqlSelect a b)
    => (   SqlExpr (Entity SnapshotPackage)
        -> SqlExpr (Entity Snapshot)
        -> SqlExpr (Entity PackageName)
        -> SqlExpr (Entity Version)
        -> SqlExprSPI
        -> SqlQuery ( a , SqlExprSPI)
       )
    -> ReaderT SqlBackend m [(b, SnapshotPackageInfo)]
snapshotPackageInfoQuery customize =
    fmap (\(extraValue, spiValues) -> (extraValue, toSnapshotPackageInfo spiValues)) <$>
    select
        (from $ \(sp `InnerJoin` s `InnerJoin` pn `InnerJoin` v) -> do
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             on (sp ^. SnapshotPackageSnapshot ==. s ^. SnapshotId)
             customize sp s pn v $
                 ( sp ^. SnapshotPackageId
                 , s  ^. SnapshotId
                 , s  ^. SnapshotName
                 , pn ^. PackageNameName
                 , sp ^. SnapshotPackageCabal
                 , v  ^. VersionVersion
                 , sp ^. SnapshotPackageRevision
                 , sp ^. SnapshotPackageOrigin
                 , sp ^. SnapshotPackageReadme
                 , sp ^. SnapshotPackageChangelog))
  where
    toSnapshotPackageInfo (spid, sid, sn, pn, spc, v, spr, spo, rm, cl) =
        SnapshotPackageInfo
            { spiSnapshotPackageId = unValue spid
            , spiSnapshotId = unValue sid
            , spiCabalBlobId = unValue spc
            , spiSnapName = unValue sn
            , spiPackageName = unValue pn
            , spiVersion = unValue v
            , spiRevision = unValue spr
            , spiOrigin = unValue spo
            , spiReadme = unValue rm
            , spiChangelog = unValue cl
            }


getSnapshotPackageLatestVersion ::
       GetStackageDatabase env m
    => PackageNameP
    -> m (Maybe SnapshotPackageInfo)
getSnapshotPackageLatestVersion pname =
    fmap snd . listToMaybe <$>
    run (snapshotPackageInfoQuery $ \_sp s pn v spiQ -> do
             where_ (pn ^. PackageNameName ==. val pname)
             orderBy
                 [ desc (stringToArray (v ^. VersionVersion) (val ("." :: String)))
                 , desc (s ^. SnapshotCreated)
                 ]
             limit 1
             pure ((), spiQ))


-- | A helper function that expects at most one element to be returned by a `select` and applies a
-- function to the returned result
selectApplyMaybe ::
       (SqlSelect a b, MonadIO m) => (b -> r) -> SqlQuery a -> ReaderT SqlBackend m (Maybe r)
selectApplyMaybe f = fmap (fmap f . listToMaybe) . select


-- | Convert a string representation of a version to an array so it can be used for sorting.
versionArray :: SqlExpr (Entity Version) -> SqlExpr (Value [Int64])
versionArray v = stringToArray (v ^. VersionVersion) (val ("." :: String))

-- | Define postgresql native function in Haskell with Esqueleto
stringToArray ::
       (SqlString s1, SqlString s2)
    => SqlExpr (Value s1)
    -> SqlExpr (Value s2)
    -> SqlExpr (Value [Int64])
stringToArray s1 s2 = unsafeSqlFunction "string_to_array" (s1, s2)

getSnapshotsForPackage
    :: GetStackageDatabase env m
    => PackageNameP
    -> Maybe Int
    -> m [(CompilerP, SnapshotPackageInfo)]
getSnapshotsForPackage pname mlimit =
    fmap (first unValue) <$>
    run (snapshotPackageInfoQuery $ \_sp s pn _v spiQ -> do
             where_ (pn ^. PackageNameName ==. val pname)
             orderBy [desc (s ^. SnapshotCreated)]
             forM_ mlimit (limit . fromIntegral)
             pure (s ^. SnapshotCompiler, spiQ))



getPackageInfo ::
       GetStackageDatabase env m => Either HackageCabalInfo SnapshotPackageInfo -> m PackageInfo
getPackageInfo (Left hci) =
    run $ do
        cabalBlob <- loadBlobById (hciCabalBlobId hci)
        pure $ toPackageInfo (parseCabalBlob cabalBlob) Nothing Nothing
getPackageInfo (Right spi) =
    run $
    case spiCabalBlobId spi of
        Just cabalBlobId -> do
            gpd <- parseCabalBlob <$> loadBlobById cabalBlobId
            mreadme <- maybe (pure Nothing) getFileByTreeEntryId (spiReadme spi)
            mchangelog <- maybe (pure Nothing) getFileByTreeEntryId (spiChangelog spi)
            pure $
                toPackageInfo
                    gpd
                    (toContentFile Readme <$> mreadme)
                    (toContentFile Changelog <$> mchangelog)
        Nothing -> error "FIXME: handle a case when cabal file isn't available but package.yaml is"
  where
    toContentFile :: (ByteString -> Bool -> a) -> (SafeFilePath, ByteString) -> a
    toContentFile con (path, bs) = con bs (isMarkdownFilePath path)

getFileByTreeEntryId ::
       (MonadIO m)
    => TreeEntryId
    -> ReaderT SqlBackend m (Maybe (SafeFilePath, ByteString))
getFileByTreeEntryId teid =
    selectApplyMaybe (bimap unValue unValue) $
    from $ \(te `InnerJoin` fp `InnerJoin` b) -> do
        on $ te ^. TreeEntryBlob ==. b ^. BlobId
        on $ te ^. TreeEntryPath ==. fp ^. FilePathId
        where_ $ te ^. TreeEntryId ==. val teid
        pure (fp ^. FilePathPath, b ^. BlobContents)

getModuleNames :: (MonadIO m) => SnapshotPackageId -> ReaderT SqlBackend m [ModuleNameP]
getModuleNames spid =
    map unValue <$>
    select
        (from $ \(spm `InnerJoin` pm) -> do
             on (spm ^. SnapshotPackageModuleModule ==. pm ^. ModuleId)
             where_ (spm ^. SnapshotPackageModuleSnapshotPackage ==. val spid)
             orderBy [desc (pm ^. ModuleName)]
             pure (pm ^. ModuleName))

------ Dependencies

getForwardDeps ::
       MonadIO m
    => SnapshotPackageInfo
    -> Maybe Int
    -> ReaderT SqlBackend m [(PackageVersionRev, VersionRangeP)]
getForwardDeps spi mlimit =
    fmap toDepRange <$>
    select
        (from $ \(user `InnerJoin` uses `InnerJoin` pn `InnerJoin` v) -> do
             on (uses ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (uses ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             on (user ^. DepUses ==. uses ^. SnapshotPackagePackageName)
             where_ $
                 (user ^. DepUser ==. val (spiSnapshotPackageId spi)) &&.
                 (uses ^. SnapshotPackageSnapshot ==. val (spiSnapshotId spi))
             orderBy [desc (pn ^. PackageNameName)]
             maybe (pure ()) (limit . fromIntegral) mlimit
             pure
                 ( pn ^. PackageNameName
                 , v ^. VersionVersion
                 , uses ^. SnapshotPackageRevision
                 , user ^. DepRange))
  where
    toDepRange (pn, v, rev, range) =
        (PackageVersionRev (unValue pn) (toVersionMRev (unValue v) (unValue rev)), unValue range)


getForwardDepsCount :: MonadIO m => SnapshotPackageInfo -> ReaderT SqlBackend m Int
getForwardDepsCount spi = P.count [DepUser P.==. spiSnapshotPackageId spi]

getReverseDepsCount :: MonadIO m => SnapshotPackageInfo -> ReaderT SqlBackend m Int
getReverseDepsCount spi =
    fromMaybe 0 <$>
    selectApplyMaybe unValue
        (from $ \(sp `InnerJoin` dep `InnerJoin` curPn) -> do
             on (dep ^. DepUses ==. curPn ^. PackageNameId)
             on (sp ^. SnapshotPackageId ==. dep ^. DepUser)
             where_ $
                 (curPn ^. PackageNameName ==. val (spiPackageName spi)) &&.
                 (sp ^. SnapshotPackageSnapshot ==. val (spiSnapshotId spi))
             pure countRows)

getDepsCount :: GetStackageDatabase env m => SnapshotPackageInfo -> m (Int, Int)
getDepsCount spi =
    run $
    (,) <$> getForwardDepsCount spi <*>
    getReverseDepsCount spi

getReverseDeps ::
       MonadIO m
    => SnapshotPackageInfo
    -> Maybe Int -- ^ Optionally limit number of dependencies
    -> ReaderT SqlBackend m [(PackageVersionRev, VersionRangeP)]
getReverseDeps spi mlimit =
    fmap toDepRange <$>
    select
        (from $ \(sp `InnerJoin` dep `InnerJoin` pn `InnerJoin` v `InnerJoin` curPn) -> do
             on (dep ^. DepUses ==. curPn ^. PackageNameId)
             on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
             on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
             on (sp ^. SnapshotPackageId ==. dep ^. DepUser)
             where_ $
                 (curPn ^. PackageNameName ==. val (spiPackageName spi)) &&.
                 (sp ^. SnapshotPackageSnapshot ==. val (spiSnapshotId spi))
             orderBy [desc (pn ^. PackageNameName)]
             maybe (pure ()) (limit . fromIntegral) mlimit
             pure
                 ( pn ^. PackageNameName
                 , v ^. VersionVersion
                 , sp ^. SnapshotPackageRevision
                 , dep ^. DepRange))
  where
    toDepRange (pn, v, rev, range) =
        (PackageVersionRev (unValue pn) (toVersionMRev (unValue v) (unValue rev)), unValue range)




----- Deprecated

-- | See if a package is deprecated on hackage and in favour of which packages.
getDeprecated :: GetStackageDatabase env m => PackageNameP -> m (Bool, [PackageNameP])
getDeprecated pname =
    run $
    lookupPackageNameId pname >>= \case
        Just pnid ->
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
    -> Origin
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
        mTreeId = entityKey <$> mTree
    packageNameId <-
        maybe (getPackageNameId (unPackageNameP pname)) (pure . treeName . entityVal) mTree
    versionId <- maybe (getVersionId (unVersionP pver)) (pure . treeVersion . entityVal) mTree
    mrevision <- maybe (pure Nothing) getHackageRevision mHackageCabalId
    mreadme <- fromMaybe (pure Nothing) $ getContentTreeEntryId <$> mTreeId <*> mreadmeQuery
    mchangelog <- fromMaybe (pure Nothing) $ getContentTreeEntryId <$> mTreeId <*> mchangelogQuery
    let snapshotPackage =
            SnapshotPackage
                { snapshotPackageSnapshot = snapshotId
                , snapshotPackagePackageName = packageNameId
                , snapshotPackageVersion = versionId
                , snapshotPackageRevision = mrevision
                , snapshotPackageCabal = treeCabal . entityVal <$> mTree
                , snapshotPackageTreeBlob = treeKey . entityVal <$> mTree
                , snapshotPackageOrigin = origin
                , snapshotPackageOriginUrl = "" -- TODO: add
                , snapshotPackageSynopsis = getSynopsis gpd
                , snapshotPackageReadme = mreadme
                , snapshotPackageChangelog = mchangelog
                , snapshotPackageIsHidden = isHidden
                , snapshotPackageFlags = flags
                }
    snapshotPackageId <- keyInsertBy snapshotPackage
    -- TODO: collect all missing dependencies
    _ <- insertDeps pid snapshotPackageId (extractDependencies compiler flags gpd)
    insertSnapshotPackageModules snapshotPackageId (extractModuleNames gpd)

getContentTreeEntryId ::
       (MonadIO m)
    => TreeId
    -> (SqlExpr (Value SafeFilePath) -> SqlExpr (Value Bool))
    -> ReaderT SqlBackend m (Maybe TreeEntryId)
getContentTreeEntryId treeId filePathQuery = do
    (mteid, _isMarkdown) <- foldl' preferMarkdown (Nothing, False) <$>
      select
          (from $ \(te `InnerJoin` p) -> do
               on $ te ^. TreeEntryPath ==. p ^. FilePathId
               where_ $ (te ^. TreeEntryTree ==. val treeId) &&. filePathQuery (p ^. FilePathPath)
               pure (p ^. FilePathPath, te ^. TreeEntryId))
    pure mteid
  where preferMarkdown (_, False) (Value path, Value teid) = (Just teid, isMarkdownFilePath path)
        preferMarkdown pref@(_, True) _ = pref

mchangelogQuery :: Maybe (SqlExpr (Value SafeFilePath) -> SqlExpr (Value Bool))
mchangelogQuery = do
  changelog <- mkSafeFilePath "changelog."
  changes <- mkSafeFilePath "changes."
  pure (\ path -> (path `ilike` val changelog ++. (%)) ||. (path `ilike` val changes  ++. (%)))

mreadmeQuery :: Maybe (SqlExpr (Value SafeFilePath) -> SqlExpr (Value Bool))
mreadmeQuery = do
  readme <- mkSafeFilePath "readme."
  pure (\ path -> path `ilike` val readme ++. (%))

getHackageRevision :: MonadIO m => HackageCabalId -> ReaderT SqlBackend m (Maybe Revision)
getHackageRevision hcid =
    selectApplyMaybe unValue $
    from $ \hc -> do
        where_ (hc ^. HackageCabalId ==. val hcid)
        pure (hc ^. HackageCabalRevision)


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
    selectApplyMaybe (\(Value hcid, Value bid, Value mtid) -> (hcid, bid, mtid)) $
    from $ \(hc `InnerJoin` pn `InnerJoin` v) -> do
        on (hc ^. HackageCabalVersion ==. v ^. VersionId)
        on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
        where_
            ((pn ^. PackageNameName ==. val pname) &&. (v ^. VersionVersion ==. val ver) &&.
             (hc ^. HackageCabalRevision ==. val (fromMaybe (Revision 0) mrev)))
        return (hc ^. HackageCabalId, hc ^. HackageCabalCabal, hc ^. HackageCabalTree)

-- | This query will return `Nothing` if the tarball for the hackage cabal file hasn't been loaded
-- yet.
getHackageCabalByKey ::
       MonadIO m
    => PackageIdentifierP
    -> BlobKey
    -> ReaderT SqlBackend m (Maybe (HackageCabalId, Maybe TreeId))
getHackageCabalByKey (PackageIdentifierP pname ver) (BlobKey sha size) =
    selectApplyMaybe (\(Value hcid, Value mtid) -> (hcid, mtid)) $
    from $ \(hc `InnerJoin` pn `InnerJoin` v `InnerJoin` b) -> do
        on (hc ^. HackageCabalCabal ==. b ^. BlobId)
        on (hc ^. HackageCabalVersion ==. v ^. VersionId)
        on (hc ^. HackageCabalName ==. pn ^. PackageNameId)
        where_
            ((pn ^. PackageNameName ==. val pname) &&. (v ^. VersionVersion ==. val ver) &&.
             (b ^. BlobSha ==. val sha) &&.
             (b ^. BlobSize ==. val size))
        return (hc ^. HackageCabalId, hc ^. HackageCabalTree)


getSnapshotPackageId ::
       MonadIO m
    => SnapshotId
    -> PackageIdentifierP
    -> ReaderT SqlBackend m (Maybe SnapshotPackageId)
getSnapshotPackageId snapshotId (PackageIdentifierP pname ver) =
    selectApplyMaybe unValue $
    from $ \(sp `InnerJoin` pn `InnerJoin` v) -> do
        on (sp ^. SnapshotPackageVersion ==. v ^. VersionId)
        on (sp ^. SnapshotPackagePackageName ==. pn ^. PackageNameId)
        where_
            ((sp ^. SnapshotPackageSnapshot ==. val snapshotId) &&.
             (pn ^. PackageNameName ==. val pname) &&.
             (v ^. VersionVersion ==. val ver))
        return (sp ^. SnapshotPackageId)


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
        select $
        from $ \m -> do
            where_ (m ^. ModuleName ==. val modName)
            return (m ^. ModuleId)
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
