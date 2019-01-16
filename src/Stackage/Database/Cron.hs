{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Stackage.Database.Cron
    ( stackageServerCron
    , newHoogleLocker
    , singleRun
    ) where

import qualified Codec.Archive.Tar               as Tar
import           Conduit
import           Control.Lens                    ((.~))
import qualified Control.Monad.Trans.AWS         as AWS (paginate)
import           Control.SingleRun
import qualified Data.ByteString.Char8           as BS8
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Lazy.Char8      as LBS8
import qualified Data.Conduit.Binary             as CB
import qualified Data.IntMap.Strict              as IntMap
import           Data.Conduit.Zlib               (WindowBits (WindowBits),
                                                  compress, ungzip)
import           Data.Streaming.Network          (bindPortTCP)
import           Database.Persist
import           Database.Persist.Postgresql
import           Distribution.PackageDescription (GenericPackageDescription)
import qualified Hoogle
import           Network.AWS                     hiding (Request, Response)
import           Network.AWS.Data.Body           (toBody)
import           Network.AWS.Data.Log            (build)
import           Network.AWS.Data.Text           (toText)
import           Network.AWS.Pager
import           Network.AWS.S3
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit     (bodyReaderSource)
import           Network.HTTP.Simple             (getResponseBody,
                                                  httpJSONEither, parseRequest)
import           Network.HTTP.Types              (status200)
import           Pantry                          (defaultHackageSecurityConfig)
import           Pantry.Hackage                  (DidUpdateOccur (..),
                                                  forceUpdateHackageIndex,
                                                  getHackageTarballOnGPD)
import           Pantry.Storage                  (HackageCabalId, TreeId,
                                                  getTreeForKey, loadBlobById,
                                                  treeCabal)
import           Pantry.Types                    (HpackExecutable (HpackBundled),
                                                  PantryConfig (..))
import           Path                            (parseAbsDir, toFilePath)
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Map                         as Map
import           RIO.Process                     (mkDefaultProcessContext)
import qualified RIO.Set                         as Set
import qualified RIO.Text                        as T
import           RIO.Time
import           Stackage.Database.Github
import           Stackage.Database.PackageInfo
import           Stackage.Database.Query
import           Stackage.Database.Schema
import           Stackage.Database.Types
import           System.Environment              (getEnv)
import           Types
import           UnliftIO.Async                  (pooledMapConcurrently)
import           UnliftIO.Concurrent             (getNumCapabilities)
import           Web.PathPieces                  (fromPathPiece, toPathPiece)

import           Data.Yaml                       (decodeFileEither)
import           Pantry.Types                    (CabalFileInfo (..),
                                                  HasPantryConfig (..),
                                                  PackageIdentifierRevision (..),
                                                  packageTreeKey)



hoogleKey :: SnapName -> Text
hoogleKey name = T.concat
    [ "hoogle/"
    , toPathPiece name
    , "/"
    , VERSION_hoogle
    , ".hoo"
    ]

hoogleUrl :: SnapName -> Text
hoogleUrl n = T.concat
    [ "https://s3.amazonaws.com/"
    , haddockBucketName
    , "/"
    , hoogleKey n
    ]


hackageDeprecatedUrl :: Request
hackageDeprecatedUrl = "https://hackage.haskell.org/packages/deprecated.json"

initStorage :: Int -> IO StackageDatabase
initStorage poolSize = do
    connstr <- encodeUtf8 . T.pack <$> getEnv "PGSTRING"
    openStackageDatabase PostgresConf {pgPoolSize = poolSize, pgConnStr = connstr}


getStackageSnapshotsDir :: RIO StackageCron FilePath
getStackageSnapshotsDir = do
    rootDir <- scStackageRoot <$> ask
    cloneOrUpdate rootDir "commercialhaskell" "stackage-snapshots"


withResponseUnliftIO :: MonadUnliftIO m =>
                 Request -> Manager -> (Response BodyReader -> m b) -> m b
withResponseUnliftIO req man f = withRunInIO $ \ run -> withResponse req man (run . f)

newHoogleLocker ::
       (HasLogFunc env, MonadIO m) => env -> Manager -> m (SingleRun SnapName (Maybe FilePath))
newHoogleLocker env man = do
    mkSingleRun hoogleLocker
  where
    hoogleLocker :: MonadIO m => SnapName -> m (Maybe FilePath)
    hoogleLocker name =
        runRIO env $ do
            let fp = T.unpack $ hoogleKey name
                fptmp = fp <.> "tmp"
            exists <- doesFileExist fp
            if exists
                then return $ Just fp
                else do
                    req' <- parseRequest $ T.unpack $ hoogleUrl name
                    let req = req' {decompress = const False}
                    withResponseUnliftIO req man $ \res ->
                        if responseStatus res == status200
                            then do
                                createDirectoryIfMissing True $ takeDirectory fptmp
                                runConduitRes $
                                    bodyReaderSource (responseBody res) .| ungzip .| sinkFile fptmp
                                renamePath fptmp fp
                                return $ Just fp
                            else do
                                return Nothing


getHackageDeprecations ::
       (HasLogFunc env, MonadReader env m, MonadIO m) => m [Deprecation]
getHackageDeprecations = do
    jsonResponseDeprecated <- httpJSONEither hackageDeprecatedUrl
    case getResponseBody jsonResponseDeprecated of
        Left err -> do
            logError $
                "There was an error parsing deprecated.json file: " <>
                fromString (displayException err)
            return []
        Right deprecated -> return deprecated


stackageServerCron :: Bool -> IO ()
stackageServerCron forceUpdate = do
    -- Hacky approach instead of PID files
    _ <- liftIO $ catchIO (bindPortTCP 17834 "127.0.0.1") $ \_ ->
        error $ "cabal loader process already running, exiting"
    --let connectionCount = 2
    connectionCount <- min 4 <$> getNumCapabilities
    storage <- initStorage connectionCount
    lo <- logOptionsHandle stdout True
    stackageRootDir <- getAppUserDataDirectory "stackage"
    pantryRootDir <- parseAbsDir (stackageRootDir </> "pantry")
    createDirectoryIfMissing True (toFilePath pantryRootDir)
    updateRef <- newMVar True
    cabalImmutable <- newIORef (error "Damn it is used") -- TODO: Map.empty
    cabalMutable <- newIORef Map.empty
    gpdCache <- newIORef IntMap.empty
    defaultProcessContext <- mkDefaultProcessContext
    aws <- newEnv Discover
    withLogFunc (setLogMinLevel LevelInfo lo) $ \ logFunc ->
      let pantryConfig = PantryConfig { pcHackageSecurity = defaultHackageSecurityConfig
                                      , pcHpackExecutable = HpackBundled
                                      , pcRootDir = pantryRootDir
                                      , pcStorage = storage
                                      , pcUpdateRef = updateRef
                                      , pcParsedCabalFilesImmutable = cabalImmutable
                                      , pcParsedCabalFilesMutable = cabalMutable
                                      , pcConnectionCount = connectionCount
                                      }
          stackage = StackageCron { scPantryConfig = pantryConfig
                                  , scStackageRoot = stackageRootDir
                                  , scProcessContext = defaultProcessContext
                                  , scLogFunc = logFunc
                                  , scForceFullUpdate = forceUpdate
                                  , scCachedGPD = gpdCache
                                  , scEnvAWS = aws
                                  }
      in runRIO stackage runStackageUpdate


runStackageUpdate :: RIO StackageCron ()
runStackageUpdate = do
    forceFullUpdate <- scForceFullUpdate <$> ask
    logInfo $ "Starting stackage-cron update" <> bool "" " with --force-update" forceFullUpdate
    runStackageMigrations
    didUpdate <- forceUpdateHackageIndex (Just "stackage-server cron job")
    case didUpdate of
        UpdateOccurred -> do
            logInfo "Updated hackage index. Getting deprecated info now"
            getHackageDeprecations >>= run . mapM_ addDeprecated
        NoUpdateOccurred -> do
            logInfo "No new packages in hackage index"
    corePackageGetters <- makeCorePackageGetters
    finalAction <-
        runConduitRes $
        sourceSnapshots .|
        foldMC (createOrUpdateSnapshot corePackageGetters) (pure ())
    runResourceT finalAction
    run $ mapM_ (flip rawExecute []) ["COMMIT", "VACUUM", "BEGIN"]



makeCorePackageGetters ::
       RIO StackageCron (Map CompilerP [CorePackageGetter])
makeCorePackageGetters = do
    rootDir <- scStackageRoot <$> ask
    contentDir <- getStackageContentDir rootDir
    liftIO (decodeFileEither (contentDir </> "stack" </> "global-hints.yaml")) >>= \case
        Right (hints :: Map CompilerP (Map PackageNameP VersionP)) ->
            Map.traverseWithKey
                (\compiler ->
                     fmap Map.elems . Map.traverseMaybeWithKey (makeCorePackageGetter compiler))
                hints
        Left exc -> do
            logError $
                "Error parsing 'global-hints.yaml' file: " <> fromString (displayException exc)
            return mempty

-- | Core package info rarely changes between the snapshots, therefore it would be wasteful to
-- load, parse and update all packages from gloabl-hints for each snapshot, instead we produce
-- a memoized version that will do it once initiall and then return information aboat a
-- package on subsequent invocations.
makeCorePackageGetter ::
       CompilerP -> PackageNameP -> VersionP -> RIO StackageCron (Maybe CorePackageGetter)
makeCorePackageGetter _compiler pname ver = do
    run (getHackageCabalRev0 pid) >>= \case
        Nothing -> do
            logError $
                "Core package from global-hints: '" <> display pid <> "' was not found in pantry."
            -- TODO: possibly try getting from gitlab, eg:
            --       https://gitlab.haskell.org/ghc/ghc/raw/ghc-7.8.4-release/
            --             libraries/bin-package-db/bin-package-db.cabal
            pure Nothing
        Just (hackageCabalId, blobId, _) -> do
            pkgInfoRef <- newIORef Nothing
            let onGPD treeId gpd = do
                    mTree <- run $ getEntity treeId
                    writeIORef pkgInfoRef $ Just (mTree, Just hackageCabalId, pid, gpd)
                getMemoPackageInfo =
                    readIORef pkgInfoRef >>= \case
                        Just pkgInfo -> return pkgInfo
                        Nothing -> do
                            logSticky $ "Loading core package: " <> display pid
                            pkg <- getHackageTarballOnGPD onGPD pir Nothing
                            -- If full package is already in pantry onGPD will not get invoked
                            -- therefore we might need to parse cabal file ourselves below
                            readIORef pkgInfoRef >>= \case
                                Nothing -> do
                                    gpd <- parseCabalBlob <$> run (loadBlobById blobId)
                                    mTree <- run $ getTreeForKey $ packageTreeKey pkg
                                    let pkgInfo = (mTree, Just hackageCabalId, pid, gpd)
                                    writeIORef pkgInfoRef $ Just pkgInfo
                                    pure pkgInfo
                                Just pkgInfo -> pure pkgInfo
            pure $ Just getMemoPackageInfo
  where
    pid = PackageIdentifierP pname ver
    pir =
        PackageIdentifierRevision (unPackageNameP pname) (unVersionP ver) (CFIRevision (Revision 0))


-- TODO: for now it is only from hackage, PantryPackage needs an update
addPantryPackage ::
       SnapshotId -> CompilerP -> Bool -> Map FlagNameP Bool -> PantryPackage -> RIO StackageCron Bool
addPantryPackage sid compiler isHidden flags (PantryPackage pc treeKey) = do
    hasAddedPackageRef <- newIORef False
    gpdCachedRef <- scCachedGPD <$> ask
    let blobKeyToInt = fromIntegral . unSqlBackendKey . unBlobKey
    let updateGPDcache blobId gpd =
            atomicModifyIORef' gpdCachedRef (\cacheMap -> (IntMap.insert blobId gpd cacheMap, gpd))
    let getGPD treeCabal =
            \case
                Just gpd -> updateGPDcache (blobKeyToInt treeCabal) gpd
                Nothing -> do
                    cacheMap <- readIORef gpdCachedRef
                    case IntMap.lookup (blobKeyToInt treeCabal) cacheMap of
                        Just gpd -> pure gpd
                        Nothing ->
                            fmap parseCabalBlob (loadBlobById treeCabal) >>=
                            updateGPDcache (blobKeyToInt treeCabal)
    let onGPD mtid mgpd = do
            getTreeForKey treeKey >>= \case
                mTree@(Just (Entity treeId Tree {treeName, treeVersion, treeCabal}))
                    | maybe True (== treeId) mtid -> do
                        mhcid <-
                            fmap entityKey <$>
                            getBy (UniqueHackage treeName treeVersion (Revision 0))
                        gpd <- getGPD treeCabal mgpd
                        addSnapshotPackage sid compiler Hackage mTree mhcid isHidden flags pid gpd
                        writeIORef hasAddedPackageRef True
                _ -> lift $ logError $ "Unexpected problem loading the tree for " <> display pid
    _pkg <-
        getHackageTarballOnGPD
            (\treeId gpd -> run $ onGPD (Just treeId) (Just gpd))
            (toPackageIdentifierRevision pc)
            (Just treeKey)
    unlessM (readIORef hasAddedPackageRef) $ run (onGPD Nothing Nothing)
    readIORef hasAddedPackageRef
  where
    pid = PackageIdentifierP (pcPackageName pc) (pcPackageVersion pc)




-- | Download a list of available .html files from S3 bucket for a particular resolver and record
-- in the database which modules have documentation available for them.
checkForDocs :: SnapshotId -> SnapName -> ResourceT (RIO StackageCron) ()
checkForDocs snapshotId snapName = do
    mods <- runConduit $
        AWS.paginate req .| concatMapC (^. lovrsContents) .| mapC (\obj -> toText (obj ^. oKey)) .|
        concatMapC (T.stripSuffix ".html") .|
        concatMapC (T.stripPrefix prefix) .|
        concatMapC pathToPackageModule .|
        sinkList
    (_, notFoundSet) <- lift $ foldM markModules (Map.empty, Set.empty) mods
    forM_ notFoundSet $ \pid ->
        lift $
        logError $
        "Documentation available for package '" <> display pid <>
        "' but was not found in this snapshot: " <>
        display snapName
  where
    prefix = textDisplay snapName <> "/"
    req = listObjectsV2 (BucketName haddockBucketName) & lovPrefix .~ Just prefix
    -- It takes too long for each package to be looked up for each module in the db, so we cache
    -- the ids for snapshot packages as well as record all packages that have documentation
    -- available, but not found in the snapshot.
    markModules (snapshotPackageIds, notFoundSet) (pid, modName) = do
        let mSnapshotPackageId = Map.lookup pid snapshotPackageIds
        mFound <- run $ markModuleHasDocs snapshotId pid mSnapshotPackageId modName
        pure $
            case mFound of
                Nothing -> (snapshotPackageIds, Set.insert pid notFoundSet)
                Just snapshotPackageId
                    | Nothing <- mSnapshotPackageId ->
                        (Map.insert pid snapshotPackageId snapshotPackageIds, notFoundSet)
                _ -> (snapshotPackageIds, notFoundSet)



sourceSnapshots ::
       ConduitT a (SnapName, UTCTime, RIO StackageCron (Maybe SnapshotFile)) (ResourceT (RIO StackageCron)) ()
sourceSnapshots = do
    snapshotsDir <- lift $ lift getStackageSnapshotsDir
    sourceDirectoryDeep False (snapshotsDir </> "lts") .| concatMapMC (getLtsParser snapshotsDir)
    sourceDirectoryDeep False (snapshotsDir </> "nightly") .|
        concatMapMC (getNightlyParser snapshotsDir)
  where
    getSnapshotParser gitDir fp mCreateDate snapName = do
        let parseSnapshot updatedOn = do
                esnap <- liftIO $ decodeFileEither fp
                case esnap of
                    Right snap
                        | snapName /= sfName snap -> do
                            logError $
                                "Snapshot name mismath. Received: " <> displayShow (sfName snap) <>
                                " in the file: " <>
                                display (T.pack fp)
                            return Nothing
                    Right snap ->
                        let createdOn =
                                sfCreatedOn snap <|> mCreateDate <|> Just (utctDay updatedOn)
                         in return $ Just snap {sfCreatedOn = createdOn}
                    Left exc -> do
                        logError $
                            "Error parsing snapshot file: " <> fromString fp <> "\n" <>
                            fromString (displayException exc)
                        return Nothing
        eUpdatedOn <- lastGitFileUpdate gitDir fp
        case eUpdatedOn of
            Left err -> do
                logError $ "Error parsing git commit date: " <> fromString err
                return Nothing
            Right updatedOn -> do
                env <- lift ask
                return $ Just $ (,,) snapName updatedOn $ runRIO env (parseSnapshot updatedOn)
    getLtsParser gitDir fp = do
        case mapM (BS8.readInt . BS8.pack) $ take 2 $ reverse (splitPath fp) of
            Just [(minor, ".yaml"), (major, "/")] ->
                getSnapshotParser gitDir fp Nothing $ SNLts major minor
            _ -> do
                logError
                    ("Couldn't parse the filepath into an LTS version: " <> display (T.pack fp))
                return Nothing
    getNightlyParser gitDir fp = do
        case mapM (BS8.readInt . BS8.pack) $ take 3 $ reverse (splitPath fp) of
            Just [(day, ".yaml"), (month, "/"), (year, "/")]
                | Just date <- fromGregorianValid (fromIntegral year) month day ->
                    getSnapshotParser gitDir fp (Just date) $ SNNightly date
            _ -> do
                logError
                    ("Couldn't parse the filepath into a Nightly date: " <> display (T.pack fp))
                return Nothing


-- | Creates a new snapshot if it is not yet present in the database and decides further if update
-- is necessary.
decideOnSnapshotUpdate ::
    (SnapName, UTCTime, RIO StackageCron (Maybe SnapshotFile))
    -> RIO StackageCron (Maybe (SnapshotId, SnapshotFile))
decideOnSnapshotUpdate (snapName, updatedOn, parseSnapshotFile) = do
    forceUpdate <- scForceFullUpdate <$> ask
    let mkLogMsg rest = "Snapshot with name: " <> display snapName <> " " <> rest
    mKeySnapFile <-
        run (getBy (UniqueSnapshot snapName)) >>= \case
            Just (Entity _key snap)
                | snapshotUpdatedOn snap == Just updatedOn && not forceUpdate -> do
                    logInfo $ mkLogMsg "already exists and is up to date."
                    return Nothing
            Just (Entity key snap)
                | Nothing <- snapshotUpdatedOn snap -> do
                    logWarn $ mkLogMsg "did not finish updating last time."
                    fmap (Just key, ) <$> parseSnapshotFile
            Just (Entity key _snap) -> do
                unless forceUpdate $
                    logWarn $ mkLogMsg "was updated, applying new patch."
                fmap (Just key, ) <$> parseSnapshotFile
            Nothing -> fmap (Nothing, ) <$> parseSnapshotFile
    -- Add new snapshot to the database, when necessary
    case mKeySnapFile of
        Just (Just snapKey, sf) -> return $ Just (snapKey, sf)
        Just (Nothing, sf@SnapshotFile {sfName, sfCompiler, sfCreatedOn})
            | Just createdOn <- sfCreatedOn ->
                fmap (, sf) <$> run (insertUnique (Snapshot sfName sfCompiler createdOn Nothing))
        _ -> return Nothing

type CorePackageGetter
     = RIO StackageCron ( Maybe (Entity Tree)
                        , Maybe HackageCabalId
                        , PackageIdentifierP
                        , GenericPackageDescription)

-- | This is an optimized version of snapshoat loading which can load a snapshot and documentation
-- info for previous snapshot at the same time. It will execute concurrently the loading of
-- current snapshot as well as an action that was passed as an argument. At the end it will return
-- an action that should be invoked in order to mark modules that have documentation available,
-- which in turn can be passed as an argument to the next snapshot loader.
createOrUpdateSnapshot ::
       Map CompilerP [CorePackageGetter]
    -> ResourceT (RIO StackageCron) ()
    -> (SnapName, UTCTime, RIO StackageCron (Maybe SnapshotFile))
    -> ResourceT (RIO StackageCron) (ResourceT (RIO StackageCron) ())
createOrUpdateSnapshot corePackageInfoGetters prevAction snapInfo = do
    snd <$> concurrently prevAction (lift loadCurrentSnapshot)
  where
    (_, updatedOn, _) = snapInfo
    loadCurrentSnapshot =
        decideOnSnapshotUpdate snapInfo >>= \case
            Nothing -> return $ pure ()
            Just (snapshotId, snapshotFile) ->
                updateSnapshot corePackageInfoGetters snapshotId updatedOn snapshotFile


updateSnapshot ::
       Map CompilerP [CorePackageGetter]
    -> SnapshotId
    -> UTCTime
    -> SnapshotFile
    -> RIO StackageCron (ResourceT (RIO StackageCron) ())
updateSnapshot corePackageGetters snapshotId updatedOn SnapshotFile {..} = do
    insertSnapshotName snapshotId sfName
    case Map.lookup sfCompiler corePackageGetters of
        Nothing -> logError $ "Hints are not found for the compiler: " <> display sfCompiler
        Just compilerCorePackages ->
            forM_ compilerCorePackages $ \getCorePackageInfo -> do
                (mTree, mhcid, pid, gpd) <- getCorePackageInfo
                run $ addSnapshotPackage snapshotId sfCompiler Core mTree mhcid False mempty pid gpd
    loadedPackageCountRef <- newIORef (0 :: Int)
    let totalPackages = length sfPackages
        progressReporter =
            forever $ do
                loadedPackageCount <- readIORef loadedPackageCountRef
                logSticky $
                    "Loading snapshot '" <> display sfName <> "' (" <>
                    displayShow loadedPackageCount <>
                    "/" <>
                    displayShow totalPackages <>
                    ")"
                threadDelay 1000000
        addPantryPackageWithReport pp = do
            let PantryCabal {pcPackageName} = ppPantryCabal pp
                isHidden = fromMaybe False (Map.lookup pcPackageName sfHidden)
                flags = fromMaybe Map.empty $ Map.lookup pcPackageName sfFlags
            curSucc <- addPantryPackage snapshotId sfCompiler isHidden flags pp
            atomicModifyIORef' loadedPackageCountRef (\c -> (c + 1, ()))
            pure curSucc
    -- ensure we leave off one connection for doc loader
    connCount <- max 1 . subtract 1 . pcConnectionCount <$> view pantryConfigL
    ePantryUpdatesSucceeded <-
        race progressReporter $
        pooledMapConcurrentlyN connCount addPantryPackageWithReport sfPackages
    logSticky $ "Loading snapshot '" <> display sfName <> "' is done. Next are the docs..."
    let pantryUpdateSucceeded = either (const False) and ePantryUpdatesSucceeded
    return $ do
        checkForDocsSucceeded <-
            tryAny (checkForDocs snapshotId sfName) >>= \case
                Left exc -> do
                    logError $ "Received exception while getting the docs: " <> displayShow exc
                    return False
                Right () -> return True
        if pantryUpdateSucceeded && checkForDocsSucceeded
            then do
                lift $ snapshotMarkUpdated snapshotId updatedOn
                logInfo $ "Created or updated snapshot '" <> display sfName <> "' successfully"
            else logError $ "There were errors while adding snapshot '" <> display sfName <> "'"



-- #if !DEVELOPMENT

--     let upload :: FilePath -> ObjectKey -> IO ()
--         upload fp key = do
--             let fpgz = fp <.> "gz"
--             runConduitRes
--                $ sourceFile fp
--               .| compress 9 (WindowBits 31)
--               .| CB.sinkFile fpgz
--             body <- chunkedFile defaultChunkSize fpgz
--             let po =
--                       set poACL (Just OPublicRead)
--                    $  putObject "haddock.stackage.org" key body
--             putStrLn $ "Uploading: " ++ tshow key
--             eres <- runResourceT $ runAWS env $ trying _Error $ send po
--             case eres of
--                 Left e -> error $ show (fp, key, e)
--                 Right _ -> putStrLn "Success"

--     db <- openStackageDatabase dbfp

--     do
--         snapshots <- runReaderT snapshotsJSON db
--         let key = ObjectKey "snapshots.json"
--             po =
--                   set poACL (Just OPublicRead)
--                $  set poContentType (Just "application/json")
--                $  putObject (BucketName "haddock.stackage.org") key (toBody snapshots)
--         putStrLn $ "Uploading: " ++ tshow key
--         eres <- runResourceT $ runAWS env $ trying _Error $ send po
--         case eres of
--             Left e -> error $ show (key, e)
--             Right _ -> putStrLn "Success"

--     names <- runReaderT (lastXLts5Nightly 50) db
--     let manager = view envManager env

--     locker <- newHoogleLocker False manager

--     forM_ names $ \name -> do
--         mfp <- singleRun locker name
--         case mfp of
--             Just _ -> putStrLn $ "Hoogle database exists for: " ++ toPathPiece name
--             Nothing -> do
--                 mfp' <- createHoogleDB db manager name
--                 forM_ mfp' $ \fp -> do
--                     let key = hoogleKey name
--                     upload fp (ObjectKey key)
--                     let dest = T.unpack key
--                     createDirectoryIfMissing True $ takeDirectory dest
--                     renamePath fp dest
-- #endif

-- createHoogleDB :: StackageDatabase -> Manager -> SnapName -> RIO Stackage (Maybe FilePath)
-- createHoogleDB db man name = handleAny (\e -> logError (display e) $> Nothing) $ do
--     logInfo $ "Creating Hoogle DB for " <> toPathPiece name
--     req' <- parseRequest $ T.unpack tarUrl
--     let req = req' { decompress = const True }

--     unlessM (doesFileExist tarFP) $ withResponse req man $ \res -> do
--         let tmp = tarFP <.> "tmp"
--         createDirectoryIfMissing True $ takeDirectory tmp
--         runConduitRes
--            $ bodyReaderSource (responseBody res)
--           .| sinkFile tmp
--         renamePath tmp tarFP

--     void $ tryIO $ removeDirectoryRecursive bindir
--     void $ tryIO $ removeFile outname
--     createDirectoryIfMissing True bindir

--     withSystemTempDirectory ("hoogle-" ++ T.unpack (toPathPiece name)) $ \tmpdir -> do
--         allPackagePairs <- runConduitRes
--             $ sourceTarFile False tarFP
--            .| foldMapMC (liftIO . singleDB db name tmpdir)

--         when (null allPackagePairs) $ error $ "No Hoogle .txt files found for " ++ T.unpack (toPathPiece name)

--         stackDir <- getAppUserDataDirectory "stack"
--         let indexTar = stackDir </> "indices" </> "Hackage" </> "00-index.tar"
--         withBinaryFile indexTar ReadMode $ \h -> do
--             let loop Tar.Done = return ()
--                 loop (Tar.Fail e) = throwIO e
--                 loop (Tar.Next e es) = go e >> loop es

--                 go e =
--                     case (Tar.entryContent e, splitPath $ Tar.entryPath e) of
--                         (Tar.NormalFile cabalLBS _, [pkg', ver', pkgcabal'])
--                           | Just pkg <- T.stripSuffix "/" (T.pack pkg')
--                           , Just ver <- T.stripSuffix "/" (T.pack ver')
--                           , Just pkg2 <- T.stripSuffix ".cabal" (T.pack pkgcabal')
--                           , pkg == pkg2
--                           , lookup pkg allPackagePairs == Just ver ->
--                                   runConduitRes
--                                 $ sourceLazy cabalLBS
--                                .| sinkFile (tmpdir </> T.unpack pkg <.> "cabal")
--                         _ -> return ()
--             L.hGetContents h >>= loop . Tar.read

--         let args =
--                 [ "generate"
--                 , "--database=" ++ outname
--                 , "--local=" ++ tmpdir
--                 ]
--         logInfo $ mconcat
--             [ "Merging databases... ("
--             , displayShow args
--             , ")"
--             ]
--         Hoogle.hoogle args

--         logInfo "Merge done"

--         return $ Just outname
--   where
--     root = "hoogle-gen"
--     bindir = root </> "bindir"
--     outname = root </> "output.hoo"

--     tarKey = toPathPiece name <> "/hoogle/orig.tar"
--     tarUrl = "https://s3.amazonaws.com/haddock.stackage.org/" <> tarKey
--     tarFP = root </> T.unpack tarKey

-- singleDB :: StackageDatabase
--          -> SnapName
--          -> FilePath -- ^ temp directory to write .txt files to
--          -> Tar.Entry
--          -> IO (Map Text Text)
-- singleDB db sname tmpdir e@(Tar.entryContent -> Tar.NormalFile lbs _) = do
--     --putStrLn $ "Loading file for Hoogle: " ++ pack (Tar.entryPath e)

--     let pkg = pack $ takeWhile (/= '.') $ Tar.entryPath e
--     msp <- flip runReaderT db $ do
--         Just (Entity sid _) <- lookupSnapshot sname
--         lookupSnapshotPackage sid pkg
--     case msp of
--         Nothing -> do
--             putStrLn $ "Unknown: " ++ pkg
--             return mempty
--         Just (Entity _ sp)  -> do
--             let out = tmpdir </> T.unpack pkg <.> "txt"
--                 -- FIXME add @url directive
--             runConduitRes $ sourceLazy lbs .| sinkFile out
--             return $ Map.singleton pkg (snapshotPackageVersion sp)
--                 {-
--                 docsUrl = concat
--                     [ "https://www.stackage.org/haddock/"
--                     , toPathPiece sname
--                     , "/"
--                     , pkgver
--                     , "/index.html"
--                     ] -}

-- singleDB _ _ _ _ = return mempty


pathToPackageModule :: Text -> Maybe (PackageIdentifierP, ModuleNameP)
pathToPackageModule txt =
    case T.split (== '/') txt of
        [pkgIdentifier, moduleNameDashes] -> do
             modName :: ModuleNameP <- fromPathPiece moduleNameDashes
             pkgId :: PackageIdentifierP <- fromPathPiece pkgIdentifier
             Just (pkgId, modName)
        _ -> Nothing


