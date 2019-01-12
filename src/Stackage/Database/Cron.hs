{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stackage.Database.Cron
    ( stackageServerCron
    , newHoogleLocker
    , singleRun
    ) where

import qualified Codec.Archive.Tar             as Tar
import           Conduit
--import           Control.Monad.Trans.AWS       (trying, _Error)
--import           Control.Monad.Logger          (MonadLogger)
import           Control.Lens                  ((.~))
import           Control.SingleRun
import qualified Data.ByteString.Lazy          as L
import qualified Data.Conduit.Binary           as CB
import           Data.Conduit.Zlib             (WindowBits (WindowBits),
                                                compress, ungzip)
import           Data.Streaming.Network        (bindPortTCP)
import           Database.Persist              (Entity (Entity))
import qualified Hoogle
import           Network.AWS                   hiding (Request, Response)
import           Network.AWS.Data.Body         (toBody)
import           Network.AWS.Data.Log          (build)
import           Network.AWS.Data.Text         (toText)
import           Network.AWS.Pager
import           Network.AWS.S3
import           Network.HTTP.Client
import           Network.HTTP.Client.Conduit   (bodyReaderSource)
import           Network.HTTP.Simple           (getResponseBody, httpJSONEither,
                                                parseRequest)
import           Network.HTTP.Types            (status200)
import           Pantry                        (defaultHackageSecurityConfig)
import           Pantry.Types                  (HpackExecutable (HpackBundled),
                                                PantryConfig (..))
import           Path                          (parseAbsDir, toFilePath)
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Map                       as Map
import           RIO.Process                   (mkDefaultProcessContext)
import qualified RIO.Text                      as T
import           Stackage.Database
import           Stackage.Database.Types       (Deprecation (..),
                                                StackageCron (..),
                                                haddockBucketName)
import           System.Environment            (getEnv)
import           Types                         (ModuleNameP(..), PackageIdentifierP)
--import           UnliftIO.Concurrent           (getNumCapabilities)
import           Web.PathPieces                (fromPathPiece, toPathPiece)

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
    [ "https://s3.amazonaws.com/haddock.stackage.org/"
    , hoogleKey n
    ]

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


initStorage :: IO StackageDatabase
initStorage = do
    connstr <- encodeUtf8 . T.pack <$> getEnv "PGSTRING"
    openStackageDatabase PostgresConf {pgPoolSize = 5, pgConnStr = connstr}


hackageDeprecatedUrl :: Request
hackageDeprecatedUrl = "https://hackage.haskell.org/packages/deprecated.json"

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
    storage <- initStorage
    lo <- logOptionsHandle stdout True
    stackageRootDir <- getAppUserDataDirectory "stackage"
    pantryRootDir <- parseAbsDir (stackageRootDir </> "pantry")
    createDirectoryIfMissing True (toFilePath pantryRootDir)
    updateRef <- newMVar True
    cabalImmutable <- newIORef Map.empty
    cabalMutable <- newIORef Map.empty
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
                                      , pcConnectionCount = 2
                                      }
          stackage = StackageCron { scPantryConfig = pantryConfig
                                  , scStackageRoot = stackageRootDir
                                  , scProcessContext = defaultProcessContext
                                  , scLogFunc = logFunc
                                  , sfForceFullUpdate = forceUpdate
                                  , sfEnvAWS = aws
                                  }
      in runRIO stackage runStackageUpdate



-- | Send request to AWS and process the response with a handler. A separate
-- error handler will be invoked whenever an error occurs, which suppose to return
-- some sort of default value. Error is logged using `MonadLoggger`
sendAWS
  :: (MonadReader env m,
      MonadResource m,
      HasLogFunc env, HasEnv env, AWSRequest a) =>
     a -> (Error -> m b) -> (Rs a -> m b) -> m b
sendAWS req onErr onResp = do
  env <- ask
  eResp <- runAWS env $ trying _Error $ send req
  case eResp of
    Left err -> do
      logError $ Utf8Builder (build err)
      onErr err
    Right resp -> onResp resp


-- | Recursively handle responses that support pagination returned by `sendAWS`.
pagerAWS
  :: (AWSPager a, HasEnv env, HasLogFunc env, MonadReader env m, MonadResource m)
  => a -> (Rs a -> m ()) -> m ()
pagerAWS req onResp = do
  sendAWS req (const $ return ()) $ \resp -> do
    onResp resp
    case page req resp of
      Just newReq -> pagerAWS newReq onResp
      _           -> return ()

-- | Conduit style `pagerAWS`, produces elements from each page continuously
-- until there is no more pages avialable.
sourcePagerAWS
  :: (AWSPager a, HasLogFunc env, HasEnv env, MonadReader env m, MonadResource m)
  => a -> (Rs a -> [b]) -> ConduitT v b m ()
sourcePagerAWS req getPage = pagerAWS req (yieldMany . getPage)

pathToPackageModule :: Text -> Maybe Text
pathToPackageModule txt =
    case T.split (== '/') txt of
        [pkgIdentifier, moduleNameDashes] -> do
             modName :: ModuleNameP <- fromPathPiece moduleNameDashes
             pkgId :: PackageIdentifierP <- fromPathPiece pkgIdentifier
             return $ textDisplay $ display pkgId <> ": " <> display modName <> "\n"
        _ -> Nothing

runStackageUpdate :: RIO StackageCron ()
runStackageUpdate = do
    let prefix = "lts-13.2/"
    let req = listObjectsV2 (BucketName haddockBucketName) & lovPrefix .~ Just prefix
    dir <- getCurrentDirectory
    --createStackageDatabase getHackageDeprecations
    runConduitRes $
        sourcePagerAWS req (^. lovrsContents) .| mapC (\obj -> toText (obj ^. oKey)) .|
        concatMapC (T.stripSuffix ".html") .|
        concatMapC (T.stripPrefix prefix) .|
        concatMapC pathToPackageModule .|
        mapC encodeUtf8 .|
        sinkFile (dir </> "lts-13.2.txt")
    return ()

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
