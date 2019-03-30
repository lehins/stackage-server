{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    ) where

import Control.AutoUpdate
import Control.Concurrent (forkIO)
import Control.Concurrent (threadDelay)
import Control.Monad.Logger (liftLoc)
import Data.WebsiteContent
import Database.Persist.Postgresql (PostgresConf(..))
import Import hiding (catch)
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware, rawPathInfo)
import Network.Wai.Handler.Warp (Settings, defaultSettings,
                                 defaultShouldDisplayException, getPort,
                                 runSettings, setHost, setOnException, setPort)
import Network.Wai.Middleware.ForceSSL (forceSSL)
import Network.Wai.Middleware.RequestLogger (Destination(Logger),
                                             IPAddrSource(..), OutputFormat(..),
                                             destination, mkRequestLogger,
                                             outputFormat)
import RIO (LogFunc, LogOptions, logOptionsHandle, newLogFunc, withLogFunc)
import RIO.Prelude.Simple (runSimpleApp)
import Stackage.Database (openStackageDatabase)
import Stackage.Database.Cron (newHoogleLocker, singleRun)
import Stackage.Database.Github (getStackageContentDir)
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2
import Yesod.Default.Handlers
import Yesod.GitRepo
import Yesod.GitRev (tGitRev)

-- Import all relevant handler modules here.
import Handler.Blog
import Handler.BuildPlan
import Handler.Download
import Handler.DownloadStack
import Handler.Feed
import Handler.Haddock
import Handler.Home
import Handler.Hoogle
import Handler.MirrorStatus
import Handler.OldLinks
import Handler.Package
import Handler.PackageDeps
import Handler.PackageList
import Handler.Sitemap
import Handler.Snapshots
import Handler.StackageHome
import Handler.StackageIndex
import Handler.StackageSdist
import Handler.System

--import Network.Wai.Middleware.Prometheus (prometheus)
--import Prometheus (register)
--import Prometheus.Metric.GHC (ghcMetrics)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation

    let middleware = id -- prometheus def
#if !DEVELOPMENT
                   . forceSSL' (appSettings foundation)
#endif
                   . logWare
                   . defaultMiddlewaresNoLogging

    -- FIXME prometheus void (register ghcMetrics)

    return (middleware appPlain)

forceSSL' :: AppSettings -> Middleware
forceSSL' settings app
    | appForceSsl settings = \req send ->
        -- Don't force SSL for tarballs, to provide 00-index.tar.gz and package
        -- tarball access for cabal-install
        if ".tar.gz" `isSuffixOf` rawPathInfo req
            then app req send
            else forceSSL app req send
    | otherwise = app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: LogFunc -> AppSettings -> IO App
makeFoundation logFunc appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    appWebsiteContent <- if appDevDownload appSettings
        then do
            fp <- runSimpleApp $ getStackageContentDir "."
            gitRepoDev fp loadWebsiteContent
        else gitRepo
            "https://github.com/fpco/stackage-content.git"
            "master"
            loadWebsiteContent

    appStackageDatabase <- openStackageDatabase PostgresConf
      { pgPoolSize = 2
      , pgConnStr = encodeUtf8 $ appPostgresString appSettings
      }

    -- Temporary workaround to force content updates regularly, until
    -- distribution of webhooks is handled via consul
    void $ forkIO $ forever $ void $ do
        threadDelay $ 1000 * 1000 * 60 * 5
        handleAny print $ grRefresh appWebsiteContent

    appLatestStackMatcher <- mkAutoUpdate defaultUpdateSettings
        { updateFreq = 1000 * 1000 * 60 * 30 -- update every thirty minutes
        , updateAction = getLatestMatcher appHttpManager
        }

    appHoogleLock <- newMVar ()

    appMirrorStatus <- mkUpdateMirrorStatus
    hoogleLocker <- newHoogleLocker logFunc appHttpManager
    let appGetHoogleDB = singleRun hoogleLocker
    let appGitRev = $$tGitRev

    return App {..}

getLogOpts :: AppSettings -> IO LogOptions
getLogOpts settings = logOptionsHandle stdout (appShouldLogAll settings)


makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    logOpts <- getLogOpts settings
    -- FIXME: finalizer is discarded. Could be OK development?
    (logFunc, _ :: IO ()) <- newLogFunc logOpts
    foundation <- makeFoundation logFunc settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv
    logOpts <- getLogOpts settings
    withLogFunc logOpts $ \ logFunc -> do
        -- Generate the foundation from the settings
        foundation <- makeFoundation logFunc settings

        -- Generate a WAI Application from the foundation
        app <- makeApplication foundation

        -- Run the application with Warp
        runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    logOpts <- getLogOpts settings
    -- Note: finalizer is discarded. Should be OK for ghci.
    (logFunc, _ :: IO ()) <- newLogFunc logOpts
    foundation <- makeFoundation logFunc settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = do
    logOpts <- logOptionsHandle stdout True
    withLogFunc logOpts $ \ logFunc ->
        getAppSettings >>= makeFoundation logFunc >>= flip unsafeHandler h
