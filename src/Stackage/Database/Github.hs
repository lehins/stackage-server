{-# LANGUAGE NoImplicitPrelude #-}
module Stackage.Database.Github
    ( cloneOrUpdate
    , lastGitFileUpdate
    , getStackageContentDir
    ) where

import qualified Data.ByteString.Lazy.Char8 as LBS8
import RIO
import RIO.Directory
import RIO.FilePath
import RIO.Process
import RIO.Time


gitLog
  :: (MonadReader env m, HasLogFunc env, HasProcessContext env,
      MonadIO m) =>
     FilePath -> String -> [String] -> m LBS8.ByteString
gitLog gitDir filePath args =
    withWorkingDir gitDir $ proc "git" ("log" : (args ++ [filePath])) readProcessStdout_


-- | From the git commit log infer the timestamp when the file was changed last .
lastGitFileUpdate ::
       (MonadReader env m, HasLogFunc env, HasProcessContext env, MonadUnliftIO m)
    => FilePath -- ^ Root dir of the repository
    -> FilePath -- ^ Relative path of the file
    -> m (Either String UTCTime)
lastGitFileUpdate gitDir filePath = do
    lastCommitTimestamps <- gitLog gitDir filePath ["-1", "--format=%cD"]
    parseGitDate rfc822DateFormat lastCommitTimestamps
  where
    parseGitDate fmt dates =
        case listToMaybe $ LBS8.lines dates of
            Nothing -> return $ Left "Git log is empty for the file"
            Just lbsDate ->
                mapLeft (displayException :: SomeException -> String) <$>
                try (parseTimeM False defaultTimeLocale fmt (LBS8.unpack lbsDate))

-- | Clone a repository locally. In case when repository is already present sync it up with
-- remote. Returns the full path where repository was cloned into.
cloneOrUpdate ::
       (MonadReader env m, HasLogFunc env, HasProcessContext env, MonadIO m)
    => FilePath -- ^ Path where the repo should be cloned
    -> String -- ^ Github user or organization name
    -> String -- ^ Repository name
    -> m FilePath
cloneOrUpdate root org name = do
    exists <- doesDirectoryExist dest
    if exists
        then withWorkingDir dest $ do
            proc "git" ["fetch"] runProcess_
            proc "git" ["reset", "--hard", "origin/master"] runProcess_
        else withWorkingDir root $
            proc "git" ["clone", url, name] runProcess_
    return dest
  where
    url = "https://github.com/" <> org <> "/" <> name <> ".git"
    dest = root </> name



getStackageContentDir ::
       (MonadReader env m, HasLogFunc env, HasProcessContext env, MonadIO m)
    => FilePath
    -> m FilePath
getStackageContentDir rootDir =
    cloneOrUpdate rootDir "commercialhaskell" "stackage-content"
