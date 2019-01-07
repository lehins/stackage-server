{-# LANGUAGE NoImplicitPrelude #-}
module Stackage.Types
  ( BuildPlan (..)
  , SystemInfo (..)
  , PackagePlan (..)
  , DocMap
  , PackageDocs (..)
  , dtDisplay
  , simpleParse
  ) where

import Types (dtDisplay)
import qualified Distribution.Text               as DT
import ClassyPrelude.Conduit
import Data.Aeson
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Typeable (TypeRep, Typeable, typeOf)
import Pantry.Types (PackageNameP(..), VersionP(..))

data BuildPlan = BuildPlan
  { bpSystemInfo :: !SystemInfo
  , bpPackages :: !(Map PackageNameP PackagePlan)
  }
instance FromJSON BuildPlan where
  parseJSON = withObject "BuildPlan" $ \o -> BuildPlan
    <$> o .: "system-info"
    <*> o .: "packages"

data SystemInfo = SystemInfo
  { siGhcVersion :: !VersionP
  , siCorePackages :: !(Map PackageNameP VersionP)
  }
instance FromJSON SystemInfo where
  parseJSON = withObject "SystemInfo" $ \o -> SystemInfo
    <$> o .: "ghc-version"
    <*> o .: "core-packages"

data PackagePlan = PackagePlan
  { ppVersion :: VersionP
  }
instance FromJSON PackagePlan where
  parseJSON = withObject "PackagePlan" $ \o -> PackagePlan
    <$> o .: "version"

type DocMap = Map Text PackageDocs

data PackageDocs = PackageDocs
    { pdVersion :: !Text
    , pdModules :: !(Map Text [Text])
    }
instance FromJSON PackageDocs where
    parseJSON = withObject "PackageDocs" $ \o -> PackageDocs
        <$> o .: "version"
        <*> o .: "modules"

data ParseFailedException = ParseFailedException TypeRep Text
    deriving (Show, Typeable)
instance Exception ParseFailedException

simpleParse :: (MonadThrow m, DT.Text a, Typeable a) => Text -> m a
simpleParse orig = withTypeRep $ \rep ->
    case DT.simpleParse str of
        Nothing -> throwM (ParseFailedException rep (pack str))
        Just v  -> return v
  where
    str = unpack orig

    withTypeRep :: Typeable a => (TypeRep -> m a) -> m a
    withTypeRep f =
        res
      where
        res = f (typeOf (unwrap res))

        unwrap :: m a -> a
        unwrap _ = error "unwrap"
