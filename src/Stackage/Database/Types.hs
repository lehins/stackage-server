{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Stackage.Database.Types
    ( SnapName(..)
    , isLts
    , isNightly
    , SnapshotBranch(..)
    , snapshotPrettyName
    , snapshotPrettyNameShort
    , CompilerP(..)
    , FlagNameP(..)
    , StackageCron(..)
    , PantryCabal(..)
    , BlobKey (..)
    , GenericPackageDescription
    , toPackageIdentifierRevision
    , PantryPackage(..)
    , SnapshotFile(..)
    , SnapshotPackageInfo(..)
    , SnapshotPackagePageInfo(..)
    , spiVersionRev
    , HackageCabalInfo(..)
    , PackageListingInfo(..)
    , ModuleListingInfo(..)
    , PackageNameP(..)
    , VersionP(..)
    , Revision(..)
    , VersionRangeP(..)
    , PackageIdentifierP(..)
    , VersionRev(..)
    , toRevMaybe
    , toVersionRev
    , toVersionMRev
    , PackageVersionRev(..)
    , dropVersionRev
    , ModuleNameP(..)
    , SafeFilePath
    , Origin(..)
    , LatestInfo(..)
    , Deprecation(..)
    , haddockBucketName
    , Changelog(..)
    , Readme(..)
    ) where

import Data.Aeson
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Network.AWS (Env, HasEnv(..))
import Pantry.SHA256 (fromHexText)
import Pantry.Storage (BlobId, HackageCabalId)
import Pantry.Types (BlobKey(..), CabalFileInfo(..), FileSize(..),
                     HasPantryConfig(..), PackageIdentifierRevision(..),
                     PackageNameP(..), PantryConfig, TreeKey(..), VersionP(..))
import RIO
import RIO.Process (HasProcessContext(..), ProcessContext)
import RIO.Time (Day)
import Stackage.Database.Schema
import Types

haddockBucketName :: Text
haddockBucketName = "haddock.stackage.org"


data StackageCron = StackageCron
    { scPantryConfig    :: !PantryConfig
    , scStackageRoot    :: !FilePath
    , scLogFunc         :: !LogFunc
    , scProcessContext  :: !ProcessContext
    , scForceFullUpdate :: !Bool
    , scCachedGPD       :: !(IORef (IntMap GenericPackageDescription))
    , scEnvAWS          :: !Env
    }

instance HasEnv StackageCron where
    environment = lens scEnvAWS (\c f -> c {scEnvAWS = f})

instance HasLogFunc StackageCron where
    logFuncL = lens scLogFunc (\c f -> c {scLogFunc = f})

instance HasProcessContext StackageCron where
    processContextL = lens scProcessContext (\c f -> c {scProcessContext = f})

instance HasPantryConfig StackageCron where
    pantryConfigL = lens scPantryConfig (\c f -> c {scPantryConfig = f})



data SnapshotFile = SnapshotFile
    { sfName      :: !SnapName
    , sfCompiler  :: !CompilerP
    , sfPackages  :: ![PantryPackage]
    , sfHidden    :: !(Map PackageNameP Bool)
    , sfFlags     :: !(Map PackageNameP (Map FlagNameP Bool))
    , sfCreatedOn :: !(Maybe Day) -- TODO: switch to UTCTime and get it from yaml
    }


data PantryCabal = PantryCabal
    { pcPackageName    :: !PackageNameP
    , pcPackageVersion :: !VersionP
    , pcCabalKey       :: !BlobKey
    } deriving (Show)

instance Display PantryCabal where
    display PantryCabal {..} =
        display (PackageIdentifierP pcPackageName pcPackageVersion) <> "@sha256:" <>
        display pcCabalKey

data PantryPackage = PantryPackage
    { ppPantryCabal :: !PantryCabal
    , ppPantryKey   :: !TreeKey
    } deriving (Show)

toPackageIdentifierRevision :: PantryCabal -> PackageIdentifierRevision
toPackageIdentifierRevision PantryCabal {..} =
    PackageIdentifierRevision
        (unPackageNameP pcPackageName)
        (unVersionP pcPackageVersion)
        (CFIHash sha (Just size))
  where
    BlobKey sha size = pcCabalKey

-- QUESTION: Potentially switch to `parsePackageIdentifierRevision`:
   -- PackageIdentifierRevision pn v (CFIHash sha (Just size)) <-
   --     either (fail . displayException) pure $ parsePackageIdentifierRevision txt
   -- return (PantryCabal pn v sha size)
-- Issues with such switch:
-- * CFILatest and CFIRevision do not make sense in stackage-snapshots
-- * Implementation below is faster
instance FromJSON PantryCabal where
    parseJSON =
        withText "PantryCabal" $ \txt -> do
            let (packageTxt, hashWithSize) = T.break (== '@') txt
                (hashTxtWithAlgo, sizeWithComma) = T.break (== ',') hashWithSize
            -- Split package identifier foo-bar-0.1.2 into package name and version
            (pkgNameTxt, pkgVersionTxt) <-
                case T.breakOnEnd ("-") packageTxt of
                    (pkgNameWithDashEnd, pkgVersionTxt)
                        | Just pkgName <- T.stripSuffix "-" pkgNameWithDashEnd ->
                            return (pkgName, pkgVersionTxt)
                    _ -> fail $ "Invalid package identifier format: " ++ T.unpack packageTxt
            pcPackageName <- parseJSON $ String pkgNameTxt
            pcPackageVersion <- parseJSON $ String pkgVersionTxt
            hashTxt <-
                maybe (fail $ "Unrecognized hashing algorithm: " ++ T.unpack hashTxtWithAlgo) pure $
                T.stripPrefix "@sha256:" hashTxtWithAlgo
            pcSHA256 <- either (fail . displayException) pure $ fromHexText hashTxt
            (pcFileSize, "") <-
                either fail (pure . first FileSize) =<<
                maybe
                    (fail $ "Wrong size format:" ++ show sizeWithComma)
                    (pure . decimal)
                    (T.stripPrefix "," sizeWithComma)
            let pcCabalKey = BlobKey pcSHA256 pcFileSize
            return PantryCabal {..}


instance FromJSON PantryPackage where
    parseJSON =
        withObject "PantryPackage" $ \obj ->
            PantryPackage <$> obj .: "hackage" <*> obj .: "pantry-tree"


instance FromJSON SnapshotFile where
    parseJSON =
        withObject "SnapshotFile" $ \obj -> do
            sfName <- obj .: "name"
            sfCompiler <- obj .: "compiler"
            sfPackages <- obj .: "packages"
            sfHidden <- obj .:? "hidden" .!= mempty
            sfFlags <- obj .:? "flags" .!= mempty
            sfCreatedOn <- obj .:? "created_on"
            return SnapshotFile {..}


data PackageListingInfo = PackageListingInfo
    { pliName     :: !PackageNameP
    , pliVersion  :: !VersionP
    , pliSynopsis :: !Text
    , pliOrigin   :: !Origin
    } deriving Show


instance ToJSON PackageListingInfo where
    toJSON PackageListingInfo {..} =
        object
            [ "name"     .= pliName
            , "version"  .= pliVersion
            , "synopsis" .= pliSynopsis
            , "origin"   .= pliOrigin
            ]


data HackageCabalInfo = HackageCabalInfo
    { hciCabalId     :: !HackageCabalId
    , hciCabalBlobId :: !BlobId
    , hciPackageName :: !PackageNameP
    , hciVersionRev  :: !VersionRev
    } deriving (Show, Eq)

data SnapshotPackageInfo = SnapshotPackageInfo
    { spiSnapshotPackageId :: !SnapshotPackageId
    , spiSnapshotId        :: !SnapshotId
    , spiCabalBlobId       :: !(Maybe BlobId)
    , spiSnapName          :: !SnapName
    , spiPackageName       :: !PackageNameP
    , spiVersion           :: !VersionP
    , spiRevision          :: !(Maybe Revision)
    , spiOrigin            :: !Origin
    , spiReadme            :: !(Maybe TreeEntryId)
    , spiChangelog         :: !(Maybe TreeEntryId)
    } deriving (Show, Eq)


data SnapshotPackagePageInfo = SnapshotPackagePageInfo
    { sppiSnapshotPackageInfo    :: !SnapshotPackageInfo
    -- ^ Info of the package on this page
    , sppiLatestHackageCabalInfo :: !(Maybe HackageCabalInfo)
    -- ^ If the package is available on hackage, show its latest info
    , sppiForwardDeps            :: ![(PackageNameP, VersionRangeP)]
    -- ^ All packages in the snapshot that this package depends on
    , sppiForwardDepsCount       :: !Int
    , sppiReverseDeps            :: ![(PackageNameP, VersionRangeP)]
    -- ^ All packages in the snapshot that depend on this package
    , sppiReverseDepsCount       :: !Int
    , sppiLatestInfo             :: ![LatestInfo]
    , sppiModuleNames            :: ![ModuleNameP]
    , sppiVersion                :: !(Maybe VersionRev)
    -- ^ Version on this page. Should be present only if different from latest
    }

toRevMaybe :: Revision -> Maybe Revision
toRevMaybe rev = guard (rev /= Revision 0) >> Just rev

-- | Add revision only if it is non-zero
toVersionRev :: VersionP -> Revision -> VersionRev
toVersionRev v = VersionRev v . toRevMaybe

-- | Add revision only if it is present and is non-zero
toVersionMRev :: VersionP -> Maybe Revision -> VersionRev
toVersionMRev v mrev = VersionRev v (maybe Nothing toRevMaybe mrev)

spiVersionRev :: SnapshotPackageInfo -> VersionRev
spiVersionRev spi = VersionRev (spiVersion spi) (spiRevision spi >>= toRevMaybe)

dropVersionRev :: PackageVersionRev -> PackageNameP
dropVersionRev (PackageVersionRev pname _) = pname


data ModuleListingInfo = ModuleListingInfo
    { mliModuleName        :: !ModuleNameP
    , mliPackageIdentifier :: !PackageIdentifierP
    } deriving Show


data LatestInfo = LatestInfo
    { liSnapName   :: !SnapName
    , liVersionRev :: !VersionRev
    } deriving (Show, Eq)



data Deprecation = Deprecation
    { depPackage    :: !PackageNameP
    , depInFavourOf :: !(Set PackageNameP)
    }
instance ToJSON Deprecation where
    toJSON d = object
        [ "deprecated-package" .= depPackage d
        , "in-favour-of" .= depInFavourOf d
        ]
instance FromJSON Deprecation where
    parseJSON = withObject "Deprecation" $ \o -> Deprecation
        <$> o .: "deprecated-package"
        <*> o .: "in-favour-of"


data Readme = Readme !ByteString !Bool
data Changelog = Changelog !ByteString !Bool
