{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Stackage.Database.Types
    ( SnapName (..)
    , isLts
    , isNightly
    , CompilerP(..)
    , FlagNameP(..)
    , StackageCron(..)
    , PantryCabal(..)
    , GenericPackageDescription
    , toPackageIdentifierRevision
    , PantryPackage(..)
    , SnapshotFile(..)
    , HackageCabalInfo(..)
    , PackageListingInfo(..)
    , ModuleListingInfo(..)
    , PackageNameP(..)
    , VersionP(..)
    , Revision(..)
    , VersionRangeP(..)
    , PackageIdentifierP(..)
    , ModuleNameP(..)
    , PackageOrigin(..)
    , LatestInfo(..)
    , Deprecation(..)
    , haddockBucketName
    ) where

import           Data.Aeson
import qualified Data.Text            as T
import           Data.Text.Read       (decimal)
import           Database.Persist     (PersistField(..), SqlType(SqlInt64))
import           Database.Persist.Sql (PersistFieldSql(..))
import           Network.AWS          (Env, HasEnv (..))
import           Pantry.SHA256        (fromHexText)
import           Pantry.Storage       (BlobId, HackageCabalId, TreeId)
import           Pantry.Types         (BlobKey (..), CabalFileInfo (..),
                                       FileSize (..), HasPantryConfig (..),
                                       PackageIdentifierRevision (..),
                                       PackageNameP (..), PantryConfig,
                                       TreeKey (..), VersionP (..))
import           RIO
import           RIO.Process          (HasProcessContext (..), ProcessContext)
import           RIO.Time             (Day)
import           Text.Blaze           (ToMarkup (..))
import           Types
import           Web.PathPieces

haddockBucketName :: Text
haddockBucketName = "haddock.stackage.org"

data SnapName = SNLts !Int !Int
              | SNNightly !Day
    deriving (Eq, Ord, Read, Show)

isLts :: SnapName -> Bool
isLts SNLts{}     = True
isLts SNNightly{} = False

isNightly :: SnapName -> Bool
isNightly SNLts{}     = False
isNightly SNNightly{} = True

instance ToJSONKey SnapName

instance ToJSON SnapName where
    toJSON = String . toPathPiece

instance PersistField SnapName where
    toPersistValue = toPersistValue . toPathPiece
    fromPersistValue v = do
        t <- fromPersistValue v
        case fromPathPiece t of
            Nothing -> Left $ "Invalid SnapName: " <> t
            Just x  -> return x
instance PersistFieldSql SnapName where
    sqlType = sqlType . fmap toPathPiece
instance PathPiece SnapName where
    toPathPiece = textDisplay
    fromPathPiece = parseSnapName

instance FromJSON SnapName where
    parseJSON = withText "SnapName" (maybe (fail "Can't parse snapshot name") pure . parseSnapName)

instance ToMarkup SnapName where
    toMarkup = toMarkup . textDisplay

instance Display SnapName where
    display =
        \case
            (SNLts x y) -> mconcat ["lts-", displayShow x, ".", displayShow y]
            (SNNightly d) -> "nightly-" <> displayShow d

parseSnapName :: Text -> Maybe SnapName
parseSnapName t0 = nightly <|> lts
  where
    nightly = fmap SNNightly $ T.stripPrefix "nightly-" t0 >>= (readMaybe . T.unpack)
    lts = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, t2) <- Just $ decimal t1
        t3 <- T.stripPrefix "." t2
        Right (y, "") <- Just $ decimal t3
        return $ SNLts x y


data StackageCron = StackageCron
    { scPantryConfig    :: !PantryConfig
    , scStackageRoot    :: !FilePath
    , scLogFunc         :: !LogFunc
    , scProcessContext  :: !ProcessContext
    , sfForceFullUpdate :: !Bool
    , sfEnvAWS          :: !Env
    }

instance HasEnv StackageCron where
    environment = lens sfEnvAWS (\c f -> c {sfEnvAWS = f})

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


-- data PantryOrigin =
--   PantryHackage HackageOrigin
--   | PantryRepo Repo
--   | PantryArchive Archive

-- data PantryPackage = PantryPackage
--     { ppPantryOrigin :: !PantryOrigin
--     , ppPantryKey    :: !TreeKey
--     } deriving (Show)


-- data HackageOrigin = HackageOrigin
--     { hoPackageName    :: !PackageNameP
--     , hoPackageVersion :: !VersionP
--     , hoCabalKey       :: !BlobKey
--     } deriving (Show)


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
    , pliIsCore   :: !Bool
    } deriving Show


instance ToJSON PackageListingInfo where
    toJSON PackageListingInfo {..} =
        object
            [ "name"     .= pliName
            , "version"  .= pliVersion
            , "synopsis" .= pliSynopsis
            , "isCore"   .= pliIsCore
            ]


data HackageCabalInfo = HackageCabalInfo
    { hciCabalId     :: !HackageCabalId
    , hciCabalBlobId :: !BlobId         -- TODO: make part of SnapshotPackageInfo
    , hciPackageName :: !PackageNameP   -- TODO: make part of SnapshotPackageInfo
    , hciVersion     :: !VersionP       -- TODO: make part of SnapshotPackageInfo
    , hciRevision    :: !(Maybe Revision)
    } deriving (Show, Eq)


data PackageOrigin
    = Core
    | Hackage
    | Archive
    | GitRepo
    | HgRepo
    deriving (Show, Eq)


instance PersistField PackageOrigin where
    toPersistValue =
        toPersistValue . \case
            Core    -> 0 :: Int64
            Hackage -> 1
            Archive -> 2
            GitRepo -> 3
            HgRepo  -> 4
    fromPersistValue v =
        fromPersistValue v >>= \case
            0 -> Right Core
            1 -> Right Hackage
            2 -> Right Archive
            3 -> Right GitRepo
            4 -> Right HgRepo
            n -> Left $ "Unknown origin type: " <> textDisplay (n :: Int64)
instance PersistFieldSql PackageOrigin where
    sqlType _ = SqlInt64


data SnapshotPackageInfo = SnapshotPackageInfo
    { spiTreeId      :: !TreeId
    , spiCabalBlobId :: !BlobId
    , spiPackageName :: !PackageNameP
    , spiVersion     :: !VersionP
    , spiOrigin      :: !PackageOrigin
    } deriving (Show, Eq)

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
