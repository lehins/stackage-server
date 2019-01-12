{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}
module Stackage.Database.Types
    ( SnapName (..)
    , isLts
    , isNightly
    , Compiler(..)
    , displayCompiler
    , parseCompiler
    , StackageCron(..)
    , PantryCabal(..)
    , toPackageIdentifierRevision
    , PantryPackage(..)
    , SnapshotFile(..)
    , HackageCabalInfo(..)
    , PackageListingInfo(..)
    , ModuleListingInfo(..)
    , LatestInfo(..)
    , Deprecation(..)
    , haddockBucketName
    ) where

import           Data.Aeson
import           Data.Bifunctor       (bimap)
import qualified Data.Text            as T
import           Data.Text.Read       (decimal)
import qualified Data.Text.Read       as T (decimal)
import           Database.Persist
import           Database.Persist.Sql hiding (LogFunc)
import           Network.AWS          (HasEnv(..), Env)
import           Pantry.SHA256
import           Pantry.Storage       (BlobId, HackageCabalId)
import           Pantry.Types
import           RIO
import           RIO.Process          (HasProcessContext (..), ProcessContext)
import           RIO.Time
import           Stackage.Types       (dtDisplay)
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
    toPathPiece = showSnapName

    fromPathPiece = parseSnapName

instance FromJSON SnapName where
    parseJSON = withText "SnapName" (maybe (fail "Can't parse snapshot name") pure . parseSnapName)

showSnapName :: SnapName -> Text
showSnapName (SNLts x y) = T.concat ["lts-", T.pack (show x), ".", T.pack (show y)]
showSnapName (SNNightly d) = "nightly-" <> T.pack (show d)

instance ToMarkup SnapName where
    toMarkup = toMarkup . showSnapName

instance Display SnapName where
    display = display . showSnapName

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

newtype Compiler =
    CompilerGHC { ghcVersion :: Version }
    deriving (Eq, Ord)

instance Show Compiler where
    show = displayCompiler

instance FromJSONKey Compiler where
    fromJSONKey = FromJSONKeyTextParser (either fail pure . parseCompiler)

displayCompiler :: (Monoid a, IsString a) => Compiler -> a
displayCompiler (CompilerGHC vghc) = "ghc-" <> dtDisplay vghc

parseCompiler :: Text -> Either String Compiler
parseCompiler txt =
    case T.stripPrefix "ghc-" txt of
        Just vTxt -> bimap displayException CompilerGHC $ parseVersionThrowing (T.unpack vTxt)
        Nothing -> Left $ "Invalid prefix for compiler: " <> T.unpack txt


instance Display Compiler where
    display = displayCompiler
instance ToJSON Compiler where
    toJSON = String . displayCompiler
instance FromJSON Compiler where
    parseJSON = withText "Compiler" (either fail return .  parseCompiler)
instance PersistField Compiler where
    toPersistValue = PersistText . displayCompiler
    fromPersistValue v = fromPersistValue v >>= mapLeft T.pack . parseCompiler
instance PersistFieldSql Compiler where
    sqlType _ = SqlString


data SnapshotFile = SnapshotFile
    { sfName      :: !SnapName
    , sfCompiler  :: !Compiler
    , sfPackages  :: ![PantryPackage]
    , sfHidden    :: !(Map PackageNameP Bool)
    , sfFlags     :: !(Map PackageNameP (Map Text Bool))
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
                    (pure . T.decimal)
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
            [ "name" .= pliName
            , "version" .= pliVersion
            , "synopsis" .= pliSynopsis
            , "isCore" .= pliIsCore
            ]


data HackageCabalInfo = HackageCabalInfo
  { hciCabalId     :: !HackageCabalId
  , hciBlobId      :: !BlobId
  , hciPackageName :: !PackageNameP
  , hciVersion     :: !VersionP
  , hciRevision    :: !(Maybe Revision)
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
