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
    , PantryHackageCabal(..)
    , pantryHackageCabalToPackageIdentifierRevision
    , PantryTree(..)
    , PantryPackage(..)
    , SnapshotFile(..)
    , PackageListingInfo(..)
    , ModuleListingInfo(..)
    , LatestInfo(..)
    ) where

import           Data.Bifunctor       (bimap)
import           Data.Aeson
import qualified Data.Text            as T
import           Data.Text.Read       (decimal)
import qualified Data.Text.Read       as T (decimal)
import           Database.Persist
import           Database.Persist.Sql hiding (LogFunc)
import           Pantry.SHA256
import           Pantry.Types
import           RIO
import           RIO.Process          (HasProcessContext (..), ProcessContext)
import           RIO.Time
import           Stackage.Types       (dtDisplay)
import           Web.PathPieces

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
    { scPantryConfig   :: !PantryConfig
    , scStackageRoot   :: !FilePath
    , scLogFunc        :: !LogFunc
    , scProcessContext :: !ProcessContext
    }

instance HasLogFunc StackageCron where
  logFuncL = lens scLogFunc (\c f -> c {scLogFunc = f})

instance HasProcessContext StackageCron where
  processContextL = lens scProcessContext (\c f -> c {scProcessContext = f})

instance HasPantryConfig StackageCron where
  pantryConfigL = lens scPantryConfig (\c f -> c {scPantryConfig = f})

instance HasStorage StackageCron where
  storageG = to (pcStorage . scPantryConfig)


data PantryHackageCabal = PantryHackageCabal
  { phcPackageName    :: !PackageName
  , phcPackageVersion :: !Version
  , phcSHA256         :: !SHA256
  , phcFileSize       :: !FileSize
  } deriving Show

data PantryTree = PantryTree
  { ptSHA256   :: !SHA256
  , ptFileSize :: !FileSize
  } deriving (Eq, Show)

data PantryPackage =
  PantryHackagePackage !PantryHackageCabal !PantryTree

-- | Convert a pantry representation for hackage cabal package.
pantryHackageCabalToPackageIdentifierRevision
  :: PantryHackageCabal -> PackageIdentifierRevision
pantryHackageCabalToPackageIdentifierRevision (PantryHackageCabal {..}) =
    PackageIdentifierRevision phcPackageName phcPackageVersion (CFIHash phcSHA256 (Just phcFileSize))

newtype Compiler =
    CompilerGHC { ghcVersion :: Version }
    deriving (Eq)

instance Show Compiler where
  show = displayCompiler

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
    { sfName     :: !SnapName
    , sfCompiler :: !Compiler
    , sfPackages :: ![PantryPackage]
    , sfHidden   :: !(Map PackageName Bool)
    , sfFlags    :: !(Map PackageName (Map Text Bool))
    }

-- QUESTION: Potentially switch to `parsePackageIdentifierRevision`:
   -- PackageIdentifierRevision pn v (CFIHash sha (Just size)) <-
   --     either (fail . displayException) pure $ parsePackageIdentifierRevision txt
   -- return (PantryHackageCabal pn v sha size)
-- Issues with such switch:
-- * CFILatest and CFIRevision do not make sense in stackage-snapshots
-- * Current implementation is faster
instance FromJSON PantryHackageCabal where
    parseJSON =
        withText "PantryHackageCabal" $ \txt -> do
            let (packageTxt, hashWithSize) = T.break (== '@') txt
                (hashTxtWithAlgo, sizeWithComma) = T.break (== ',') hashWithSize
            -- Split package identifier foo-bar-0.1.2 into package name and version
            (pkgNameTxt, pkgVersionTxt) <-
                case T.breakOnEnd ("-") packageTxt of
                    (pkgNameWithDashEnd, pkgVersionTxt)
                        | Just pkgName <- T.stripSuffix "-" pkgNameWithDashEnd ->
                            return (pkgName, pkgVersionTxt)
                    _ -> fail $ "Invalid package identifier format: " ++ T.unpack packageTxt
            phcPackageName <- parseJSON $ String pkgNameTxt
            phcPackageVersion <- parseJSON $ String pkgVersionTxt
            hashTxt <-
                maybe (fail $ "Unrecognized hashing algorithm: " ++ T.unpack hashTxtWithAlgo) pure $
                T.stripPrefix "@sha256:" hashTxtWithAlgo
            phcSHA256 <- either (fail . displayException) pure $ fromHexText hashTxt
            (phcFileSize, "") <-
                either fail (pure . first FileSize) =<<
                maybe
                    (fail $ "Wrong size format:" ++ show sizeWithComma)
                    (pure . T.decimal)
                    (T.stripPrefix "," sizeWithComma)
            return PantryHackageCabal {..}


instance FromJSON PantryTree where
    parseJSON = withObject "PantryTree" $ \obj -> do
      ptSHA256 <- obj .: "sha256"
      ptFileSize <- obj .: "size"
      return PantryTree {..}


instance FromJSON PantryPackage where
    parseJSON =
        withObject "PantryPackage" $ \obj ->
            PantryHackagePackage <$> obj .: "hackage" <*> obj .: "pantry-tree"


instance FromJSON SnapshotFile where
    parseJSON =
        withObject "SnapshotFile" $ \obj -> do
            sfName <- obj .: "name"
            sfCompiler <- obj .: "compiler"
            sfPackages <- obj .: "packages"
            sfHidden <- obj .:? "hidden" .!= mempty
            sfFlags <- obj .:? "flags" .!= mempty
            return SnapshotFile {..}


data PackageListingInfo = PackageListingInfo
    { pliName     :: !Text
    , pliVersion  :: !Text
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


data ModuleListingInfo = ModuleListingInfo
    { mliName           :: !Text
    , mliPackageVersion :: !Text
    } deriving Show


data LatestInfo = LatestInfo
    { liSnapName :: !SnapName
    , liVersion  :: !Text
    } deriving (Show, Eq, Ord)
