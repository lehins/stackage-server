{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import ClassyPrelude.Yesod
import Data.Aeson
import Data.Hashable (hashUsing)
import Text.Blaze (ToMarkup)
import Database.Persist.Sql (PersistFieldSql (sqlType))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Read as Reader
import Data.Char (ord)
import Control.Monad.Catch (MonadThrow, throwM)

data SnapshotBranch = LtsMajorBranch Int
                    | LtsBranch
                    | NightlyBranch
                    deriving (Eq, Read, Show)
instance PathPiece SnapshotBranch where
    toPathPiece NightlyBranch = "nightly"
    toPathPiece LtsBranch     = "lts"
    toPathPiece (LtsMajorBranch x) = "lts-" ++ tshow x

    fromPathPiece "nightly" = Just NightlyBranch
    fromPathPiece "lts" = Just LtsBranch
    fromPathPiece t0 = do
        t1 <- stripPrefix "lts-" t0
        Right (x, "") <- Just $ Reader.decimal t1
        Just $ LtsMajorBranch x

newtype PackageName = PackageName { unPackageName :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField, IsString)
instance ToJSON PackageName where
    toJSON = toJSON . unPackageName
instance ToJSONKey PackageName
instance PersistFieldSql PackageName where
    sqlType = sqlType . liftM unPackageName
newtype Version = Version { unVersion :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField)
instance ToJSON Version where
    toJSON = toJSON . unVersion
instance PersistFieldSql Version where
    sqlType = sqlType . liftM unVersion
newtype PackageSetIdent = PackageSetIdent { unPackageSetIdent :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField)
instance PersistFieldSql PackageSetIdent where
    sqlType = sqlType . liftM unPackageSetIdent

data PackageNameVersion = PNVTarball !PackageName !Version
                        | PNVNameVersion !PackageName !Version
                        | PNVName !PackageName
    deriving (Show, Read, Typeable, Eq, Ord)

instance PathPiece PackageNameVersion where
    toPathPiece (PNVTarball x y) = concat [toPathPiece x, "-", toPathPiece y, ".tar.gz"]
    toPathPiece (PNVNameVersion x y) = concat [toPathPiece x, "-", toPathPiece y]
    toPathPiece (PNVName x) = toPathPiece x
    fromPathPiece t' | Just t <- stripSuffix ".tar.gz" t' =
        case T.breakOnEnd "-" t of
            ("", _) -> Nothing
            (_, "") -> Nothing
            (T.init -> name, version) -> Just $ PNVTarball (PackageName name) (Version version)
    fromPathPiece t = Just $
        case T.breakOnEnd "-" t of
            ("", _) -> PNVName (PackageName t)
            (T.init -> name, version) | validVersion version ->
                PNVNameVersion (PackageName name) (Version version)
            _ -> PNVName (PackageName t)
      where
        validVersion =
            all f
          where
            f c = (c == '.') || ('0' <= c && c <= '9')

newtype HoogleVersion = HoogleVersion Text
    deriving (Show, Eq, Ord, Typeable, PathPiece)
currentHoogleVersion :: HoogleVersion
currentHoogleVersion = HoogleVersion VERSION_hoogle

data UnpackStatus = USReady
                  | USBusy
                  | USFailed !Text

data StackageExecutable
    = StackageWindowsExecutable
    | StackageUnixExecutable
    deriving (Show, Read, Eq)

instance PathPiece StackageExecutable where
    -- TODO: distribute stackage, not just stackage-setup
    toPathPiece StackageWindowsExecutable = "stackage-setup.exe"
    toPathPiece StackageUnixExecutable = "stackage-setup"

    fromPathPiece "stackage-setup" = Just StackageUnixExecutable
    fromPathPiece "stackage-setup.exe" = Just StackageWindowsExecutable
    fromPathPiece _ = Nothing

data GhcMajorVersion = GhcMajorVersion !Int !Int
  deriving (Eq)

data GhcMajorVersionFailedParse = GhcMajorVersionFailedParse Text
  deriving (Show, Typeable)
instance Exception GhcMajorVersionFailedParse

ghcMajorVersionToText :: GhcMajorVersion -> Text
ghcMajorVersionToText (GhcMajorVersion a b)
  = LText.toStrict
  $ Builder.toLazyText
  $ Builder.decimal a <> "." <> Builder.decimal b

ghcMajorVersionFromText :: MonadThrow m => Text -> m GhcMajorVersion
ghcMajorVersionFromText t = case Reader.decimal t of
  Right (a, T.uncons -> Just ('.', t')) -> case Reader.decimal t' of
    Right (b, t'') | T.null t'' -> return $ GhcMajorVersion a b
    _ -> failedParse
  _ -> failedParse
  where
    failedParse = throwM $ GhcMajorVersionFailedParse t

instance PersistFieldSql GhcMajorVersion where
    sqlType = sqlType . liftM ghcMajorVersionToText

instance PersistField GhcMajorVersion where
    toPersistValue = toPersistValue . ghcMajorVersionToText
    fromPersistValue v = do
        t <- fromPersistValueText v
        case ghcMajorVersionFromText t of
            Just ver -> return ver
            Nothing -> Left $ "Cannot convert to GhcMajorVersion: " <> t

instance Hashable GhcMajorVersion where
  hashWithSalt = hashUsing ghcMajorVersionToText

instance FromJSON GhcMajorVersion where
  parseJSON = withText "GhcMajorVersion" $
    either (fail . show) return . ghcMajorVersionFromText

instance ToJSON GhcMajorVersion where
  toJSON = toJSON . ghcMajorVersionToText


data SupportedArch
    = Win32
    | Win64
    | Linux32
    | Linux64
    | Mac32
    | Mac64
    deriving (Enum, Bounded, Show, Read, Eq)

instance Hashable SupportedArch where
    hashWithSalt = hashUsing fromEnum

instance PathPiece SupportedArch where
    toPathPiece Win32 = "win32"
    toPathPiece Win64 = "win64"
    toPathPiece Linux32 = "linux32"
    toPathPiece Linux64 = "linux64"
    toPathPiece Mac32 = "mac32"
    toPathPiece Mac64 = "mac64"

    fromPathPiece "win32" = Just Win32
    fromPathPiece "win64" = Just Win64
    fromPathPiece "linux32" = Just Linux32
    fromPathPiece "linux64" = Just Linux64
    fromPathPiece "mac32" = Just Mac32
    fromPathPiece "mac64" = Just Mac64
    fromPathPiece _ = Nothing

type Year = Int
newtype Month = Month Int
  deriving (Eq, Read, Show, Ord)
instance PathPiece Month where
  toPathPiece (Month i)
    | i < 10 = pack $ '0' : show i
    | otherwise = tshow i
  fromPathPiece "10" = Just $ Month 10
  fromPathPiece "11" = Just $ Month 11
  fromPathPiece "12" = Just $ Month 12
  fromPathPiece (unpack -> ['0', c])
    | '1' <= c && c <= '9' = Just $ Month $ ord c - ord '0'
  fromPathPiece _ = Nothing
