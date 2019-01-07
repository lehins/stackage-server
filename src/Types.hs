{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Types
    ( SnapshotBranch(..)
    , PackageNameP(..)
    , unPackageName -- TODO: rename to packageNameText
    , VersionP(..)
    , Revision(..)
    , VersionRev(..)
    , ModuleNameP(..)
    , PackageIdentifierP(..)
    , PackageNameVersion(..)
    , HoogleVersion(..)
    , currentHoogleVersion
    , UnpackStatus(..)
    , StackageExecutable(..)
    , GhcMajorVersion(..)
    , GhcMajorVersionFailedParse(..)
    , ghcMajorVersionToText
    , ghcMajorVersionFromText
    , dtDisplay
    , SupportedArch(..)
    , Year
    , Month(Month)
    ) where

import RIO
import Data.Aeson
import Data.Hashable (hashUsing)
import Text.Blaze (ToMarkup(..))
import Database.Persist.Sql (PersistFieldSql (sqlType))
import Database.Persist
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Read as Reader
import Data.Char (ord)
import Control.Monad.Catch (MonadThrow, throwM)
import Pantry.Types
import Web.PathPieces
import Data.Hashable
import Database.Esqueleto.Internal.Language
import ClassyPrelude.Yesod (ToBuilder(..))
import Distribution.ModuleName (ModuleName(..))
import qualified Distribution.Text as DT (Text, display)

dtDisplay :: (DT.Text a, IsString b) => a -> b
dtDisplay = fromString . DT.display

data SnapshotBranch = LtsMajorBranch Int
                    | LtsBranch
                    | NightlyBranch
                    deriving (Eq, Read, Show)
instance PathPiece SnapshotBranch where
    toPathPiece NightlyBranch = "nightly"
    toPathPiece LtsBranch     = "lts"
    toPathPiece (LtsMajorBranch x) = "lts-" <> T.pack (show x)

    fromPathPiece "nightly" = Just NightlyBranch
    fromPathPiece "lts" = Just LtsBranch
    fromPathPiece t0 = do
        t1 <- T.stripPrefix "lts-" t0
        Right (x, "") <- Just $ Reader.decimal t1
        Just $ LtsMajorBranch x

unPackageName :: PackageNameP -> Text
unPackageName = utf8BuilderToText . display

newtype PackageSetIdent = PackageSetIdent { unPackageSetIdent :: Text }
    deriving (Show, Read, Typeable, Eq, Ord, Hashable, PathPiece, ToMarkup, PersistField)
instance PersistFieldSql PackageSetIdent where
    sqlType = sqlType . liftM unPackageSetIdent

data PackageNameVersion = PNVTarball !PackageNameP !VersionP
                        | PNVNameVersion !PackageNameP !VersionP
                        | PNVName !PackageNameP
    deriving (Read, Show, Eq, Ord)

data PackageIdentifierP =
    PackageIdentifierP !PackageNameP
                       !VersionP
    deriving (Eq, Ord, Show)

instance PathPiece PackageIdentifierP where
    toPathPiece (PackageIdentifierP x y) = T.concat [toPathPiece x, "-", toPathPiece y]
    fromPathPiece t = do
        let (tName', tVer) = T.breakOnEnd "-" t
        (tName, '-') <- T.unsnoc tName'
        guard $ not (T.null tName || T.null tVer)
        PackageIdentifierP <$> fromPathPiece tName <*> fromPathPiece tVer
instance ToMarkup PackageIdentifierP where
    toMarkup = toMarkup . toPathPiece

instance Hashable PackageNameP where
  hashWithSalt = hashUsing unPackageName
instance ToBuilder PackageNameP Builder where
    toBuilder = getUtf8Builder . display

instance PathPiece PackageNameP where
    fromPathPiece = fmap PackageNameP . parsePackageName . T.unpack
    toPathPiece (PackageNameP pn) = T.pack $ packageNameString pn
instance ToMarkup PackageNameP where
    toMarkup (PackageNameP pn) = toMarkup $ packageNameString pn
instance SqlString PackageNameP

instance PathPiece VersionP where
    fromPathPiece = fmap VersionP . parseVersion . T.unpack
    toPathPiece (VersionP v) = T.pack $ versionString v
instance ToMarkup VersionP where
    toMarkup (VersionP v) = toMarkup $ versionString v
instance ToBuilder VersionP Builder where
    toBuilder = getUtf8Builder . display

instance ToMarkup Revision where
    toMarkup (Revision r) = "rev:" <> toMarkup r

data VersionRev = VersionRev !VersionP !(Maybe Revision) deriving (Eq, Show)

instance ToMarkup VersionRev where
    toMarkup (VersionRev version mrev) =
        toMarkup version <> maybe "" (("@" <>) . toMarkup) mrev


instance PathPiece PackageNameVersion where
    toPathPiece (PNVTarball x y) = T.concat [toPathPiece x, "-", toPathPiece y, ".tar.gz"]
    toPathPiece (PNVNameVersion x y) = T.concat [toPathPiece x, "-", toPathPiece y]
    toPathPiece (PNVName x) = toPathPiece x
    fromPathPiece t'
        | Just t <- T.stripSuffix ".tar.gz" t' = do
            PackageIdentifierP name version <- fromPathPiece t
            return $ PNVTarball name version
    fromPathPiece t =
        case T.breakOnEnd "-" t of
            ("", _) -> PNVName <$> fromPathPiece t
            (fromPathPiece . T.init -> Just name, fromPathPiece -> Just version) ->
                Just $ PNVNameVersion name version
            _ -> PNVName <$> fromPathPiece t


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
        | i < 10 = T.pack $ '0' : show i
        | otherwise = tshow i
    fromPathPiece "10" = Just $ Month 10
    fromPathPiece "11" = Just $ Month 11
    fromPathPiece "12" = Just $ Month 12
    fromPathPiece (T.unpack -> ['0', c])
        | '1' <= c && c <= '9' = Just $ Month $ ord c - ord '0'
    fromPathPiece _ = Nothing



newtype ModuleNameP = ModuleNameP {unModuleNameP :: ModuleName }
  deriving (Eq, Ord, Show, Read, Data, NFData, IsString)
instance Display ModuleNameP where
  display = dtDisplay . unModuleNameP
instance PersistField ModuleNameP where
  toPersistValue (ModuleNameP pn) = PersistText $ dtDisplay pn
  fromPersistValue v = ModuleNameP . fromString . T.unpack <$> fromPersistValue v
instance PersistFieldSql ModuleNameP where
  sqlType _ = SqlString
instance ToMarkup ModuleNameP where
  toMarkup = dtDisplay . unModuleNameP
