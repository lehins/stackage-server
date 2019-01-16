{-# LANGUAGE NoImplicitPrelude #-}
module Stackage.Database.PackageInfo
    ( PackageInfo(..)
    , toPackageInfo
    , parseCabalBlob
    , parseCabalBlobMaybe
    , extractDependencies
    , getModuleNames
    , getSynopsis
    ) where

import           Data.Coerce
import           Data.Map.Merge.Strict                  as Map
import qualified Data.Text                              as T
import           Distribution.Compiler                  (CompilerFlavor (GHC))
import           Distribution.Package                   (Dependency (..),
                                                         PackageIdentifier (..))
import           Distribution.PackageDescription        (CondTree (..),
                                                         Condition (..),
                                                         ConfVar (..),
                                                         Flag (flagDefault, flagName),
                                                         FlagName,
                                                         GenericPackageDescription,
                                                         author,
                                                         condExecutables,
                                                         condLibrary,
                                                         description,
                                                         genPackageFlags,
                                                         homepage, license,
                                                         maintainer, package,
                                                         packageDescription,
                                                         synopsis)
import           Distribution.PackageDescription.Parsec (parseGenericPackageDescription,
                                                         runParseResult)
import           Distribution.Pretty                    (prettyShow)
import           Distribution.System                    (Arch (X86_64),
                                                         OS (Linux))
import           Distribution.Types.CondTree            (CondBranch (..))
import           Distribution.Types.Library             (exposedModules)
import           Distribution.Types.VersionRange        (VersionRange,
                                                         intersectVersionRanges,
                                                         normaliseVersionRange,
                                                         withinRange)
import           Distribution.Version                   (simplifyVersionRange)
import           RIO
import qualified RIO.Map                                as Map
import qualified RIO.Map.Unchecked                      as Map (mapKeysMonotonic)
import           Stackage.Database.Haddock              (renderHaddock)
import           Text.Blaze.Html                        (Html)
import           Types                                  (CompilerP(..),
                                                         FlagNameP(..),
                                                         ModuleNameP (..),
                                                         PackageNameP (..),
                                                         VersionP (..),
                                                         VersionRangeP (..))

data PackageInfo = PackageInfo
    { piName        :: PackageNameP
    , piVersion     :: VersionP
    , piSynopsis    :: Text
    , piDescription :: Html
    , piChangelog   :: Html
    , piAuthor      :: Text
    , piMaintainer  :: Text
    , piHomepage    :: Text
    , piLicenseName :: Text
    }


toPackageInfo :: GenericPackageDescription -> PackageInfo
toPackageInfo gpd =
    PackageInfo
        { piName = PackageNameP $ pkgName $ package pd
        , piVersion = VersionP $ pkgVersion $ package pd
        , piSynopsis = T.pack $ synopsis pd
        , piDescription = renderHaddock (description pd) -- FIXME: use README.md if available
        , piChangelog = mempty -- FIXME: get changelog
        , piAuthor = T.pack $ author pd
        , piMaintainer = T.pack $ maintainer pd
        , piHomepage = T.pack $ homepage pd
        , piLicenseName = T.pack $ prettyShow $ license pd
        }
  where pd = packageDescription gpd

getSynopsis :: GenericPackageDescription -> Text
getSynopsis = T.pack . synopsis . packageDescription

getModuleNames :: GenericPackageDescription -> [ModuleNameP]
getModuleNames = maybe [] (coerce . exposedModules . condTreeData) . condLibrary


extractDependencies ::
       CompilerP -> Map FlagNameP Bool -> GenericPackageDescription -> Map PackageNameP VersionRangeP
extractDependencies compiler flags gpd =
    fmap VersionRangeP $
    combineDeps $
    maybeToList (getDeps' <$> condLibrary gpd) ++ map (getDeps' . snd) (condExecutables gpd)
  where
    getDeps' :: CondTree ConfVar [Dependency] a -> Map PackageNameP VersionRange
    getDeps' = getDeps (getCheckCond compiler (Map.mapKeysMonotonic unFlagNameP flags) gpd)

-- | Parse a cabal blob and throw an error on failure.
parseCabalBlob :: ByteString -> GenericPackageDescription
parseCabalBlob cabalBlob =
    case snd $ runParseResult $ parseGenericPackageDescription cabalBlob of
        Left err  -> error $ "Problem parsing cabal blob: " <> show err
        Right pgd -> pgd


parseCabalBlobMaybe ::
       (MonadIO m, MonadReader env m, HasLogFunc env)
    => PackageNameP
    -> ByteString
    -> m (Maybe GenericPackageDescription)
parseCabalBlobMaybe packageName cabalBlob =
    case snd $ runParseResult $ parseGenericPackageDescription cabalBlob of
        Left err ->
            Nothing <$
            logError
                ("Problem parsing cabal blob for '" <> display packageName <> "': " <>
                 displayShow err)
        Right pgd -> pure $ Just pgd

getCheckCond ::
       CompilerP -> Map FlagName Bool -> GenericPackageDescription -> Condition ConfVar -> Bool
getCheckCond compiler overrideFlags gpd = go
  where
    go (Var (OS os)) = os == Linux -- arbitrary
    go (Var (Arch arch)) = arch == X86_64 -- arbitrary
    go (Var (Flag flag)) = fromMaybe False $ Map.lookup flag flags
    go (Var (Impl flavor range)) = flavor == compilerFlavor && compilerVersion `withinRange` range
    go (Lit b) = b
    go (CNot c) = not $ go c
    go (CAnd x y) = go x && go y
    go (COr x y) = go x || go y
    (compilerFlavor, compilerVersion) =
        case compiler of
            CompilerGHC ver -> (GHC, unVersionP ver)
    flags =
        Map.merge
            Map.dropMissing -- unknown flags should be discarded
            Map.preserveMissing -- non-overriden flags stay as default
            (Map.zipWithMatched (\_flagName new _default -> new)) -- override the flag
            overrideFlags $
        Map.fromList $ map toPair $ genPackageFlags gpd
      where
        toPair f = (flagName f, flagDefault f)

getDeps ::
       (Condition ConfVar -> Bool)
    -> CondTree ConfVar [Dependency] a
    -> Map PackageNameP VersionRange
getDeps checkCond = goTree
  where
    goTree (CondNode _data deps comps) =
        combineDeps $
        map (\(Dependency name range) -> Map.singleton (PackageNameP name) range) deps ++
        map goComp comps
    goComp (CondBranch cond yes no)
        | checkCond cond = goTree yes
        | otherwise = maybe Map.empty goTree no


combineDeps :: [Map PackageNameP VersionRange] -> Map PackageNameP VersionRange
combineDeps =
    Map.unionsWith
        (\x -> normaliseVersionRange . simplifyVersionRange . intersectVersionRanges x)
