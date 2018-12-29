{-# LANBUAGE NoImplicitPrelude #-}
module Stackage.Database.PackageInfo
    ( PackageInfo(..)
    , formatPackageInfo
    , parseCabalBlob
    , extractDependencies
    ) where

import qualified Data.Text as T
import           Distribution.Compiler (CompilerFlavor(GHC))
import           Distribution.Package (Dependency(..), PackageIdentifier(..), PackageName)
import           Distribution.Package.ModuleForest
import           Distribution.PackageDescription
                         (CondTree(..), Condition(..), ConfVar(..),
                          Flag(flagName, flagDefault), GenericPackageDescription,
                          PackageDescription, author, packageDescription,
                          condLibrary, condExecutables, package, synopsis,
                          description, genPackageFlags, homepage, license, maintainer)
import           Distribution.PackageDescription.Parsec
                         (parseGenericPackageDescription, runParseResult)
import           Distribution.Pretty (prettyShow)
import           Distribution.System (Arch(X86_64), OS(Linux))
import           Distribution.Types.CondTree (CondBranch (..))
import           Distribution.Types.Library (exposedModules)
import           Distribution.Types.VersionRange
                         (VersionRange, intersectVersionRanges, normaliseVersionRange, withinRange)
import           Distribution.Version (mkVersion, simplifyVersionRange)
import           Pantry.Types (PackageNameP(..))
import           RIO
import qualified RIO.Map as Map
import           Stackage.Database.Haddock (renderHaddock)
import           Stackage.Types (dtDisplay)
import           Text.Blaze.Html (Html)

data PackageInfo = PackageInfo
    { piName         :: Text
    , piSynopsis     :: Text
    , piDescription  :: Html
    , piChangelog    :: Html
    , piAuthor       :: Text
    , piMaintainer   :: Text
    , piHomepage     :: Text
    , piLicenseName  :: Text
    , piModuleForest :: ModuleForest
    }


formatPackageInfo :: GenericPackageDescription -> PackageInfo
formatPackageInfo gpd =
    PackageInfo
        { piName = dtDisplay $ pkgName $ package pd
        , piSynopsis = T.pack $ synopsis pd
        , piDescription = renderHaddock (description pd) -- FIXME: use README.md if available
        , piChangelog = mempty -- FIXME: get changelog
        , piAuthor = T.pack $ author pd
        , piMaintainer = T.pack $ maintainer pd
        , piHomepage = T.pack $ homepage pd
        , piLicenseName = T.pack $ prettyShow $ license pd
        , piModuleForest = moduleForest $ maybe [] toForest $ condLibrary gpd
        }
  where pd = packageDescription gpd
        toForest = exposedModules . condTreeData

extractDependencies :: GenericPackageDescription -> Map PackageName VersionRange
extractDependencies gpd =
    combineDeps $
    maybeToList (getDeps' <$> condLibrary gpd) ++ map (getDeps' . snd) (condExecutables gpd)
  where
    getDeps' :: CondTree ConfVar [Dependency] a -> Map PackageName VersionRange
    getDeps' = getDeps (getCheckCond gpd)


parseCabalBlob ::
       (MonadIO f, MonadReader env f, HasLogFunc env)
    => PackageName
    -> ByteString
    -> f (Maybe GenericPackageDescription)
parseCabalBlob packageName cabalBlob =
    case snd $ runParseResult $ parseGenericPackageDescription cabalBlob of
        Left err ->
            Nothing <$
            logError
                ("Problem parsing of cabal file '" <> display (PackageNameP packageName) <> "': " <>
                 displayShow err)
        Right pgd -> pure $ Just pgd

-- TODO: supply ghc version from the snapshot?
getCheckCond :: GenericPackageDescription -> Condition ConfVar -> Bool
getCheckCond gpd = go
  where
    go (Var (OS os)) = os == Linux -- arbitrary
    go (Var (Arch arch)) = arch == X86_64 -- arbitrary
    go (Var (Flag flag)) = fromMaybe False $ Map.lookup flag flags -- arbitrary
    go (Var (Impl flavor range)) = flavor == GHC && ghcVersion `withinRange` range
    go (Lit b) = b
    go (CNot c) = not $ go c
    go (CAnd x y) = go x && go y
    go (COr x y) = go x || go y
    ghcVersion = mkVersion [8, 0, 2] -- arbitrary
    flags = Map.fromList $ map toPair $ genPackageFlags gpd
      where
        toPair f = (flagName f, flagDefault f)

getDeps ::
       (Condition ConfVar -> Bool)
    -> CondTree ConfVar [Dependency] a
    -> Map PackageName VersionRange
getDeps checkCond = goTree
  where
    goTree (CondNode _data deps comps) =
        combineDeps $
        map (\(Dependency name range) -> Map.singleton name range) deps ++ map goComp comps
    goComp (CondBranch cond yes no)
        | checkCond cond = goTree yes
        | otherwise = maybe Map.empty goTree no


combineDeps :: [Map PackageName VersionRange] -> Map PackageName VersionRange
combineDeps =
    Map.unionsWith
        (\x -> normaliseVersionRange . simplifyVersionRange . intersectVersionRanges x)
