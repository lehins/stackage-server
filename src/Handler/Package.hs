{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- | Lists the package page similar to Hackage.

module Handler.Package
    ( getPackageR
    , getPackageSnapshotsR
    , packagePage
    , getPackageBadgeR
    , renderNoPackages
    ) where

import Control.Lens
import Data.Char
import Data.Coerce
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Distribution.Package.ModuleForest
import Graphics.Badge.Barrier
import Import
import Stackage.Database
import Stackage.Database.PackageInfo (PackageInfo(..))
import Stackage.Database.Types (HackageCabalInfo(..), LatestInfo(..),
                                ModuleListingInfo(..))
import qualified Text.Blaze.Html.Renderer.Text as LT
import Text.Email.Validate
import Yesod.GitRepo

-- | Page metadata package.
getPackageR :: PackageNameP -> Handler Html
getPackageR = track "Handler.Package.getPackageR" . packagePage Nothing

getPackageBadgeR :: PackageNameP -> SnapshotBranch -> Handler TypedContent
getPackageBadgeR pname branch = track "Handler.Package.getPackageBadgeR" $ do
    cacheSeconds (3 * 60 * 60)
    snapName     <- maybe notFound pure =<< newestSnapshot branch
    Entity sid _ <- maybe notFound pure =<< lookupSnapshot snapName
    mVersion <- getPackageVersionForSnapshot sid pname

    mLabel <- lookupGetParam "label"
    mStyle <- lookupGetParam "style"

    respond typeSvg $ case mStyle of
      Just "plastic"     -> renderStackageBadge plastic    mLabel snapName mVersion
      Just "flat-square" -> renderStackageBadge flatSquare mLabel snapName mVersion
      _                  -> renderStackageBadge flat       mLabel snapName mVersion

renderStackageBadge :: (Badge b, HasRightColor b)
                    => b          -- ^ Style
                    -> Maybe Text -- ^ Label
                    -> SnapName
                    -> Maybe VersionP
                    -> LByteString
renderStackageBadge style mLabel snapName = \case
    Nothing -> renderBadge (style & right .~ lightgray) badgeLabel "not available"
    Just v -> renderBadge style badgeLabel $ toPathPiece v
  where
    badgeLabel = fromMaybe ("stackage " <> badgeSnapName snapName) mLabel

    badgeSnapName (SNNightly _) = "nightly"
    badgeSnapName (SNLts x _)   = "lts-" <> tshow x

checkSpam :: PackageNameP -> Handler Html -> Handler Html
checkSpam pname inner = do
    wc <- getYesod >>= liftIO . grContent . appWebsiteContent
    if pname `member` wcSpamPackages wc
      then defaultLayout $ do
        setTitle $ "Spam package detected: " <> toHtml pname
        $(widgetFile "spam-package")
      else inner

packagePage :: Maybe SnapshotPackageInfo
            -> PackageNameP
            -> Handler Html
packagePage mspi pname =
    track "Handler.Package.packagePage" $
    checkSpam pname $
        maybe (getSnapshotPackageLatestVersion pname) (return . Just) mspi >>= \case
          Nothing -> notFound -- getHackageLatestVersion pname >>= maybe notFound return
          Just spi -> handleSnapshotPackage spi

handleSnapshotPackage :: SnapshotPackageInfo -> Handler Html
handleSnapshotPackage spi = do
    (isDeprecated, inFavourOf) <- getDeprecated pname
    latests <- getLatests pname
    PackageInfo {..} <- getPackageInfo spi
    deps <- map (first dropVersionRev) <$> getForwardDeps spi (Just maxDisplayedDeps)
    revDeps <- map (first dropVersionRev) <$> getReverseDeps spi (Just maxDisplayedDeps)
    (depsCount, revDepsCount) <- getDepsCount spi
    mhciLatest <-
        case spiOrigin spi of
            Hackage -> getHackageLatestVersion pname
            _       -> pure Nothing
    let mdocs = Just (spiSnapName spi, piVersion, piModuleNames)
        mSnapName = Just $ spiSnapName spi
        mdisplayedVersion = Just $ spiVersionRev spi
        (packageDepsLink, packageRevDepsLink) =
            case mSnapName of
                Nothing -> (PackageDepsR pname, PackageRevDepsR pname)
                Just snap ->
                    let wrap f = SnapshotR snap $ f $ PNVNameVersion pname (spiVersion spi)
                     in (wrap SnapshotPackageDepsR, wrap SnapshotPackageRevDepsR)
    let mhomepage =
            case T.strip piHomepage of
                x
                    | null x -> Nothing
                    | otherwise -> Just x
        authors = enumerate (parseIdentitiesLiberally piAuthor)
        maintainers =
            let ms = enumerate (parseIdentitiesLiberally piMaintainer)
             in if ms == authors
                    then []
                    else ms
    defaultLayout $ do
        setTitle $ toHtml piName
        $(combineScripts 'StaticR [js_highlight_js])
        $(combineStylesheets 'StaticR [css_font_awesome_min_css, css_highlight_github_css])
        let hoogleForm name =
                let exact = False
                    mPackageName = Just pname
                    queryText = "" :: Text
                 in $(widgetFile "hoogle-form")
        $(widgetFile "package")
  where
    pname = spiPackageName spi
    dropVersionRev (PackageVersionRev pname' _) = pname'
    enumerate = zip [0 :: Int ..]
    renderModules sname packageIdentifier = renderForest [] . moduleForest . coerce
      where
        renderForest _ [] = mempty
        renderForest pathRev trees =
            [hamlet|<ul .docs-list>
                        $forall tree <- trees
                          ^{renderTree tree}
              |]
          where
            renderTree Node {..} =
                [hamlet|
                  <li>
                    $if isModule
                      <a href=@{haddockUrl sname mli}>#{modName}
                    $else
                      #{modName}
                    ^{renderForest pathRev' subModules}
                |]
              where
                mli = ModuleListingInfo modName packageIdentifier
                pathRev' = component : pathRev
                modName = moduleNameFromComponents (reverse pathRev')
    maxDisplayedDeps :: Int
    maxDisplayedDeps = 40

-- | An identifier specified in a package. Because this field has
-- quite liberal requirements, we often encounter various forms. A
-- name, a name and email, just an email, or maybe nothing at all.
data Identifier
  = EmailOnly !EmailAddress -- ^ An email only e.g. jones@example.com
  | Contact !Text
            !EmailAddress -- ^ A contact syntax, e.g. Dave Jones <jones@example.com>
  | PlainText !Text -- ^ Couldn't parse anything sensible, leaving as-is.
  deriving (Show,Eq)

-- | An author/maintainer field may contain a comma-separated list of
-- identifiers. It may be the case that a person's name is written as
-- "Einstein, Albert", but we only parse commas when there's an
-- accompanying email, so that would be:
--
--  Einstein, Albert <emc2@gmail.com>, Isaac Newton <falling@apple.com>
--
-- Whereas
--
-- Einstein, Albert, Isaac Newton
--
-- Will just be left alone. It's an imprecise parsing because the
-- input is wide open, but it's better than nothing:
--
-- λ> parseIdentitiesLiberally "Chris Done, Dave Jones <chrisdone@gmail.com>, Einstein, Albert, Isaac Newton, Michael Snoyman <michael@snoyman.com>"
-- [PlainText "Chris Done"
-- ,Contact "Dave Jones" "chrisdone@gmail.com"
-- ,PlainText "Einstein, Albert, Isaac Newton"
-- ,Contact "Michael Snoyman" "michael@snoyman.com"]
--
-- I think that is quite a predictable and reasonable result.
--
parseIdentitiesLiberally :: Text -> [Identifier]
parseIdentitiesLiberally =
    filter (not . emptyPlainText) .
    map strip .
    concatPlains .
    map parseChunk .
    T.split (== ',')
    where emptyPlainText (PlainText e) = T.null e
          emptyPlainText _             = False
          strip (PlainText t) = PlainText (T.strip t)
          strip x             = x
          concatPlains = go
            where go (PlainText x:PlainText y:xs) =
                    go (PlainText (x <> "," <> y) :
                        xs)
                  go (x:xs) = x : go xs
                  go [] = []

-- | Try to parse a chunk into an identifier.
--
-- 1. First tries to parse an \"email@domain.com\".
-- 2. Then tries to parse a \"Foo <email@domain.com>\".
-- 3. Finally gives up and returns a plain text.
--
-- λ> parseChunk "foo@example.com"
-- EmailOnly "foo@example.com"
-- λ> parseChunk "Dave Jones <dave@jones.com>"
-- Contact "Dave Jones" "dave@jones.com"
-- λ> parseChunk "<x>"
-- PlainText "<x>"
-- λ> parseChunk "Hello!"
-- PlainText "Hello!"
--
parseChunk :: Text -> Identifier
parseChunk chunk =
    case emailAddress (T.encodeUtf8 (T.strip chunk)) of
      Just email -> EmailOnly email
      Nothing ->
        case T.stripPrefix
               ">"
               (T.dropWhile isSpace
                            (T.reverse chunk)) of
          Just rest ->
            case T.span (/= '<') rest of
              (T.reverse -> emailStr,this) ->
                case T.stripPrefix "< " this of
                  Just (T.reverse -> name) ->
                    case emailAddress (T.encodeUtf8 (T.strip emailStr)) of
                      Just email ->
                        Contact (T.strip name) email
                      _ -> plain
                  _ -> plain
          _ -> plain
    where plain = PlainText chunk

-- | Render email to text.
renderEmail :: EmailAddress -> Text
renderEmail = T.decodeUtf8 . toByteString

getPackageSnapshotsR :: PackageNameP -> Handler Html
getPackageSnapshotsR pn =
    track "Handler.Package.getPackageSnapshotsR" $ do
        snapshots <- getSnapshotsForPackage pn Nothing
        defaultLayout
            (do setTitle ("Packages for " >> toHtml pn)
                $(combineStylesheets 'StaticR [css_font_awesome_min_css])
                $(widgetFile "package-snapshots"))

renderNoPackages :: Int -> Text
renderNoPackages n = T.pack $ show n ++ " package" ++ if n == 1 then "" else "s"
