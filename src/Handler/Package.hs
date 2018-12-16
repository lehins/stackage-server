{-# LANGUAGE NoImplicitPrelude #-}

-- | Lists the package page similar to Hackage.

module Handler.Package
    ( getPackageR
    , getPackageSnapshotsR
    , packagePage
    , getPackageBadgeR
    , renderNoPackages
    ) where

import           Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Distribution.Package.ModuleForest
import           Graphics.Badge.Barrier
import           Control.Lens
import           Import
import qualified Text.Blaze.Html.Renderer.Text as LT
import           Text.Email.Validate
import           Stackage.Database
import           Yesod.GitRepo

-- | Page metadata package.
getPackageR :: PackageName -> Handler Html
getPackageR = track "Handler.Package.getPackageR" . packagePage Nothing

getPackageBadgeR :: PackageName -> SnapshotBranch -> Handler TypedContent
getPackageBadgeR pname branch = track "Handler.Package.getPackageBadgeR" $ do
    cacheSeconds (3 * 60 * 60)
    snapName     <- maybe notFound pure =<< inRIO (newestSnapshot branch)
    Entity sid _ <- maybe notFound pure =<< inRIO (lookupSnapshot snapName)
    mVersion <- do mSnapPackage <- inRIO $ lookupSnapshotPackage sid (unPackageName pname)
                   pure (Version . snapshotPackageVersion . entityVal <$> mSnapPackage)

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
                    -> Maybe Version
                    -> LByteString
renderStackageBadge style mLabel snapName = \case
    Nothing          -> renderBadge (style & right .~ lightgray) badgeLabel "not available"
    Just (Version x) -> renderBadge style badgeLabel x
  where
    badgeLabel = fromMaybe ("stackage " <> badgeSnapName snapName) mLabel

    badgeSnapName (SNNightly _) = "nightly"
    badgeSnapName (SNLts x _)   = "lts-" <> tshow x

checkSpam :: PackageName -> Handler Html -> Handler Html
checkSpam name inner = do
    wc <- getYesod >>= liftIO . grContent . appWebsiteContent
    if name `member` wcSpamPackages wc
      then defaultLayout $ do
        setTitle $ "Spam package detected: " <> toHtml name
        $(widgetFile "spam-package")
      else inner

packagePage :: Maybe (SnapName, Version)
            -> PackageName
            -> Handler Html
packagePage mversion pname = track "Handler.Package.packagePage" $ checkSpam pname $ do
    let pname' = toPathPiece pname
    (deprecated, inFavourOf) <- inRIO $ getDeprecated pname'
    latests <- inRIO $ getLatests pname'
    deps' <- inRIO $ getDeps pname' $ Just maxDisplayedDeps
    revdeps' <- inRIO $ getRevDeps pname' $ Just maxDisplayedDeps
    (depsCount, revdepsCount) <- inRIO $ getDepsCount pname'
    Entity _ package <- inRIO (getPackage pname') >>= maybe notFound return

    mdocs <-
        case mversion of
            Just (sname, version) -> do
                ms <- inRIO $ getPackageModules sname pname'
                return $ Just (sname, toPathPiece version, ms)
            Nothing ->
                case latests of
                    li:_ -> do
                        ms <- inRIO $ getPackageModules (liSnapName li) pname'
                        return $ Just (liSnapName li, liVersion li, ms)
                    [] -> return Nothing

    let ixInFavourOf = zip [0::Int ..] inFavourOf
        mdisplayedVersion = toPathPiece . snd <$> mversion
        latestVersion = packageLatest package

    let homepage = case T.strip (packageHomepage package) of
                     x | null x -> Nothing
                       | otherwise -> Just x
        synopsis = packageSynopsis package
        deps = enumerate deps'
        revdeps = enumerate revdeps'
        authors = enumerate (parseIdentitiesLiberally (packageAuthor package))
        maintainers = let ms = enumerate (parseIdentitiesLiberally (packageMaintainer package))
                      in if ms == authors
                            then []
                            else ms
    defaultLayout $ do
        setTitle $ toHtml pname
        $(combineScripts 'StaticR
                          [ js_highlight_js
                          ])
        $(combineStylesheets 'StaticR
            [ css_font_awesome_min_css
            , css_highlight_github_css
            ])
        let pn = pname
            toPkgVer x y = concat [x, "-", y]
            hoogleForm name =
              let exact = False
                  mPackageName = Just pname
                  queryText = "" :: Text
               in $(widgetFile "hoogle-form")
        $(widgetFile "package")
  where enumerate = zip [0::Int ..]
        renderModules sname version = renderForest [] . moduleForest . map moduleName
          where
            renderForest _ [] = mempty
            renderForest pathRev trees =
              [hamlet|<ul .docs-list>
                        $forall tree <- trees
                          ^{renderTree tree}
              |]
              where
                renderTree (Node{..}) = [hamlet|
                  <li>
                    $if isModule
                      <a href=@{haddockUrl sname version path'}>#{path'}
                    $else
                      #{path'}
                    ^{renderForest pathRev' subModules}
                |]
                  where
                    pathRev' = component:pathRev
                    path'    = T.intercalate "." $ reverse pathRev'

        maxDisplayedDeps :: Int
        maxDisplayedDeps = 40

        (packageDepsLink, packageRevDepsLink) =
          case mversion of
            Nothing -> (PackageDepsR pname, PackageRevDepsR pname)
            Just (snap, version) ->
              let wrap f = SnapshotR snap $ f $ PNVNameVersion pname version
               in (wrap SnapshotPackageDepsR, wrap SnapshotPackageRevDepsR)

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
        emptyPlainText _ = False
        strip (PlainText t) = PlainText (T.strip t)
        strip x = x
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

getPackageSnapshotsR :: PackageName -> Handler Html
getPackageSnapshotsR pn = track "Handler.Package.getPackageSnapshotsR" $
  do snapshots <- inRIO $ getSnapshotsForPackage $ toPathPiece pn
     defaultLayout
       (do setTitle ("Packages for " >> toHtml pn)
           $(combineStylesheets 'StaticR
                                [css_font_awesome_min_css])
           $(widgetFile "package-snapshots"))

renderNoPackages :: Int -> Text
renderNoPackages n =
  T.pack $ show n ++ " package" ++ (if n == 1 then "" else "s")
