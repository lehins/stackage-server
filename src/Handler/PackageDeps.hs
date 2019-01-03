{-# LANGUAGE NoImplicitPrelude #-}
module Handler.PackageDeps
  ( getPackageDepsR
  , getPackageRevDepsR
  , getSnapshotPackageDepsR
  , getSnapshotPackageRevDepsR
  ) where

import Import
import Stackage.Database

getPackageDepsR :: PackageNameP -> Handler Html
getPackageDepsR = packageDeps Nothing

getSnapshotPackageDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageDepsR snap (PNVNameVersion pname version) =
  packageDeps (Just (snap, version)) pname
getSnapshotPackageDepsR _ _ = notFound

packageDeps :: Maybe (SnapName, VersionP) -> PackageNameP -> Handler Html
packageDeps = helper Deps

getPackageRevDepsR :: PackageNameP -> Handler Html
getPackageRevDepsR = packageRevDeps Nothing

getSnapshotPackageRevDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageRevDepsR snap (PNVNameVersion pname version) =
  packageRevDeps (Just (snap, version)) pname
getSnapshotPackageRevDepsR _ _ = notFound

packageRevDeps :: Maybe (SnapName, VersionP) -> PackageNameP -> Handler Html
packageRevDeps = helper Revdeps

data DepType = Deps | Revdeps

helper :: DepType -> Maybe (SnapName, VersionP) -> PackageNameP -> Handler Html
helper depType mversion pname = track "Handler.PackageDeps.helper" $ do
  deps <-
    (case depType of
       Deps -> getDeps
       Revdeps -> getRevDeps) pname Nothing
  let packagePageLink =
        case mversion of
          Nothing -> PackageR pname
          Just (snap, version) -> SnapshotR snap $ StackageSdistR $ PNVNameVersion pname version
  defaultLayout $ do
    let title = toHtml $
          (case depType of
            Deps -> "Dependencies"
            Revdeps -> "Reverse dependencies") ++ " for " ++ toPathPiece pname
    setTitle title
    [whamlet|
      <h1>#{title}
      <p>
        <a href=#{packagePageLink}>Return to package page
      <ul>
        $forall (name, range) <- deps
          <li>
            <a href=@{PackageR name} title=#{range}>#{name}
    |]
