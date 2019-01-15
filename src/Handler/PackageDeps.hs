{-# LANGUAGE NoImplicitPrelude #-}
module Handler.PackageDeps
  ( getPackageDepsR
  , getPackageRevDepsR
  , getSnapshotPackageDepsR
  , getSnapshotPackageRevDepsR
  ) where

import Handler.Package (deduceHackageCabalInfo)
import Handler.StackageSdist (pnvToHackageCabalInfo)
import Import
import Types (PackageVersionRev(..))
import Stackage.Database
import Stackage.Database.Types (HackageCabalInfo(..))

getPackageDepsR :: PackageNameP -> Handler Html
getPackageDepsR pname = do
    (mSnapName, hci, _hackageLatest) <- deduceHackageCabalInfo Nothing pname
    case mSnapName of
        Nothing -> redirect $ PackageR pname
        Just snapName -> helper Deps snapName hci
            -- helper
            --     (getForwardDeps snapName (hciCabalId hci) Nothing)
            --     Deps
            --     (getPackagePageLink (Just (snapName, hciVersion hci)) pname)
            --     pname
                     -- (getForwardDeps sname (hciCabalId hci) Nothing)
                     -- Deps
                     -- (getPackagePageLink (Just (sname, hciVersion hci)) (hciPackageName hci))
                     -- (hciPackageName hci)

getSnapshotPackageDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageDepsR snapName pnv =
    pnvToHackageCabalInfo snapName pnv (\_ _ -> notFound) $ \isSameVersion hci ->
        if isSameVersion
            then helper Deps snapName hci
            else redirect $
                 SnapshotR snapName $
                 SnapshotPackageDepsR $ PNVNameVersion (hciPackageName hci) (hciVersion hci)

getPackageRevDepsR :: PackageNameP -> Handler Html
getPackageRevDepsR pname = do
    (mSnapName, hci, _hackageLatest) <- deduceHackageCabalInfo Nothing pname
    case mSnapName of
        Nothing -> redirect $ PackageR pname
        Just snapName -> helper RevDeps snapName hci
            -- helper
            --     (getReverseDeps snapName (hciCabalId hci) Nothing)
            --     RevDeps
            --     (getPackagePageLink (Just (snapName, hciVersion hci)) pname)
            --     pname

getSnapshotPackageRevDepsR :: SnapName -> PackageNameVersion -> Handler Html
getSnapshotPackageRevDepsR snapName pnv =
    pnvToHackageCabalInfo snapName pnv (\_ _ -> notFound) $ \isSameVersion hci ->
        if isSameVersion
            then helper RevDeps snapName hci
            else redirect $
                 SnapshotR snapName $
                 SnapshotPackageRevDepsR $ PNVNameVersion (hciPackageName hci) (hciVersion hci)


getPackagePageLink :: SnapName -> PackageVersionRev -> Route App
getPackagePageLink snapName (PackageVersionRev pname (VersionRev version _)) =
  SnapshotR snapName $ StackageSdistR $ PNVNameVersion pname version

data DepType = Deps | RevDeps

helper :: DepType -> SnapName -> HackageCabalInfo -> Handler Html
helper depType snapName hci =
    track "Handler.PackageDeps.helper" $ do
        let (depsGetter, header) =
                case depType of
                    Deps -> (getForwardHackageDeps, "Dependencies for ")
                    RevDeps -> (getReverseHackageDeps, "Reverse dependencies on ")
        deps <- depsGetter snapName (hciCabalId hci) Nothing
        render <- getUrlRender
        let title = toHtml $ header ++ toPathPiece (hciPackageName hci)
            packagePageUrl =
                render $
                SnapshotR snapName $
                StackageSdistR $ PNVNameVersion (hciPackageName hci) (hciVersion hci)
        defaultLayout $ do
            setTitle title
            [whamlet|
              <h1>#{title}
              <h3>There is a total of #{length deps} dependencies in <em>#{snapName}</em>
              <p>
                <a href=#{packagePageUrl}>&lt;&lt; Return to package page
              <ul>
                $forall (depNameVerRev, verRange) <- deps
                  <li>
                    <a href=@{getPackagePageLink snapName depNameVerRev} title="'#{hciPackageName hci}' version bounds: #{verRange}">#{depNameVerRev}
            |]
