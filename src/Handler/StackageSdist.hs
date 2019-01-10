{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageSdist
    ( getStackageSdistR
    , pnvToHackageCabalInfo
    ) where

import Import
import Stackage.Database
import Stackage.Database.Types (HackageCabalInfo(..))
import Handler.Package (packagePage)

handlePNVTarball :: PackageNameP -> VersionP -> Handler TypedContent
handlePNVTarball name version =
    redirect $
    concat -- TODO: Should this be switched to HTTPS by now?
                -- unfortunately using insecure HTTP for cabal's sake
        [ "http://hackage.fpcomplete.com/package/"
        , toPathPiece name
        , "-"
        , toPathPiece version
        , ".tar.gz"
        ]


getStackageSdistR
  :: SnapName -> PackageNameVersion -> HandlerFor App TypedContent
getStackageSdistR sname pnv =
    track "Handler.StackageSdist.getStackageSdistR" $ do
        pnvToHackageCabalInfo sname pnv handlePNVTarball $ \isSameVersion hci ->
            if isSameVersion
                then packagePage (Just (sname, hci)) (hciPackageName hci) >>= sendResponse
                else redirect $
                     SnapshotR sname $
                     StackageSdistR $ PNVNameVersion (hciPackageName hci) (hciVersion hci)


pnvToHackageCabalInfo ::
       SnapName
    -> PackageNameVersion
    -> (PackageNameP -> VersionP -> HandlerFor App b)
    -> (Bool -> HackageCabalInfo -> HandlerFor App b)
    -> HandlerFor App b
pnvToHackageCabalInfo sname pnv tarballHandler hciHandler =
    case pnv of
        PNVName pname -> hciHelper sname pname >>= hciHandler False
        PNVNameVersion pname version ->
            hciHelper sname pname >>= \hci -> hciHandler (version == hciVersion hci) hci
        PNVTarball name version -> tarballHandler name version


hciHelper :: SnapName -> PackageNameP -> Handler HackageCabalInfo
hciHelper sname pname = getVersionForSnapshot sname pname >>= maybe notFound return

