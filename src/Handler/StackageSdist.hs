{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageSdist
    ( getStackageSdistR
    ) where

import Import
import Stackage.Database
import Stackage.Database.Types (HackageCabalInfo(..))
import Handler.Package (packagePage)

getStackageSdistR :: SnapName -> PackageNameVersion -> Handler TypedContent
getStackageSdistR _ (PNVTarball name version) =
    track "Handler.StackageSdist.getStackageSdistR" $ do
        redirect $
            concat -- TODO: Should this be switched to HTTPS by now?
                -- unfortunately using insecure HTTP for cabal's sake
                [ "http://hackage.fpcomplete.com/package/"
                , toPathPiece name
                , "-"
                , toPathPiece version
                , ".tar.gz"
                ]


getStackageSdistR sname (PNVName pname) =
    track "Handler.StackageSdist.getStackageSdistR" $ do
        hci <- hciHelper sname pname
        redirect $ SnapshotR sname $ StackageSdistR $ PNVNameVersion pname (hciVersion hci)
getStackageSdistR sname (PNVNameVersion pname version) =
    track "Handler.StackageSdist.getStackageSdistR" $ do
        hci <- hciHelper sname pname
        if version == hciVersion hci
            then packagePage (Just (sname, hci)) pname >>= sendResponse
            else redirect $ SnapshotR sname $ StackageSdistR $ PNVNameVersion pname (hciVersion hci)

hciHelper :: SnapName -> PackageNameP -> Handler HackageCabalInfo
hciHelper sname pname = getVersionForSnapshot sname pname >>= maybe notFound return

