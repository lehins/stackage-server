{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageSdist
    ( getStackageSdistR
    ) where

import Import
import Stackage.Database
import Handler.Package (packagePage)

getStackageSdistR :: SnapName -> PackageNameVersion -> Handler TypedContent
getStackageSdistR _ (PNVTarball name version) = track "Handler.StackageSdist.getStackageSdistR" $ do
    redirect $ concat
        -- unfortunately using insecure HTTP for cabal's sake
        [ "http://hackage.fpcomplete.com/package/"
        , toPathPiece name
        , "-"
        , toPathPiece version
        , ".tar.gz"
        ]
getStackageSdistR sname (PNVName pname) = track "Handler.StackageSdist.getStackageSdistR" $ do
    version <- versionHelper sname pname
    redirect $ SnapshotR sname $ StackageSdistR $ PNVNameVersion pname version
getStackageSdistR sname (PNVNameVersion pname version) = track "Handler.StackageSdist.getStackageSdistR" $ do
    version' <- versionHelper sname pname
    if version == version'
        then packagePage (Just (sname, version)) pname >>= sendResponse
        else redirect $ SnapshotR sname $ StackageSdistR $ PNVNameVersion pname version'

versionHelper :: SnapName -> PackageName -> Handler Version
versionHelper sname pname = do
    Entity sid _ <- inRIO (lookupSnapshot sname) >>= maybe notFound return
    Entity _ sp <- inRIO (lookupSnapshotPackage sid (toPathPiece pname)) >>= maybe notFound return
    maybe notFound return $ fromPathPiece $ snapshotPackageVersion sp
