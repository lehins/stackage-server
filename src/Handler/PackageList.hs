{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Handler.PackageList where

import Import
import Stackage.Database


-- FIXME maybe just redirect to the LTS or nightly package list
getPackageListR :: Handler Html
getPackageListR = track "Handler.PackageList.getPackageListR" $
    defaultLayout $ do
        setTitle "Package list"
        packages <- getAllPackages
        $(widgetFile "package-list")
  where strip x = fromMaybe x (stripSuffix "." x)
