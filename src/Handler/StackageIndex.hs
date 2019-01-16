{-# LANGUAGE NoImplicitPrelude #-}
module Handler.StackageIndex where

import Import
import Stackage.Database

getStackageIndexR :: SnapName -> Handler TypedContent
getStackageIndexR slug = do
    redirect $ concat
        [ "https://haddock.stackage.org/package-index/"
        , toPathPiece slug
        , ".tar.gz"
        ]
