{-# LANGUAGE NoImplicitPrelude #-}
module Stackage.Database.Types
    ( SnapName (..)
    , isLts
    , isNightly
    , Stackage(..)
    , HasStorage(..)
    ) where

import RIO
import RIO.Time
import qualified RIO.Text as T
import Web.PathPieces
import Data.Aeson
import Data.Text.Read (decimal)
import Database.Persist
import Database.Persist.Sql hiding (LogFunc)
import Pantry.Types

data SnapName = SNLts !Int !Int
              | SNNightly !Day
    deriving (Eq, Ord, Read, Show)

isLts :: SnapName -> Bool
isLts SNLts{}     = True
isLts SNNightly{} = False

isNightly :: SnapName -> Bool
isNightly SNLts{}     = False
isNightly SNNightly{} = True

instance ToJSONKey SnapName

instance ToJSON SnapName where
    toJSON = String . toPathPiece

instance PersistField SnapName where
    toPersistValue = toPersistValue . toPathPiece
    fromPersistValue v = do
        t <- fromPersistValue v
        case fromPathPiece t of
            Nothing -> Left $ "Invalid SnapName: " <> t
            Just x -> return x
instance PersistFieldSql SnapName where
    sqlType = sqlType . fmap toPathPiece
instance PathPiece SnapName where
    toPathPiece (SNLts x y) = T.concat ["lts-", T.pack (show x), ".", T.pack (show y)]
    toPathPiece (SNNightly d) = "nightly-" <> T.pack (show d)

    fromPathPiece t0 =
        nightly <|> lts
      where
        nightly = fmap SNNightly $ T.stripPrefix "nightly-" t0 >>= (readMaybe . T.unpack)
        lts = do
            t1 <- T.stripPrefix "lts-" t0
            Right (x, t2) <- Just $ decimal t1
            t3 <- T.stripPrefix "." t2
            Right (y, "") <- Just $ decimal t3
            return $ SNLts x y


data Stackage = Stackage
    { sPantryConfig :: PantryConfig
    , sStackageRoot :: FilePath
    , sLogFunc :: LogFunc
    }

instance HasLogFunc Stackage where
  logFuncL = lens sLogFunc (\c f -> c {sLogFunc = f})

instance HasPantryConfig Stackage where
  pantryConfigL = lens sPantryConfig (\c f -> c {sPantryConfig = f})

instance HasStorage Stackage where
  storageG = to (pcStorage . sPantryConfig)
