{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Stackage.Database.Schema
    ( -- * Database
      run
    , StackageDatabase
    , GetStackageDatabase(..)
    , openStackageDatabase
    , closeStackageDatabase
    , runStackageMigrations
    -- * Tables
    , Unique(..)
    , EntityField(..)
    -- ** Snapshot
    , Snapshot(..)
    , SnapshotId
    , Lts(..)
    , Nightly(..)
    -- ** Package
    , SnapshotPackage(..)
    , SnapshotPackageId
    , Module(..)
    , ModuleId
    , SnapshotPackageModule(..)
    , SnapshotPackageModuleId
    , Dep(..)
    , DepId
    , Deprecated(..)
    , DeprecatedId
    -- ** Pantry
    , module PS
    ) where

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Data.Aeson as A
import Data.Pool (destroyAllResources)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Pantry.Storage as PS (BlobId, HackageCabalId, PackageNameId, Tree(..),
                             TreeEntry(..), TreeEntryId, TreeId, Unique(..),
                             VersionId, unBlobKey)
import qualified Pantry.Storage as Pantry (migrateAll)
import Pantry.Types (HasPantryConfig(..), PantryConfig(..), Revision,
                     Storage(..))
import RIO
import RIO.Time
import Types (CompilerP(..), FlagNameP, ModuleNameP, Origin, SnapName,
              VersionRangeP)

currentSchema :: Int
currentSchema = 1

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Schema
    val Int
    deriving Show

Snapshot
    name SnapName
    compiler CompilerP
    created Day
    updatedOn UTCTime Maybe
    UniqueSnapshot name
Lts
    snap SnapshotId
    major Int
    minor Int
    UniqueLts major minor
Nightly
    snap SnapshotId
    day Day
    UniqueNightly day
SnapshotPackage
    snapshot SnapshotId
    packageName PackageNameId
    version VersionId
    revision Revision Maybe
    cabal BlobId Maybe
    treeBlob BlobId Maybe
    origin Origin
    originUrl Text
    synopsis Text
    readme TreeEntryId Maybe
    changelog TreeEntryId Maybe
    isHidden Bool -- used for pantry, but is not relevant for stackage
    flags (Map FlagNameP Bool)
    UniqueSnapshotPackage snapshot packageName
Module
    name ModuleNameP
    UniqueModule name
SnapshotPackageModule
    snapshotPackage SnapshotPackageId
    module ModuleId
    hasDocs Bool
    UniqueSnapshotPackageModule snapshotPackage module
Dep
    user SnapshotPackageId
    uses PackageNameId
--  TODO: potentially resolve dependencies upon import
--  usesInSnapshot SnapshotPackageId Maybe
    range VersionRangeP
    UniqueDep user uses
Deprecated
    package PackageNameId
    inFavourOf [PackageNameId]
    UniqueDeprecated package
|]

_hideUnusedWarnings :: (SchemaId, LtsId, NightlyId) -> ()
_hideUnusedWarnings _ = ()


instance A.ToJSON Snapshot where
  toJSON Snapshot{..} =
    A.object [ "name"     A..= snapshotName
             , "ghc"      A..= ghc -- TODO: deprecate? since it's encapsulated in `compiler`
             , "compiler" A..= snapshotCompiler
             , "created"  A..= formatTime defaultTimeLocale "%F" snapshotCreated
             ]
    where CompilerGHC ghc = snapshotCompiler


-- | Re-use Pantry's database connection pool
type StackageDatabase = Storage

class (MonadThrow m, MonadUnliftIO m) => GetStackageDatabase env m | m -> env where
    getStackageDatabase :: m StackageDatabase


instance HasPantryConfig env => GetStackageDatabase env (RIO env) where
    getStackageDatabase = pcStorage <$> view pantryConfigL

run :: GetStackageDatabase env m => SqlPersistT m a -> m a
run inner = do
    Storage pool <- getStackageDatabase
    runSqlPool inner pool


openStackageDatabase :: MonadIO m => Bool -> PostgresConf -> m Storage
openStackageDatabase shouldLog pg =
    liftIO $ do
        Storage <$>
            if shouldLog
                then runStdoutLoggingT $ createPostgresqlPool (pgConnStr pg) (pgPoolSize pg)
                else runNoLoggingT $ createPostgresqlPool (pgConnStr pg) (pgPoolSize pg)


closeStackageDatabase :: GetStackageDatabase env m => m ()
closeStackageDatabase = do
    Storage pool <- getStackageDatabase
    liftIO $ destroyAllResources pool


getSchema :: (HasLogFunc env, GetStackageDatabase env (RIO env)) => RIO env (Maybe Int)
getSchema =
    run $ do
        eres <- tryAny (selectList [] [])
        lift $ logInfo $ "getSchema result: " <> displayShow eres
        case eres of
            Right [Entity _ (Schema v)] -> return $ Just v
            _                           -> return Nothing

runStackageMigrations :: (HasLogFunc env, GetStackageDatabase env (RIO env)) => RIO env ()
runStackageMigrations = do
    actualSchema <- getSchema
    run $ do
        runMigration Pantry.migrateAll
        runMigration migrateAll
        unless (actualSchema == Just currentSchema) $ do
            lift $
                logWarn $
                "Current schema does not match actual schema: " <>
                displayShow (actualSchema, currentSchema)
            deleteWhere ([] :: [Filter Schema])
            insert_ $ Schema currentSchema
