{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Models(
Run(..), initDB, runDB, insertBegin, insertEnd, insertCompleteBegin, insertCompleteEnd, insertFile, insertResult, fetchFinal)where

import Control.Monad.IO.Class
import Control.Monad.Logger    (runStderrLoggingT,runNoLoggingT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Trans.Reader
import Data.Pool
import qualified Data.Aeson.Parser
import Data.Aeson.Compat 
import Data.Aeson.Types
import Data.Attoparsec.ByteString hiding (count)
import Data.ByteString (ByteString)
import Data.Time.LocalTime
import Data.Time
import Data.Maybe (fromJust)
import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
RepoDB json
    url String
    nodes Int
    start UTCTime
    end UTCTime Maybe
    deriving Show
CommitDB json
    repoId RepoDBId
    commit String
    start UTCTime 
    end UTCTime Maybe
    deriving Show
CommitResults json
    repoId RepoDBId
    commit String
    filename String
    complexity String Maybe
    deriving Show
|]
data Run = Run{
	url::String,
	dir::FilePath,
	numNodes :: Int,
	commit::String,
	id :: Key RepoDB
}

connStr = "host=localhost dbname=cloud_complex user=postgres password=root port=5432" 

runDB::Control.Monad.IO.Class.MonadIO m => Data.Pool.Pool SqlBackend -> SqlPersistT IO a -> m a
runDB pool query = liftIO $ runSqlPool query pool

initDB :: IO ConnectionPool
initDB = do
  pool <- runNoLoggingT $ createPostgresqlPool connStr 10
  runDB pool doMigrations
  return pool
	
doMigrations = runMigration migrateAll

fetchFinal id commit = count [CommitResultsRepoId==.id, CommitResultsCommit ==. commit]

insertBegin id commit = do
	time <- liftIO getCurrentTime
	insert $ CommitDB id commit time Nothing
	return ()
insertEnd id commit = do
	time <- liftIO getCurrentTime
	updateWhere [CommitDBRepoId ==. id, CommitDBCommit ==. commit] [CommitDBEnd =. Just time]
	return()
insertFile id commit file = do
	insert $ CommitResults id commit file Nothing
	return ()
insertCompleteBegin url numNodes = do
	time <- liftIO getCurrentTime
	id <- insert $ RepoDB url numNodes time Nothing
	return id
insertCompleteEnd url numNodes = do
	time <- liftIO getCurrentTime
	updateWhere [ RepoDBNodes ==. numNodes, RepoDBUrl ==. url][RepoDBEnd =.Just time]
	return ()
insertResult id commit file result = do
	updateWhere [CommitResultsRepoId ==. id, CommitResultsFilename ==. file][CommitResultsComplexity =. Just result]
	return ()

