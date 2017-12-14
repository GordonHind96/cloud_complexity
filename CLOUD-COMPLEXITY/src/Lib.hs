{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                     (initRemoteTable)
import Control.Monad
import Network.Transport.TCP                                (createTransport,defaultTCPParameters)
import Data.Binary
import Data.Either 
import Pipes
import Pipes.Safe (runSafeT)
import Pipes.Prelude as P hiding (show, length)
import GHC.Generics (Generic)
import Argon hiding (defaultConfig)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import           System.Environment                                 (getArgs)
import           System.Exit
import Funcs
import Models

doWork :: String -> IO String
doWork = complexity

worker :: (ProcessId, ProcessId) -> Process ()
worker (manager, workQueue) = do 
	us <- getSelfPid
	liftIO $ putStrLn $ "Starting worker: " ++ show us
	go us
   where
    go :: ProcessId -> Process ()
    go us = do

      send workQueue us -- Ask the queue for work. Note that we send out process id so that a message can be sent to us

      -- Wait for work to arrive. We will either be sent a message with an integer value to use as input for processing,
      -- or else we will be sent (). If there is work, do it, otherwise terminate
      receiveWait
        [ match $ \f  -> do
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] given work: " ++ show f
            result <- liftIO $ doWork f
            send manager (f, result)
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] finished work."
            go us -- note the recursion this function is called again!
        , match $ \ () -> do
            liftIO $ putStrLn $ "Terminating node: " ++ show us
            return ()
        ]

remotable ['worker]
rtable :: RemoteTable 
rtable = Lib.__remoteTable initRemoteTable

--manager :: Integer -> [NodeId] -> Process Integer
manager runData@Run{..} workers pool = do
  us <- getSelfPid
  count <- liftIO $ atomically $ newTVar 0 
  liftIO $ fetch (url,dir,commit)
  liftIO $ runDB pool $ insertBegin id commit
  
  let source = allFiles dir
  workQueue <- spawnLocal $ do 
    runSafeT $ runEffect $ for source (\ f -> lift $ lift $ sendwork id f commit pool)
    final <- liftIO $ runDB pool $ fetchFinal id commit
    liftIO $ atomically $ writeTVar count final
    forever $ do
      pid <- expect
      send pid ()
  forM_ workers $ \ nid -> spawn nid $ $(mkClosure 'worker) (us,workQueue)
  fetchResults id count 0 commit pool
  
  liftIO $ runDB pool $ insertEnd id commit
  return ()

sendwork id f commit pool = do
  liftIO $ runDB pool $ insertFile id commit f
  pid <- expect
  send pid f

fetchResults id count curCount commit pool = do
  count' <- liftIO $ atomically $ readTVar count
  unless (curCount == count') $ do
    (f,result) <- expect :: Process(String, String)
    liftIO $ runDB pool $ insertResult id commit f result
    fetchResults id count (curCount + 1) commit pool

