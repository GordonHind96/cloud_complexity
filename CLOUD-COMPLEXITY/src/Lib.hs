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
import GHC.Generics (Generic)
import Argon hiding (defaultConfig)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import           System.Environment                                 (getArgs)
import           System.Exit
import Funcs

doWork :: String -> Integer
doWork = complexity

worker :: ( ProcessId  -- The processid of the manager (where we send the results of our work)
         , ProcessId) -- the process id of the work queue (where we get our work from)
       -> Process ()
worker (manager, workQueue) = do
    us <- getSelfPid              -- get our process identifier
    liftIO $ putStrLn $ "Starting worker: " ++ show us
    go us
  where
    go :: ProcessId -> Process ()
    go us = do

      send workQueue us -- Ask the queue for work. Note that we send out process id so that a message can be sent to us

      -- Wait for work to arrive. We will either be sent a message with an integer value to use as input for processing,
      -- or else we will be sent (). If there is work, do it, otherwise terminate
      receiveWait
        [ match $ \n  -> do
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] given work: " ++ show n
            send manager (doWork n)
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] finished work."
            go us -- note the recursion this function is called again!
        , match $ \ () -> do
            liftIO $ putStrLn $ "Terminating node: " ++ show us
            return ()
        ]

remotable ['worker]

rtable :: RemoteTable 
rtable = Lib.__remoteTable initRemoteTable

manager :: Integer    -- The number range we wish to generate work for (there will be n work packages)
        -> [NodeId]   -- The set of cloud haskell nodes we will initalise as workers
        -> Process Integer
manager n workers = do
  us <- getSelfPid


  workQueue <- spawnLocal $ do
    -- Return the next bit of work to be done
    forM_ [1 .. n] $ \m -> do
      pid <- expect   -- await a message from a free worker asking for work
      send pid m     -- send th  
    forever $ do
      pid <- expect
      send pid ()
  forM_ workers $ \ nid -> spawn nid ($(mkClosure 'worker) (us, workQueue))
  liftIO $ putStrLn $ "[Manager] Workers spawned"
  return 1
  --sumIntegers (fromIntegral n)

someFunc :: IO ()
someFunc = do


  args <- getArgs

  case args of
    ["manager", host, port, n] -> do
      putStrLn "Starting Node as Manager"
      backend <- initializeBackend host port rtable
      startMaster backend $ \workers -> do
        result <- manager (read n) workers
        liftIO $ print result
    ["worker", host, port] -> do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> putStrLn "Bad parameters"