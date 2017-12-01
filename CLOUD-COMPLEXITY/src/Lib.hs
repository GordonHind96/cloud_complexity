module Lib
    ( someFunc
    ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Closure
import           Control.Distributed.Process.Node                   (initRemoteTable)
import           Control.Monad
import           Network.Transport.TCP                              (createTransport,
                                                                     defaultTCPParameters)
import           System.Environment                                 (getArgs)
import           System.Exit

doWork :: Integer -> IO String
doWork = complexity

worker :: (ProcessId, ProcessId) -> Process ()
worker (manager, workQueue) = do 
	us <- gerSelfPid
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

manager :: Integer -> [NodeId] -> Process Integer
manager n workers = do
	us <- getSelfPid
	workQueue <- spawnLocal $ do
		forM_ [1 .. n] $ \m -> do
			pid <- expect
			send pid m

		forever $ do
			pid <- expect
			send pid ()

	forM_ workers $\nid -> spawn nid ($(mkClosure 'worker) (us,workQueue))
	listIO $ putStrLn $ "[Manager] Workers Spawned"

rtable :: RemoteTable 
rtable = Lib.__remoteTable initRemoteTable