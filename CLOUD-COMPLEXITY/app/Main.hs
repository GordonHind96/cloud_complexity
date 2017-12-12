module Main where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node                   (initRemoteTable)
import Control.Monad
import Network.Transport.TCP                              (createTransport,defaultTCPParameters)
import System.Environment                                 (getArgs)
import System.Exit
import System.FilePath
import Prelude hiding (log)
import Lib 
import Funcs
import Models
import Types
import Data.List.Split


getDir :: String -> String
getDir = last . splitOn "/"

startManager :: URL -> String -> String ->IO ()
startManager url host port = do
	pool <- initDB
	let dir = getDir url
	commits<-get url dir
	backend <- initializeBackend host port rtable 
	startMaster backend $ \workers -> do
		id <- runDB pool $ insertCompleteBegin url (length workers)
		mapM_(\commit -> do
			let runData = Run url "ghc" (length workers) commit id
			manager runData workers pool) commits
		runDB pool $ insertCompleteEnd url (length workers)
		terminateAllSlaves backend

	return ()

main :: IO ()
main = do
	args <- getArgs
	case args of
		["worker", host, port] -> do
			putStrLn "Starting worker node"
			backend <- initializeBackend host port rtable
			startSlave backend
		["manager", url, host, port] -> do
			putStrLn "Staring manager node"
			startManager url host port
		_ -> putStrLn "ooops"
