module Funcs(
complexity, clone,get, fetch,Repo,start_process
)where

import System.IO 
import Control.Modad.State
import System.FilePath
import Control.Distributed.Process
import Control.Exception
import Prelude
import System.Process
import System.Directory

import Types

complexity :: String -> IO String
complexity f = start_process("stack","exec -- argon --json "++f)



clone :: URL -> Directory -> IO ()
clone url directory = do
	real <- doesDirectoryExist dir
	if real
		then callProcess "git" ["clone",url]
		else return ()

get :: URL -> Directory -> IO [String]
get url dir = do
	clone url dir
	commits <- sendCommand("git","log --pretty=format: '%H' "++ dir)
	return words commits

fetch :: Repo -> IO ()
fetch (url, dir,commit) = do 
	clone url dir
	readCreateProcess ((proc "git" ["reset","--hard",commit]){cwd = Just dir})""
	return ()

start_process :: String -> String -> IO String
start_process cmd args = do 
	(_,Just hout,ph) <- createProcess (proc cmd $ words args) { std_out = CreatePipe }
	hGetContents hout