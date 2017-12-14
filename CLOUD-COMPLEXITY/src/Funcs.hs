module Funcs(
complexity, clone,get, fetch,Repo,start_process
)where

import System.IO 
import Control.Monad
import System.FilePath
import Control.Exception
import Prelude
import System.Process
import System.Directory

import Types

complexity :: String -> IO String
complexity f = start_process("stack","exec -- argon --json "++f)



clone :: URL -> Directory -> IO ()
clone url directory = do
	real <- doesDirectoryExist directory
	if real
		then callProcess "git" ["clone",url]
		else return ()

get :: URL -> Directory -> IO [String]
get url dir = do
	clone url dir
	commits <- start_process("git","--git-dir " ++ dir ++"--pretty=format: '%H' ")
	return $ words commits

fetch :: Repo -> IO ()
fetch (url, dir,commit) = do 
	clone url dir
	readCreateProcess ((proc "git" ["reset","--hard",commit]){cwd = Just dir})""
	return ()

start_process :: (String, String) -> IO String
start_process (cmd,arg) = do
  (_,Just hout,_,ph) <- createProcess (proc cmd $ words arg){ std_out = CreatePipe }
  hGetContents hout

