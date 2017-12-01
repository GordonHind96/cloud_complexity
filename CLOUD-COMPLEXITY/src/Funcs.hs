module Funcs(
complexity, clone,getAll, fetch,Repo
)where

import System.IO 
import Control.Modad.State
import System.FilePath
import Control.Distributed.Process
import Control.Exception
import Prelude
import System.Process
import System.Directory

complexity :: String -> IO String
complexity file

type Repo = (String,String,String)
