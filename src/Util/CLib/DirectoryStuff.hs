module Util.CLib.DirectoryStuff (removeTextFiles) where

import System.Directory
import System.FilePath
import Text.Printf
import Control.Monad


isTxtFile = (== ".txt") . takeExtension

removeTextFiles :: String -> IO ()
removeTextFiles dir = do    
  c <- getDirectoryContents dir
  let cf = filter isTxtFile c
  putStrLn $ printf "Removing %d .txt files\n from '%s'\n" (length cf) dir
  forM_ cf (\filename ->
    removeFile $ dir ++ "/" ++ filename)
  
