
-- an app that finds the script file associated with the latest sib file,
-- compiles it, and moves the executable to App/Mp/from-script

import Text.Printf
import Util.FileUtil
import System.Process
import System.FilePath

main = do
  sib <-
    treeFiles ((==".sib") . takeExtension) "/Users/Mike/Dropbox/music/" >>=
    mostRecentFileL 
  let basename = takeDirectory sib ++ "/" ++ takeBaseName sib
      cName    = basename ++ ".hs"
      execName = basename
  putStrLn $ "Using: " ++ cName
  let cmd = printf "ghc %s --make -i/Users/Mike/haskell" cName
  (_,_,_,h) <- createProcess (shell cmd)
  waitForProcess h
  let mv = printf "cp %s /Users/Mike/haskell/App/Mp/from-script" execName
  (_,_,_,h2) <- createProcess (shell mv)
  waitForProcess h
  putStrLn "copied to App/Mp/from-script, run with alias 'm'"

      
