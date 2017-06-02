
module App.Cac.Execute where

import Text.Printf
import System.IO
import System.FilePath
import System.Process
import System.Directory
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as BL 
import Data.Binary
import Data.Char
import qualified Data.Map as M
import Text.Parsec
import Control.Exception
import App.Cac.ParseCmdLine
import App.Cac.AppCacData
import Cac.CacData
import Cac.CacCore
import Util.Showable
import Util.FileUtil 
import Util.UtilData
import Util.RandomState
import Util.Exception
import Cac.CacConv
import CacToCsound.Instruments.Export 
import CacToCsound.CacToCsoundConv
import qualified CacToCsound.CacToCsoundConv as CCC
import Csound.CsoundConv


data LoopContext = LoopContext 
  { lcCompName :: Maybe String
  , lcPh :: Maybe ProcessHandle }

loop :: LoopContext -> IO ()
loop loopContext@(LoopContext compFileName _) = do
  parsedCmd <- lineFetch   -- `catches` allHandlersM
  case (parsedCmd,compFileName) of
     (Nothing,_) -> do
        putStrLn "some kind of problem"
        loop loopContext

     (Just c, Nothing) ->
        case c of
           cc@(CmdSelect s ) -> runCmdSelect c loopContext
           CmdQuit -> return ()
           otherwise -> do
              putStrLn "Have to select a file before any other command."
              loop loopContext 
     (Just c, Just fn) -> case c of
        cs@(CmdSelect _) -> runCmdSelect cs loopContext

        CmdQuit -> return ()

        CmdReport -> do
          showReport fn
          loop loopContext

        CmdPlay mTestPhraseName _ -> do
          newMaybeHandle <- playIt fn (Just mTestPhraseName) 
              `catches` allHandlersM
          loop loopContext { lcPh = newMaybeHandle }



isCmdSelect :: Cmd -> Bool
isCmdSelect (CmdSelect _) = True
isCmdSelect _ = False
 

----------------------------------------------------------------------
----------------------------------------------------------------------
--                    conversion data

convData = M.fromList [("instr10", instr10)]

----------------------------------------------------------------------
----------------------------------------------------------------------
runCmdSelect (CmdSelect fname) lc = do
  flag <- checkFileExistence fname
  if flag
    then do
      putStrLn "Found file."
      loop lc { lcCompName = Just fname }
    else do
      putStrLn $ "Cannot find file! " ++ fname
      loop lc

----------------------------------------------------------------------
----------------------------------------------------------------------
checkFileExistence :: FilePath -> IO Bool
checkFileExistence s = do
  d <- getCurrentDirectory
  doesFileExist $ d </> s
  

----------------------------------------------------------------------
----------------------------------------------------------------------
--                     playIt

playIt :: String -> Maybe String -> IO (Maybe ProcessHandle)
playIt compFilename mTName = do
  -- Read composition
  comp <- loadComp compFilename
  masterOrc <- readFile CCC.masterOrcFileName
  let computation = compToNodes mTName comp >>= nodesToCNotes convData >>= 
        csoundNotesToCsd masterOrc True
  case runCM computation (randData 1) of
    ((Left err,_),_) -> do
       putStrLn err
       return Nothing
    ((Right csd,log),_) -> do
       putStrLn $ "Finished making CSD. Log:\n" ++ log
       B.writeFile "out.csd" . B.pack . map (fromIntegral . ord) $ csd
       putStrLn "Wrote out.csd"
       cwd <- getCurrentDirectory
       ph <- runProcess "c:/Program Files/Csound/bin/csound.exe" ["out.csd"]
         (Just cwd) Nothing Nothing Nothing Nothing
       return $ Just ph
  

----------------------------------------------------------------------
----------------------------------------------------------------------
-- 
showReport :: String -> IO ()
showReport s = do
  (Comp phrases tests) <- loadComp s
  putStrLn . unlines . M.keys $ tests
  

----------------------------------------------------------------------
----------------------------------------------------------------------
--             read and parse the command line


-- As written the code here can only return a Just value. But
-- this will be used with exception handlers that can return a Nothing
lineFetch :: IO (Maybe Cmd)
lineFetch = do
  d <- getCurrentDirectory
  putStrLn d
  putStr "(cac)---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then lineFetch
    else do  
      r <- evaluate $ lineParse li
      case r of
        Left err -> do
          putStrLn $ "Parse err:\n" ++ err
          lineFetch
        Right cmd -> return (Just cmd)

lineParse ::  String -> Either String Cmd
lineParse s =
  case parse parseCmdLine "" s of 
    Left err -> Left (show err)
    Right cmd -> Right cmd





----------------------------------------------------------------------
----------------------------------------------------------------------

allHandlersM = [Handler myHandlerM, Handler ioHandlerM]

myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing

ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing

----------------------------------------------------------------------
----------------------------------------------------------------------

allHandlers = [Handler myHandler, Handler ioHandler]

myHandler :: MyException -> IO ()
myHandler (MyException s) = putStrLn $ "Error: " ++ s

ioHandler :: IOException -> IO ()
ioHandler e = putStrLn (show e)

