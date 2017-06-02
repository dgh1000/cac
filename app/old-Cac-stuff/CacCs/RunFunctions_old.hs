
module App.CacCs.RunFunctions where

import Text.Parsec
import Data.List(partition, sortBy)
import Control.Exception
import System.Directory
import System.Process
import System
import System.IO
import System.Time
import System.CPUTime
import Data.Maybe
import PlaybackCac.PlaybackCacExport
import PlaybackCac.ParseCmdLine
import Cac.CacExport
import Util.RandomState
import CacToCsound.CacToCsound
import CacToCsound.CacToCsoundData
import Util.Exception

options1 = "-d -W -o/temp/out.wav"


data LoopContext = LoopContext
  { lcNode :: Node
  , lcConvMap :: CacToCsMap
  , lcHandle :: Maybe ProcessHandle }

runCacLoop :: Node -> CacToCsMap -> IO ()
runCacLoop node map = do
  loop (LoopContext node map Nothing)


loop :: LoopContext -> IO ()
loop lc = do
  parsedCmd <- lineFetch `catches` allHandlersM

  case parsedCmd of
    Nothing -> loop lc
    Just com -> case com of
      InpTerminateProcess ->
        case lcHandle lc of
          Nothing -> loop lc
          Just handle -> do
            terminateProcess handle `catches` allHandlers
            loop lc
      InpQuit -> return ()
      InpPlay _ -> do
        masterOrc <- readFile $ "c:/Users/Mike/crit/Music/algo/haskell/" ++
                     "CacToCsound/masterOrc.csd"
        case nodeToCsd masterOrc options1 (lcConvMap lc) (lcNode lc) of
          Left err -> do { putStrLn err; loop lc }
          Right csdText -> do
            playCsd csdText
            loop lc -- { lcHandle = Just handle }
      InpParseError s -> do
        putStrLn s
        loop lc

playCsd :: String -> IO ProcessHandle
playCsd csd = do 
  fname <- computeCsdName Nothing
  -- let fname = "out.csd"
  writeFile fname csd
  putStrLn $ "Wrote " ++ fname
  cwd <- getCurrentDirectory
  runProcess "c:/Program Files/Csound/bin/csound.exe" [fname] (Just cwd)
    Nothing Nothing Nothing Nothing



-- 
computeCsdName :: Maybe String -> IO String
computeCsdName (Just s) = return s
computeCsdName Nothing = do
  let possibleNames = map (\d -> "out" ++ show d ++ ".csd") [1..9]
  cwd <- getCurrentDirectory
  fs <- getDirectoryContents cwd
  -- pns:: [(FilePath, Maybe ClockTime)]
  pns <- mapM (checkPossibleName fs) possibleNames
  let (pns1,pns2) = partition (\(_,mc) -> isNothing mc) pns
  case pns1 of
    x:_       -> return $ fst x
    otherwise ->
      let pns2s = sortBy (\(_,jt1) (_,jt2) -> compare (fromJust jt1)
                                              (fromJust jt2)) pns2
      in return $ fst $ head pns2s

checkPossibleName :: [FilePath] -> FilePath -> IO (FilePath, Maybe ClockTime)
checkPossibleName contents possibleName =
  if any (== possibleName) contents
  then do
    t <- getModificationTime possibleName
    return $ (possibleName, Just t)
  else return $ (possibleName, Nothing)


----------------------------------------------------------------------
--        
----------------------------------------------------------------------
     
lineFetch :: IO (Maybe InpCmd)
lineFetch = do
  d <- getCurrentDirectory
  putStrLn d
  putStr "(cound) ---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then  lineFetch
    else do
      r <- evaluate $ lineParse li
      return r

lineParse ::  String -> Maybe InpCmd
lineParse s =
  case parse parseCmdLine "" s of 
    Left err -> Just $ InpParseError (show err)
    Right cmd -> Just $ cmd

--------------------------------------------------------------------
--          Exception handlers
--------------------------------------------------------------------

allHandlersM = [Handler myHandlerM, Handler ioHandlerM]

myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing

ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing

{-
someExceptionM :: SomeException -> IO (Maybe a)
someExceptionM e = do
  putStrLn (show e)
  return Nothing
-}

allHandlers = [Handler myHandler, Handler ioHandler]

myHandler :: MyException -> IO ()
myHandler (MyException s) = putStrLn $ "Error: " ++ s

ioHandler :: IOException -> IO ()
ioHandler e = putStrLn (show e)

