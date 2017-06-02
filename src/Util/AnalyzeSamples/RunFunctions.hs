{-
We have a command 

(1) running the pvsanal on a sound file

We start with a source sound file, run a csd that runs the pvsanal opcode,
and puts pvs data in a destination directory. Some configuration such as
fft size and analysis rate need to be specified at this time.

(2) doing things with the pvs dump

  - making gen 10 tables, perhaps corresponding to spectrum at different
      times

  - auditioning playback with selected gen 10 tables and times

  - configuring one of my Csound/Instruments module instruments

      - some way to translate a set of useful xml files to csound data and
        audition them

  - make gnu plots

We'll have a kind of 'make' facility which tracks whether a pvs analysis
needs to be re-done, if the related configuration has changed since the
last time it was done in that directory. I guess we store in the directory 
the last-used parameters


  - source file name and pvs data directory
  - sound file fundamental (if this is used to derive fft size, analysis rate)
  - fft size
  - analysis rate 
  - offset and total duration of original reading of sound file

- we have a master list of sample names and pvsanal directories

- to run pvsanal csd on a particular filename - 

- to do analysis of a particular filename - all config is read from a 
  text file

an "filename.txt"

- to playback


-}

import Data.Binary
import System.Directory
import qualified Data.Map as M
import Text.Parsec
import Text.Printf
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (catch)
import System.IO
import Control.Exception
import Util.Exception
import Util.ParseBracketed
import Util.AnalyzeSamples.Execute( execMap )


main = do
  putStrLn "\nAdvice: you type the file names of config files at the prompt.\n"
  cmdLoop

cmdLoop :: IO ()
cmdLoop = do
  putStr $ "\n\n -> "
  hFlush stdout
  li <- getLine
  if li == "q"
    then return ()
    else 
      do processCmdLine li
         cmdLoop

processCmdLine :: String -> IO ()
processCmdLine li = do
  s <- B.readFile li  `catch` (\e -> ioHandler e >> return B.empty)
  if B.length s == 0
    then return ()
    else case parse parseManySquareBracketed  "" s of
      Left err -> putStrLn (show err)
      Right xs -> executeWholeConfig xs `catches` allHandlers


executeWholeConfig :: [SquareBracketed] -> IO ()
executeWholeConfig = executeWholeConfig2 False

-- executeWholeConfig2 
--
-- Inputs
--   Bool  :: flag to indicate if any SquareBracketed's have successfully
--            completed. if this is true, we will do all remaining
--            SquareBracketed's without checking if they've been done
--            previously
--   [SquareBracketed]
executeWholeConfig2 :: Bool -> [SquareBracketed] -> IO ()
executeWholeConfig2 _ [] = return ()
executeWholeConfig2 flag (sb@(SquareBracketed groupName _):es) =
  case M.lookup groupName execMap of
    Nothing -> do
      putStrLn $
         printf ("\n\nSaw group '%s' but there's no execute fn for it;" ++
                     " aborting.\n\n") groupName
      return ()
    Just g -> do 
      newFlag <- handleOne flag g sb 
      executeWholeConfig2 newFlag es

-- 
-- 
handleOne :: Bool -> (SquareBracketed -> IO ()) -> SquareBracketed -> IO Bool
handleOne haveDoneAny execFn sb@(SquareBracketed groupName _) = do
  isAlreadyDone <- alreadyDone sb
  -- The logic here is that we'll skip this if it's already done; that is,
  -- provided NO GROUPS have executed yet. If any groups have executed then
  -- we aren't sure of dependencies so we'll execute it.
  if not haveDoneAny && isAlreadyDone
    then do
      putStrLn $ printf "\n\nSkipping group '%s' because we did this before"
                 groupName
      return False
    else do
      putStrLn $ "\n\nStarting to execute '" ++ groupName ++ "'\n"
      execFn sb
      noteItsDone sb
      return True
      
pathToSquareBracketedMemos = "/Temp/memos/AnalyzeSamples/"

alreadyDone :: SquareBracketed -> IO Bool
alreadyDone sb@(SquareBracketed groupName _ ) = do
  let filename = pathToSquareBracketedMemos ++ groupName ++ ".bin"
  flag <- doesFileExist filename
  if flag
    then do
      binaryData <- BL.readFile filename
      let sbData = decode binaryData
      return $ sbData == sb
    else return False

noteItsDone :: SquareBracketed -> IO ()
noteItsDone sb@(SquareBracketed groupName _) = do
  let filename = pathToSquareBracketedMemos ++ groupName ++ ".bin"
      binaryData = encode sb
  BL.writeFile filename binaryData

allHandlersBool = [Handler myHandlerBool, Handler ioHandlerBool ]
  
myHandlerBool :: MyException -> IO Bool
myHandlerBool (MyException s) = do
  putStrLn $ "Error: " ++ s
  return False

ioHandlerBool :: IOException -> IO Bool
ioHandlerBool e = do
 putStrLn (show e)
 return False
  
allHandlers = [Handler myHandler, Handler ioHandler]

myHandler :: MyException -> IO ()
myHandler (MyException s) = putStrLn $ "Error: " ++ s

ioHandler :: IOException -> IO ()
ioHandler e = putStrLn (show e)
