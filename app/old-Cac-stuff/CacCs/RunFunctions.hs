
module App.CacCs.RunFunctions where

import Data.Array.IArray
import qualified Data.Map as M
import Text.Parsec
import Data.List(partition, sortBy)
import Data.Maybe
import Cac.CacExport
import Util.RandomState
import CacToCsound.CacToCsound
import CacToCsound.CacToCsoundData
import Util.Exception
import CacToCsound.Instruments.I1
import Cac.CacUtil
import Cac.Comp
import Cac.Search
import Csound.MakeCsd
import qualified Cac.CacData as CD


options1 = "-d -W -o/temp/out.wav"

convMap = M.fromList 
  [ ("convI1", convI1)
  ] 


weights1 = array (0,13) (zip [0..13]
  [ -1000000.0
  , 1.0
  , 0.0
  , 1.0
  , 1.0
  , 4.0  -- 5
  , 1.0
  , 4.0  -- 7
  , 3.0  -- 8
  , 1.0  -- 9
  , 2.0  -- 10
  , 2.0  -- 11
  , -100000  -- 12
  , 2.0])  -- 13 


mkComp :: String ->  ErrorRand String
mkComp masterOrc = do
  c <- generateTestComp 4 4 (0, 16)
  let n = compSize c
  listPitches <- fitNPitches 5 n 60 84 weights1
  let timeOrdList = compToTimeOrderedList c
      g :: Node -> Int -> Node
      g node pit = updateInt "pitch" pit node
      h node = node { CD.instr = Just (InstrConfig "convI1") }
      pitchAssigned = zipWith g timeOrdList listPitches
      instrAssigned = map h pitchAssigned
      finalComp = compFromList instrAssigned
  cnotes <- compToCsoundNotes_ convMap finalComp
  makeCsd masterOrc options1 cnotes

main = do
  rd <- freshRandData
  masterOrc <- readFile $ "c:/Users/Mike/crit/Music/algo/haskell/" ++
    "CacToCsound/masterOrc.csd"
  case evalER (mkComp masterOrc) rd of
    Left err -> putStrLn err
    Right csdCode -> do
      writeFile "out.csd" csdCode

{-

The idea is that we have a composition and want to add a section to
it. arranging notes by duration and rhythm, then assigning pitches by
scoring them. I want to try to get close to a specific PC set. or
multiply . okay play sections alone. then attach to tree? modify . how
about maximize.

add notes in context so we know timing and tages etc. so somehow we
need to have a composition, identify the notes to have pitches added
and present context. the general algorithm should even be able to do
some backgracking and be configurable as to random deviation
allowed. run from a seed I give it. autosave
should have a default

let n = 

so we present the chooser with time and dur of note to be chosen as
well as its name, and a composition


-}










{-



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

-}
