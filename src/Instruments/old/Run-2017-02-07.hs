

-- concept: "ghci music scripts": haskell modules that serve as "config
-- files," that is setting up meta-instrs and timing variation and so form
--
-- this module, Run.hs is the entry point for ghci music scripts



module Instruments.Run where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Exception as E
import qualified Sound.PortMidi as SP
import Text.Parsec
import Text.Parsec.String
import System.Console.Haskeline
import System.FilePath
import System.IO
import System.Random
import Control.Concurrent
import Control.Monad.Except
import Control.Monad.State
import Control.Arrow hiding(loop)
import Control.DeepSeq
import Text.Printf
import Text.XML.Light
import Data.Function
import Data.Either
import Data.Map(Map)
import Data.Maybe
import Util.Showable
import Util.FileUtil
import Util.Exception
import Sound.PortMidi hiding(name,initialize)
import Score.ScoreData
import Score.ShowScore
import Score.XmlToScore
import XmlDoc.ParseXml
import Translation.TranslationData
import Instruments.InstrumentsData
import Instruments.ShowInstruments
import Instruments.ToMidi
import Midi.Interface
import Midi.MidiData
import Common.CommonData hiding(msrNum)
import Common.CommonUtil

data LoopContext = LoopContext
  { lcStreams :: [PMStream]
  , lcTid     :: Maybe ThreadId
  , lcRd      :: RunData
  , lcMsrs    :: (Int,Maybe Int)
  }


----------------------------------------------------------------------
--         the following section is an attempt to use ghci for interactive
--         configuration and run


-- this is a command to use with ghci's :def command. It computes the
-- correct config to use, loads it, and executes main.
defCmd s = do
  sib <- treeFiles ((== ".sib") . takeExtension)
    "/Users/Mike/Dropbox/music/comp"
      >>= mostRecentFileL
  let config = sib -<.> ".hs"
  
  return $ printf ":lo %s" config



runGhciWay :: RunData -> String -> IO ()
runGhciWay inp = do
  case parse cmdLine "" inp of
    Left error -> putStrLn $ show err
    Right cmd -> case cmd of
      MsrNum (beg,mEnd) -> do
        mStreams <- ghcStartMidi
        case mStreams of
          Nothing -> return ()
          Just streams -> loopIO (Context streams Nothing
                                  

ghcStartMidi :: IO (Maybe [PMStream])
ghcStartMidi = do
  i <- openMidiPipeInput2
  result <- initMidi i i
  case result of
    Left err -> putStrLn (show err) >> return Nothing
    Right streams -> return $ Just streams
        
----------------------------------------------------------------------


run :: RunData -> IO ()
run runData = do
  putStrLn "\nRun.hs: mostly likely this is a program compiled from a "
  putStrLn "configuration script.\n"

  {-
  c <- countDevices
  mapM_ showMidiDevice [0..c-1]
  putStr "Enter midi port number or hit enter for Csound:"
  hFlush stdout
  li <- getLine
  case reads li of
    []      -> do 
      putStrLn "Something wrong in port number."
      return ()
    (i,_):_ -> do
  -}
  i <- openMidiPipeInput2
  do
      result <- initMidi i i
      case result of
        Left err -> putStrLn $ "MIDI error: " ++ show err
        Right streams -> do
         runInputT defaultSettings $
           loop (LoopContext streams Nothing runData (1,Nothing))
         stopMidi streams
         return ()


data CmdLine = MsrNum (Int,Maybe Int)
             | Quit
             | Stop
             | Blank
             | Redo


cmdLine :: Parser CmdLine
cmdLine = msrNum <|> quit <|> stop <|> blank <|> redo


msrNum = do
  ds1 <- many1 digit
  ds2 <- option Nothing (char '-' >> many1 digit >>= return . Just)
  return $ MsrNum (read ds1,read <$> ds2)

quit = char 'q' >> eof >> return Quit

stop = char 's' >> eof >> return Stop

blank = eof >> return Blank

redo = char 'r' >> eof >> return Redo
  

loop :: LoopContext -> InputT IO ()
loop context@(LoopContext streams tid _ _) = do
  s <- getInputLine "midi > "
  case s of
    Nothing -> loop context
    Just l -> case parse cmdLine "" l of
      Left err -> liftIO (putStrLn $ show err) >> loop context
      Right cmd -> case cmd of
        MsrNum (beg,mEnd) -> do
          contextOut <- liftIO $
            (loopIO context {lcMsrs = (beg,mEnd)} `catches` myHandler)
          loop (maybe context id contextOut)
        Quit -> liftIO (killAll context) >> return ()
        Stop -> liftIO (killAll context) >> loop context
        Blank -> loop context
        Redo  -> do
          contextOut <- liftIO $ loopIO context
          loop (maybe context id contextOut)
          

-- Need to 
loopIO :: LoopContext -> IO (Maybe LoopContext)
loopIO con@(LoopContext streams _ r@(RunData metas tVar) msrs) = do
  score <- readXml
  putStrLn "Writing score.txt."
  writeFile "score.txt" . showIString $ score
  gen <- newStdGen
  let metaMap = M.fromList $ map (metaName &&& id) metas
      s = TrState score metaMap tVar gen M.empty (VMap M.empty)
          (VMap M.empty) (VMap M.empty) [] [] []
      (err_or_shorts,finalState) = runState (runExceptT $ toMidi msrs) s
  -- putStrLn $ showIString $ tsMetaState finalState
  let shorts = case err_or_shorts of
        Left msg -> throwMine msg
        Right r  -> r
  feb_debug_dump finalState
  
  raws <- E.evaluate $ force shorts
  -- kill process, stop all
  liftIO (killAll con)
  tid <- launchPlayRawEvents streams raw
  return $ Just (LoopContext streams (Just tid) r msrs)


launchPlayRawEvents :: [PMStream] -> [Short] -> IO ThreadId
launchPlayRawEvents streams shorts = do
  beginTime <- fromIntegral `liftM` time
  tid <- forkIO $ playRawEvents streams (beginTime+200) shorts
  return tid
  


killAll :: LoopContext -> IO ()
killAll (LoopContext streams mId _ _) = do
  case mId of
    Nothing -> return ()
    Just i  -> killThread i
  allOff streams


readXml :: IO Score
readXml = do
  buf <- readFileStrictly "/Users/Mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e


openMidiPipeInput2 :: IO DeviceID
openMidiPipeInput2 = do
  c <- countDevices
  let test n = do info <- getDeviceInfo n
                  return $ SP.name info == "MidiPipe Input 2"
  tests <- mapM test [0..c-1]
  case L.find fst (zip tests [0..c-1]) of
    Nothing -> throwMine "is MidiPipe running?"
    Just (_,x) -> putStrLn "using MidiPipe Input 2" >> return x
    

decideInputOrOutput di = if input di then "Input :" else "Output:"


openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Left stream -> return $ Right stream
    Right err -> return $ Left err


showMidiDevice :: Int -> IO ()
showMidiDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (SP.name di) 




----------------------------------------------------------------------
--       initialize, open, close, and terminate PortMidi streams

initMidi :: Int -> Int -> IO (Either PMError [PMStream])
initMidi midiDevNumLow midiDevNumHigh = do
  SP.initialize
  
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> return $ Right streams
    (err:_) -> return $ Left err


stopMidi :: [PMStream] -> IO ()    
stopMidi streams = do
  mapM_ close streams
  terminate
  return ()    

----------------------------------------------------------------------
--                     exception handlers

myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing

myHandler = [Handler myHandlerM]


-------------------------------------------------------------------------
--

feb_debug_dump :: TrState -> IO ()
feb_debug_dump ts = do
  putStrLn "Dumping to 'feb.txt'"
  let VCurve c = rLookup "1" . rLookup "loudness" .
                 rLookup "Keyboard-staff1" $ tsStaffState ts
  let Component _ _ metaStateElems = showI $ tsMetaState ts
      Component _ _ staffStateElems = showI $ tsStaffState ts
      s1 = showiToString $ Component "meta state" True metaStateElems
      s2 = showiToString $ Component "staff state" True staffStateElems
      s3 = showiToString $ Component "notes" True
                           . map showI . L.sortBy (compare `on` noteOnTime)
                           . concat $ tsNotes ts
      sTimeMap = showIString $ r2Lookup "Keyboard-staff1" $ tsTimeMaps ts
  writeFile "feb.txt" $ s3
  writeFile "dyn.txt" $ showIString c
  putStrLn "writing atm.txt..." >> writeFile "atm.txt" sTimeMap 


noteOnTime = onTime . tnOnOff


r2Lookup k m = case M.lookup k m of {Just x -> x}

rLookup :: String -> Value -> Value
rLookup s (VMap m) = case M.lookup s m of {Just x -> x}

instance ShowItemClass AbsTimeMap where
  showI (AbsTimeMap m) = Component "atm" True (map f $ M.toAscList m)
    where
      f (loc,d) = SingleLine $ printf "%s %9.4f" (showLoc2 loc) d
