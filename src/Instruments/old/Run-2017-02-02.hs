
module Instruments.Run where

import qualified Data.Map as M
import qualified Sound.PortMidi as SP
import qualified Data.List as L
import qualified Control.Exception as E
import Text.Parsec
import Text.Parsec.String
import System.Console.Haskeline
import Control.Concurrent
import Control.Monad.State
import Control.Arrow hiding(loop)
import Control.DeepSeq
import System.IO
import System.Random
import Text.Printf
import Text.XML.Light
import Data.Either
import Data.Map(Map)
import Util.Showable
import Util.FileUtil
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


data LoopContext = LoopContext [PMStream] (Maybe ThreadId) RunData


run :: RunData -> IO ()
run runData = do
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
      result <- initMidi i i
      case result of
        Left err -> putStrLn $ "MIDI error: " ++ show err
        Right streams -> do
         runInputT defaultSettings $ loop (LoopContext streams Nothing runData)
         stopMidi streams
         return ()


data CmdLine = MsrNum Int
             | Quit
             | Stop


cmdLine :: Parser CmdLine
cmdLine = (MsrNum <$> int) <|> quit <|> stop

int = many1 digit >>= return . read

quit = char 'q' >> eof >> return Quit

stop = char 's' >> eof >> return Stop
  

loop :: LoopContext -> InputT IO ()
loop context@(LoopContext streams tid _) = do
  s <- getInputLine "midi > "
  case s of
    Nothing -> loop context
    Just l -> case parse cmdLine "" l of
      Left err -> liftIO $ putStrLn $ show err
      Right cmd -> case cmd of
        MsrNum beg -> do
          contextOut <- liftIO $ loopIO beg context
          loop contextOut
        Quit -> return ()
        Stop -> case tid of
          Nothing -> loop context
          Just i  -> liftIO (killThread i >> allOff streams) >> loop context
          

loopIO :: Int -> LoopContext -> IO LoopContext
loopIO msrB (LoopContext streams _ r@(RunData xmlFilename metas tVar _ _)) = do
  score <- readXml
  putStrLn "Writing score.txt."
  writeFile "score.txt" . showIString $ score
  gen <- newStdGen
  let metaMap = M.fromList $ map (name &&& id) metas
      s = TrState score metaMap tVar gen M.empty (SMap M.empty)
          (SMap M.empty) (SMap M.empty) [] []
      (raws,finalState) = runState (toMidi msrB) s
  -- putStrLn $ showIString $ tsMetaState finalState
  raws <- E.evaluate $ force raws
  tid <- launchPlayRawEvents streams raws
  return (LoopContext streams (Just tid) r)


launchPlayRawEvents :: [PMStream] -> [RawMidiEvent] -> IO ThreadId
launchPlayRawEvents streams evts = do
  beginTime <- fromIntegral `liftM` time
  tid <- forkIO $ playRawEvents streams (beginTime+200) evts
  return tid
  

readXml :: IO Score
readXml = do
  buf <- readFileStrictly "/Users/Mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e


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
  

allOff :: [PMStream] -> IO ()
allOff streams = mapM_ allNotesOffAllChans streams >>
                 mapM_ pedalOffAllChans    streams


