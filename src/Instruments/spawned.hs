
-- The idea: a config file will be a Haskell script that spawns MIDI
-- playback. It will call routines in this module.

import qualified Data.Map as M
import qualified Control.Exception as E
import qualified Sound.PortMidi as SP
import qualified Data.List as L
import Sound.PortMidi hiding (name,initialize)
import System.Random
import Control.Exception
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.DeepSeq
import Text.XML.Light
import Data.Map(Map)
import Data.Either
import Instruments.InstrumentsData
import Instruments.ToMidi
import Instruments.Piano
import Translation.TranslationData
import Midi.Interface
import Score.ScoreData
import Score.XmlToScore
import XmlDoc.ParseXml
import Util.FileUtil
import Util.Exception

{-
dests = M.fromList [ ("Keyboard-staff1", (0,1))
                   , ("Keyboard-staff2", (0,2)) ]

metas = Meta "piano1" (M.keys dests) (pianoInit dests) pianoRun

tVar = TimingVariation 3 7 0.95 1.05 (-0.3) 0.3

main = do
  runSpawned (RunData [metas] tVar) (2,Just 2)
-}

runSpawned :: RunData -> (Int,Maybe Int) -> IO ()
runSpawned (RunData metas tVar) msrs = do
  mStreams <- startMidi
  case mStreams of
    Nothing -> return ()
    Just streams -> do
      score <- readXml
      gen <- newStdGen
      let metaMap = M.fromList $ map (metaName &&& id) metas
          s = TrState score metaMap tVar gen M.empty (VMap M.empty)
            (VMap M.empty) (VMap M.empty) [] [] []
          (err_or_shorts,finalState) = runState (runExceptT $ toMidi msrs) s
      case err_or_shorts of
        Left msg -> putStrLn msg >> return ()
        Right shorts -> do
          raws <- E.evaluate $ force shorts
          allOff streams
          beginTime <- fromIntegral `liftM` time
          playRawEvents streams (beginTime+200) raws `catches` handlers
          allOff streams
          stopMidi streams
          return ()

      
startMidi :: IO (Maybe [PMStream])
startMidi = do
  i <- openMidiPipeInput2
  result <- initMidi i i
  case result of
    Left err -> putStrLn (show err) >> return Nothing
    Right streams -> return $ Just streams


stopMidi :: [PMStream] -> IO ()    
stopMidi streams = do
  mapM_ close streams
  terminate
  return ()    


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


initMidi :: Int -> Int -> IO (Either PMError [PMStream])
initMidi midiDevNumLow midiDevNumHigh = do
  SP.initialize
  
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> return $ Right streams
    (err:_) -> return $ Left err


openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Left stream -> return $ Right stream
    Right err -> return $ Left err


handlers = [Handler asyncHandlerM]


asyncHandlerM :: AsyncException -> IO ()
asyncHandlerM _ = do
  putStrLn "Interrupted!"
  
