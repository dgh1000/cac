
module Instruments.RunOnce where

-- The idea: a config file will be a Haskell script that spawns MIDI
-- playback. It will call routines in this module.

import qualified Data.Map as M
import qualified Control.Exception as E
import qualified Sound.PortMidi as SP
import qualified Data.List as L
import qualified Data.Set as S
import qualified Text.XML.Light as XL
import Debug.Trace
import Sound.PortMidi hiding (name,initialize)
import System.Random
import Control.Exception
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.DeepSeq
import Control.Concurrent
import Text.XML.Light hiding (qName)
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Data.Map(Map)
import Data.Either
import Data.Maybe
import Data.Set (Set)
import Instruments.InstrumentsData
import Instruments.ToMidi
import Instruments.Piano
import Midi.Interface
import Score.ScoreData
import Score.XmlToScore
import Score.ShowScore
import XmlDoc.ParseXml
import Util.FileUtil
import Util.Exception
import Util.Showable


-- <beg msr> <maybe end msr> <maybe solo meta-instr>
data PlayCmd = PlayCmd Int (Maybe Int) (Maybe String)

-- we want to validate 

runOnce :: RunData -> [String] -> IO ()
runOnce rd args =
  runOnce_2 args rd `catches` handlers


runOnce_2 :: [String] -> RunData -> IO ()
runOnce_2 args (RunData metasIn tVar) = do
  let PlayCmd mBeg mEnd mSolo = parseArgs args
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn (show err) >> return ()
    Right streams -> do
      score <- readXml
      -- putStrLn "writing score.txt"
      --   >> writeFile "score.txt" (showIString score)
      gen <- newStdGen
      let scoreStaffNs = M.keysSet $ scStaves score
      -- if a meta is solo, do that now
      let metas = case mSolo of
            Nothing -> metasIn
            Just m  -> case L.find ((==m) . mName) metasIn of
              Nothing -> throwMine $ printf ("you asked to solo meta with" ++
                 "name '%s' but no such meta exists") m
              Just x -> [x]

      -- do some error checking
      let coveredStaffNs = computeCovered metas
      when (coveredStaffNs /= scoreStaffNs)
        (putStrLn $ printf ("WARNING! score staff names are %s; meta " ++
          "coverage is %s") (show scoreStaffNs) (show coveredStaffNs))
      let names = map mName metas :: [String]
      when (length names /= length (S.fromList names))
        (throwMine $ printf ("meta names are %s; these are not unique")
           (show names))

      -- proceed after error checking
      let metaMap = M.fromList $ map (mName &&& id) metas
          s = TrState score metaMap tVar gen M.empty M.empty [] [] [] M.empty
          (err_or_shorts,finalState) =
            runState (runExceptT (toMidi (mBeg,mEnd))) s
      case err_or_shorts of
        Left msg -> putStrLn msg >> return ()
        Right shorts -> do
          -- debugDump finalState 
          raws <- E.evaluate $ force shorts
          allOff streams
          beginTime <- fromIntegral `liftM` time
          playRawEvents streams (beginTime+200) raws `catches` handlers
          threadDelay 1000000
          allOff streams
          stopMidi streams
          return ()

----------------------------------------------------------------------
--               parsing argument


parseArgs :: [String] -> PlayCmd
parseArgs [s] = case parse playCmd "" s of
  Left err -> throwMine $ "parsing error in argument in excecuting script " ++
                          "that calls RunOnce: " ++ show err
  Right c  -> c
parseArgs _ = throwMine $ "more than one arg in executing script " ++
                          "that calls RunOnce"


playCmd :: Parser PlayCmd
playCmd = do
  begMsr <- many1 digit
  char '-'
  endMsr <- many1 digit
  soloStaff <- between (char '{') (char '}') (many $ noneOf "{}")
  return $ PlayCmd (read begMsr)
                   (case read endMsr of {0 -> Nothing; x -> Just x})
                   (case soloStaff of {"" -> Nothing; s -> Just s})
  


----------------------------------------------------------------------


computeCovered :: [Meta] -> Set String
computeCovered metas
  | length listing /= length s = throwMine $ printf ("metas cover staves "++
      "%s; some of these are duplicates") (show listing)
  | otherwise = s
  where
    listing = concatMap mStaffNs metas
    s = S.fromList listing

debugDump :: TrState -> IO ()
debugDump ts = do
  let s1 = showiToString $ Component "time mods" True
                           (map showI $ M.toAscList $ tsTimeMods ts)
  let s2 = showiToString $ Component "notes" True
           (map showI $ concat $ tsNotes ts)
      s3 = showIString $ tsScore ts
  putStrLn "writing feb.txt..." >> writeFile "feb.txt" s1


computeMsrRange :: [String] -> (Int,Maybe Int)
computeMsrRange args = case args of
  [beg,"-"] -> (ri beg,Nothing)
  [beg,end] -> (ri beg,Just $ ri end)
  where
    ri :: String -> Int
    ri s = case reads s of { (i,_):_ -> i}

{-
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
-}

readXml :: IO Score
readXml = do
  buf <- readFileStrictly "/Users/Mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . XL.qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e

{-
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
-}

handlers = [Handler asyncHandlerM,Handler myHandlerM,Handler ioHandlerM]


myHandlerM :: MyException -> IO ()
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s


ioHandlerM :: IOException -> IO ()
ioHandlerM e = do
  putStrLn (show e)


asyncHandlerM :: AsyncException -> IO ()
asyncHandlerM _ = do
  putStrLn "Interrupted!"
  
