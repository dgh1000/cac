module App.MidiPlayer.RunFunctions where

import Control.DeepSeq
import Control.Concurrent
import Control.Exception
import System.Directory
import System.Process
import System.FilePath
import System.IO
import System.Random
import System.IO.Unsafe
import Sound.PortMidi
import Text.XML.Light
import Text.Parsec.ByteString as TPB
import Text.Parsec as TP
import Text.Printf
import qualified Data.List as L
import qualified Data.ByteString as B
import Data.List(sort, sortBy, null)
import Data.Either
import Midi.Interface
import PlaybackPI.ParseConfig( parseConfigFile )
import PlaybackPI.ParseCommandLine( parseCmdLine )
import PlaybackPI.PlaybackPIExport
import qualified PlaybackPI.PlaybackPIData as PLD
import Common.CommonExport
import Util.Showable
import Util.Exception
import Util.FileUtil
import qualified MidiPI.MidiPIData as MPID
import Midi.MidiExport 
import Midi.ShowMidi
import MidiPI.MasterInstruments
import PerfDoc.PerfDocExport
import qualified PerfDoc.PerfDocData as PD
import PerfDoc.Finalize
import qualified PerfDoc.FromSibDoc as FS
import SibDoc.FromXml( xScoreToSibDoc )
import XmlDoc.ParseXml( parseXScore )
import PlaybackPI.Directories( hardCodedDirectories )
import SibDoc.ParseSibDoc( parseSibDoc )
import qualified SibDoc.FromParsed as FP
import PlaybackPI.Snip

------------------------------------------------------------------------
----------------- set this global to determine input source ------------
-----------------      set to InputXml or InputTxt ---------------------

global_inputType = InputXml


----------------------------------------------------------------------
----------------------------------------------------------------------
--          where the input comes from, txt or xml
global_textInputFileName = "c:/users/mike/out.txt"
global_xmlInputFileName = "c:/users/mike/out.xml"


----------------------------------------------------------------------
----------------------------------------------------------------------


data LoopContext = LoopContext
  { lcStreams :: [PMStream]
  , lcThreadID :: Maybe ThreadId
  , lcTempoRatio :: Double }




-- topLevel
--  
topLevel :: Int -> Int -> IO ()
topLevel midiDevNumLow midiDevNumHigh = do
  initialize
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> do
      loop (LoopContext streams Nothing 1.0)
      mapM_ close streams
      terminate
      return ()
    (err:_) -> putStrLn $ "Error opening output midi device.\n" ++ show err
      

openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Left stream -> return $ Right stream
    Right err -> return $ Left err



loop :: LoopContext -> IO ()
loop con = do
  parsedCmd <- lineFetch `catches` allHandlersM
  case parsedCmd of
    Nothing -> loop con
    Just com -> case com of
      InpTerminateProcess ->
        case lcThreadID con of
          Nothing -> loop con
          Just tid -> do {killThread tid; allOff con; loop con}
      InpQuit -> allOff con
      iPlay@InpPlay{} -> handleInpPlayCommand iPlay con
      InpParseError s -> do {putStrLn s; loop con}
      InpCwd i
        | i >= length hardCodedDirectories -> 
            do {putStrLn "Error:cwd index too large."; loop con}
        | otherwise -> do
            let s = hardCodedDirectories !! i
            putStrLn $ "Changing directory to " ++ s
            setCurrentDirectory s
            loop con
      InpDumpPerfDoc -> do
        dumpPerfDoc `catches` allHandlers
        loop con
      InpChangeTempo r -> loop con {lcTempoRatio = r}


handleInpPlayCommand :: InpCmd -> LoopContext -> IO () 
handleInpPlayCommand iCmd@(InpPlay rArgs _ _ _) con
  | not . validateMsrRangeArgs $ rArgs = do
      putStrLn $ "Something wrong with measure ranges. Must be in " ++
                 "ascending order and only last one can be open " ++
                 "range."
      loop con
  | otherwise = do
      cName <- computeConfigName
      putStrLn $ "Using this config: " ++ takeFileName cName
      case lcThreadID con of
        Just t -> killThread t
        Nothing -> return ()
      allOff con
      tid <- runPlay con cName iCmd `catches` allHandlersM
      allOff con
      loop con {lcThreadID = tid}
      

allOff :: LoopContext -> IO ()
allOff con = do
  mapM_ allNotesOffAllChans (lcStreams con)
  mapM_ pedalOffAllChans (lcStreams con)

-- runPlay
--
--  Read out.txt or out.xml, make a PerfDoc, translate it to MIDI events
--  with the help of data in the config file, and send it to MIDI streams.
--
runPlay :: LoopContext -> String -> InpCmd -> IO (Maybe ThreadId)
runPlay (LoopContext streams _ tRatio) cName (InpPlay ranges _ _ _) = do
  (config,finalPerfDoc) <- readConfigAndMusic cName ranges (Just tRatio)
  dumpPerfDoc2 finalPerfDoc
  -- dumpPerfDoc fileData
  if PD.isNullDoc finalPerfDoc
    then do {putStrLn "Null finalized PerfDoc; skipping." ; return Nothing}
    else do
      seed <- getStdRandom next
      let evts = toMidiEvts seed finalPerfDoc config
      writeMidiEvents "midi.txt" evts
      raws <- toRaws evts
      -- dumpRawEvents "raw.txt" raws
      beginTime <- time
      tid <- forkIO $ 
        playRawEvents streams (beginTime + fromIntegral 200) raws
      return $ Just tid

readConfigAndMusic :: String -> [MsrRange] -> Maybe Double -> 
                      IO (AllConfig,PerfDoc)
readConfigAndMusic configName ranges mTempo = do
  configData <- readConfig configName
  sibDoc <- readSibDoc
  let initialPerfDoc = FS.toPerfDoc MPID.configBeatTimeDivision mTempo sibDoc
      snips = computeSnipMsrRanges ranges initialPerfDoc 2
      pdFinalized =
        finalizePerfDoc (PLD.susPed configData) Nothing Nothing
        snips initialPerfDoc
  return (configData,pdFinalized)


toRaws :: [MidiEvent] -> IO [(MidiTime,RawMidiEvent)]
toRaws evts = do
  raws <- evaluate $ force $ mkRawEvents MPID.initialEventsDelay evts
  let (MidiTime tEnd,_) = last raws
  putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Double)
  return raws


-- computeMostRecentConfigName
--   computes filename, including path, of the config file to use. This
--   is computed by taking the most recent .sib file in the current directory
--   and changing its extension to .cfg
computeConfigName :: IO String
computeConfigName = do
  cwd <- getCurrentDirectory
  mostRecentSib <- mostRecentFile cwd "sib"
  let cfn = replaceExtension mostRecentSib "cfg"
  return cfn



readSibDoc = if global_inputType == InputTxt
  then readSibDocText
  else readSibDocXml

readSibDocText = do
  parsed <- TPB.parseFromFile parseSibDoc global_textInputFileName
  case parsed of
    Left err -> throwMine $ "Error parsing Sib. dump: " ++ show err
    Right psd -> return $ FP.convert psd

readSibDocXml = do
  -- buf <- readFileStrictly global_xmlInputFileName
  buf <- B.readFile global_xmlInputFileName
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xScoreToSibDoc . parseXScore $ e
        

validateMsrRangeArgs :: [MsrRange] -> Bool
validateMsrRangeArgs rs 
  | x1 /= sort x1 = False -- throwMine "Input msr ranges not in order"
  | not (all isTwoNums $ take (l-1) rs) = False 
    -- "There is a non-range msr number before the end"
  | otherwise = True
  where
  x1 = concatMap toList rs
  l = length rs
  toList (i,(Just j)) = [i,j]
  toList (i,Nothing) = [i]
  isTwoNums = (==2) . length . toList

-- As written the code here can only return a Just value. But
-- this will be used with exception handlers that can return a Nothing
lineFetch :: IO (Maybe InpCmd)
lineFetch = do
  putStr "(midi)---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then lineFetch
    else do  r <- evaluate $ lineParse li
             return (Just r)

lineParse ::  String -> InpCmd
lineParse s =
  case TP.parse parseCmdLine "" s of 
    Left err -> InpParseError (show err)
    Right cmd -> cmd


-- toMidiEvents
--
-- Converts to an unsorted list of MidiEvent 
--
-- Inputs
--   Int :: seed that will be used in some of the randomizations of 
--          conversion to midi
--   PerfDoc
--   AllConfig
--
toMidiEvts :: Int -> PerfDoc -> AllConfig -> [MidiEvent]
toMidiEvts seed pd allConfig = instrumentEventComputation seed pd allConfig

readConfig :: String -> IO AllConfig
readConfig configFileName = do
  parsed <- TPB.parseFromFile (parseConfigFile global_inputType) configFileName
  case parsed of
     Left err -> throwMine $ " in config: " ++ (show err)
     Right cs -> return cs

allHandlersM = [Handler myHandlerM, Handler ioHandlerM]

myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing

ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing

dumpRawEvents :: String -> [(MidiTime,RawMidiEvent)] -> 
                 IO [(MidiTime,RawMidiEvent)]
dumpRawEvents s es = do
  writeFile s (unlines $ map (\(t,e) -> printf "%15s: %s" (show t)
                                        (briefShowRawMidiEvent e)) es)
  return es

allHandlers = [Handler myHandler, Handler ioHandler]

myHandler :: MyException -> IO ()
myHandler (MyException s) = putStrLn $ "Error: " ++ s

ioHandler :: IOException -> IO ()
ioHandler e = putStrLn (show e)


dumpPerfDoc = do
  cName <- computeConfigName
  sibDoc <- readSibDoc
  allConfig <- readConfig cName
  let perfDoc = FS.toPerfDoc MPID.configBeatTimeDivision Nothing sibDoc
      perfDoc2 = finalizePerfDoc (PLD.susPed allConfig) Nothing Nothing [] 
                 perfDoc
  writeFile (replaceExtension cName "txt") . showItem . showi $ 
            perfDoc2
  putStrLn $ "\n!Dumped PerfDoc (_WITHOUT_ applying any commands or snips " ++
             "[but with finalizing])"

dumpPerfDoc2 perfDoc = do
  cName <- computeConfigName
  writeFile (replaceExtension cName "txt") . showItem .showi $
    perfDoc
  putStrLn "\n!Dumped PerfDoc with snips"

dumpSibDoc sibDoc = do
  writeFile "sib.txt" (showItem . showi $ sibDoc)
  putStrLn "!Dumped SibDoc"

writeMidiEvents :: String -> [MidiEvent] -> IO ()
writeMidiEvents filename evts = do
  let s = concatMap (showItem . showi) evts
  writeFileStrictly filename s