module App.MidiPlayer.RunFunctions where

import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec
import Data.List(sort)
import System.IO
import qualified Text.XML.MusicXML as X
import qualified Text.XML.MusicXML.Partwise as X
import qualified Text.XML.MusicXML.Score as X
import Control.Monad
import System.Directory
import System.FilePath
import Prelude hiding (catch)
import Sound.PortMidi
import Control.Concurrent
import Control.Exception
import Foreign.C.Types
import Util.FileUtil
import Util.Exception
import Util.Showable
import Util.Map
import MusDoc.Processing
import Midi.FromPerfDoc
import Midi.MidiData( MidiTime(..)
                , MidiClump(..)
                    , MyMidiEvent(..) )
import qualified Midi.MidiData as MiD
import Midi.MidiUtil
import App.MidiPlayer.ParseCmdLine( parseCmdLine )
import App.MidiPlayer.MidiPlayerData

-- Stuff borrowed from CsoundPlayer
import App.CsoundPlayer.ParseConfig( parseConfigFile )
import App.CsoundPlayer.CsoundPlayerData( AllConfig(..) 
                                        , Command(..) )
import qualified App.CsoundPlayer.CsoundPlayerData as CPD
import App.CsoundPlayer.Execute

import qualified PerfDoc.FromMusDoc as PF
import qualified MusDoc.FromXml as MF
import MusDoc.MusDocData( Loc(..)
                        , Part(..) 
                        , DirectionWords(..) )
import qualified MusDoc.MusDocData as MD
import PerfDoc.PerfDocData( PerfDoc(..)
                          , AbsoluteTimeMap(..) )
import qualified PerfDoc.PerfDocData as PD
import qualified PerfDoc.BasicTiming as BT
import PerfDoc.ShowPerfDoc
import Util.Showable
import Midi.ShowMidi


hardCodedDirectories = 
  [ "/mike/Music/compositions/2011/viola"
  , "/mike/Music/compositions/2011/mozart-like"
  , "/mike/Music/algo/compositions/finale/2011"]

topLevel = do
  initialize
  result <- openOutput 13 0
  case result of
    Left stream -> do
      loop stream Nothing
      allNotesOffAllChans stream
      close stream
      terminate
      return ()
    Right err -> do
      putStrLn $ "Error opening midi device.\n" ++ show err

allNotesOffAllChans stream = do
  let oneOfThem chan = writeShort stream $ mkEvt 0xb0 chan 123
  mapM_ oneOfThem [1..16]
      

loop :: PMStream -> Maybe ThreadId -> IO ()
loop stream maybeId = do
  parsedCmd <- lineFetch `catches` allHandlersM
  cwd <- getCurrentDirectory
  xfn <- mostRecentFile cwd "xml"  
  let cfn = replaceExtension xfn "cfg"
  case parsedCmd of
    Nothing -> loop stream maybeId
    Just com -> case com of
      InpTerminateProcess ->
        case maybeId of
          Nothing -> loop stream maybeId
          Just tid -> do
            killThread tid
            allNotesOffAllChans stream
            loop stream maybeId
      InpQuit -> return ()
      InpPlay rs
        | not (validateMsrRangeArgs rs) ->
          do putStrLn $ "Something wrong with measure numbers. They must be "++
                      "in ascending order, and only the last one can be " ++
                      "a single number (non-range) "
             loop stream maybeId
        | otherwise ->
           do tid <- runPlay stream xfn cfn rs
                `catches` allHandlersM
              loop stream tid
      InpParseError s -> do
        putStrLn s
        loop stream maybeId
      InpCwd i -> do
        let s = hardCodedDirectories !! i
        putStrLn $ "Changing directory to " ++ s
        setCurrentDirectory s
        loop stream maybeId
                     


runPlay :: PMStream -> String -> String -> [MsrRange] -> IO (Maybe ThreadId)
runPlay stream xfn cfn msrRanges = do
  xmlData <- readTheXml xfn
  (commands,allConfig) <- readConfig cfn
  let musDoc = MF.toMusDoc xmlData
      perfDoc = PF.toPerfDoc musDoc (CPD.timeDiv allConfig)
      perfDoc2 = applyCommands commands perfDoc 

      finalPerfDoc = perfDoc2

      bmsrs = CPD.blankMsrs allConfig 
      snipTimeRanges = 
        let x = computeSnipTimeRanges msrRanges finalPerfDoc bmsrs
        in (printf "\n%s\n" (show x)) `trace` x
      groups = toMidi finalPerfDoc snipTimeRanges
  writeFile "out.txt" (showItem $ showi groups)
  tid <- forkIO (playMidiClumps stream groups)
  return $ Just tid

validateMsrRangeArgs :: [MsrRange] -> Bool
validateMsrRangeArgs rs 
  | x1 /= sort x1 = False -- throwMine "Input msr ranges not in order"
  -- | not (all isTwoNums $ take (l-1) rs) = False 
  --  -- "There is a non-range msr number before the end"
  | otherwise = True
  where
  x1 = concatMap toList rs
  l = length rs
  toList (i,(Just j)) = [i,j]
  toList (i,Nothing) = [i]
  isTwoNums = (==2) . length . toList

lineFetch :: IO (Maybe InpCmd)
lineFetch = do
  putStr "---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then return Nothing
    else do  r <- evaluate $ lineParse li
             return r

lineParse ::  String -> Maybe InpCmd
lineParse s =
  case parse parseCmdLine "" s of 
    Left err -> Just $ InpParseError (show err)
    Right cmd -> Just $ cmd


--
-- Result:
--   X.Score_Partwise:  the parsed XML
--   String:            the XML full file path and name (and extension)
readTheXml :: String -> IO X.Score_Partwise
readTheXml fileName = do
  parsed <- X.read_FILE X.read_MusicXML_Partwise fileName
  if X.isOK parsed
     then return $ X.fromOK parsed
     else throwMine $ X.fromError parsed


{-
toTimestamp :: Float -> CULong
toTimestamp = fromIntegral . round . (* 1000.0)
-}

mkEvt :: CLong -> Int -> Int -> PMEvent
mkEvt status chan pitch = PMEvent msg 0
  where msg = PMMsg (status+fromIntegral chan-1) data1 data2
        data1 = fromIntegral pitch
        data2 = fromIntegral 64
  
{-
doStuff :: PMStream -> PMEvent -> PMEvent -> PMEvent -> PMEvent -> IO ()
doStuff stream evt1 evt2 evt3 evt4 =
  catch (doTheStuff stream evt1 evt2 evt3 evt4)
        (\e -> do let err = show (e :: AsyncException)
                  putStrLn $ "Thread killed. " ++ err
                  return ())

doTheStuff stream noteOn noteOff note2On note2Off = do
  let doOne delayBefore delayAfter = do
        threadDelay (delayBefore * 1000)
        writeShort stream noteOn
        writeShort stream note2On
        threadDelay (delayAfter * 1000)
        writeShort stream noteOff
        writeShort stream note2Off
  --doOne 2000 2000
  --doOne 2000 2000
  --doOne 2000 2000
  sequence_ (replicate 20 (doOne 10 90))
-}

playMidiClumps :: PMStream -> [(MidiTime,MidiClump)] -> IO ()
playMidiClumps stream xs = do
  playMidiClumps2 stream zeroMidiTime zeroMidiTime xs
  putStrLn "\nDone!\n"
     

-- playMidiClumps
-- 
playMidiClumps2 :: PMStream -> MidiTime -> MidiTime -> 
                   [(MidiTime,MidiClump)] -> IO ()
playMidiClumps2 stream lastTime lastStatusMsg [] = return ()
playMidiClumps2 stream lastTime lastStatusMsg ((t,clump):remain) = do
  let dmicroseconds = round (1000000 * MiD.midiTimeDiff t lastTime)
      statMicro = round (1000000 * MiD.midiTimeDiff t lastStatusMsg)
  when (dmicroseconds > 100) (threadDelay dmicroseconds)
  lastStatusMsg2 <- if statMicro > 1000000
                    then do { putStr "." ; return t }
                    else return lastStatusMsg
  playOneClump stream clump
  playMidiClumps2 stream t lastStatusMsg2 remain

playOneClump stream clump = do
  -- play note off first, assuming we don't want any of the control or
  -- keyswitch events to affect notes sounding now. Then play control and
  -- keyswitch  so those influence the notes on. Then notes on.
  playEvents stream $ MiD.noteOffEvents clump
  playEvents stream $ MiD.controlEvents clump
  playEvents stream $ MiD.keyswitchEvents clump
  playEvents stream $ MiD.noteOnEvents clump

playEvents stream evts =
  mapM_ (writeShort stream . toPMEvent) evts

toPMEvent mme = PMEvent msg 0
        where 
          (status,data1,data2) = case mme of
            NoteOnEvent chan ndata1 ndata2 -> 
              (fromIntegral 0x90 + fromIntegral chan
               - 1, fromIntegral ndata1 , fromIntegral ndata2)
            NoteOffEvent chan ndata1 ndata2 -> 
              (fromIntegral 0x80 + fromIntegral chan
               - 1, fromIntegral ndata1 , fromIntegral ndata2)
            ControlEvent nchan nstatus ndata1 ndata2 ->
              (fromIntegral nchan-1 + fromIntegral nstatus,fromIntegral ndata1
              , fromIntegral ndata2 )
            KeyswitchEvent chan nstatus ndata1 ndata2 ->
              (fromIntegral chan-1 + fromIntegral nstatus, fromIntegral ndata1
              , fromIntegral ndata2 )
          msg = PMMsg status data1 data2

computeSnipTimeRanges :: [MsrRange] -> PerfDoc -> Int -> [(Float,Float)]
computeSnipTimeRanges playRanges pd nblanks = 
  map (\(i,j) -> (msrNumToTime atm i, msrNumToTime atm j)) snipHoles
  where
  doc = PD.pMusDoc pd
  n = numMsrs doc
  actualMsrRanges :: [(Int,Int)] 
  actualMsrRanges = map mkActualRange playRanges
  mkActualRange :: MsrRange -> (Int,Int)
  mkActualRange (i,Nothing) = (i, firstNEmptyMeasures doc nblanks i - 1)
  mkActualRange (i, Just j) = (i,j)
  snipHoles = computeSnipHoles actualMsrRanges n
  atm = BT.deltaToAbsolute (PD.timeMap pd)

-- computeSnipHoles
--   Given a set of ranges to INCLUDE, this routine calculated the ranges
--   of measures to EXCLUDE
-- Inputs
--   [(Int,Int)] :: the ranges of measures to play (inclusive)
--   Int         :: the number of measures in the document
computeSnipHoles :: [(Int,Int)] -> Int -> [(Int,Int)]
computeSnipHoles playRanges n = catMaybes $ map processPair pairs
  where
  addendum = [(0,0)] ++ playRanges ++ [(n+1,0)]
  pairs = zip addendum (tail addendum)
  processPair :: ((Int,Int),(Int,Int)) -> Maybe (Int,Int)
  processPair ((_,x),(y,_)) = 
    if (x+1) == y 
    then Nothing
    else Just (x+1,y)


msrNumToTime :: AbsoluteTimeMap -> Int -> Float
msrNumToTime atm num = BT.locToTime atm (Loc num 1)
  
{-
delayMiliseconds :: Int -> IO ()
delayMiliseconds = threadDelay . (* 1000)
-}

readConfig :: String -> IO ([Command],AllConfig)
readConfig configFileName = do
  configBuffer <- readFile configFileName
  case parse parseConfigFile "" configBuffer of
     Left err -> throwMine $ "Config file parse error: " ++ (show err)
     Right cs -> return cs

applyCommands :: [Command] -> PerfDoc -> PerfDoc
applyCommands cs pd = foldl step pd cs
  where
    atm = BT.deltaToAbsolute (PD.timeMap pd)
    mdw = mergeDirectionWords (map PD.partData . M.elems . PD.pParts $ pd)
    step :: PerfDoc -> Command -> PerfDoc
    step pd' com = execute pd' com mdw atm

mergeDirectionWords :: [Part] -> Map DirectionWords [Loc]
mergeDirectionWords parts = reverseLMap u
  where
    wordMaps :: [Map Loc [DirectionWords]]
    wordMaps = map MD.directionWords parts
    u = M.unionsWith (++) wordMaps

    

allHandlersM = [Handler myHandlerM, Handler ioHandlerM]

myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing

ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing

