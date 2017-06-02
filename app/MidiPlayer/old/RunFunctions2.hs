module App.MidiPlayer.RunFunctions where

import System
import System.FilePath
import System.IO.Unsafe
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec
import Data.List(sort, sortBy, null)
import qualified Data.List as L
import System.IO
import System.Process
import Sound.PortMidi

import qualified Text.XML.MusicXML as X
import qualified Text.XML.MusicXML.Partwise as X
import qualified Text.XML.MusicXML.Score as X
import Control.Monad
import System.Directory
import System.FilePath
import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Foreign.C.Types
import Util.FileUtil
import Util.Exception
import Util.Showable
import Util.Map
import MusDoc.Processing
import Midi.MidiData( MidiEvent(..)
                    , RawMidiEvent(..)
                    , RawMidiEventType(..) 
                    , MidiTime(..) 
                    , mtAdd, mtSub )
import qualified Midi.MidiData as MiD
import Playback.ParseConfig( parseConfigFile )
import Playback.ParseCommandLine( parseCmdLine )
import Playback.PlaybackData( AllConfig(..) 
                            , Command(..) 
                            , InstrMap (..)
                            , InpCmd(..)
                            , MsrRange(..) )
import qualified Playback.PlaybackData as PLD
import Playback.Execute
import Playback.Directories( hardCodedDirectories )
import qualified PerfDoc.FromMusDoc as PF
import qualified MusDoc.FromXml as MF
import MusDoc.MusDocData( Part(..) 
                        , DirectionWords(..) 
                        , MusDoc )
import qualified MusDoc.MusDocData as MD
import Common.CommonData( Loc(..) )
import MusDoc.ShowMusDoc
import PerfDoc.PerfDocData( PerfDoc(..)
                          , AbsoluteTimeMap(..) )
import qualified PerfDoc.PerfDocData as PD
import qualified PerfDoc.BasicTiming as BT
import PerfDoc.Prepare
import PerfDoc.ShowPerfDoc
import Util.Showable
import Midi.ShowMidi
import Midi.Lookup
import Midi.Instruments.InstrumentsData( configXSampleShearEarly )

topLevel :: Int -> IO ()
topLevel deviceNum = do
  {-
  args <- getArgs
  let deviceNum = 
       if null args 
         then 13
         else case reads $ head args of
          [] -> throwMine "can't parse first cmd line arg as an int"
          (i,_):_ -> i
  -}
  initialize
  result <- openOutput deviceNum 0
  case result of
    Left stream -> do
      loop $ LoopContext stream Nothing
      -- allNotesOffAllChans stream
      close stream
      terminate
      return ()
    Right err -> do
      putStrLn $ "Error opening midi device.\n" ++ show err

allNotesOffAllChans :: PMStream -> IO ()
allNotesOffAllChans stream =
  mapM_ (allNotesOff stream) [1..16]


allNotesOff stream chan = writeShort stream $
        toPMEvent (RawMidiEvent chan 0xb0 123 0)
  

data LoopContext = LoopContext
  { lcStream :: PMStream
  , lcTid :: Maybe ThreadId }

loop :: LoopContext -> IO ()
loop con = do
  parsedCmd <- lineFetch `catches` allHandlersM

  {-
  -- THE FOLLOWING: case of saving XML to ram disk
  cwd <- getCurrentDirectory
  let ramDiskPath = "g:\\"
  xfn <- mostRecentFile ramDiskPath "xml"  
  let (_,xfnBase) = splitFileName xfn
      cfn = joinPath [cwd, (replaceExtension xfnBase "cfg")]
  -}



  -- THE FOLLOWING: XML always saved to ram disk with fixed name "out.xml"
  --   and cfg has named "current.cfg" in current working directory
  cwd <- getCurrentDirectory
  let xfn = "g:/out.xml"
      cfn = joinPath [cwd, "current.cfg"]


  {-  THE FOLLOWING: FOR CASE THAT XML IS SAVED TO HARD DRIVE to current dir
  cwd <- getCurrentDirectory
  xfn <- mostRecentFile cwd "xml"
  let cfn = replaceExtension xfn "cfg"
  -}

  case parsedCmd of
    Nothing -> loop con
    Just com -> case com of
      InpTerminateProcess ->
        case lcTid con of
          Nothing -> loop con
          Just tid -> do
            killThread tid
            allNotesOffAllChans (lcStream con)
            loop con
      InpQuit -> return ()
      InpPlay rs _ mTempoRatio mTransposition
        | not (validateMsrRangeArgs rs) ->
          do putStrLn $ "Something wrong with measure numbers. They must be "++
                      "in ascending order, and only the last one can be " ++
                      "a single number (non-range) "
             loop con
        | otherwise ->
           do case lcTid con of
                 Just t -> killThread t
                 Nothing -> return ()
              allNotesOffAllChans (lcStream con)
              tid <- runPlay (lcTid con) (lcStream con) xfn cfn rs mTempoRatio 
                     mTransposition `catches` allHandlersM
              loop con { lcTid = tid }
      InpParseError s -> do
        putStrLn s
        loop con
      InpCwd i -> do
        let s = hardCodedDirectories !! i
        putStrLn $ "Changing directory to " ++ s
        setCurrentDirectory s
        loop con
      InpDumpMusDoc -> do
        dumpMusDoc xfn `catches` allHandlers
        loop con
      InpDumpPerfDoc -> do
        dumpPerfDoc xfn cfn `catches` allHandlers
        loop con
        

dumpMusDoc :: String -> IO ()
dumpMusDoc xfn = do
  xml <- readTheXml xfn
  let doc = MF.toMusDoc xml
  writeFile (replaceExtension xfn "txt") $ showItem $ showi doc
  putStrLn "Sucessful dump."



{-
runPlay :: Maybe ThreadId -> PMStream -> String -> String -> [MsrRange] -> 
           IO (Maybe ThreadId)
runPlay maybeId stream xfn cfn playRanges = do
  xmlData <- readTheXml xfn
  (commands,allConfig) <- readConfig cfn
  let musDoc = MF.toMusDoc xmlData
      perfDoc = PF.toPerfDoc musDoc (PLD.timeDiv allConfig)
      perfDoc2 = applyCommands commands perfDoc 
      blankSepNeed = PLD.blankMsrs allConfig 
      snips = computeSnipMsrRanges playRanges musDoc blankSepNeed
      pdPrepared = preparePerfDoc snips perfDoc2
      -- pdPrepared = (let x = preparePerfDoc snips perfDoc2
      --              in unsafePerformIO (dumpPerfDoc "outp.txt" x))
      
      midiEvts = toMidiEvents pdPrepared (PLD.midiInstrMap allConfig)
      -- Delay caused by moving absolute start time into the past, in order
      -- to handle midi events with negative start times
      preStartDelay = 200
  rmes <- evaluate $ mkRawEvents midiEvts 
  -- dumpRawEvents "outr.txt" rmes
  beginTime <- time
  tid <- forkIO $ 
         playRawEvents stream (beginTime + fromIntegral preStartDelay) rmes
  return $ Just tid
-}

{-
data PlayContext = PlayContext
  { pcThreadId :: Maybe ThreadId
  , pcStream :: PMStream
  , pcXfn :: String  -- xml file
  , pcCfn :: String -- config file
  , pcRanges :: [MsrRange]
  , pcTempoRatio :: Maybe Float
  , pcTransposition :: Maybe Int 
  , usePercussion :: Bool }
-}

runPlay :: Maybe ThreadId -> PMStream -> String -> String -> [MsrRange] -> 
           Maybe Float -> Maybe Int -> IO (Maybe ThreadId)
runPlay maybeId stream xfn cfn playRanges mTempoRatio mTransposition = do
  xmlData <- readTheXml xfn
  (commands,allConfig) <- readConfig cfn
  let musDoc = MF.toMusDoc xmlData
      perfDoc = PF.toPerfDoc musDoc (PLD.timeDiv allConfig)
      perfDoc2 = applyCommands commands perfDoc 
      blankSepNeed = PLD.blankMsrs allConfig 
      snips = computeSnipMsrRanges playRanges musDoc blankSepNeed
      pdPrepared = preparePerfDoc configXSampleShearEarly 
                   mTempoRatio mTransposition snips perfDoc2
                   
  if PD.isNullDoc pdPrepared
    then do putStrLn "Null prepared document; skipping."
            return Nothing
    else do
      let midiEvts = toMidiEvents pdPrepared (PLD.midiInstrMap allConfig)
         -- Delay caused by moving absolute start time into the past, in order
         -- to handle midi events with negative start times
          preStartDelay = 200
      rmes <- evaluate $ mkRawEvents midiEvts 
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Float)
      beginTime <- time
      tid <- forkIO $ 
         playRawEvents stream (beginTime + fromIntegral preStartDelay) rmes
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
  d <- getCurrentDirectory
  putStrLn d
  putStr "(midi)---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then lineFetch
    else do  r <- evaluate $ lineParse li
             return (Just r)

lineParse ::  String -> InpCmd
lineParse s =
  case parse parseCmdLine "" s of 
    Left err -> InpParseError (show err)
    Right cmd -> cmd


--
-- Result:
--   X.Score_Partwise:  /the parsed XML
--   String:            the XML full file path and name (and extension)
readTheXml :: String -> IO X.Score_Partwise
readTheXml fileName = do
  parsed <- X.read_FILE X.read_MusicXML_Partwise fileName
  if X.isOK parsed
     then return $ X.fromOK parsed
     else throwMine $ X.fromError parsed

{-
-- toMidiEvents
--
-- Converts to *unsorted* list of MidiEvent
toMidiEvents :: PerfDoc -> InstrMap -> [MidiEvent]
toMidiEvents pd instrMap = concatMap midiOnePart . M.toList . PD.parts $ pd
  where
    midiOnePart (pid, part) = 
      let (ip, fn) = lookupInstr pid instrMap
      in fn ip part    
-}

-- toMidiEvents
--
-- Converts to *unsorted* list of MidiEvent
toMidiEvents :: PerfDoc -> InstrMap -> [MidiEvent]
toMidiEvents pd instrMap = 
  concatMap midiOnePart . M.toList . PD.parts $ pd
  where
    midiOnePart (pid, part) = 
      case lookupInstr pid instrMap of
        Nothing -> 
            printf "No instrm or instrc for part ID %s found.. skipping" pid
                       `trace` []
        -- here it skills calling the "translation to midi function" if there
        --   are no notes in the part, because otherwise the translation
        --   function will try to generate a loudness curve anyway
        Just (ip, fn) -> if PD.isNullPart part then [] else  fn ip part
  
{-
lookupWithPercussion :: Bool -> PartId -> InstrMap ->Maybe (InstrPlay, InstrFn)
lookupWithPercussion percFlag pid imap =
  case lookupInstr pid imap of
    Nothing -> Nothing
    Just (ip, fn) -> 
      if percFlag then (ip, g(PlD.ipChan ip)) else (ip, fn)
-}  

-- computeRawEvents
--
-- Given a list of MidiEvent (in which a note-type MidiEvent will be
-- several raw midi events) translate to a list of RawMidiEvent
-- 
-- Will SORT the list.
-- 
-- Will normalize to zero MidiTime
--
-- Also this will shift "note modifiers" 6 ms into the past to allow any 
-- controllers/keyswitches to take effect before it is played.
--
mkRawEvents :: [MidiEvent] -> [(MidiTime, RawMidiEvent)]
mkRawEvents evts = sortNormalizeEvents raws
  where
    xMidiEvent (NoteEvent t on off timeOff mods) =
      [(t, on), (timeOff, off)] ++ map g mods
      where
        g e = (t `mtAdd` MidiTime (-6), e)
    xMidiEvent (SingleEvent t evt) = [(t, evt)]
    raws = concatMap xMidiEvent evts

sortNormalizeEvents :: [(MidiTime, RawMidiEvent)] -> [(MidiTime, RawMidiEvent)]
sortNormalizeEvents evts = resort
  where
    sorted = sortBy (\(t1,_) (t2,_) -> compare t1 t2) evts
    finded = L.find (\(t,_) -> t /= MiD.tMeansSartOfTrack) $  sorted
    tMin =  case finded of
      Nothing -> throwMine "87ki1"
      Just (t,_) -> let x = ("t: " ++ show t) `trace` t in x 
    tOffset = tMin `mtSub` MiD.initialEventsDelay
    xTime (t,e) = if t == MiD.tMeansSartOfTrack
                  then (MidiTime 0, e)
                  else (t `mtSub` tOffset, e)
    xed = map xTime sorted
    resort = sortBy (\(t1,_) (t2,_) -> compare t1 t2) xed
    

     
playRawEvents :: PMStream -> CULong -> [(MidiTime, RawMidiEvent)] -> IO ()
playRawEvents _ _ [] = do
  putStrLn "\nDone." 
  return ()
playRawEvents stream absBegin ((MidiTime t, rme): remain) = do
  -- let tMs = round $ 1000 * t
  spinUntil $ fromIntegral absBegin + t
  writeShort stream $ toPMEvent rme
  playRawEvents stream absBegin remain

spinUntil :: Integer -> IO ()
spinUntil t = do
  c <- time
  if fromIntegral c < t then spinUntil t else return ()


-- toPMEvent
--   Converts RawMidiEvent to PMEvent (note, the latter includes a time,
--   but since we do playback with the PortMidi's function writeShort, 
--   the PortMidi library will ignore the time, so we just set it to zero)
toPMEvent :: RawMidiEvent -> PMEvent
toPMEvent (RawMidiEvent chan status data1 data2) = PMEvent msg 0
  where msg = PMMsg (fromIntegral $ chan + status - 1) 
              (fromIntegral data1) (fromIntegral data2)

{-
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
-}


computeSnipMsrRanges :: [MsrRange] -> MusDoc -> Int -> [(Int,Int)]
computeSnipMsrRanges playRanges doc nblanks = snipHoles
  where
    n = numMsrs doc
    explicitMsrRanges = map computeExplicitRange playRanges
    computeExplicitRange :: MsrRange -> (Int,Int)
    computeExplicitRange (i,Nothing) = (i,firstNEmptyMeasures doc nblanks i-1)
    computeExplicitRange (i, Just j) = (i,j)
    snipHoles = computeSnipHoles explicitMsrRanges n


-- computeSnipHoles
--   Given a set of ranges to INCLUDE, this routine calculated the ranges
--   of measures to EXCLUDE
-- Inputs
--   [(Int,Int)] :: the ranges of measures to play (inclusive)
--   Int         :: the number of measures in the document
--
-- Output
--  [(Int,Int)] :: the measures to snip, where the second one IS ONE PAST
--                  the last measure to snip.
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
    -- mdw = mergeDirectionWords (map PD.partData . M.elems . PD.pParts $ pd)
    step :: PerfDoc -> Command -> PerfDoc
    step pd' com = execute pd' com atm

{-
mergeDirectionWords :: [Part] -> Map DirectionWords [Loc]
mergeDirectionWords parts = reverseLMap u
  where
    wordMaps :: [Map Loc [DirectionWords]]
    wordMaps = map MD.directionWords parts
    u = M.unionsWith (++) wordMaps
-}
    

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
  writeFile s (unlines $ map (\(t,e) -> printf "%s: %s" (show t)
                                        (briefShowRawMidiEvent e)) es)
  return es

allHandlers = [Handler myHandler, Handler ioHandler]

myHandler :: MyException -> IO ()
myHandler (MyException s) = putStrLn $ "Error: " ++ s

ioHandler :: IOException -> IO ()
ioHandler e = putStrLn (show e)

dumpPerfDoc :: String -> String -> IO ()
dumpPerfDoc xfn cfn = do
  xml <- readTheXml xfn
  let doc = MF.toMusDoc xml
  (commands,allConfig) <- readConfig cfn
  let perfDoc = PF.toPerfDoc doc (PLD.timeDiv allConfig)
  writeFile (replaceExtension xfn "txt") $ showItem $ showi perfDoc
  putStrLn "Dumped PerfDoc (_WITHOUT_ applying any commands)"
