module App.MidiPlayer.RunFunctionsXml where

import System.Cmd
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
import Control.Monad
import System.Directory
import System.FilePath
import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Foreign.C.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import XmlDoc.ParseXml
import XmlDoc.XmlDocExport
import Util.FileUtil
import Util.Exception
import Util.Showable
import Util.Map
import Midi.MidiExport 
import Midi.Interface
import qualified Midi.MidiData as MiD
import PlaybackPI.ParseConfig( parseConfigFile )
import PlaybackPI.ParseCommandLine( parseCmdLine )
import PlaybackPI.PlaybackPIExport
import qualified PlaybackPI.PlaybackPIExport
import qualified PlaybackPI.PlaybackPIData as PLD
import PlaybackPI.Execute
import PlaybackPI.Directories( hardCodedDirectories )
import PlaybackPI.Snip
import Common.CommonExport
import MusDoc.FromXmlDoc
import PerfDoc.PerfDocExport
import qualified PerfDoc.PerfDocData as PD
import qualified PerfDoc.BasicTiming as BT
import PerfDoc.Finalize
import qualified PerfDoc.PerfDocUtil as PU
import PerfDoc.ShowPerfDoc
import PerfDoc.FinalizeUtil
import SibDoc.ShowSibDoc
import qualified PerfDoc.FromSibDoc as FS
import Util.Showable
import Midi.ShowMidi
import MidiPI.Lookup
import qualified MidiPI.MidiPIData as MPID
import MidiPI.Instruments.InstrumentsData( configXSampleShearEarly )
import qualified PerfDoc.FromMusDoc as FM


topLevel :: Int -> IO ()
topLevel deviceNum = realTimeMidiRun loop deviceNum

data LoopContext = LoopContext
  { lcStream :: PMStream
  , lcTid :: Maybe ThreadId }

instance MidiLoopContext LoopContext where
  mkInitMidiLoopContext stream = LoopContext stream Nothing

loop :: LoopContext -> IO ()
loop con = do
  parsedCmd <- lineFetch `catches` allHandlersM

  -- find most recent sib file
  cwd <- getCurrentDirectory
  mostRecentSib <- mostRecentFile cwd "sib"
  putStrLn $ "Using this sib doc: " ++ (takeFileName mostRecentSib)

  let dfn = "g:/out.xml"             -- plugin dump filename
      --- cfn = joinPath [cwd, "current.cfg"]
      cfn = replaceExtension mostRecentSib "cfg"


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
      InpPlay rs mFNameWriPerfDoc mTempoRatio mTransposition
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
              tid <- runPlay (lcTid con) (lcStream con) dfn cfn rs mTempoRatio 
                     mTransposition mFNameWriPerfDoc `catches` allHandlersM
              loop con { lcTid = tid }
      InpParseError s -> do
        putStrLn s
        loop con
      InpCwd i -> do
        let s = hardCodedDirectories !! i
        putStrLn $ "Changing directory to " ++ s
        setCurrentDirectory s
        loop con
      {-
      InpDumpMusDoc -> do
        dumpMusDoc dfn `catches` allHandlers
        loop con
      -}
      InpDumpPerfDoc -> do
        dumpPerfDoc dfn cfn `catches` allHandlers
        loop con

-- runPlay
--  Maybe ThreadId :: == "Just <id>" if a thread has been started
--  PMStream :: midi stream
--  String :: (dfn) MusicXML file name
--  String :: (cfn) config file name
--  [MsrRange] :: I think this is ranges of measures to be played
--  mTempoRatio :: if Just <ratio>,  modify the tempo by this much
--  mTransposition :: maybe transpose the result by this many half steps
--  mFNameWriPerfDoc :: maybe filename to write perf doc dump to
runPlay :: Maybe ThreadId -> PMStream -> String -> String -> [MsrRange] -> 
           Maybe Double -> Maybe Int -> Maybe String -> IO (Maybe ThreadId)
runPlay maybeId stream dfn cfn playRanges mTempoRatio mTransposition
        mFNameWriPerfDoc = do
  xmlDoc <- readXmlDoc dfn
  let musDoc = fromXmlDoc xmlDoc
  (commands,allConfig) <- readConfig cfn
  let perfDoc1 = 
        FM.toPerfDoc (PLD.susPedParts allConfig) (PLD.timeDiv allConfig) $
         musDoc 
      snips = computeSnipMsrRanges playRanges perfDoc1 blankSepNeed
      pdFinalized =
        applyCommands commands .
        finalizePerfDoc mTempoRatio mTransposition snips $
        perfDoc1
      blankSepNeed = PLD.blankMsrs allConfig 

  if PD.isNullDoc pdFinalized
    then do putStrLn "Null finalized PerfDoc; skipping."
            return Nothing
    else do
      case mFNameWriPerfDoc of
        Nothing -> return ()
        Just s -> writeFile s (showItem . showi $ pdFinalized)

      let midiEvts = toMidiEvents pdFinalized (PLD.midiInstrMap allConfig)
         -- Delay caused by moving absolute start time into the past, in order
         -- to handle midi events with negative start times
          preStartDelay = 200
      rmes <- evaluate $ mkRawEvents MPID.modifierEarlyBy 
              MPID.initialEventsDelay midiEvts 
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Double)
      beginTime <- time
      tid <- forkIO $ 
           playRawEvents stream (beginTime + fromIntegral preStartDelay) rmes
      return $ Just tid

readXmlDoc fn = do
  buf <- readFile fn
  let Document _ _ elem _ = xmlParse fn buf
  return $ parseMusXml elem

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

-- As written the code here can only return a Just value. But
-- this will be used with exception handlers that can return a Nothing
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


dumpPerfDoc dfn cfn = do
  xmlDoc <- readXmlDoc dfn
  let musDoc = fromXmlDoc xmlDoc
  (_,allConfig) <- readConfig cfn
  let perfDoc = FM.toPerfDoc (PLD.susPedParts allConfig) 
                (PLD.timeDiv allConfig) musDoc
      perfDoc2 = finalizePerfDoc Nothing Nothing [] $ perfDoc
  writeFile (replaceExtension cfn "txt") . showItem . showi $ perfDoc2
  putStrLn "Dumped PerfDoc (_WITHOUT_ applying any commands or snips [but with finalizing])"

