

import qualified Data.List as L
import Data.List( sortBy, foldl' )
import qualified Midi.MidiData as MiD
import Data.Maybe
import Debug.Trace
import Data.Map(Map)
import qualified Data.Map as M
import Text.Printf
import Text.Parsec
import Control.Exception
import System
import System.Directory
import System.IO
import System.FilePath
import MusDoc.MusDocData( Part(..) 
                        , DirectionWords(..) 
                        , MusDoc )
import qualified SibDoc.FromParsed as FP
import SibDoc.ParseSibDoc( parseSibDoc )
import SibDoc.SibDocData( ParsedSibDoc )
import qualified MusDoc.FromSibDoc as MFS
import PerfDoc.Prepare
import Util.Exception
import qualified PerfDoc.PerfDocData as PD
import qualified PerfDoc.FromMusDoc as PF
import MusDoc.Processing
import Midi.MidiData( MidiEvent(..)
                    , RawMidiEvent(..)
                    , RawMidiEventType(..) 
                    , MidiTime(..) 
                    , mtAdd, mtSub )
import PerfDoc.PerfDocData( PerfDoc(..)
                          , AbsoluteTimeMap(..) )
import Playback.ParseConfig( parseConfigFile )
import Playback.ParseCommandLine( parseCmdLine )
import Playback.PlaybackData( AllConfig(..) 
                            , Command(..) 
                            , InstrMap (..)
                            , InpCmd(..)
                            , MsrRange(..) )
import qualified Playback.PlaybackData as PLD
import Midi.Instruments.InstrumentsData( configXSampleShearEarly )
import Midi.Lookup
import qualified PerfDoc.BasicTiming as BT
import Playback.Execute


sortNormalizeEvents :: [(MidiTime, RawMidiEvent)] -> [(MidiTime, RawMidiEvent)]
sortNormalizeEvents evts = resort
  where
    sorted = sortBy (\(t1,_) (t2,_) -> compare t1 t2) evts
    finded = L.find (\(t,_) -> t /= MiD.tMeansSartOfTrack) $  sorted
    tMin =  case finded of
      Nothing -> throwMine "87ki1"
      Just (t,_) -> t 
    tOffset = tMin `mtSub` MiD.initialEventsDelay
    xTime (t,e) = if t == MiD.tMeansSartOfTrack
                  then (MidiTime 0, e)
                  else (t `mtSub` tOffset, e)
    xed = map xTime sorted
    resort = sortBy (\(t1,_) (t2,_) -> compare t1 t2) xed
    

applyCommands :: [Command] -> PerfDoc -> PerfDoc
applyCommands cs pd = foldl step pd cs
  where
    atm = BT.deltaToAbsolute (PD.timeMap pd)
    -- mdw = mergeDirectionWords (map PD.partData . M.elems . PD.pParts $ pd)
    step :: PerfDoc -> Command -> PerfDoc
    step pd' com = execute pd' com atm

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

computeSnipMsrRanges :: [MsrRange] -> MusDoc -> Int -> [(Int,Int)]
computeSnipMsrRanges playRanges doc nblanks = snipHoles
  where
    n = numMsrs doc
    explicitMsrRanges = map computeExplicitRange playRanges
    computeExplicitRange :: MsrRange -> (Int,Int)
    computeExplicitRange (i,Nothing) = (i,firstNEmptyMeasures doc nblanks i-1)
    computeExplicitRange (i, Just j) = (i,j)
    snipHoles = computeSnipHoles explicitMsrRanges n


mkRawEvents :: [MidiEvent] -> [(MidiTime, RawMidiEvent)]
mkRawEvents evts = sortNormalizeEvents raws
  where
    xMidiEvent (NoteEvent t on off timeOff mods) =
      [(t, on), (timeOff, off)] ++ map g mods
      where
        g e = (t `mtAdd` MidiTime (-6), e)
    xMidiEvent (SingleEvent t evt) = [(t, evt)]
    raws = concatMap xMidiEvent evts


runPlay :: String -> String -> [MsrRange] -> 
           Maybe Float -> Maybe Int -> IO ()
runPlay dfn cfn playRanges mTempoRatio mTransposition = do
  parsedSibDoc <- readSibDocDump dfn
  let sibDoc = FP.convert parsedSibDoc
  (commands,allConfig) <- readConfig cfn
  let musDoc = MFS.fromSibDoc sibDoc
      perfDoc = PF.toPerfDoc musDoc (PLD.timeDiv allConfig)
      perfDoc2 = applyCommands commands perfDoc 
      blankSepNeed = PLD.blankMsrs allConfig 
      snips = computeSnipMsrRanges playRanges musDoc blankSepNeed
      pdPrepared = preparePerfDoc configXSampleShearEarly 
                   mTempoRatio mTransposition snips perfDoc2
                   
  if PD.isNullDoc pdPrepared
    then do putStrLn "Null prepared document; skipping."
            return ()
    else do
      let midiEvts = toMidiEvents pdPrepared (PLD.midiInstrMap allConfig)
         -- Delay caused by moving absolute start time into the past, in order
         -- to handle midi events with negative start times
          preStartDelay = 200
      rmes <- evaluate $ mkRawEvents midiEvts 
      let l = length rmes
          bits = drop (l - 100) rmes
          showTime (MidiTime t,_) = show t
          (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Float)
      -- mapM_ (putStrLn . showTime) bits
      writeFile "tmp.txt" $  show . sumMidiEvtNumbers $ rmes


sumMidiEvtNumbers :: [(MidiTime,RawMidiEvent)] -> Integer
sumMidiEvtNumbers rmes = foldl' step 0 rmes
  where 
    step :: Integer -> (MidiTime, RawMidiEvent) -> Integer
    step accum ((MidiTime t), (RawMidiEvent f1 f2 f3 f4)) = 
       accum + t + fromIntegral f1 + fromIntegral f2  + fromIntegral f3  
                 + fromIntegral f4
 
      {-
      beginTime <- time
      tid <- forkIO $ 
         playRawEvents stream (beginTime + fromIntegral preStartDelay) rmes
      return $ Just tid
      -}

readSibDocDump :: String -> IO ParsedSibDoc
readSibDocDump fn = do
  b <- readFile fn
  case parse parseSibDoc "" b of
    Left err -> throwMine $ "Error parsing Sib. dump: " ++ show err
    Right psd -> return psd

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
  


main = do
  cwd <- getCurrentDirectory
  let dfn = "g:/out.txt"
      cfn = joinPath [cwd, "current.cfg"]
  runPlay dfn cfn [(1, Nothing)] Nothing Nothing
