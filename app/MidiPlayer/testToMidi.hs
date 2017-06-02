import qualified Data.List as L
import qualified Data.Map as M
import Data.Bits
import Data.Map(Map)
import Text.Printf
import Text.Parsec
import Control.Exception
import PerfDoc.Finalize(finalizePerfDoc)
import PlaybackPI.ParseConfig_new( parseConfigFile )
import PlaybackPI.PlaybackPIExport_new
import PerfDoc.PerfDocExport
import qualified PerfDoc.PerfDocData as PD
import qualified PerfDoc.FromSibDoc as FS
import qualified SibDoc.FromParsed as FP
import SibDoc.ParseSibDoc( parseSibDoc )
import SibDoc.SibDocData( ParsedSibDoc )
import Midi.MidiDataNew
import qualified MidiPI.MidiPIData as MPID
import MidiPI.MidiPIData( PartwiseFn(..) )
import MidiPI.NewInstruments.QSoloViolin1( qSoloViolin1 )
import Util.Exception
import qualified PlaybackPI.PlaybackPIData_new as PLD
import PlaybackPI.Snip( computeSnipMsrRanges )


data FileDataType = FDText | FDXML

data FileData = FileData 
 { fdType :: FileDataType
 , fdFilename :: String  -- name of source data, such as Sibelius text dump,
                         -- or MusicXML file
 , fdConfig :: String    -- config filename
 }

main = do
  let fileData = FileData FDText "c:/out.txt" "test1.cfg"
      playRanges = [(1, Nothing)]
      mTempoRatio = Nothing
      mTransposition = Nothing


  initialPerfDoc <- computeInitialPerfDoc fileData
  configData <- readConfig (fdConfig fileData)
  let snips = computeSnipMsrRanges playRanges initialPerfDoc blankSepNeed
      pdFinalized =
        finalizePerfDoc mTempoRatio mTransposition snips initialPerfDoc
      blankSepNeed = 2
  if PD.isNullDoc pdFinalized
    then do putStrLn "Null finalized PerfDoc; skipping."
            return ()
    else do
      -- compute midi events
      let midiEvts = toMidiEvents pdFinalized configData
          -- Delay caused by moving absolute start time into the past, in order
          -- to handle midi events with negative start times
          preStartDelay = 200
      rmes <- evaluate $ mkRawEvents MPID.initialEventsDelay midiEvts
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Double)

-- computeInitialPerfDoc
--   Looking at the source data and config files specified in the input
--   FileData, (which might specify music xml or might specify a sibelius
--   text dump), compute the initial PerfDoc
computeInitialPerfDoc :: FileData -> IO PerfDoc
computeInitialPerfDoc (FileData typ source config) = case typ of
  {- FDXML -> do
    xmlDoc <- readXmlDoc source
    let musDoc = fromXmlDoc xmlDoc
    (_,allConfig) <- readConfig config
    return (FM.toPerfDoc (PLD.susPedParts allConfig) (PLD.timeDiv allConfig)
            $ musDoc)
  -}
  FDText -> do
    parsedSibDoc <- readSibDocDump source
    let sibDoc = FP.convert parsedSibDoc
    allConfig <- readConfig config
    return (FS.toPerfDoc [] (PLD.timeDiv allConfig) $
            sibDoc )
  _ -> error "cannot read xml files; had problem installing haxml"

readConfig :: String -> IO AllConfig
readConfig configFileName = do
  configBuffer <- readFile configFileName
  case parse parseConfigFile "" configBuffer of
     Left err -> throwMine $ "Config file parse error: " ++ (show err)
     Right cs -> return cs

readSibDocDump :: String -> IO ParsedSibDoc
readSibDocDump fn = do
  b <- readFile fn
  case parse parseSibDoc "" b of
    Left err -> throwMine $ "Error parsing Sib. dump: " ++ show err
    Right psd -> return psd
   
-- toMidiEvents
--
-- Converts to *unsorted* list of MidiEvent
--
toMidiEvents :: PerfDoc -> AllConfig -> [MidiEvent]
toMidiEvents pd allConfig = 
  case L.lookup (masterFn allConfig) masterFnList of
    Nothing -> throwMine $ printf ("Config said master function was '%s';" ++ 
               " no function in the masterFnList with that name was found")
               (masterFn allConfig)
    Just fn -> fn pd allConfig



-- partWiseConversion 
--   This conversion function proceeds on the following assumptions/basis:
--
--   - the part instrument parameters include the name of a specific part
--      instrument such as "qSoloViolin1"
--   - this 
partWiseConversion :: PerfDoc -> AllConfig -> [MidiEvent]
partWiseConversion 
  perfDoc@PerfDoc { PD.parts = parts }
  allConfig@AllConfig { PLD.piParamsMap = partInstrParamsMap
                      , PLD.midiLayout = midiLayout } = out
  where
    x :: [([MidiDest],[MidiEvent])]
    x = map (partWise_doOnePart partInstrParamsMap midiLayout) (M.toList parts)
    -- Need to verify that event destinations from each part are unique
    allDest = concatMap fst x
    out = if length (L.nub allDest) == length allDest
      then concatMap snd x
      else throwMine $ "some part instruments tried to use the same MIDI " ++
                        "stream/channel destinations"

    
partWise_doOnePart :: Map String PartInstrParams -> [MidiInstr] ->
                      (String,PPart) -> ([MidiDest],[MidiEvent])
partWise_doOnePart
  partInstrParamsMap midiLayout (docPartName,ppart) =
    case M.lookup docPartName partInstrParamsMap of
      Nothing -> throwMine $ printf ("For document's part name %s, there " ++
                 "was no corresponding part name in the config file. ")
                 docPartName
      Just p@(PartInstrParams name _) -> 
        case L.lookup name globalPartWiseFnsList of
          Nothing -> throwMine $ printf ("For document part name %s and " ++
            "MIDI instrument name %s, there is not a registered MIDI instr " ++
            "function.") docPartName name 
          Just fn -> fn ppart p midiLayout
  
-- mkRawEvents
--
-- Given a list of MidiEvent (in which a note-type MidiEvent will be
-- several raw midi events) translate to a list of RawMidiEvent
-- 
-- Will SORT the list.
-- 
-- Will normalize so that events that should be at the start of the
-- track will
-- be played at time 0, and everything else will be normalized to
-- start at 'initialEvtsDelay'. SingleEvent-type midi events have a field
-- which is a flag indicating they should go at the start of the track.
-- NoteEvent-type midi events have no flag like this.
--
-- Also this will shift "note modifiers" into the past 
-- by 'modifierEarlyBy' to allow any 
-- controllers/keyswitches to take effect before it is played.
--
-- Return: sorted list of (time,event) pairs
-- 
mkRawEvents :: Integer -> [MidiEvent] -> [(MidiTime, RawMidiEvent)]
mkRawEvents initialEvtsDelay evts 
  = sortNormalizeEvents initialEvtsDelay raws
  where
    {-
    xMidiEvent (NoteEvent t on off timeOff mods) =
      [(t, on), (timeOff, off)] ++ map g mods
      where
        g e = (t `mtAdd` MidiTime (-modifierEarlyBy), e)
    xMidiEvent (SingleEvent t evt) = [(t, evt)]
    raws = concatMap xMidiEvent evts
    -}
    raws :: [(MidiTime,Bool,RawMidiEvent)] -- (<time>,<supposed to be start
                                           --  of track?>, <event>)
    raws = concatMap xMidiEvent evts
    xMidiEvent (SingleEvent t isTrackStart rme) = [(t,isTrackStart,rme)]
    xMidiEvent (NoteEvent t on off timeOff mods _) =
      [(t,False,on), (timeOff,False,off)] ++ concatMap g mods
      where
        g e 
          | isNoteOn e = 
            [ (t `mtAdd` MidiTime (- MPID.modifierEarlyBy),False,e)
            , (t `mtAdd` MidiTime (- MPID.modifierOffEarlyBy), False, 
                   convertOnToOff e)]
          | otherwise = [(t `mtAdd` MidiTime (- MPID.modifierEarlyBy),False,e)]
          where
            isNoteOn (RawMidiEvent _ _ x _ _) = x .&. 0xf0 == 0x90
            convertOnToOff (RawMidiEvent str ch st dat1 dat2) =
              RawMidiEvent str ch (st .&. 0xef) dat1 dat2

-- sortNormalizeEvents
--   sort and normalize events.
-- Will normalize so that events marked to be at start of track will
-- be played at time 0, and everything else will be normalized to
-- start at 'initialEvtsDelay'
--
--  Inputs
--    MidiTime: initial evts delay in milliseconds
--    
sortNormalizeEvents :: Integer -> [(MidiTime, Bool,RawMidiEvent)] -> 
                       [(MidiTime, RawMidiEvent)]
sortNormalizeEvents initialEvtsDelay evts = final
  where
    (startEvts,regularEvts) = L.partition (\(_,flag,_) -> flag) evts
    sortedRegEvts = L.sortBy (\(t1,_,_) (t2,_,_) -> compare t1 t2) regularEvts
    tMin = case sortedRegEvts of
     [] -> throwMine "in sortNormalizeEvents: no regular midi events found."
     (t,_,_):_ -> t
    tOffset = tMin `mtSub` (MidiTime initialEvtsDelay)
    xRegEvt (t1,_,e) = (t1 `mtSub` tOffset, e)
    xStartEvt (_,_,e) = (MidiTime 0, e)
    final = if tMin `mtSub` tOffset <= MidiTime 1
      then throwMine "fdl9kj432"
      else map xStartEvt startEvts ++ map xRegEvt sortedRegEvts
    
