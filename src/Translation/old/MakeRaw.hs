
module Translation.MakeRaw where

import qualified Data.List as L
import qualified Data.Map as M
import System.IO.Unsafe
import Data.Bits
import Data.Function
import Data.Maybe
import Data.Map(Map)
import Translation.TranslationData
import Translation.ShowTranslation
import Midi.MidiData
import Util.Exception
import Util.Showable

{-

make raw events: 

  change each EventsRecord into a sorted list [(Integer,RawMidiEvent)]




-}

-- mkRawEvents
--
-- Inputs:
--  normalMin ::  
mkRawEvents :: Integer -> [EventsRecord] -> [(Integer,RawMidiEvent)]
mkRawEvents normalMin records = 
    mergeLists (-1) . map (erToRaw $ normalMin-currMin) $ records
  where
    currMin = case mapMaybe findERMinTime records of
      [] -> throwMine "no events found in MakeRaw.hs:mkRawEvents"
      xs -> unsafePerformIO $ 
              do { {- writeRecords records; -} return $ minimum xs}


writeRecords :: [EventsRecord] -> IO ()
writeRecords ers = do
  let ss = unlines $ map showIString ers
  writeFile "records.txt" ss


mergeLists :: Ord k => k -> [[(k,a)]] -> [(k,a)]
mergeLists expectedMin ls
  | null fs = []
  | otherwise = 
      let m = let x = minimum . map fst $ fs
              in if x <= expectedMin then throwMine "error in mergeLists"
                                     else x
          nexts = concatMap (takeWhile ((==m) . fst)) ls
          remains = map (dropWhile ((==m) . fst)) ls
      in nexts ++ mergeLists expectedMin remains
  where
    fs = concatMap (take 1) $ ls


findERMinTime :: EventsRecord -> Maybe Integer
findERMinTime (FixedEventsRecord _ _) = Nothing
findERMinTime r = case erEvts r of
  [] -> Nothing
  xs -> Just . minimum . map meTime $ xs


-- Convert "record" of MidiEvents into a list of (<time>,<raw events>) pairs,
-- in which the final raw events are sorted. This assumes that the MidiEvents
-- in the input record are sorted, and that all records have only single
-- events except for NoteEventsRecord which has only NoteEvents.
--
erToRaw :: Integer -> EventsRecord -> [(Integer,RawMidiEvent)]
erToRaw _     (FixedEventsRecord  _ evts) = map toRawSingleTrackStart evts
erToRaw delta (ModEventsRecord    _ evts) = map (toRawSingle delta)   evts
erToRaw delta (SusPedEventsRecord _ evts) = map (toRawSingle delta)   evts
erToRaw delta (NoteEventsRecord   _ evts) = 
  L.sortBy (compare `on` fst) . concatMap (toRawNote delta) $ evts


toRawSingleTrackStart (SingleEvent _ True raw _) = (0,raw)


toRawSingle delta (SingleEvent t False raw _) = (t+delta,raw)


-- toRawNote
--
-- delta :: adjust all times by this amount
--
-- Handling legato in Quantum Leap: a signal to turn on legato must be sent
-- after the beginning of this note but before its end in order to ensure that
-- it receives a legato connection to the following note. So this is a
-- different kind of modifier. c
--
toRawNote :: Integer -> MidiEvent -> [(Integer,RawMidiEvent)]
toRawNote delta (NoteEvent tOn tOff eOn eOff mods _ _ _ _ _ _) =
  [(tOn+delta,eOn),(tOff+delta,eOff)] ++ concatMap (doOneMod (tOn+delta)) mods


doOneMod :: Integer -> RawMidiEvent -> [(Integer,RawMidiEvent)]
doOneMod t e | isNoteOn e = [ (t-configModifierEarlyBy,e)
                            , (t-configModifierOffBy, convertOnToOff e) ]
             | otherwise  = [ (t-configModifierEarlyBy,e) ]


convertOnToOff (RawMidiEvent str ch st dat1 dat2) =
  RawMidiEvent str ch (st .&. 0xef) dat1 dat2


isNoteOn (RawMidiEvent _ _ x _ _) = x .&. 0xf0 == 0x90
