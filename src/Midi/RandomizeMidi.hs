module Midi.RandomizeMidi where


import System.Random
import System.IO.Unsafe
import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.List as L
import Midi.MidiExport 
import qualified Midi.MidiData as MD
import Util.Map
import Common.CommonExport
import qualified Common.CommonUtil as CU

----------------------------------------------------------------------
----------------------------------------------------------------------
--   randomLegato

{-
-- randomLegato
--
-- Does two things:
-- 
--   - adds random extension of note off times
--   - checks gap between repeated notes, and possibly shortens earlier note
-- 
--  Int :: seed
--  Int :: min amount to extend in milliseconds
--  Int :: max amount to extend in milliseconds
--  Int :: minimium separation of repeated notes in milliseconds: notes will
--         be shorted to maintain this separation as long as they don't get
--         shorter than minDur
--  Int :: minimum duration allowed when shortening a note
--  [MidiEvent] :: events to possibly modify
-- Output:
--  events in which some notes are modified
randomLegato :: Int -> Int -> Int -> Int -> Int -> [MidiEvent] -> [MidiEvent]
randomLegato seed minExtend maxExtend minSep minDur evts = 
  zipWith (computeNewEvt evtMap minSep minDur) rands evts
  where
    rands = randomRs (minExtend,maxExtend) (mkStdGen seed)
    evtMap :: Map MidiTime [MidiEvent]
    evtMap = M.fromListWith (++) . map (\e -> (meTime e, [e])) $ evts

-}

-- computeNewEvt
--
-- First try to extend the note, respecting the minimum separation.
-- 
-- (1) Start by finding the proposed end P after adding 
--     an extension. Find a repeated note up to time P plus minSep. 
--
--   - If a repeated note exists at time R, find the minimum of M=(P, R-minSep).
--   - If no repeated note exists, set M = P.
--
-- Then avoid making the note shorter than minimum duration.
-- 
-- (2) Then check the start time of the note tBeg. Compute maximum
--     newEnd = (tBeg+minDur,M).
--
computeNewEvt :: Map MidiTime [MidiEvent] -> Int -> Int -> Int -> MidiEvent -> 
                 MidiEvent
computeNewEvt _ _ _ _ e@(SingleEvent _ _ _) = e
computeNewEvt evtMap minSep minDur randomExtend 
  evtIn@(NoteEvent timeOn on _ timeOff _ _) =
  evtIn {MD.neTimeOff=newTEnd}
  where
    (RawMidiEvent stream chan _ pitch _) = on
    proposedEnd = timeOff `mtAdd` (MidiTime $ fromIntegral randomExtend)
    searchRangeEnd = proposedEnd `mtAdd` 
                     (MidiTime $ fromIntegral $ minSep + 2)
    -- Compute m, the new ending that respects both the proposed
    -- extension and the minimum separation
    m = case firstNoteBeforeTime evtMap (timeOff `mtSub` MidiTime 1)
             searchRangeEnd pitch stream chan of
      Nothing -> proposedEnd
      Just tNextMatch -> min proposedEnd 
        (tNextMatch `mtSub` (MidiTime $ fromIntegral minSep))
    -- Compute newTEnd, the new ending that respects the minimum note duration
    newTEnd = max m (timeOn `mtAdd` (MidiTime $ fromIntegral minDur))

-- firstNoteBeforeTime
--   If a note is found within the given time range (tBeg,tEnd) in the given
--     pitch, stream, and chan, returns (Just <time of first such note found>).
--   Otherwise returns Nothing.
firstNoteBeforeTime :: Map MidiTime [MidiEvent] -> MidiTime -> MidiTime -> 
                       Int -> Int -> Int -> Maybe MidiTime
firstNoteBeforeTime evtMap tBeg tEnd pitch stream chan = 
  case L.find pred $ takeWhile isWithinRange evts of
    Nothing -> Nothing
    Just (t,_) -> Just t
  where
    evts = lMapToList $ snd $ M.split tBeg evtMap
    pred (_,SingleEvent _ _ _) = False
    pred (_,NoteEvent _ onEvt _ _ _ _ ) = 
      let RawMidiEvent stream2 chan2 _ pitch2 _ = onEvt
      in pitch == pitch2  && chan == chan2  && stream == stream2
         -- Might not want to match channel as in some instruments a note off
         -- if any channel will turn off a sounding note in any other chan
    isWithinRange :: (MidiTime,MidiEvent) -> Bool
    isWithinRange (t,_) = t <= tEnd



----------------------------------------------------------------------
----------------------------------------------------------------------
-- randomVelocity

-- randomVelocity

randomVelocity :: Int -> Map String ConfigValue -> [MidiEvent] -> [MidiEvent]
randomVelocity seed values evts = zipWith g adjustments evts
  where
    delta = CU.configLookupInt "In some 'part' statement" "velDelta" values
    adjustments = randomRs (-delta,delta) (mkStdGen seed)
    g :: Int -> MidiEvent -> MidiEvent
    g _ e@(SingleEvent _ _ _) = e
    g adjustment e@(NoteEvent _ onEvt _ _ _ _) =
      e {MD.neOn=newOnEvt}
      where 
        (RawMidiEvent stream chan status pit vel) = onEvt
        raw = vel + adjustment
        adjVel = min 127 . max 0 $ raw
        newOnEvt = RawMidiEvent stream chan status pit adjVel

----------------------------------------------------------------------
----------------------------------------------------------------------

-- expressiveMidiTiming
--
-- 
-- Adjusts timing to each midi event, mapping to new time using the following
-- function.
--   
--   A be the amplitude of the sine wave used for adjustment
--   P be the initial phase
--   F is frequency of sine wave used for adjustment (I expect values like 
--     0.5 to be used)
--   t is time of midi event
-- 
--   Then, 
--      time_delta = A*sin(2*PI*F*t + P)
-- A, P, and F fall within ranges as follows:
--  A: (0.010, 0.020)
--  P: (0, 2*PI)
--  F: (0.4, 0.6)
-- They are determined pseudrandomly using the given seed. 
-- 
expressiveMidiTiming :: Int -> [MidiEvent] -> [MidiEvent]
expressiveMidiTiming seed = map processEvt
  where
    gen1 = mkStdGen seed
    (ampl,gen2) = randomR (0.009::Double, 0.010) gen1
    (phase,gen3) = randomR (0::Double, 2*pi) gen2
    (freq,_) = randomR (0.4::Double, 0.6) gen3
    processEvt :: MidiEvent -> MidiEvent
    processEvt ne = 
      let t = fromMidiTime . meTime $ ne
          offset = ampl * (sin $ 2*pi*freq*t + phase) 
          newMidiTime = toMidiTime $ t + offset
      in alterMidiTime newMidiTime ne 
    alterMidiTime t e@(SingleEvent timeIn isTrackStart rawEvt) 
      | isTrackStart = e
      | otherwise = SingleEvent t isTrackStart rawEvt
    alterMidiTime t e = e {MD.meTime = t} 


-- new expressive midi timing
-- 
-- simulate what was happening due to thunk evaluation slowness... some
-- notes were delayed. midi events still happened in order. we come
-- along to an event and track last event time. if events can be shifted
-- back to "real time" then do so, otherwise maintain offset. every nth
-- event or every event chosen with certain probability has event time 
-- shifted. note on events that is. there is no delay in control events,
-- how do we get those back to steady state? compress them. can move control
-- event back to within very short distance of prior control event
--
{-

  - initialize current offset to zero
  - initialize variable holding loop state previous event type and time

  - two modes: A. considering whether to introduce delay to note on, or 
     B. cramming in things following a delay
  - initialize mode to A.
  - initialize lastEventTime to None
  - initialize lastModeADelay to None
  - loop: get next midi event
    - if mode A
       - if it's a note on, and 25% chance of random delay
          - compute random delay. set lastModeADelay
          - output note on with new time
          - set mode to B, set lastEventTime
       - output event with same time
    


  - consider next event
     - if it's a control event:
         - if offset is > 0:
            - reduce offset slightly
     - else if a note on:
         - if offset > 0:
            - set offset to minimum that won't move this note before previous
               note on or note off
            - else
               - if (pick true 30% of the time)

-}
