
module Midi.Split where

import qualified Data.List as L
import Common.CommonExport
import Midi.MidiExport
import qualified Midi.MidiData as MD
import Util.Exception

-- extractSegment
-- 
--   Extract midi evts between locBeg and locEnd. This will 
--   include 
--      - NoteEvents which have a source note chordLoc such that
--        locBeg <= chordLoc < locEnd
--      - SingleEvents which have a time t such that 
--          tBeg - controlPreTime <= t <= tEnd
--        with the additional contraint that SingleEvents with seIsTrackStart
--        set to True are excluded,
--        where tBeg and tEnd are the times associated with locBeg and locEnd,
--   
-- Inputs
--   AbsoluteTimeMap
--   Double :: amount of time prior to locBeg, in seconds, to accept
--             control events
--   Loc
--   Loc
--   [MidiEvent]
extractSegment :: Double -> Loc -> Loc -> Double -> Double -> [MidiEvent] -> 
                  [MidiEvent]
extractSegment controlPreTime locBeg locEnd tBeg tEnd evts = filter g evts
  where
    -- filter function
    -- for NoteEvent, checks if loc is within locBeg and locEnd 
    --   (such that locBeg <= loc < locEnd) 
    g :: MidiEvent -> Bool
    g (NoteEvent _ loc _ _ _ _ _ _) = locBeg <= loc && loc < locEnd
    g (SingleEvent t trackStart _) = not trackStart && 
                                     (toMidiTime $ tBeg - controlPreTime)
                                       < t &&
                                     t < toMidiTime tEnd

-- timeShiftSegment 
--   Given a list of MidiEvent, which should not contain any "track start
--   SingleEvents" (an error will be thrown if it does), it will be
--   time shifted to begin at newBegin. The new list and newEnd will be
--   computed, where newEnd is the greatest time of any SingleEvent or
--   greatest timeOff of any NoteEvent, whichever is greater.
--
-- Inputs
--  MidiTime :: new begin time
--  [MidiEvent]
--
-- Output
--  (<new list>, <new end time>)
timeShiftSegment :: MidiTime -> [MidiEvent] -> ([MidiEvent], MidiTime)
timeShiftSegment newBegin evts
  | null evts = throwMine "l92^"
  | any isTrackStart evts = throwMine "nmc38"
  | otherwise = (newEvts, findMaxTime newEvts)
  where
    isTrackStart (NoteEvent _ _ _ _ _ _ _ _) = False
    isTrackStart (SingleEvent _ flag _) = flag
    findBeginTime (NoteEvent t _ _ _ _ _ _ _) = t
    findBeginTime (SingleEvent t _ _) = t
    findEndTime (NoteEvent _ _ _ _ _ tOff _ _) = tOff
    findEndTime (SingleEvent t _ _) = t
    findMinTime es = minimum . map findBeginTime $ es
    findMaxTime es = maximum . map findEndTime $ es
    tMin = findMinTime evts
    newEvts = map (addTime $ newBegin `mtSub` tMin) evts
    addTime delta
      ne@NoteEvent{ MD.meTime=tIn, MD.neTimeOff=tOffIn } = 
      ne { MD.meTime = tIn `mtAdd` delta
         , MD.neTimeOff = tOffIn `mtAdd` delta }
    addTime delta
      se@SingleEvent{ MD.meTime=tIn } =
      se { MD.meTime = tIn `mtAdd` delta }

----------------------------------------------------------------------

type LatestEndInPreviousNotes = Loc
type CurrentBeginLoc = Loc
type CurrentEndLoc = Loc


-- scanForGap
--
--  Loc :: start scanning here
--  Int :: number of measures of gap to look for
--  [MidiEvent] :: a *sorted* list of MidiEvent, an unchecked precondition
--
scanForGap :: Loc -> Int -> [MidiEvent] -> Loc
scanForGap begin nMsrs evts = 
  where
    evts2 = mapEventsToGapData evts
    evts3 = dropWhile (\(_,l) -> l < begin) evts2
    


mapEventsToGapData :: [MidiEvent] -> [( LatestEndInPreviousNotes
                                      , CurrentBeginLoc )]
mapEventsToGapData evts = snd $ L.mapAccumL h (Loc 1 1) evtsData
  where
    evtsData = concatMap g evts
    g :: MidiEvent -> [(CurrentBeginLoc,CurrentEndLoc)]
    g (SingleEvent _ _ _) = []
    g (NoteEvent _ b e _ _ _ _ _) = [(b,e)]
    h :: LatestEndInPreviousNotes -> 
         (CurrentBeginLoc, CurrentEndLoc) -> 
         ( LatestEndInPreviousNotes
         , (LatestEndInPreviousNotes,CurrentBeginLoc))
    h lastEndPrev (currBeg,currEnd) = 
            (max lastEndPrev currEnd, (lastEndPrev, currBeg))
