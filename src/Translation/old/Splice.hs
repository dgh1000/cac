
module Translation.Splice where

import qualified Data.Map as M
import Debug.Trace
import Text.Printf
import Control.Arrow
import Data.Map(Map)
import Data.Maybe
import Midi.MidiData
import Midi.ShowMidi
import Util.Showable
import Util.Map(listToLMap)





-- cut
--
-- For each event, consider the following cases:
--
--   If it lies entirely inside the cut, remove it.
--
--   If it intersects the begin time of the cut, truncate it.
--
--   If it intersects the end of the cut, decapitate it.
--
--   Otherwise preserve it.
--
-- Definitions: for an event (E1, E2) to "lie within" a range L1, L2 means 
--
--   E1 >= L1 and E2 <= L2
--
-- To intersect L means
--
--   E1 < L and E2 > L
--
cut :: Bool -> Integer -> Integer -> [MidiEvent] -> [MidiEvent]
cut needsHealing t1 t2 evts
 | needsHealing = heal t1 t2 all preCutTrunc postCutTrunc
 | otherwise    = all
 where
  isPreCutTrunc e = (meTimeOff e) == t1 && (meOrigTimeOff e) /= (meTimeOff e)
  isPostCut     e = (meTime e) == t2
  (all,(preCutTrunc,postCutTrunc)) = 
    (id &&& filter isPreCutTrunc &&& filter isPostCut) .
    mapMaybe (cutOne t1 t2) $ evts


{-
cutSingles :: Integer -> Integer -> [MidiEvent] -> [MidiEvent]
cutSingles t1 t2 = filter g
  where
    g SingleEvent {meTime=t} = t < t1 || t >= t2
-}  

cutOne :: Integer -> Integer -> MidiEvent -> Maybe MidiEvent
cutOne t1 t2 e@SingleEvent {} 
  | t1 <= meTime e && meTime e < t2 = Nothing
  | otherwise = Just e
cutOne t1 t2 e@NoteEvent {}
  | e `within` (t1,t2) = Nothing
  | e `intersects` t1  = Just (meSetEnd e t1)
  | e `intersects` t2  = Just (meSetBeg e t2)
  | otherwise          = Just e


intersects :: MidiEvent -> Integer -> Bool
intersects e t = meTime e < t &&  meTimeOff e > t


within :: MidiEvent -> (Integer,Integer) -> Bool
within e (t1,t2) = meTime e >= t1 && meTimeOff e <= t2


-- isNear x y = x > y-0.0005 && x < y+0.0005


heal :: Integer -> Integer -> [MidiEvent] -> [MidiEvent] -> [MidiEvent] ->
        [MidiEvent]
heal t1 t2 evts preCutEvts postCutEvts = 
  mapMaybe (healOne t1 t2 preCutEvts postCutEvts) evts


-- healOne
--
-- For each event X:
--   If the ending of x is at t1 and x its ending has changed
--     If it matches in pitch/stream/chan an event Y in evtsEndCut
--       Extend duration of X by duration of Y, and output
--     else
--       Output X unchanged
--   else If beginning of x is at t2
--     If it matches in pitch/stream/chan an event Y in evtsBegCut
--       Output Nothing
--     else
--       shift left by (t2-t1) and output
--   else if starts past t2
--       shift X left by (t2-t1) and output
--   else
--       output X unchanged
--
healOne :: Integer -> Integer -> [MidiEvent] -> [MidiEvent] -> MidiEvent -> 
           Maybe MidiEvent
healOne t1 t2 evtsBegCut evtsEndCut e
  | meTimeOff e == t1 && meOrigTimeOff e /= meTimeOff e =
      case filter (`matches` e) evtsEndCut of
        []   -> Just e
        e2:_ -> Just $ extendDur e e2
  | meTime e == t2 = 
      case filter (`matches` e) evtsBegCut of
        []   -> Just $ meShift (t1-t2) e
        _    -> Nothing
  | meTime e > t2 = Just $ meShift (t1-t2) e
  | otherwise = Just e
  where
    extendDur e1 e2 = let d2   = meTimeOff e2 - meTime e2
                      in meSetEnd e1 (meTimeOff e1 + d2)


-- matches
--
-- Determine if two MidiEvents are match in pitch, stream, and channel.
matches :: MidiEvent -> MidiEvent -> Bool
matches e1 e2 = 
  bothEq getPitch e1 e2 && bothEq getStream e1 e2 && bothEq getChan e1 e2
  where
    getPitch  = rmeData1  . meOn
    getStream = rmeStream . meOn
    getChan   = rmeChan   . meOn
    bothEq f x y = f x == f y

{-

Splicing pedal.

So what if pedal is down at t1 and up at t2. We probably want to have pedal
go up at splice point. Hmm no I think we probably want pedal up at t2 to tell
the truth. Of course some pedal ups come slightly after the point they are
marked in the score: pedal lift. Looks like 



-}
