
module Translation.AlterTimes where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf
import Data.Set(Set)
import Data.Map(Map)
import Midi.MidiData
import Util.Exception
import Common.CommonUtil


-- we want to alter times to enforce the following:
--
--   create legato via overlap
--
--     for every MidiEvent for which the overlap flag is set, extend by the
--     configured amount
--
--   minimum duration of any note
--
--     extend any note that is too short
--
--       should we ever move start time earlier? only sensible in case of an
--       arpeggiated note that was decapitated too much. but that is not an
--       important enough case.
-- 
--   minimum separation between notes of same pitch
--
--     two purposes:
--
--       for a note N "extended too much in prior two phases": that is, the
--       new end time of N is past begin time of note N2 in the same channel
--       that has the same pitch
--
--         in this case, truncate to 2 ms before N2 beginning
--
--       for a meta instrument which requires separation between notes of same
--       pitch
--
--     
--
--
-- will we work with MidiEvent, or RawMidiEvent, or shorts? only MidiEvent has
-- "OnOff" structures
--
-- we need to look up any notes that start within a certain range. note ends
-- at t. There is a maximum that t_new can be based on not overlapping with
-- following note of same pitch. there is the maximum that we could extend it
-- which is greater of t_minDur


-- algorithm.


alterTimes :: [MidiEvent] -> [MidiEvent]
alterTimes evts = concat . map alterTimes2 . map filterDest $ S.toList allDests
  where
    filterDest :: (Int,Int) -> [MidiEvent]
    filterDest x = filter ((==x) . meDest) evts
    allDests :: Set (Int,Int)
    allDests = S.fromList $ map meDest evts


alterTimes2 :: [MidiEvent] -> [MidiEvent]
alterTimes2 evts = map (doEvt m) evts
  where
    m = M.map S.fromList . M.fromListWith (++) $ concatMap f evts
    f :: MidiEvent -> [(Int,[Double])]
    f e@NoteEvent{} = [(mePitch e,[onTime $ meOnOff e])]
    f e@TrillTremEvent{} = map (\((p,_),o) -> (p,[onTime o])) $ meList e


doEvt :: Map Int (Set Double) -> MidiEvent -> MidiEvent
doEvt m evt = case evt of
    NoteEvent{} -> evt {meOnOff = one (mePitch evt) (meOnOff evt)}
    TrillTremEvent{} -> evt {meList = map oneTrill $ meList evt}
  where
    oneTrill ((pit,vel),onOff) = ((pit,vel),one pit onOff)
    one pit onOff = doTime m evt {- leg ss -} pit onOff
    leg = meLegato evt
    ss = meSepSame evt
    

    
doTime :: Map Int (Set Double) -> MidiEvent -> Int -> OnOff -> OnOff
doTime m evt pitch onOff = tN
  where
    t1 = onTime onOff
    t2 = offTime onOff
    -- first consider needed truncation by computing these possible new tOff
    -- values:
    --
    --   tTrunc1: must in any circumstance not exceed this value in order to
    --            prevent this note's tOff from equalling or exceeding next
    --            notes's tOn
    --
    --   tTrunc2: must exceed this value by as little as possible: this
    --            provides silence before note of next same pitch, if the
    --            instrument requests it.
    --
    tFol = M.lookup pitch m >>= S.lookupGT t1
    tTrunc1 = tFol >>= (\x -> return $ x-0.002)
    tTrunc2 = do
      t <- tFol
      d <- meSepSame evt
      return $ t-d

    -- now consider extension
    --
    --   tExt1: desire to get as close to this value as possible to provide
    --          legato extension
    --  
    --   tExt2: must be at least this value to satisfy min duration
    tExt1 = meLegato evt >>= (\x -> return $ t2+x)
    tExt2 = Just $ t1+0.04

    -- algorithm could be
    --
    --   extend to tExt1
    --
    --   truncate to tTrunc1
    --
    --   truncate to tTrunc2
    --
    --   extend to tExt2
    --
    --   throw exception if final result is greater than tTrunc1
    --
    tN' = extend "min dur" tExt2 . trunc "same pit sep" tTrunc2 .
          trunc "non stomp" tTrunc1 . extend "legato ext" tExt1 $ onOff
    tN = case tTrunc1 of
      Nothing -> tN'
      Just x | offTime tN' > x ->
                 throwMine $ printf ("AlterTimes.hs error at staff '%s' %s" ++
                 ": altering end times makes " ++
                 "a note end time too close (or past) following note of " ++
                 "same pitch") (fst $ meId evt) (showLoc2 . snd $ meId evt)
             | otherwise -> tN'
               


extend :: String -> Maybe Double -> OnOff -> OnOff
extend _ Nothing x = x
extend s (Just x) o | x < e     = o
                    | otherwise = consTimes s b x o
  where (b,e) = headTimes o


trunc :: String -> Maybe Double -> OnOff -> OnOff
trunc _ Nothing x = x
trunc s (Just y) o | y < e     = consTimes s b y o
                   | otherwise = o
  where (b,e) = headTimes o
    
