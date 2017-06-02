
module Translation.AlterTOff where


import qualified Data.List as L
import qualified Data.Map as M
import Data.Function
import System.IO.Unsafe
import Debug.Trace
import Text.Printf
import Control.Arrow
import Control.Monad
import Data.Map(Map)
import Data.Maybe
import Midi.MidiData
import Midi.ShowMidi
import Util.Map
import Util.Showable
import Translation.TranslationData
import Translation.ShowTranslation
import Translation.Lookup


{-

algorithm to modify the off time of midi events for two purposes

  1. truncate notes that are followed by same pitch for instruments that need
     it for clear articulation or to avoid having the midi off event "step
     on" the midi on event of a repeated note

  2. create legato via overlap for instruments that need it (for now, that's
     piano)

for each midi event

  we have a configuration that tells us if notes should be extended or
  overlapped

  for extension by E milliseconds when time off is TOff

    if note is staccato or any shortened articulation, ignore it completely

    filter out notes with TOn in the range TOff to TOff + E (inclusive) that
    are the same pitch: choose the earliest one

    if there is one, call its on time TOn2. Compute the maximum extension
    EMax = TOn2 - 1. extend to TOff + E or EMax, whichever is smaller

    if there is no following note, extend to tOff+E

    

  for trunctation by Tr millisecons, with minimum note duration of DMin

    find notes of same pitch in range TOff to TOff + Tr - 1 inclusive: find
    minimum one and call its start TOn2

    if TOn2 exists, set TOff of note under consideration to maximum of 
    TOn + DMin and TOn2 - Tr


algorithm for new TrExtConfig

note 2016-12-01: trying to figure out what most of this was for. Something to
do with splicing? Would splicing require any alteration of tOff?

we have note X. This is case that we find one or more Y_on following X within
range maximum (tecMinSep, tecSustainedSep, tecExtend, tecLegExtend)

compute maximum possible new X_off, call it X_off_max, by taking minimum of
following

    if no Y fitting criteria, then there is no maximum specified

    Y_on minus tecMinSep (usually 1 millisecond)

    if X and Y are sustained, and tecSustainedSep is Just s, then compute 
      Y_on - s


compute X_off_goal as follows:

    if tecExtend is Just x  then

      X_off_goal = X_off + x

    else 
     
      if X is marked legato and if tecLegExtend is Just x, 

        X_off_goal = X_off + x

      else

        X_off_goal = X_off

Now choose minimum (X_off_max,X_off_goal) = New_T_off

then compute X_minDur = X_on plus tecMinDur. 

final_T_off = maximum (New_T_off, X_minDur)

-}


alterTOff_record :: EventsRecord -> Tr EventsRecord
alterTOff_record (NoteEventsRecord staffName evts) = do
  a <- iAlterTOff `liftM` getInstrTr staffName
  return $ NoteEventsRecord staffName .
    concat . map snd . M.toAscList $ 
    alterTOff a (listToLMap $ map (meTime &&& id) evts)

alterTOff_record e = return e


-- alterTOff
-- 
--   Map a function 'g' over every MidiEvent e, adjusting meTimeOff as
--   necessary. Outline of algorithm is this:
--
-- Calculate eOffMax. (1) Caculate the separation needed between eOff and any
-- following note of the same pitch and channel, when both are sustained, from
-- tecSustainedSep (2) calculate separation between any two notes of same
-- pitch, from tecMinSep. (3) eOff itself if note is marked 'short'.  (4) (eOn
-- + maxRatio* (eOff-eOn)). Take minimum of these four.
--
-- Then calculate eOffGoal, which is based on eOff plus any desired
-- extension. Desired extension comes from (1) legato extension for notes
-- marked 'legato' (2) extension that applies to any note (typical use: piano).
-- (3) simply eOff if no extension applies. Take maximum of these three.
-- 
-- Calculate eOffMin which is minimum possible eOff based on minimum duration
-- possible of e.  
--
-- Calculate tOffFinal by taking minimum of eOffGoal and eOffMax. If that is
-- greater than eMin, set it to eMin.
--
alterTOff :: TruncExtConfig -> Map Integer [MidiEvent] -> 
             Map Integer [MidiEvent]
alterTOff tconf m = M.map (map g) m
  where
    seekRange = computeSeekRange tconf
    g :: MidiEvent -> MidiEvent
    g e = e {meTimeOff = max eOffMin $ min eOffMax eOffGoal }
      where
        eOff = meTimeOff e
        eOn  = meTime    e
        followMatching = seekMatching seekRange e m
        -- 
        -- compute eOffMax
        --
        eOffMax = minimum . catMaybes $ 
          [ if meShort e then Just eOff else Nothing
          , case followMatching of 
              Just (t2,_) -> Just (t2 - tecMinSep tconf)
              Nothing     -> Nothing
          , case followMatching of
              Nothing -> Nothing 
              Just (t2,sus) | sus && meSustained e -> 
                case tecSustainedSep tconf of
                  Nothing -> Nothing
                  Just d  -> Just (t2 - d)
                            | otherwise -> Nothing
          , Just $ eOn + (round $ tecMaxRatio tconf * 
                                    (fromIntegral $ eOff-eOn)) 
          ]
        -- 
        -- compute eOffGoal
        --
        eOffGoal = maximum . catMaybes $ 
          [ case tecExtend tconf of
              Nothing -> Nothing
              Just t  -> Just $ eOff + t
          , case tecLegExtend tconf of
              Nothing -> Nothing
              Just t | meLegatoFlag e -> Just $ eOff + t
                     | otherwise      -> Nothing
          , Just eOff ]
        --
        -- compute eOffMin
        --
        eOffMin = eOn + tecMinDur tconf




-- computeSeekRange
--
-- Determine time range we need to seek for notes following x.
--
-- Assume a note x has off time xOff. We are looking for the first note y
-- following x that matches in pitch and channel and is close enough to x that
-- it might influence out attempt to extend the x off time. 
--
-- The maximum we might extend is
--   maximum . catMaybes $ [tecExtend tconf, tecLegExtend tconf, Just 0]
--
-- The maximum gap we might need after extending is 
--   maximum . catMaybes $ [Just (tecMinSep tconf), tecSustainedSep tconf]
-- 
-- The sum of these is the seek range.
computeSeekRange :: TruncExtConfig -> Integer
computeSeekRange tconf = 
  (maximum $ catMaybes [tecExtend tconf, tecLegExtend tconf, Just 0]) +
  (maximum $ catMaybes [Just $ tecMinSep tconf, tecSustainedSep tconf])


seekMatching :: Integer -> MidiEvent -> Map Integer [MidiEvent] ->
                Maybe (Integer,Bool)
seekMatching range eIn m = case L.sortBy (compare `on` fst) . 
    filter (matches . snd) . lMapToList . fst . M.split upperSplit . snd . 
    M.split (tOff-1) $ m of
      []      -> Nothing
      (t,e):_ -> Just (t, meSustained e)
  where
    tOff = meTimeOff eIn
    upperSplit = tOff + range + 1
    matches NoteEvent {meOn=onEvt} =
          (rmeData1 $ meOn eIn) == rmeData1 onEvt && 
          (rmeChan  $ meOn eIn) == rmeChan  onEvt
      
    


{-
-- alterTOff
--
--  Either (Integer,Integer) Integer :: 
--    Left  (<truncate amt> , <min dur after truncate>)
--    Right (<extend amount>, <max allowed duration as ratio to original
--            duration>)
alterTOff :: Either (Integer,Integer) (Integer,Double) -> 
             Map Integer [MidiEvent] -> Map Integer [MidiEvent]
alterTOff which m = M.map (map g) m
  where
    g :: MidiEvent -> MidiEvent
    g = case which of 
      Left (amt,dMin) -> truncateEvt amt dMin m
      Right(amt,rMax) -> extendEvt amt rMax m


truncateEvt :: Integer -> Integer -> Map Integer [MidiEvent] -> MidiEvent -> 
            MidiEvent
tuncateEvt  _   _      _ e@SingleEvent {} = e
truncateEvt amt minDur m e@NoteEvent {meTime=tOn, meTimeOff=tOff} =
  case getFollowing tOff (tOff+amt-1) (rmeData1 $ meOn e) m of
    Nothing        -> e
    Just tOnFollow -> e {meTimeOff = max (tOn+minDur) (tOnFollow-amt)}


extendEvt _   _    _ e@SingleEvent {} = e
extendEvt amt rMax m e@NoteEvent {meTime=tOn, meTimeOff=tOff, meShortened=sh}
  | sh        = e
  | otherwise = case getFollowing tOff (tOff+amt) (rmeData1 $ meOn e) m of
      Nothing        -> e {meTimeOff = min (tOff+amt) maxRatio}
      Just tOnFollow -> e {meTimeOff = minimum [tOnFollow-1,tOff+amt,maxRatio]}
      where
        maxRatio = tOn + round (rMax * fromIntegral (tOff-tOn))


getFollowing :: Integer -> Integer -> Int -> Map Integer [MidiEvent] -> 
                Maybe Integer
getFollowing tMin tMax pitch =
    listToMaybe . map meTime . filter rightPitch . concat . map snd . 
    takeWhile ((<=tMax) . fst) . M.toAscList . snd . splitInclude tMin
  where
    rightPitch = (==pitch) . rmeData1 . meOn
-}
