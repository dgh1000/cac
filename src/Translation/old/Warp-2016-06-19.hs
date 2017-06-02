
module Translation.Warp where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Debug.Trace
import Text.Printf
import Data.Set(Set)
import Data.Map(Map)
import Data.Maybe
import Data.Ratio
import Score.ScoreData
import Common.CommonData
import Common.CommonUtil
import Translation.TimeMap
import Translation.TranslationData
import Util.Exception
import Util.Map

{-

warp processing

  - general processing required

    - compute a loc +- number of beats

    - compute duration when given as number of beats

      - which beat shall be chosen? Probably beat right at warp location

    - we have three warp locations L_1, L, and L_2. for each interval (L_1,L)
      and (L,L_2) we need to choose which segments to operate on (and count
      them). The locs may not be at exact segment boundaries, but we need two
      conditions:

        - if two locs are are 1/N beats apart, where N is the slices per loc,
          then one slice will be chosen

          - if they are rounded in the same direction, then they will
            be. because beats are represented as rational, we know they will
            be. 

          - how do I round a loc to a slice? do we assume we can compute all
            slice locations?

          - wait: for a pause expressed as a warp, it must be slice before the
            one in which the target note appears: okay, have to handle pause
            as its own case

        - there will be no overlap in slices chosen for the left and right
          intervals

          - if L is rounded in the same direction and forms the end boundary
            of one and the start boundary of the next

  - determine start and end locs

  


-}


-- We must find the w location for a given warp. merged warps have no
-- indication of where w is. 
--
-- the list [(<loc>,<warp>)] must contain only warps, no other kind of mark
applyWarps :: [(Loc,Mark)] -> RelTimeMap -> Tr RelTimeMap
applyWarps warps tm = do
  timeSigs <- scTimeSigs `liftM` gets tsScore
  return $ foldr (oneWarp timeSigs) tm warps



-- oneWarp
--
--   - compute amount of warp in seconds by calling computeDur on
--     'amount', then multiplying by direction. Call that D.
-- 
--   - for each side
--     - construct interval: if from locs L1 to L2, 
--       - first round L1 and L2 to slice boundares: L1_r, L2_r
-- 
--     - determine # of slices by calling diffInBeats on L1_r, L2_r and
--       multiplying by configSlicesPerBeat: call this N
--
--     - determine amount to adjust each slice. for left side, it's D/N. For
--       right side, the additive inverse of that.
--
--     - call M.map to modify slices. throw exception when a slice adjustment
--       would result in <=0 slice duration
oneWarp :: Map Int TimeSig -> (Loc,Mark) -> RelTimeMap -> RelTimeMap
oneWarp timeSigs (loc,Warp _ leftLoc rightLoc amount direction) (RelTimeMap tm)
  = RelTimeMap . 
    (case leftLoc of 
       Nothing -> id
       Just l  -> warpOneSide timeSigs l   loc   amtSecs) .
    (case rightLoc of 
       Nothing -> id
       Just l  -> warpOneSide timeSigs loc l   (-amtSecs)) $ tm
  where
    amtSecs = direction * computeDur loc (RelTimeMap tm) (DurBeats amount)


warpOneSide :: Map Int TimeSig -> Loc -> Loc -> Double -> 
               Map Loc Double -> Map Loc Double
warpOneSide timeSigs l1 l2 delta
  | r1 == r2  = throwMine $ printf ("warp points too close together at"++
                " %s") (showLoc2 l1)
  | otherwise = M.mapWithKey g
  where
    r1 = roundToSlice timeSigs l1
    r2 = roundToSlice timeSigs l2
    ns = fromIntegral (numSlices timeSigs r1 r2)
    d  = delta / ns
    g loc sliceDur | loc < r1 = sliceDur
                   | r1 <= loc && loc < r2 
                                    = if sliceDur + d <= 0
                                      then throwMine $ printf ("warp from " ++
                                           "%s to %s would require negative "++
                                           "slice duration") (showLoc2 r1)
                                           (showLoc2 r2)
                                      else sliceDur+d
                   | otherwise = sliceDur


-- First round beats by round (n*x) / n. Then check if we are at beat 1 of
-- next measure.
roundToSlice :: Map Int TimeSig -> Loc -> Loc
roundToSlice timeSigs (Loc msr beat) = 
  let x = Loc newMsr newBeat
  in if b == beat then x
                  else "changed" `trace` x
  where
    numer = fromIntegral . tsNumer . fromJust . M.lookup msr $ timeSigs
    n = fromIntegral configSlicesPerBeat
    b :: Rational
    b = fromIntegral (round $ n*beat) / n
    (newBeat,newMsr) | b <  numer+1 = (b, msr  )
                     | b == numer+1 = (1, msr+1)
  

numSlices :: Map Int TimeSig -> Loc -> Loc -> Int
numSlices timeSigs l1 l2 | x == fromIntegral rx = rx
  where
    diffBeats = locDiff timeSigs l1 l2
    x  = fromIntegral configSlicesPerBeat * diffBeats
    rx = round x

----------------------------------------------------------------------
----------------------------------------------------------------------
--                pauses

applyPauses :: [(Loc,Mark)] -> RelTimeMap -> Tr RelTimeMap
applyPauses pauses tm = do
  timeSigs <- scTimeSigs `liftM` gets tsScore
  return $ foldl (applyOnePause timeSigs) tm pauses


applyOnePause :: Map Int TimeSig -> RelTimeMap -> (Loc,Mark) -> RelTimeMap
applyOnePause timeSigs (RelTimeMap tm) (loc,Pause dur) =
  RelTimeMap $ M.adjust (+durSecs) segLoc tm
  where
    loc2 = case locSub timeSigs loc (1%fromIntegral configSlicesPerBeat) of
      Nothing -> throwMine $ printf "pause at %s is too close to beginning"
                 (simpleShowLoc loc)
      Just x  -> x
    segLoc = fst . fromJust . M.lookupLE loc2 $ tm
    durSecs = computeDur loc (RelTimeMap tm) dur


