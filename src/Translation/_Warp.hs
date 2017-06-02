
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



-- applyWarps
--
--   Given a staff name, apply both staff-only warps on that staff and global
--   warps (on any staff) to the time map.
--
--   
applyWarps :: String -> Map Loc (Map String [Mark]) -> RelTimeMap ->
              Tr RelTimeMap
applyWarps staff marks (RelTimeMap tm) =
  where
    -- global warps anywhere
    globalWarps = findGlobalWarps marks


-- findWarps
--
--  Identify all global and staff warps.
--
-- Result: ( <global warps>, <staff, sorted by staff name> )
findWarps :: Map Int TimeSig -> Map Loc (Map String [Mark]) ->
             ([Warp2], Map String [Warp2])
findWarps timeSigs marksIn = (concat xs, M.fromList ys)
  where
    fl :: Map String (Map Loc [Mark])
    fl = flipMap marksIn
    (xs,ys) = unzip . map doStaff $ M.toList fl
    doStaff :: (String,Map Loc [Mark]) -> ([Warp2],(String,[Warp2]))
    doStaff (name,staffMarks) = _
      where
        ws = S.fromList . map fst . filter (isW . snd) . M.toList $ marks
        -- should we look for w's on same staff or any staff? does it matter
        -- if the warp is global?
        doGlob :: (Loc,[Mark]) -> Maybe Warp2
        doGlob (loc,marks) =
          case [w | w@Warp{} <- marks, isGlobalWarp w] of
            [] -> Nothing
            [Warp leftArr rightArr wid amt _] ->
              

            


{-

findGlobalWarps :: Map Int TimeSig -> Map Loc (Map String [Mark]) -> [Warp2]
findGlobalWarps timeSigs marks = _
  where
    f :: [Mark] -> Maybe [Mark]
    f xs = mapMaybeToJust maybeGlobalWarp xs
    xs :: Map Loc [(String,[Mark])]
    xs = M.map M.toList marks


tupleMaybe :: (b -> Maybe c) -> (a,b) -> Maybe (a,c)
tupleMaybe g (x,y) = case g y of {Nothing -> Nothing; Just z -> Just (x,z)}

  

mapMaybeToJust :: (a -> Maybe b) -> [a] -> Maybe [a]
mapMaybeToJust g xs = case mapMaybe g xs of {[] -> Nothing; ys -> Just ys}


maybeGlobalWarp :: Mark -> Maybe Mark
maybeGlobalWarp w@(Warp _ _ _ _ flag) | flag      = Just w
                                      | otherwise = Nothing
maybeGlobalWarp _ = Nothing


maybeStaffWarp :: Mark -> Maybe Mark
maybeStaffWarp w@(Warp _ _ _ _ flag) | flag      = Nothing
                                     | otherwise = Just w
maybeStaffWarp _ = Nothing

-}




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
oneWarp :: Set Loc -> Map Int TimeSig -> (Loc,Mark) -> RelTimeMap -> RelTimeMap

oneWarp ws timeSigs (loc,Warp leftArrow rightArrow mWidth amt global)
  (RelTimeMap tm) =
  where
    direction | leftArrow /= 0  = fromIntegral leftArrow
              | rightArrow /= 0 = fromIntegral rightArrow
    -- we need to figure out left loc. okay in the syntax for a warp, the
    -- direction is the direction of both arrows if they are present. 
    leftLoc | leftArrow /= 0 = case mWidth of
                Nothing -> 

oneWarp timeSigs (loc,Warp _ leftLoc rightLoc amount direction) (RelTimeMap tm)
  = RelTimeMap . 
    (case leftLoc of 
       Nothing -> id
       Just l  -> warpOneSide timeSigs l   loc (amount*direction)) .
    (case rightLoc of 
       Nothing -> id
       Just l  -> warpOneSide timeSigs loc l   (-amount*direction)) $ 
    tm


-- warpOneSide
--
-- Inputs:
--   Map Int TimeSig
--   Loc :: left Loc
--   Loc :: right Loc
--   Double :: warp amount as number of quarters
--   Map Loc Double :: input time map
--
-- Algorithm: check the number of quarters within warp interval. Call that
-- L. Call the warp amount W. Make sure L+W is greater than zero. Compute
-- R = (L+W)/L and multiply each time map segment by R.
--
warpOneSide :: Map Int TimeSig -> Loc -> Loc -> Double -> 
               Map Loc Double -> Map Loc Double
warpOneSide timeSigs loc1Raw loc2Raw w
  | loc1 == loc2 = throwMine $ printf ("warp end points %s and %s too close"++
                   " together; they rounded to the same slice ") 
                   (showLoc2 loc1) (showLoc2 loc2)
  | l+w <= 0     = throwMine $ printf ("warp delta would result in " ++
                   "negative final interval, at %s and %s") 
                   (showLoc2 loc1) (showLoc2 loc2)
  | otherwise    = M.mapWithKey g
  where
    loc1 = roundToSlice timeSigs loc1Raw
    loc2 = roundToSlice timeSigs loc2Raw
    l = fromRational $ locDiffQuarters timeSigs loc1 loc2
    r = (l+w)/l
    g loc dur | loc < loc1                = dur
              | loc1 <= loc && loc < loc2 = dur*r
              | otherwise                 = dur
    
{-    
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
-}


{-
numSlices :: Map Int TimeSig -> Loc -> Loc -> Int
numSlices timeSigs l1 l2 | x == fromIntegral rx = rx
  where
    diffBeats = locDiff timeSigs l1 l2
    x  = fromIntegral configSlicesPerBeat * diffBeats
    rx = round x
-}


----------------------------------------------------------------------
----------------------------------------------------------------------

applyAbsWarps :: [(Loc,Mark)] -> RelTimeMap -> Tr RelTimeMap
applyAbsWarps absWarps tm = do
  timeSigs <- scTimeSigs `liftM` gets tsScore
  return $ foldr (oneAbsWarp timeSigs) tm absWarps 


oneAbsWarp :: Map Int TimeSig -> (Loc,Mark) -> RelTimeMap -> RelTimeMap
oneAbsWarp timeSigs (loc1In,AbsWarp loc2In dur) (RelTimeMap tm) =
  RelTimeMap $ M.mapWithKey g tm
  where
    loc1Rounded = roundToSlice timeSigs loc1In
    loc2Rounded = roundToSlice timeSigs loc2In
    (loc1,loc2) | loc1Rounded == loc2Rounded = throwMine ("in absolute " ++
                    "warp at %s, warp points are too close together; they "++
                    "rounded to the same slice") (showLoc2 loc1In)
                | loc1Rounded < loc2Rounded  = (loc1Rounded,loc2Rounded)
                | otherwise                  = (loc2Rounded,loc1Rounded)
    origDur = fromRational $ locDiffQuarters timeSigs loc1 loc2
    r = dur/origDur
    g loc dur | loc < loc1                = dur
              | loc1 <= loc && loc < loc2 = dur*r
              | otherwise                 = dur




----------------------------------------------------------------------
----------------------------------------------------------------------
--                pauses

applyPauses :: [(Loc,Mark)] -> RelTimeMap -> Tr RelTimeMap
applyPauses pauses tm = do
  timeSigs <- scTimeSigs `liftM` gets tsScore
  return $ foldr (applyOnePause timeSigs) tm pauses


-- we need to determine how many quarters is at a loc. Look at slice in map
applyOnePause :: Map Int TimeSig -> (Loc,Mark) -> RelTimeMap -> RelTimeMap
applyOnePause timeSigs (locIn@(Loc msrIn _),Pause durIn) (RelTimeMap tm) =
  RelTimeMap $ M.adjust (+pauseDur) actualSliceLoc tm
  where
    numSlicesPerQuarter :: Double
    numSlicesPerQuarter = case M.lookup msrIn timeSigs of
      Just (TimeSig _ denom) -> fromIntegral configSlicesPerBeat 
                                * fromIntegral denom / 4
    pauseDur = case M.lookupLE locIn tm of
      Just (locR,sliceDur) -> case durIn of
        Right psQuarters -> psQuarters * numSlicesPerQuarter * sliceDur
        Left  seconds    -> seconds
    actualSliceLoc =
        case locSub timeSigs locIn (1%fromIntegral configSlicesPerBeat) of
          Nothing -> throwMine $ printf "pause at %s is too close to beginning"
                     (simpleShowLoc locIn)
          Just x  -> case M.lookupLE x tm of
            Just (l,_) -> l
    


