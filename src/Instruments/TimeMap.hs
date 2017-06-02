
module Instruments.TimeMap where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad.State
import Data.Either
import Data.Maybe
import Data.Map(Map)
import Data.List(tails)
import Data.Ratio
import Data.Set(Set)
import Debug.Trace
import Text.Printf
import Instruments.InstrumentsData
import Instruments.ApplyTimeMod
import Instruments.ToUnitTimeMods
import Score.ScoreData
import Util.Exception
import Util.Math
import Util.Map
import Common.CommonUtil
import Common.CommonData

{-

analysis of **ORDER** in which to apply unit time mods

  what affects time BUT ISN'T a unit time mod?

    the following marks: tempo, ramp, and rit/accel

    all these are done first

  what marks trigger unit time mods?

    StaffAdjust   UnitWarp (3 loc variety, one staff)

    GlobAdjust    UnitWarp (3 loc variety, all staves)

    RitAccel      UnitRitAccel (for now all staves)

    Warp          UnitWarp (could be 2- or 3-loc, could be one staff or glob)

    AbsWarp       UnitAbsWarp

    Pause         UnitPause

    MultPause     UnitPause (possible multiples)

  what marks can overlap?

    Warps and RitAccel? maybe if warp spans more. in this case I don't know if
    it matters which is first

    multiple RitAccel overlapping? not likely.

    multiple warp overlapping? not likely

    pause inside warp or ritaccel? possible

  answer: do warps first, then other things, order should be consistent in
  ascending loc location


need to add some randomness to timing

  timing features

    large scale tempos

      don't want to change these

    warp

      

    pause

    ramps

    rit/accel


tempo ('set tempo' and ramps) and warps, how do they interact?

  we probably want 'set tempo' to come first

  warps refer to durations of time as number of quarters-- actual time in
  seconds then depends on tempo 

    for absolute warps

      we have absolute warp location L_W1. We have the other side of the warp,
      L_W2. And we have warp duration D (in quarters)

      We can look at the numer of quarters apart L_W1 and L_W2 are. We take
      the ratio abs(L_W1-L_W2)/D and multiply each slide duration by that
      ratio.

    for relative warps

      we have warp location L_W1 and other side L_W2. We have warp amt in
      quarters, A_Q, a signed amount. We compute D = abs(L_W1-L_W2), then
      compute ratio (D+A_Q)/D and multiply each slice duration by that ratio


timing variation


-}

----------------------------------------------------------------------


----------------------------------------------------------------------
--                     time lookup


lookupTime :: Loc -> AbsTimeMap -> Double
lookupTime loc (AbsTimeMap tm) = case M.lookup loc tm of
  Just t -> t
  Nothing -> 
    let (loc1,t1) = case M.lookupLT loc tm of {Just x  -> x}
        (   _,t2) = case M.lookupGT loc tm of {Just x  -> x}
        Loc _ b  = loc
        Loc _ b1 = loc1
        r = fromRational $ (b-b1)/(1%fromIntegral slicesPerBeat)
    in t1 + (t2-t1)*r


-- Given a Duration, compute it in seconds
-- 
-- DurSecs is easy.
--
-- DurBeats means we need to find the duration of a beat at the given
-- Loc. Look up relevant slice and take its duration.
computeDurFromBeats :: Loc -> RelTimeMap -> Double -> Double
computeDurFromBeats loc (RelTimeMap tm) nBeats = case M.lookupLE loc tm of
  Just (_,sliceD) -> sliceD*nBeats*fromIntegral slicesPerBeat


-- First round beats by round (n*x) / n. Then check if we are at beat 1 of
-- next measure.
roundToSlice :: Map Int TimeSig -> Loc -> Loc
roundToSlice timeSigs (Loc msr beat) = Loc newMsr newBeat
  where
    numer = fromIntegral . tsNumer . fromJust . M.lookup msr $ timeSigs
    n = fromIntegral slicesPerBeat
    b :: Rational
    b = fromIntegral (round $ n*beat) / n
    (newBeat,newMsr) | b <  numer+1 = (b, msr  )
                     | b == numer+1 = (1, msr+1)
  

tmMapLookup :: Ord k => k -> Map k a -> a
tmMapLookup k m = case M.lookup k m of {Just a -> a}

----------------------------------------------------------------------
----------------------------------------------------------------------
--               redoing time maps, December 2016

----------------------------------------------------------------------
--           derive need time modifiers (UnitTimeMod) from score


----------------------------------------------------------------------
--              top level TimeMap construction


-- computeBaseTimeMap
--
-- Computes "base" time map. 
--
-- The base time map is computed based on the following kinds of tempo marks:
--
--   set tempo marks (T=...)
--
--   current values of any variables
--
--   ramp beginnings, ramp ends, and ramp end/begins
--
--   rit. or accel. marks
--
-- What is not added at this time:
--
--   time signature patterns
--
--   warps
--
--   pauses
--
computeBaseTimeMap :: Score -> TimingVariation -> Double ->
                      [UnitTimeMod] -> RelTimeMap
computeBaseTimeMap score tVar ratio mods = t1
    {- addTimingVariation score timeSigs tVar t1  -}
  where
    -- from Tr, uses tsScore, tsConfigFile
    timeSigs = scTimeSigs score
    -- Map Loc (Map String [Mark])  
    marks = scMarks score
    -- timing variation config
    tempoDataMap = computeTempoDataMap marks
    -- sliceLocs = allSliceLocs timeSigs
    t1 = applyGlobTimeMods timeSigs mods $
         timeMapFromSliceLocs ratio timeSigs tempoDataMap

{-
computeBaseTimeMap :: Double -> Map Int TimeSig -> Map Loc [Mark] -> RelTimeMap
computeBaseTimeMap ratio timeSigs marks = 
     -- tempo 
    timeMapFromSliceLocs ratio timeSigs tempoDataMap
  where
    tempoDataMap = computeTempoDataMap marks
    sliceLocs = allSliceLocs timeSigs
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
-- applyGlobTimeMods
--
--   we need to apply time mods in this order (not sure what makes the most
--   sense, other than pauses occur before ramps because they might be part of
--   a multi-pause
--
--     UnitRamp,  UnitPause, UnitWarp, UnitAbsWarp
--
applyGlobTimeMods :: TimeSigs -> [UnitTimeMod] -> RelTimeMap -> RelTimeMap
applyGlobTimeMods ts mods = g isPause . g isTwoModify . g isAdjust .
  g isAbsWarp . g isWarpGlob
  where
    g pred m = foldl (applyTimeMod ts) m (filter pred mods)



----------------------------------------------------------------------

addTimingVariation :: Score -> Map Int TimeSig -> TimingVariation ->
                      RelTimeMap -> RelTimeMap
addTimingVariation score timeSigs tv tmIn = applyVariationMap variationMap tmIn
  where
    locs = S.map (roundToSlice timeSigs) . S.unions . 
           map (M.keysSet . stChords) . M.elems . scStaves $ score
    -- Map Loc Double
    variationMap = computeVariationMap timeSigs tv locs


applyVariationMap :: Map Loc Double -> RelTimeMap -> RelTimeMap
applyVariationMap varMap (RelTimeMap tm) = RelTimeMap $ M.mapWithKey g tm
  where
    g :: Loc -> Double -> Double
    g loc delta = case M.lookupLE loc varMap of
      Just (_,x) -> x*delta
      Nothing    -> delta


-- computeVariationMap
--
-- Given a set of all Locs at which notes start, make Map Loc Double which
-- indicates the ratio that time map slices should be multiplied by.
--
computeVariationMap :: Map Int TimeSig -> TimingVariation -> Set Loc -> 
                       (Map Loc Double)
computeVariationMap timeSigs tv@(TimingVariation minN maxN _ _ _ _) locsIn=out
  where
    subLengthsSource = cycle [4,5]
    -- subLengthsSource <- trRandomRs (minN,maxN)
    -- ds :: [(Double,Double)]: for each interval, this gives (<ratio>,<max
    -- change in beats>) we need to change this into pure ratio, which requires
    -- knowing start and end Loc
    locPairs = case S.toAscList locsIn of
      ls | length ls > 1  -> zip ls (tail ls)
         | otherwise      -> throwMine $
                             ("for TimeMap.hs:computeVariationMap to work,"++
                              " there must be at least two distinct " ++
                              "times at which notes start")
    ds :: [(Double,Double)] -- for each Loc pair, this gives maxDelta and
                            -- ratio
    ds = concatMap (oneVariation tv) subLengthsSource
    -- return $ M.fromList $ zipWith (variationChange timeSigs) ds locPairs
    out = M.fromList $ zipWith (variationChange timeSigs) ds locPairs

oneVariation ::TimingVariation -> Int -> [(Double,Double)]
oneVariation (TimingVariation _ _ ratio1 ratio2 delta1 delta2) n = 
    map g [0..n-1]
  where
    g i = (r,d)
      where
        r = scale 0 (fromIntegral i) (fromIntegral $ n-1) ratio1 ratio2
        d = scale 0 (fromIntegral i) (fromIntegral $ n-1) delta1 delta2


-- variationChange 
--
--  Given a 'maxDelta' and 'ratio' expressing the parameters for creating
--  variation in the time segment (from loc1 to loc2), compute that computed
--  variation (as a ratio to multiply each segment by) and return it tupled
--  with loc1.
--
--  maxDelta: expressed in beats
--  ratio:    a ratio
--  
variationChange :: Map Int TimeSig -> (Double,Double) -> (Loc,Loc) -> 
                   (Loc,Double)
variationChange timeSigs (ratio,maxDelta) (loc1,loc2)
  | outputRatio > 0 = (loc1,outputRatio)
  where
    intervalDur :: Double
    intervalDur = fromRational $ locDiffQuar timeSigs loc1 loc2
    -- change in intervalBeats when applying ratio
    dR = intervalDur * (ratio-1)
    outputRatio 
      | abs dR < abs maxDelta = ratio
      | otherwise             = (intervalDur+maxDelta)/ intervalDur


showVarMap :: Map Loc Double -> String
showVarMap = concatMap g . M.toAscList
  where
    g :: (Loc,Double) -> String
    g (loc,d) = printf "%s: %8.4f\n" (showLoc2 loc) d


----------------------------------------------------------------------
----------------------------------------------------------------------
--          time map alteration based on pattern

{-

        PATTERN STUFF REMOVED FOR NOW


alterTimeMapByPattern :: PatternData -> RelTimeMap -> RelTimeMap
alterTimeMapByPattern (PatternData pat) (RelTimeMap tm) =
  RelTimeMap (M.mapWithKey g tm)
  where
    g :: Loc -> Double -> Double
    g tmLoc dur = case M.lookupLE tmLoc pat of
      Just (_,(_,tempoAlt)) -> dur/tempoAlt
      Nothing               -> throwMine $ "internal error in " ++
                               "alterTimeMapByPattern"
-}


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 fold tempo marks: data

{-

what has to be changed now that fold doesn't require knowing variable values?

  we could pair prevailing tempo with any tempo-related marks while folding on
  the whole Map Loc [Mark] map

-}


data TempoData = TempoData 
  { tdTempoModifier   :: Maybe TempoModifier
  , tdPrevailingTempo :: Double
  , tdIsSetTempoHere  :: Bool
  }


data TempoModifier = TmRampBeg Double
                   | TmRampEnd Double
                   | TmRampEndBeg Double Double
                     -- ^ <ratio that is end of prior segment>
                     --   <ratio that is beginining of following segment>
                   | TmRitAccel
                     

timeMapFromSliceLocs :: Double -> Map Int TimeSig -> Map Loc TempoData ->
                        RelTimeMap
timeMapFromSliceLocs ratio timeSigs tempoDataMap = RelTimeMap m2
  where
    m2 :: Map Loc Double
    m2 = M.insert finalLoc 0 m
    m :: Map Loc Double
    m = M.fromList . map g . allSliceLocs $ timeSigs
    finalLoc :: Loc 
    finalLoc = Loc (length timeSigs+1) 1
    g :: Loc -> (Loc,Double)
    g loc@(Loc msr _) = (loc, (60/t)*(4/denom)/(spb*ratio))
      where
        t = tempoAtLoc timeSigs loc tempoDataMap
        spb = fromIntegral slicesPerBeat
        denom = fromIntegral . tsDenom . fromJust . M.lookup msr $ 
                timeSigs
        -- t is quarters per minute
        -- 1/t is minutes per quarter
        -- 60/t is seconds per quarter
        -- 4/denominator is quarters per beat
        -- 60/t * (4/denominator) is seconds per beat
        -- 60/t * (4/denominator) / spb is seconds per slice


computeTempoDataMap :: Map Loc (Map String [MarkD]) -> Map Loc TempoData
computeTempoDataMap marks = M.fromList . catMaybes . snd $ ys
  where
    xs :: [(Loc,[MarkD])]
    xs = M.toAscList $ M.map (concat . M.elems) marks
    ys :: (Double,[Maybe (Loc,TempoData)])
    ys = L.mapAccumL step 60 xs
    step :: Double -> (Loc,[MarkD]) -> (Double,Maybe (Loc,TempoData))
    step prevailing (loc,marks) = (newTempo,output)
      where
        -- output is 
        --
        --   in the case no set tempo or tempo modifier is at this
        --   location: Nothing
        --
        --   in the case there is a set tempo only and tempo mod is Nothing
        --
        --     Just (loc,TempoData Nothing newTempo True)
        --
        --   in the case there is a tempo modifier and no set tempo
        --
        --     Just (loc,TempoData tempoMod newTempo  False)
        --
        --   in the case there is both tempo mod and set tempo
        --
        --     Just (loc,TempoData tempoMod newTempo True)
        --
        --   Or to distill this down, for latter three cases:
        --
        --     Just (loc,TempoData tempoMod newTempo setTempoFlag
        output
          | isNothing tempoMod && not setTempoFlag = Nothing
          | otherwise = Just (loc,TempoData tempoMod newTempo setTempoFlag)
        tempoMod = listToMaybe $ mapMaybe maybeTempoModMark marks
        (setTempoFlag,newTempo) = 
          case listToMaybe $ mapMaybe maybeSetTempo marks of
            Nothing -> (False, prevailing)
            Just t  -> (True , t)


maybeTempoModMark :: MarkD -> Maybe TempoModifier
maybeTempoModMark (RampBeg r)        = Just $ TmRampBeg r
maybeTempoModMark (RampEnd r)        = Just $ TmRampEnd r
maybeTempoModMark (RampEndBeg r1 r2) = Just $ TmRampEndBeg r1 r2
maybeTempoModMark RitAccel           = Just TmRitAccel
maybeTempoModMark _                  = Nothing


maybeSetTempo :: MarkD -> Maybe Double
maybeSetTempo (SetTempo nv) = Just nv
maybeSetTempo _             = Nothing


{-
computeNumVar :: Loc -> Map Loc (Map String Double) -> NumVar -> Double
computeNumVar _ _ (NumVar x Nothing) = x
computeNumVar loc vars (NumVar x (Just name)) = case M.lookupLE loc vars of
  Nothing     -> errMsg
  Just (_,vs) -> case M.lookup name vs of
    Nothing    -> errMsg
    Just value -> x*value
  where
    errMsg = throwMine $ printf "at %s, varaible '%s' is undefined"
             (showLoc2 loc) name
-}

-- tempoAtLoc
--
--   Before calling this, you must compute Map Loc TempoData which is a
--   distillation of tempo-related marks occurring in Map Loc [Mark].
--
--   This function computes the tempo at a specific location.
tempoAtLoc :: Map Int TimeSig -> Loc -> Map Loc TempoData -> Double
tempoAtLoc timeSigs locIn tDataMap = result
  where
    msg1 loc = throwMine $ 
               printf ("at %s, a ramp beginning that is not followed " ++
               "by a tempo mark of an ending-ramp type, or not " ++
               "followed by any tempo mark at all") (showLoc2 loc)
    msg2 loc = throwMine $
               printf ("at %s, a rit or accel is not followed by a tempo "++
               "mark of some type other than a set tempo, or is not " ++
               "followed by any tempo mark") (showLoc2 loc)

    priorTData = M.lookupLE locIn tDataMap

    followTData = M.lookupGT locIn tDataMap

    prevailingTempo = case priorTData of
      Nothing                  -> 60
      Just (_,TempoData _ x _) -> x

    result = case priorTData of
      Nothing -> prevailingTempo
      Just (loc1,tData) -> case tdTempoModifier tData of
        Nothing                  -> tdPrevailingTempo tData
        Just (TmRampEnd _)       -> tdPrevailingTempo tData
        Just (TmRampBeg r1)      -> doRamp loc1 tData r1
        Just (TmRampEndBeg _ r1) -> doRamp loc1 tData r1
        Just TmRitAccel          -> doRitAccel loc1

    doRamp :: Loc -> TempoData -> Double -> Double
    doRamp loc1 tData r1 = case followTData of
      Nothing -> msg1 loc1
      -- modifer will be Nothing if what follows is a SetTempo Mark.
      Just (loc2,endTData) -> case tdTempoModifier endTData of
        Nothing                  -> msg1 loc1
        Just (TmRampBeg  _)      -> throwMine $ msg1 loc1
        Just (TmRampEnd r2)      -> doRamp_help loc1 loc2 r1 r2
        Just (TmRampEndBeg r2 _) -> doRamp_help loc1 loc2 r1 r2
        Just TmRitAccel          -> msg1 loc1
    
    doRamp_help :: Loc -> Loc -> Double -> Double -> Double
    doRamp_help loc1 loc2 r1 r2 = 
        scale 0  diff1 diff2 tempo1 tempo2
      where
        diff1 = fromRational $ locDiff timeSigs loc1  locIn
        diff2 = fromRational $ locDiff timeSigs loc1  loc2
        tempo1 = prevailingTempo*r1
        tempo2 = prevailingTempo*r2
        
    doRitAccel :: Loc -> Double
    doRitAccel loc1 = scale 0 diff1 diff2 prevailingTempo tempo2
      where
        (loc2,tempo2) = case followTData of
          Nothing -> msg2 loc1
          Just (locX,endTData)
            | tdIsSetTempoHere endTData -> (locX,tdPrevailingTempo endTData)
            | otherwise                 -> msg2 loc1
        diff1 = fromRational $ locDiff timeSigs loc1  locIn
        diff2 = fromRational $ locDiff timeSigs loc1  loc2


-- allSliceLocs
--
-- Generate a list of all slice locs by systematically going through every
-- beat of every measure. If there are N measures, and the last beat of the
-- last measure is B, then the final Loc generated here will be 
--
-- (Loc N (B + 1 - 1%spb)) 
-- 
-- where spb is slices per beat. Note that actual time map 
allSliceLocs :: Map Int TimeSig -> [Loc]
allSliceLocs timeSigs = concatMap doMsr [1..length timeSigs]
  where
    doMsr :: Int -> [Loc]
    doMsr n = concatMap doBeat [1..num]
      where
        spb = fromIntegral slicesPerBeat
        num = tsNumer . fromJust . M.lookup n $ timeSigs
        doBeat :: Int -> [Loc]
        doBeat b = 
          map 
          (\s -> Loc n (fromIntegral b + s % spb))
          [0.. spb-1]

----------------------------------------------------------------------
----------------------------------------------------------------------
--                     conversion to absolute

toAbsolute :: RelTimeMap -> AbsTimeMap
toAbsolute (RelTimeMap m) = AbsTimeMap . snd . M.mapAccum g 0 $ m
  where
    g :: Double -> Double -> (Double,Double)
    g timeIn sliceDur = (timeIn+sliceDur,timeIn)
    

----------------------------------------------------------------------

timeToLoc :: Double -> TimeSigs -> Map Double Loc -> Loc
timeToLoc d timeSigs m = case locAddQuar timeSigs l1 dQuar of {Just x -> x}
  where
    (d1,l1) = case M.lookupLE d m of {Just a -> a}
    (d2,l2) = case M.lookupGT d m of {Just a -> a}
    qs = fromRational $ locDiffQuar timeSigs l1 l2
    dQuar = approxRational (scale d1 d d2 0 qs) 0.125
    
      
