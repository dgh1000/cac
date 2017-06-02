{-# LANGUAGE TupleSections #-}
module Translation.LoudnessFunc where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Common.CommonData as CD
import Control.Monad.State
import Control.Arrow
import Debug.Trace
import Text.Printf
import Data.Maybe
import Data.Map(Map)
import Data.Ratio
import Common.CommonExport
import Common.CommonUtil
import Score.ScoreExport
import Score.ScoreData
import Translation.TimeMap(lookupTime)
import Translation.TranslationData
import Translation.TranslationUtil(hasModifier)
import Util.Exception
import Util.Math(scale)
import Util.Map(splitInclude)



----------------------------------------------------------------------
----------------------------------------------------------------------


makeLoudnessFuncs :: Score -> Map String AbsTimeMap ->
                     (Map String [LoudnessDebug],Map String StaffLoudnessFunc)
makeLoudnessFuncs score atms = (allLDebugs,staffLFs)
  where
    doStaffLF staffName = ((staffName,lf),(staffName,lDebug))
      where
        (lf,lDebug) = makeStaffLoudnessFunc score atms staffName
        -- return ((s,lf),(s,lDebug))
    processLF xs = let (lfs,debugs) = unzip xs
                   in (M.fromList lfs,M.fromList debugs)
    (staffLFs,allLDebugs) = processLF . map doStaffLF . M.keys $ scStaves score
    -- (staffLFs,allLDebugs) <- processLF `liftM`
    --                       mapM doStaffLF (M.keys $ scStaves score)
    -- modify (\s -> s {tsLoudnessDebugs=allLDebugs,tsLoudFuncs=staffLFs})


----------------------------------------------------------------------
----------------------------------------------------------------------


-- makeStaffLoudnessFunc
--
--   construction of loudness functions for staff taking into account
--
--     dynamic and hairpin markings 
--
--     accents
-- 
--     marks, specifically <> and ><
--
-- comment:
--
--   as originally written this function took the number of measures and made
--   the dynamics/hairpin curves only out to that number of measures. however,
--   it made the accent curves over the whole composition. I don't know why
--   the discrepancy.
--
--   so I have preserved that behavior
--
makeStaffLoudnessFunc :: Score -> Map String AbsTimeMap -> String ->
                         (StaffLoudnessFunc,[LoudnessDebug])
makeStaffLoudnessFunc score atms name = (resultF,resultDebug)
  where
    -- config <- gets tsPlaybackConfig
    atm = lfMapLookup name atms
  
    -- [LoudnessCurve] -> LoudnessFunc
    curvesToFunc = foldr1 addLF . map (toLoudFunc atm)

    -- compute dynamics/hairpin curve, loudness func, and debugging information
    -- needs score
    dynHairpinC = makeDynHairpinLC score name
    dynHairpinDebug = LoudnessDebug (name++" dyn/hairpin") dynHairpinC
    -- dynHairpinF = toLoudFunc atm dynHairpinC
  
    -- compute curves for accents, for each voice
    -- accentCMap :: Map Int [LoudnessCurve]
    -- makeAccentLCs: needs score, atm, 
    (accentCMap,accentDebugs) = makeAccentLCs score atms name
    accentFMap :: Map Int LoudnessFunc
    accentFMap = M.map curvesToFunc accentCMap

    -- compute curves for <> and >< Marks
    (dynMarkCs,dynMarkDebugs) = makeDynShapes score name

    -- Create part of LoudnessFunc that is same for all voices
    base = curvesToFunc $ [dynHairpinC] ++ dynMarkCs

    -- Now make each voice
    resultF      = M.map (addLF base) accentFMap
    resultDebug = [dynHairpinDebug] ++ accentDebugs ++ dynMarkDebugs


lfMapLookup :: Ord k => k -> Map k a -> a
lfMapLookup k m = case M.lookup k m of
  Just a -> a


----------------------------------------------------------------------
----------------------------------------------------------------------
--                combinators

addLF :: (a -> Double) -> (a -> Double) -> a -> Double
addLF f g x = f x + g x

preferLeftLF :: (a -> Double) -> (a -> Double) -> a -> Double
preferLeftLF f g x = case f x of
  a | a /= 0    -> a
    | otherwise -> g x 


----------------------------------------------------------------------
----------------------------------------------------------------------
--              convert loudness curve to loudness function


toLoudFunc :: AbsTimeMap -> LoudnessCurve Loc -> Either Loc Double -> Double
toLoudFunc tm curve t = case t of
  Left  loc -> lookupLoudLoc tm curve loc
  Right t   -> lookupLoudDouble tm curveD t
  where
    curveD = loudCurveLToD tm curve


lookupLoudLoc :: AbsTimeMap -> LoudnessCurve Loc -> Loc -> Double
lookupLoudLoc tm curve loc = 
  case M.maxViewWithKey . fst . splitInclude loc $ curve of
    Nothing -> 0
    Just ((k,DynSeg l1 l2 end),_) | loc < end -> scale tBeg t tEnd l1 l2
                                  | otherwise -> 0
      where
        tBeg = lookupTime k   tm
        tEnd = lookupTime end tm
        t    = lookupTime loc tm


lookupLoudDouble :: AbsTimeMap -> LoudnessCurve Double -> Double -> Double
lookupLoudDouble tm curve t =
  case M.maxViewWithKey . fst . splitInclude t $ curve of
    Nothing -> 0
    Just ((k,DynSeg l1 l2 end),_) | t < end   -> scale k t end l1 l2
                                  | otherwise -> 0


loudCurveLToD :: AbsTimeMap -> Map Loc (DynSeg Loc) -> 
                 Map Double (DynSeg Double)
loudCurveLToD tm = M.fromList . map doSeg . M.toList
  where
    doSeg (loc,DynSeg level1 level2 end) = (t,DynSeg level1 level2 tEnd)
      where
        t    = lookupTime loc tm
        tEnd = lookupTime end tm


----------------------------------------------------------------------
----------------------------------------------------------------------
-- makeDynHairpinLF: make dynamics corresponding to Sibelius dynamics and
--                   hairpins

-- is there any harm in making a loudness curve for entire composition when we
-- are only going to play back a few measures? does any code look to the
-- loudness curve to decide where the playback starts and ends?

makeDynHairpinLC :: Score -> String -> LoudnessCurve Loc
makeDynHairpinLC score staffName =
  makeHairpins hairpins . makeSegs maxTrueEnd $ M.mapWithKey toOneDyn dynamics
  where
    staves = scStaves score
    staff = case M.lookup staffName staves of {Just s -> s}
    Staff { stHairpins = hairpins
          , stDynamics = dynamics
          , stTrueEndsMap = trueEnds } = staff
    maxTrueEnd = case M.maxViewWithKey trueEnds of {Just ((k,_),_) -> k}
    toOneDyn :: Loc -> [Dynamic] -> Dynamic
    toOneDyn loc ds = case ds of
      [x] -> x
      _   -> throwMine $ printf "at %s, there are 2 or more dynamic marks"
             (showLoc2 loc)

    
makeSegs :: Loc -> Map Loc Dynamic -> LoudnessCurve Loc
makeSegs maxTrueEnd dyns
  | M.null dyns = throwMine "no dynamics in score"
  | otherwise   = M.fromList . map toSeg $ zip segData (tail segData)
  where
    segData = M.toAscList dyns ++
              [(computeLoudCurveEnd maxTrueEnd dyns, SimpleDyn 0 0)]
    toSeg ((loc1,d1),(loc2,_)) = case d1 of
      SimpleDyn level _ -> 
        (loc1, DynSeg (fromIntegral level) (fromIntegral level) loc2)



-- makeHairpins
--
-- given a LoudnessCurve which contains the Sibelius dynamics, modify it to
-- account for hairpins in the score
makeHairpins :: Map Loc Hairpin -> LoudnessCurve Loc -> LoudnessCurve Loc
makeHairpins hairpins dyns = M.fromList . concatMap g . M.toAscList $ dyns
  where
    g :: (Loc,DynSeg Loc) -> [(Loc,DynSeg Loc)]
    g inp@(loc1,DynSeg level1 level2 loc2) = 
      let nextDynLevel :: Maybe Double
          nextDynLevel = fmap (h . fst) . M.minView . snd . M.split loc1 $ dyns
            where h (DynSeg l1 _ _) = l1
          insideHairpins = M.toList .
            M.filterWithKey   (\locH _ -> loc1 <= locH && locH < loc2) $ 
            hairpins
      in case insideHairpins of
          [] -> [inp]
          [(locH,Hairpin type_ locHEnd)]
            | isNothing nextDynLevel -> throwMine $ printf ("hairpin at %s " ++
                "is not followed by a dynamic") (simpleShowLoc locH)
            | locHEnd > loc2 -> throwMine $ printf ("hairpin at %s crosses " ++
                "past the following dynamic") (simpleShowLoc locH)
            | badHairpinEnd  -> throwMine $ printf ("hairpin at %s has an " ++
                "end loc that isn't close enough to the following dynamic") 
                (simpleShowLoc locH)
            | locH == loc1   ->
                [ checkHPType type_
                  (loc1 , DynSeg level1 (fromJust nextDynLevel) loc2)
                ]
            | otherwise      -> 
                [ (loc1 , DynSeg level1 level2 locH)
                , checkHPType type_
                  (locH , DynSeg level2 (fromJust nextDynLevel) loc2) 
                ]
            where
              badHairpinEnd = msrHEnd < msr2-1
                where Loc msrHEnd _ = locHEnd
                      Loc msr2    _ = loc2
          _ -> throwMine $ printf ("two or more hairpins between dynamics; "++
               "first dynamic is at %s") (simpleShowLoc loc1)


computeLoudCurveEnd :: Loc -> Map Loc Dynamic -> Loc
computeLoudCurveEnd maxTrueEnd m = max (Loc (lastDynMsr+1) 1) maxTrueEnd
  where
    Loc lastDynMsr _ = case M.maxViewWithKey m of
      Just ((k,_),_) -> k


checkHPType type_ inp@(loc1,DynSeg level1 level2 _)
  | type_ == Crescendo  && level1 >= level2 = throwMine $ printf ("at %s, " ++
      "hairin is Crescendo, but next dynamic is not louder") 
      (simpleShowLoc loc1)
  | type_ == Diminuendo && level1 <= level2 = throwMine $ printf ("at %s, " ++
      "hairpin is Diminuendo, but next dynamic is not quieter")
      (simpleShowLoc loc1)
  | otherwise = inp

    
----------------------------------------------------------------------
----------------------------------------------------------------------
--         computing loudness curve expressing patterns


{-

-- makePatternLoudCurve
--
-- A loudness curve is a Map Loc (begin Loc) to segment data DynSeg Loc. Our
-- input here is a map of Loc (begin Loc) to a pair (<dynamics>,<tempo>).
--
makePatternLoudCurve numMeasures (PatternData pats)
  | null segs = throwMine "internal error in Dynamics.hs:patternLoudness"
  | otherwise = M.fromList $ map g segs
  where
    -- We need to add a segment ffrom final 
    locLoudnessPairs :: [(Loc,Double)]
    locLoudnessPairs = map (second fst) (M.toList pats) ++
                       [(Loc (numMeasures+1) 1,0)]
    segs = zip locLoudnessPairs $ drop 1 locLoudnessPairs
    g ((l1,d),(l2,_)) = (l1, DynSeg d d l2)

-}


----------------------------------------------------------------------
----------------------------------------------------------------------


makeDynShapes :: Score -> String -> ([LoudnessCurve Loc],[LoudnessDebug])
makeDynShapes score name =
  unzip . mapMaybe (oneDynShape ts markers) $ M.toAscList marks
  where
    -- look up Marks and variables for this staff
    dbg = concatMap (\n -> " " ++ n) . M.keys $ scMarksByStaff score
    marks   = lfMapLookup name $ scMarksByStaff score
    markers = lfMapLookup name $ scMarkers score
    ts      = scTimeSigs score


oneDynShape :: TimeSigs -> Markers -> (Loc,[MarkD]) ->
               Maybe (LoudnessCurve Loc,LoudnessDebug)
oneDynShape ts (Markers _ cds dcs) (loc,marks) =
  case mapMaybe maybeDynShape marks of
    []  -> Nothing
    [(position,amount)] ->
       case S.lookupGT loc (if amount > 0 then cds else dcs) of
          Nothing -> throwMine $ printf ("no end marker %s found " ++
                     "at %s") (if amount > 0 then "<>" else "><")
                     (showLoc2 loc)
          Just loc2 ->
            let c = oneDynShape' ts loc loc2 position amount
            in  Just ( c
                     , LoudnessDebug (printf "dyn shape %s" (showLoc2 loc)) c)
    _ -> throwMine $ printf ("multiple dynamic shape marks at %s")
         (showLoc2 loc)


oneDynShape' :: TimeSigs -> Loc -> Loc -> Rational -> Double ->
                LoudnessCurve Loc
oneDynShape' ts loc1 loc3 peakRatio delta =
    M.fromList [(loc1,DynSeg 0 delta loc2),(loc2,DynSeg delta 0 loc3)]
  where
    qs   = locDiffQuar ts loc1 loc3
    loc2 = case locAddQuar ts loc1 $ qs*peakRatio of {Just x -> x}


maybeDynShape :: MarkD -> Maybe (Rational,Double)
maybeDynShape (DynShape flag peakPosition delta) =
    Just (approxRational peakPosition 0.01, multiplier*delta)
  where
    multiplier = if flag then 1 else -1
maybeDynShape _ = Nothing


isCDMarker EndCrescDescr = True
isCDMarker _             = False


isDCMarker EndDescrCresc = True
isDCMarker _             = False


----------------------------------------------------------------------
----------------------------------------------------------------------
--                   handling accented notes


makeAccentLCs :: Score -> Map String AbsTimeMap -> String ->
                 (Map Int [LoudnessCurve Loc],[LoudnessDebug])
makeAccentLCs score atms staffName =
    (M.fromList curves, concatMap makeVoiceDebug curves)
  where
    staff = lfMapLookup staffName $ scStaves score
    atm   = lfMapLookup staffName atms
    makeVoice voiceNum = [ mkCurve StrongAccent 2, mkCurve Accent 1
                         , mkCurve UpBow 0.5 , mkCurve DownBow (-0.5)
                         , mkCurve Tenuto (-1) ]
       where
         mkCurve mod accentSize = oneAccentLC (hasModifier mod) voiceNum
                                  accentSize staff 
    curves :: [(Int,[LoudnessCurve Loc])]
    curves = map (id &&& makeVoice) [1..4]
    makeDebug :: Int -> LoudnessCurve Loc -> LoudnessDebug
    makeDebug vn c = LoudnessDebug (printf "%s: voice %d" staffName vn) c
    makeVoiceDebug :: (Int,[LoudnessCurve Loc]) -> [LoudnessDebug]
    makeVoiceDebug (vn,cs) = map (makeDebug vn) cs
  
                       
oneAccentLC :: (ChordKey -> Bool) -> Int -> Double -> Staff ->
               LoudnessCurve Loc
oneAccentLC pred voiceNum accentSize =
    M.fromList . map toSeg . filter ((==voiceNum) . ckVoiceNum) .
    filter pred . getChordKeys_staff
  where
    toSeg chordKey = ( begLocK chordKey
                     , DynSeg accentSize accentSize (endLocK chordKey) )

{-

-- computeChordLoudCur
--
-- Compute a loudness curve that expresses the idea that certain chords
-- will have their loudness adjusted. The predicate function 'pred' is
-- applied to a ChordKey in order to determine which chords to adjust, and
-- 'accentSize' is the amount of the adjustment (can be positive or
-- negative)
computeChordLoudCur :: (ChordKey -> Bool) -> Int -> Double -> Staff ->
                       LoudnessCurve Loc
computeChordLoudCur pred voiceNum accentSize =
  M.fromList . map toSeg . filter ((==voiceNum) . ckVoiceNum) . filter pred .
  getChordKeys_staff
  where
    toSeg chordKey = ( getChordLoc chordKey
                     , DynSeg accentSize accentSize (getChordEnd chordKey)
                     )


-- standardModifiersToLoudFunc
--
-- A function useful to multiple instruments: make a loudness function based
-- on certain modifiers:
--
--   StrongAccent: add 2 levels
--   Accent:       add 1 level
--   UpBow:        add 0.5 level
--   DownBow:      subtract 0.5 level
--   Tenuto:       subtract 1 level
--
-- (Is this useful for quantum leap instruments? yes I think so)
standardModifiersToLoudFunc :: Staff -> AbsTimeMap -> Map Int LoudnessFunc
standardModifiersToLoudFunc staff tm = 
    M.fromList $ map (id &&& makeVoice) [1..4]
  where
    makeVoice voiceNum = mkCurve StrongAccent 2 `addLF`
                         mkCurve Accent       1 `addLF`
                         mkCurve UpBow      0.5 `addLF`
                         mkCurve DownBow (-0.5) `addLF`
                         mkCurve Tenuto    (-1)
      where
        mkCurve mod accentSize = 
          toLoudFunc tm $ computeChordLoudCur (hasModifier mod) voiceNum
                          accentSize staff
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--              utilies useful to multiple instruments


toD :: (Either Loc Double -> Double) -> Double -> Double
toD f x = f (Right x)


lMapLookup :: Ord k => k -> Map k a -> a
lMapLookup k m = case M.lookup k m of {Just x -> x}

