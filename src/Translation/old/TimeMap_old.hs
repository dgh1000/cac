{-# LANGUAGE TupleSections #-}

{-

We want to modify the time map for each staff so they aren't perfectly in
sync. I'm thinking of putting a sine wave on each part and picking a random
period and amplitude for the sine wave on each playback. This might be easily
computed on an absolute time map.

Map Loc (Rational,Double)


-}

module Translation.TimeMap where

import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Data.Map(Map)
import Data.Maybe
import Data.Ratio
import Data.Monoid
import Common.CommonExport
import Common.CommonUtil(diffInBeats)
import Translation.TranslationData
import Score.ScoreData
import Util.Map(lookupMaxLower)
import Util.Exception
import Util.Math(scale)
import Util.Map(lookupMinUpperWithKey,lMapToList,splitInclude)



-- A time map is a way of representing how Locs can be transformed to a time
-- tag in seconds. It works like this. The compostion has many measures, 
-- each of which has several beats. The beats are divided into 'slicesPerBeat'
-- slices and the beginning Loc of each slice is determined. 
-- 
-- An ***absolute*** time map is then created as follows:
--
-- Map <slice Loc> (<slice duration in beats>, <slice begin time in seconds>)


-------------------------------------------------------------------
-------------------------------------------------------------------
--        creation and modification of "base" time map
--           (the one that is common to all staves)

-- computeBaseTimeMap
--
-- Compute the RelTimeMap that is the starting point for all staves. 
computeBaseTimeMap :: Double -> Tr RelTimeMap
computeBaseTimeMap tempoRatio = do
  score <- gets tsScore
  let absTempos = computeAbsoluteTempos score
  return $ processTempoLines score absTempos $
    computeInitialDeltaTM tempoRatio configSlicesPerBeat 
    (scTimeSigs score) absTempos


-- computeAbsoluteTempos
--
-- The folding function
--
--   data is Maybe (<tempo of prev abs marking>, <prev section tempo>)
--   
-- Algorithm
--   case (AbsTempo i), produce:
--     Just ( <i which is new "previous" abs tempo for future points>
--          , <i which is new section tempo> ) and <same as snd tuple point>
--   case (TempoRatioDouble isRelToPriorAbs r)
--     Just ( <prev abs tempo taken from input data>
--          , if isRelToPriorAbs then <prev abs marking * r>
--                               else <prev section     * r>
--          ) and <same as snd tuple point>
--
--   
computeAbsoluteTempos :: Score -> Map Loc Double
computeAbsoluteTempos score = m2
  -- M.mapAccumWithKey f Nothing . mapMaybe (L.find isTempoMark) . scMarks
  where
    m1 :: Map Loc TempoData
    m1 = M.mapMaybe g . scMarks $ score
    m2 :: Map Loc Double
    m2 = snd . M.mapAccumWithKey f Nothing $ m1
    g :: [Mark] -> Maybe TempoData
    g = getFirst . mconcat . map (First . maybeTempoData)
    maybeTempoData :: Mark -> Maybe TempoData
    maybeTempoData (TempoMark d) = Just d
    maybeTempoData _             = Nothing
    f :: Maybe (Double,Double) -> Loc -> TempoData -> 
         (Maybe (Double,Double),Double)

    f _ _ (AbsTempo i) = let f = fromIntegral i in (Just (f,f), f)

    f (Just (prevAbs,prev)) _ (TempoRatioDouble isRelAbs r) =
          let new = if isRelAbs then prevAbs*r else prev*r
          in (Just (prevAbs, new), new)

    f (Just (prevAbs,prev)) _ (TempoRatioFrac isRelAbs n d) = 
          let r = fromIntegral n / fromIntegral d
              new = if isRelAbs then prevAbs*r else prev*r
          in (Just (prevAbs, new), new)

    f _ loc _ = throwMine $ printf ("encounted a relative tempo mark at %s" ++
                            " but there is no previous absolute tempo") 
                            (simpleShowLoc loc)
              


-- computeInitialDeltaTM
--
-- This is an initial time map that accounts for 
--    (1) metronome markings
--    (2) tempo adjustment ratio
--
-- But not rit, accel, or pauses.
--
-- It's a *delta* time map.
--
computeInitialDeltaTM :: Double -> Int -> Map Int TimeSig -> Map Loc Double -> 
                         RelTimeMap
computeInitialDeltaTM tempoRatio slicesPerBeat timeSigs metMarks = 
  RelTimeMap . M.insert finalLoc (1 % fromIntegral slicesPerBeat,0) . 
    M.fromList . map g $ sliceLocs
  where
    finalLoc = Loc (length timeSigs + 1) 1
    sliceLocs = computeSliceLocs slicesPerBeat timeSigs
    g :: Loc -> (Loc,(Rational,Double))
    g locIn@(Loc msrIn _) = (locIn,(1%fromIntegral slicesPerBeat,durSlice))
       where
         TimeSig _ denom = case M.lookup msrIn timeSigs of {Just t -> t}
         tempo = case M.lookupLE locIn metMarks of
           Nothing -> throwMine $ 
                      printf "Error: no tempo marking prior to loc %s"
                      (simpleShowLoc locIn)
           Just (_,x) -> x
         -- 'tempo' is in quarters per minute, not beats per second. If denom
         -- is 4, then beat duration works out to 60/tempo.  If denom is
         -- greater, then beats are smaller and the 'durBeat' will be
         -- computed to be smaller. And vice-versa.
         durBeat = 4 * 60 / tempo / (fromIntegral denom)
         durSlice = durBeat / fromIntegral slicesPerBeat / tempoRatio


computeSliceLocs :: Int -> Map Int TimeSig -> [Loc]
computeSliceLocs slicesPerBeat timeSigs = concatMap doMsr [1..length timeSigs]
  where
    sliceDuration = 1 % fromIntegral slicesPerBeat
    doMsr :: Int -> [Loc]
    doMsr msrNum = map g [0..numSlices-1]
      where
        TimeSig numer _ = case M.lookup msrNum timeSigs of {Just x -> x}
        numSlices = fromIntegral numer * fromIntegral slicesPerBeat
        g :: Integer -> Loc
        g segN = Loc msrNum (1 + (segN%1) * sliceDuration)



----------------------------------------------------------------------
----------------------------------------------------------------------


-- a map segment spanning s1 to s2 is relevant if
--
--   - if this segment is entirely contained within (l1,l2), l1 < s1 and l2
--     >= s2
--
--   - if upper portion of this segment is contained within the interval
--      (s1 <= l1 < s2)
--
--   we would like to split the map. map key is slice start loc. any slice
--   that starts within between l1 - sliceDur and l2 needs to be included.
--   
--
--   what if timemaps are arrays? lookup would be faster
-- 
-- relTimeMapDelta :: RelTimeMap -> Loc -> Loc -> Double
-- relTimeMapDelta (RelTimeMap tm) l1 l2 =
--  where
    


----------------------------------------------------------------------
----------------------------------------------------------------------
--              tempo line processing


processTempoLines :: Score -> Map Loc Double -> RelTimeMap -> RelTimeMap
processTempoLines Score {scTempoLines = tempoLines, scTimeSigs = timeSigs}
    absTempos (RelTimeMap mapIn)
  = RelTimeMap $ foldr doOneLine mapIn (M.toList tempoLines)
  where
    doOneLine :: (Loc,TempoLine) -> Map Loc (Rational,Double) -> 
                 Map Loc (Rational,Double)
    doOneLine (lineBeg, TempoLine type_ lineEnd) = M.mapWithKey mapSlice
      where
        prevMetMark = case M.lookupLE lineBeg absTempos of
          Nothing -> throwMine $ printf ("No prior metronome mark for tempo"++
                     " line at %s") (simpleShowLoc lineBeg)
          Just (_,t) -> t
        (nextMetMarkLoc,nextMetMark) =
          case M.lookupGE lineEnd absTempos of
            Nothing     -> throwMine $ printf ("No following metronome mark "++
                           "for tempo line at %s") (simpleShowLoc lineEnd)
            Just (loc,x)-> (loc,x)
        lineDur = fromRational $ diffInBeats timeSigs lineBeg lineEnd
        mapSlice  sliceBeg = second (mapSlice' sliceBeg)
        mapSlice' sliceBeg secDur
          | sliceBeg <= lineBeg  || sliceBeg >= nextMetMarkLoc = secDur
          | lineBeg  <= sliceBeg && sliceBeg <= lineEnd        = secDur/ratio
          | lineEnd  <= sliceBeg && sliceBeg <= nextMetMarkLoc =
              secDur*prevMetMark/nextMetMark
          where
            x = fromRational $ diffInBeats timeSigs lineBeg sliceBeg
            ratio = (scale 0 x lineDur prevMetMark nextMetMark)/prevMetMark


----------------------------------------------------------------------
----------------------------------------------------------------------
--              random time command alteration

{-
alterTimeCmds :: Tr ()
alterTimeCmds = do
  sc <- gets tsScore
  ss <- mapM alterStaffTimeCmds $ scStaves sc
  
  modify (\s -> s {tsScore = sc {scStaves = ss}})


alterStaffTimeCmds :: Staff -> Tr Staff
alterStaffTimeCmds st = do
  let cs = lMapToList `liftM` stTimeCmds st
  cs' <- mapM alterTimeCmd cs
  return st {stTimeCmds=listToLMap cs'}


alterTimeCmd :: (Loc,TextCmd) -> Tr (Loc,TextCmd)
alterTimeCmd (loc,Pause d) = do
  r <- trRandomR (0.8,1.2)
  return (loc,Pause (r*d))
alterTimeCmd (loc,Warp s l r d) = do
  r <- trRandomR (0.9,1.1)
  return (loc, Warp s l r (d*r))
alterTimeCmd = return
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--              sine wave time modification
--
-- This stretches and compresses individual segments using a sine wave
-- function to indicate the amountt that a segment should be moved in time.

configMinPeriod = 1.5
configMaxPeriod = 2.5
configMinAmpl   = 0.005
configMaxAmpl   = 0.010


isSineWaveStaff :: String -> Tr Bool
isSineWaveStaff staffName = do
  x <- (fromJust . L.findIndex (==staffName) . L.sort . M.keys . scStaves) 
       `liftM` gets tsScore
  return $ x == 0


sineWaveTime :: AbsTimeMap -> Tr AbsTimeMap
sineWaveTime (AbsTimeMap tm) = do
  period <- trRandomR (configMinPeriod, configMaxPeriod)
  ampl   <- trRandomR (configMinAmpl  , configMaxAmpl  )
  let f t = ampl * sin (2*pi*t/period)
      tr t = t + f t
  return $ AbsTimeMap $ M.map (second tr) tm





----------------------------------------------------------------------
----------------------------------------------------------------------
--                    warp processing


-- Given a location on a beat, we are going to shift the absolute location of
-- that beat forward or backward and compress or stretch every slice in the
-- adjoinging beats. So we need to determine the loc range of the beats that
-- will be compressed and the amount they will be compressed, same for
-- stretching. So this involves determining the number of slices in each
-- beat, which will be NS. The shift amount is D. So each slice has D/NS
-- subtracted from it or added to it. 
--


{-

warps. need to find global and staff warps by looking in marks. need
maybeWarp or something. creating list [(Loc,Mark)] probably lMapToList
then filter (\(_,m) -> isWarpmark m). oh wait isGlobalWarp 
isStaffWarp

distinction between staff and global warps need not be made here

we are going to need to find out a beat duration. could do that by looking up
on the time map. oh we need to look at its current state? I guess so

-}

{-
applyStaffWarps :: String -> RelTimeMap -> Tr RelTimeMap
applyStaffWarps staffName tm = do
  cmds <- (fromJust . M.lookup staffName) `liftM` gets tsTextCmds
  applyWarps
    [(loc,c) | (loc,c@(WarpCmd _ _ _ _)) <- lMapToList cmds] tm


applyGlobalWarps :: RelTimeMap -> Tr RelTimeMap
applyGlobalWarps tm = do
  cmds <- gets tsGlobalTextCmds
  applyWarps 
    [(loc,c) | (loc,c@(WarpCmd _ _ _ _)) <- lMapToList cmds] tm
-}

applyWarps :: [(Loc,Mark)] -> RelTimeMap -> Tr RelTimeMap
applyWarps marks tm = do
  sigs <- scTimeSigs `liftM` gets tsScore
  foldM (applyOneWarp sigs) tm marks


beatDuration :: Loc -> RelTimeMap -> Double
beatDuration loc (RelTimeMap m) = 
  sum . map snd . M.elems . snd . splitInclude lower . fst . M.split upper $ m
  where
    lower = case L.find isIntegralBeat . M.toDescList . fst . 
                 splitInclude loc $ m of
              Just (l,_) -> l
    upper = case L.find isIntegralBeat . M.toAscList  . snd .
                 M.split loc $ m of
              Just (l,_) -> l
    isIntegralBeat (Loc _ b,_) = fromIntegral (round b) == b


applyOneWarp :: Map Int TimeSig -> RelTimeMap -> (Loc,Mark) -> Tr RelTimeMap
applyOneWarp timeSigs tm (mid,WarpMark direction _ left right amt) = do
    maybeWarp left tm >>= maybeWarp right
  where
    delta = (if direction then id else negate) (case amt of
      DeltaDouble d -> d
      DeltaBeat num den -> fromIntegral num * beatDuration mid tm /
                           fromIntegral den)
    maybeWarp :: Maybe Loc -> RelTimeMap -> Tr RelTimeMap
    maybeWarp Nothing = return
    maybeWarp (Just loc)
      | loc < mid = warp1 timeSigs loc mid delta
      | otherwise = warp1 timeSigs mid loc (-delta)


warp1 :: Map Int TimeSig -> Loc -> Loc -> Double -> RelTimeMap -> Tr RelTimeMap
warp1 timeSigs beg end amt tm
  | begR >= endR = throwMine $ printf ("warp beg/end times %s and %s are " ++
                   "too close together or in wrong order") (simpleShowLoc beg) 
                   (simpleShowLoc end)
  | otherwise = warp2 begR endR amt tm
  where
    begR = roundToSlice timeSigs beg
    endR = roundToSlice timeSigs end


roundToSlice :: Map Int TimeSig -> Loc -> Loc
roundToSlice timeSigs (Loc msr beat)
  | 1 <= rounded && rounded < maxBeat = Loc msr rounded
  | rounded == maxBeat = Loc (msr+1) 1
  where
    rounded = round (beat * fromIntegral configSlicesPerBeat) 
              % fromIntegral configSlicesPerBeat
    TimeSig n _ = fromJust . M.lookup msr $ timeSigs
    maxBeat = 1 + fromIntegral n


warp2 :: Loc -> Loc -> Double -> RelTimeMap -> Tr RelTimeMap
warp2 beg end amt rtm@(RelTimeMap tm) = do
      let n = computeNumWarpSlices rtm beg end
      when (n<0) (throwMine $ printf "internal error in warp2")
      let sliceAmt = amt / fromIntegral n
          g :: Loc -> (Rational,Double) -> (Rational,Double)
          g loc (r,d) 
            | loc < beg || loc >= end = (r,d)
            | newDur <= 0 = throwMine $ printf ("in warp from %s to %s, "++
                            "amount is" ++
                            " too much, resulting in negative slice duration")
                            (simpleShowLoc beg) (simpleShowLoc end)
            | otherwise = (r,newDur)
            where newDur = d+sliceAmt
      return $ RelTimeMap $ M.mapWithKey g tm


computeNumWarpSlices :: RelTimeMap -> Loc -> Loc -> Int
computeNumWarpSlices (RelTimeMap tm) begLoc endLoc =
  length . filter (\l -> l >= begLoc && l < endLoc) . M.keys $ tm



----------------------------------------------------------------------
----------------------------------------------------------------------
--                   pause processing



applyPauses :: [(Loc,Mark)] -> RelTimeMap -> RelTimeMap
applyPauses pauses tm = foldl applyOnePause tm pauses


applyOnePause :: RelTimeMap -> (Loc,Mark) -> RelTimeMap
applyOnePause (RelTimeMap tm) (loc,PauseMark delta) =
  RelTimeMap $ M.adjust (second (+delay)) segLoc tm
  where
    segLoc = case M.toDescList . fst . splitInclude loc $ tm of
      _:(k,_):_ -> k
      _         -> throwMine $ printf ("found ps text at %s, too early to"++
                   " add a pause before the beat, probably at first beat "++
                   "of score") (simpleShowLoc loc)
    delay = case delta of
      DeltaDouble d -> d
      DeltaBeat n d -> fromIntegral n * beatDuration loc (RelTimeMap tm)
                       / fromIntegral d


    
----------------------------------------------------------------------
----------------------------------------------------------------------
--                     conversion to absolute

toAbsolute :: RelTimeMap -> AbsTimeMap
toAbsolute (RelTimeMap m) = AbsTimeMap . snd . M.mapAccum g 0 $ m
  where
    g :: Double -> (Rational,Double) -> (Double,(Rational,Double))
    g timeIn (durInBeats,sliceDur) = (timeIn+sliceDur,(durInBeats,timeIn))


