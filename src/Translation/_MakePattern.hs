module Translation.MakePattern where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace
import Text.Printf
import Data.Maybe
import Data.Ratio
import Data.Map(Map)
import Data.Set(Set)
import Score.ScoreData
import Common.CommonData
import Common.CommonUtil
import Translation.TranslationData
import Util.Showable
import Util.Exception



{-

what is pattern data?

  pattern data is a list of time segments with associated alterations of
  dynamics and tempo. time segments need not correspond to timemap slice
  boundaries.

what data is needed to compute pattern data?

  we have several patterns of names "foo", "bar", etc. We read score "marks"
  to determine which named pattern applies to which score regions

  we compute pattern data measure-by-measure. for each measure:

  steps to compute pattern for measure M.

    (1) find named pattern for this measure, call it P

    (2) note time sig of M. Call it T.

    (3) Look for submeasure description in P for T, call it S.

    (4) S will specify the number of subdivisions NS. 

    (5) Look for subdivision description in P for NS.

    (6) That gives us everything for computing pattern for M.

what needs to be validated in pattern statement?

  (1) no duplicate sub-measure descriptions (both for same time sig)

  (2) no duplicate number subdivision descriptions (both for same n)

  (3) any number X of subdivisions mentioned in submeasure descr. should exist
      as subdivision

    
do we have access to score marks here? yes.
  

-}



----------------------------------------------------------------------
---------------------------------------------------------------------
--                   entry point

{-

Now that we have changed pattern statements and added pattern marks, can we
keep PatternData the same? yes I think so.

(1) pull out all pattern marks from score marks

(2) iterate over measures. start with measure number and time sig.

  (1) look up relevant pattern, or note there is none

  (2) generate [(Loc,(Double,Double))] for this measure using given pattern

    - if no pattern, just generate [(<first beat>,(1,1))]

    - if pattern with M submeasures and D subdivisions, in measure with time
      sig T

      - look up PatternSubMeas associated with T. Generate list of Locs
        List_SM which subdivides the measure into M parts, and associate this
        with the list of tempo and dynamics. That is
        [(Loc,Loc),(Double,Double))].

      - look up PatternSubDiv associated with the PatternSubMeas associated
        with T. Subdivide each submeasure, apply dyn/tempo list combined with
        that submeasure's dyn/tempo

(3) concat [(Loc,(Double,Double))] for each measure. Convert to Map and put in
PatternData

    

-}


-- Needs to determine a series of submeasure spans, choosing for each one a
-- pattern of dyns and tempo, then for each one divide into sub-submeasures
-- and choose another pattern of 
toPatternData :: Score -> Map String PatternSt -> PatternData
toPatternData score patStatements = 
    PatternData . M.fromList . 
    concatMap (doMeasure patMarks patStatements) . 
    M.toAscList . scTimeSigs $ score
  where
    patMarks :: Map Loc String
    patMarks = M.mapMaybeWithKey g . scMarks $ score
    g :: Loc -> [Mark] -> Maybe String
    g loc ms = case mapMaybe maybePatternMark ms of
      []  -> Nothing
      [s] -> Just s
      _   -> throwMine $ printf ("two pattern marks at %s") (showLoc2 loc)
    

doMeasure :: Map Loc String -> Map String PatternSt -> (Int,TimeSig) -> 
             [(Loc,(Double,Double))]
doMeasure patMarks patStatements (msrNum,timeSig) =
  case M.lookupLE (Loc msrNum 1) patMarks of
    -- In the case no applicable pattern mark is found, use a pattern that
    -- will cause no alteration to dynamics or tempo.
    Nothing    -> [(Loc msrNum 1,(0,1))]
    Just (loc,name) -> case M.lookup name patStatements of
      Nothing -> throwMine $printf("a pattern mark at %s has name '%s', but "++
                 "there is no pattern by that name in the config file.")
                 (showLoc2 loc) name
      Just ps -> doMeasure_help msrNum timeSig ps


doMeasure_help :: Int -> TimeSig -> PatternSt -> [(Loc,(Double,Double))]
doMeasure_help msrNum (TimeSig num den) 
               (PatternSt patName subMeasMap subDivMap) =
  case M.lookup (num,den) subMeasMap of
    Nothing -> throwMine $ printf ("at msr %d, can't find submeasure entry "++
               "of time sig %d/%d in pattern '%s'") msrNum num den patName
    Just (PatternSubMeas _ nSubMeas nSubDiv subMeasNums) ->
      -- now compute [(Loc,Loc)] by dividing measure into nSubMeas. zip with
      -- numbers. pass that on to compute subdivisions
      --
      let durSubMeas :: Rational
          durSubMeas = fromIntegral num % fromIntegral nSubMeas
          beats = [1,1+durSubMeas..fromIntegral num + 1]
          beatPairs = zip beats $ tail beats
          subDivNums = case M.lookup nSubDiv subDivMap of
            Just (PatternSubDiv _ x)  -> x
            Nothing -> throwMine $ printf ("at measure %d, pattern '%s' " ++
                       "specifies %d subdivisions, but there is no " ++
                       "subdivision entry for %d in the pattern statement " ++
                       "in the config file")
                       msrNum patName nSubDiv nSubDiv
      -- call doSubdiv with the number of subdivisions and the established
      -- submeasure pattern, along with the numbers to use for subdivisions.
      in doSubDiv msrNum nSubDiv (zip beatPairs subMeasNums) subDivNums


doSubDiv :: Int -> Int -> [((Rational,Rational),(Double,Double))] -> 
            [(Double,Double)] -> [(Loc,(Double,Double))]
doSubDiv msrNum nSubDiv subMeasIntervals subDivNums = 
  concatMap g subMeasIntervals
  where
    g :: ((Rational,Rational),(Double,Double)) -> [(Loc,(Double,Double))]
    g ((beg,end),(dyn1,tempo1)) = zipWith h subDivisionBeats subDivNums
      where
        subDivLength = (end - beg) / fromIntegral nSubDiv
        subDivisionBeats = [beg, beg+subDivLength .. end-subDivLength]
        h :: Rational -> (Double,Double) -> (Loc,(Double,Double))
        h b (dyn2,tempo2) = (Loc msrNum b,(dyn1+dyn2,tempo1*tempo2))
    


----------------------------------------------------------------------
----------------------------------------------------------------------

{-
lookupTimeSig :: PatternDescr -> Int -> Int -> Int
lookupTimeSig (PatternDescr _ timeSigs) num den = 
  case L.lookup (num,den) timeSigs of
    Nothing -> throwMine $ printf ("failed to find any entry for time"++
               " signature of %d/%d in pattern statement") num den
    Just n  -> n


lookupDynTempo :: PatternDescr -> Int -> [(Double,Double)]
lookupDynTempo (PatternDescr m _) n = case M.lookup n m of
  Just xs -> xs
  Nothing -> throwMine $ printf ("did not find dynamics and tempo entries "++
             "of length %d in pattern statement") n


data Measure = Measure Int [SubMeasure] [Double] [Double]
  -- <measure num> <list of submeasures> <dynamics alteration list>
  -- <time alteration list>


data SubMeasure = SubMeasure Int Loc Loc Double Double
  -- <numerator of measure> <begin loc> <end loc> <dyn alter> <tempo alter>


-- subSubMeasuresOf
--
-- Given a SubMeasure, which means we have a span of locs and an associated
-- dynamic alteration and tempo alteration that come from its place within
-- the measure, divide it further into N sub-sub-measures by examining where
-- notes occur within the submeasure, look up the pattern of length N, and
-- figure out the start Loc of each sub-sub-measure along with its dyn
-- alter. and tempo alter.
subSubMeasuresOf :: PatternDescr -> Set Loc -> SubMeasure -> 
                    [(Loc,(Double,Double))]
subSubMeasuresOf (PatternDescr pats _) begins 
                 (SubMeasure numer loc1 loc2 dyn1 tempo1) =
    zipWith g beats dynsTempos
  where
    -- 'n' is the number of subdivisions of this sub-measure
    n = subSubDivs begins loc1 loc2
    -- 
    dynsTempos = case M.lookup n pats of
      Nothing -> throwMine $ printf ("cannot find in the pattern statement "++
                 "a pattern of length %d, needed to divide the submeasure " ++
                 "starting at loc %s") n (simpleShowLoc loc1)
      Just x  -> x
    -- divide loc1 to loc2 into n parts. Some assumptions: either loc1 and
    -- loc2 are the same measure, or loc2 is beat 1 of the measure following
    -- loc1.
    Loc msr1 beat1 = loc1
    Loc msr2 beat2 = loc2
    beat2Use | msr2 == msr1+1 && beat2 == 1 = 1 + fromIntegral numer
             | msr1 == msr2                 = beat2
    deltaBeat = (beat2Use-beat1)/fromIntegral n
    beats = map (\n -> beat1 + fromIntegral n * deltaBeat) [0..n-1]
    g :: Rational -> (Double,Double) -> (Loc,(Double,Double))
    g beatIn (dyn2,tempo2) = (Loc msr1 beatIn,(dyn1+dyn2,tempo1*tempo2))
    

{-

  we need to come up with locs l1, l2, l3, ... etc. representing places
  within the submeasure


-}


-- subSubDivs
--
-- Compute the number of subdivisions of this submeasure. We look for notes
-- that occur at certain fractions of the way through the measure
subSubDivs :: Set Loc -> Loc -> Loc -> Int
subSubDivs _ _ _ = 4


subMeasuresOfMeas :: PatternDescr -> (Int,TimeSig) -> [SubMeasure]
subMeasuresOfMeas pd (msrNum,TimeSig num den) = zipWith g locPairs dynTempos
  where
    -- lookup dyn/tempo pattern
    dynTempos = case M.lookup nSubMeas (pdDynTempo pd) of
      Nothing -> throwMine $ printf ("could not find dyn/tempo pattern " ++
                 "of length %d while trying to divide measure %d") nSubMeas
                 msrNum
      Just x  -> x
    -- number of submeasures within this measure
    nSubMeas = case L.lookup (num,den) (pdTimeSigs pd) of
      Nothing -> throwMine $ printf ("cannot find entry for time sig of "++
                 "%d/%d (at measure %d) in pattern statement") num den msrNum
      Just n  -> n
    nSubMeasI = fromIntegral nSubMeas
    numerI = fromIntegral num
    -- boundary locs of submeasures
    locs = map (Loc msrNum) 
         [1%1, 1+numerI%nSubMeasI .. 1+numerI%1-numerI%nSubMeasI]
         ++ [Loc (msrNum+1) 1]
    -- paired locs giving begin and end of each submeasure
    locPairs = zip locs $ drop 1 locs
    g (loc1,loc2) (dyn,tempo) = SubMeasure num loc1 loc2 dyn tempo



dividedNoteReport :: Score -> [PatternEntry] -> String
dividedNoteReport score pes = 
  showiToString $ Component "submeasures" True (map g ranges)
  where
    ranges = concatMap (subMeasureSpans pes) . M.toAscList . scTimeSigs $ score
    noteBegins = unionNoteBegins score
    g :: (Loc,Loc) -> ShowItem
    g (loc1,loc2) = Component (printf "%s %s" (showLoc2 loc1) (showLoc2 loc2))
                    True (map h includedLocs) 
      where
        includedLocs = 
          S.toAscList . fst . S.split loc2 . snd . S.split loc1 $ noteBegins
        h :: Loc -> ShowItem
        h l = SingleLine $ showLoc2 l





subMeasureSpans :: [PatternEntry] -> (Int,TimeSig) -> [(Loc,Loc)]
subMeasureSpans pes (msrNum,TimeSig num den) = zip locs $ drop 1 locs
  where
    -- we need to find the measure numerator, that says how many beats. Then
    -- we divide into that many 
    locs    = map (Loc msrNum) [1%1, 1+numI%nDivsI..1+numI%1-numI%nDivsI]
              ++ [Loc (msrNum+1) 1]
    nDivsI  = fromIntegral nDivs
    numI    = fromIntegral num
    nDivs = case [nDivs | PETimeSig n d nDivs <- pes, n==num && d==den] of
      []  -> throwMine $ printf ("in applying pattern statement to measure " ++
             "%d, no pattern entry was found for time sig %d/%d") msrNum num den
      x:_ -> x
   

unionNoteBegins :: Score -> Set Loc
unionNoteBegins = S.unions . map (M.keysSet . stChords) . M.elems . scStaves 

-}

