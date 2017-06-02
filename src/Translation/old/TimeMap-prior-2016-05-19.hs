
module Translation.TimeMap where

import qualified Data.Map as M
import Debug.Trace
import Text.Printf
import Common.CommonData
import Data.Maybe
import Data.Map(Map)
import Data.List(tails)
import Data.Ratio
import Util.Map
import Translation.TranslationData
import Score.ScoreData
import Util.Exception
import Util.Math
import Common.CommonUtil
import Common.CommonData

{-

need to add some randomness to timing

  timing features

    large scale tempos

      don't want to change these

    warp

      

    pause

    ramps

    rit/accel



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
        r = fromRational $ (b-b1)/(1%fromIntegral configSlicesPerBeat)
    in t1 + (t2-t1)*r


getBegEndTr :: NoteKey -> Tr (Double,Double)
getBegEndTr nk = do
  tm <- getTimeMapTr $ nkStaffName nk
  return (lookupTime (getChordLoc nk) tm, lookupTime (getTrueEnd nk) tm)


-- Given a Duration, compute it in seconds
-- 
-- DurSecs is easy.
--
-- DurBeats means we need to find the duration of a beat at the given
-- Loc. Look up relevant slice and take its duration.
computeDur :: Loc -> RelTimeMap -> Duration -> Double
computeDur _ _ (DurSecs s) = s
computeDur loc (RelTimeMap tm) (DurBeats nBeats) = case M.lookupLE loc tm of
  Just (_,sliceD) ->
    sliceD * (fromRational $ nBeats * fromIntegral configSlicesPerBeat)


----------------------------------------------------------------------
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
computeBaseTimeMap :: Double -> Map Int TimeSig -> Map Loc [Mark] -> RelTimeMap
computeBaseTimeMap ratio timeSigs marks = 
    timeMapFromSliceLocs ratio timeSigs finalEntries
  where
    tempoMarks = reduceToTempoMarks marks
    (TempoFold _ _ finalEntries) = foldl foldT (TempoFold (60%1) M.empty 
                                   M.empty) (M.toAscList tempoMarks)
    sliceLocs = allSliceLocs timeSigs

----------------------------------------------------------------------
----------------------------------------------------------------------
--          time map alteration based on pattern

alterTimeMapByPattern :: PatternData -> RelTimeMap -> RelTimeMap
alterTimeMapByPattern (PatternData pat) (RelTimeMap tm) =
  RelTimeMap (M.mapWithKey g tm)
  where
    g :: Loc -> Double -> Double
    g tmLoc dur = case M.lookupLE tmLoc pat of
      Just (_,(_,tempoAlt)) -> dur/tempoAlt
      Nothing               -> throwMine $ "internal error in " ++
                               "alterTimeMapByPattern"

----------------------------------------------------------------------
----------------------------------------------------------------------
--                 fold tempo marks: data

{-

what has to be changed now that fold doesn't require knowing variable values?

  we could pair prevailing tempo with any tempo-related marks while folding on
  the whole Map Loc [Mark] map

-}








data FData = FDataSetTempo   Rational
           | FDataRampBeg    Rational
           | FDataRampEndBeg Rational Rational
           | FDataRampEnd    Rational  Rational   
                             -- ^ <ramp end> <new prevaling tempo>
           | FDataRitAccel   Rational   
                             -- ^ <prevailing tempo at time of RitAccel>


data TempoFold = TempoFold
  { tfPrevailing :: Rational  -- prevailing tempo going into next tempo mark
                              -- (is not changed at a RampBeg)
  , tfVars     :: Map String Rational
  , tfEntries  :: Map Loc FData
  }


data TempoFold = Tempofold
  { tfPrevailing :: Double
  , }




----------------------------------------------------------------------
----------------------------------------------------------------------
--                  fold tempo marks: fucntions


reduceToTempoMarks :: Map Loc [Mark] -> Map Loc Mark
reduceToTempoMarks = M.mapMaybeWithKey reduceLoc
  where
    reduceLoc :: Loc -> [Mark] -> Maybe Mark
    reduceLoc loc ms = case mapMaybe maybeTempoMark ms of
      []  -> Nothing
      [m] -> Just m
      ms  -> throwMine $ printf "more than one tempo-related mark at %s: %s"
             (simpleShowLoc loc) (concatMap (\m -> show m ++ " ") ms)


{-
maybeTempoMark m@SetTempo   {} = Just m
maybeTempoMark m@SetVar     {} = Just m
maybeTempoMark m@RampBeg    {} = Just m
maybeTempoMark m@RampEnd    {} = Just m
maybeTempoMark m@RampEndBeg {} = Just m
maybeTempoMark RitAccel        = Just RitAccel
maybeTempoMark _               = Nothing
-}


data TempoMark = TmSetTempo Double
               | TmRampBeg NumRatio
               | TmRampEnd NumRatio
               | TmRampEndBeg NumRatio NumRatio
               | TmRitAccel


anythingButSetTempo :: Mark -> Maybe Double -> Maybe TempoMark
anythingButSetTempo (SetTempo _) = Nothing
anythingButSetTempo (RampBeg nr) = Just $ TmRampBeg nr
anythingButSetTempo (RampEnd nr) = Just $ TmRampEnd nr
anythingButSetTempo (RampEndBeg nr1 nr2) = Just $ TmRampEndBeg nr1 nr2
anythingButSetTempo RitAccel = Just TmRitAccel


maybeSetTempo (SetTempo x) = Just x
maybeSetTempo _            = Nothing

{-

  how is tempo folding different now? no need to evaluate or track
  variables. only things that alter tempo are set tempos and ramps, and
  rit/accel. also need to allow set tempos at same location as RampEnds.

-}


-- foldLookupTempo
--
-- Given 'Tempo' data, which can indicate the tempo via
--  - absolute specification
--  - ratio of some current variable setting or of prevailing tempo
-- find the actual value as a rational.
--
foldLookupTempo :: Loc -> Tempo -> TempoFold -> Rational
foldLookupTempo loc (TempoAbs i) _ = fromIntegral i
foldLookupTempo loc (TempoRelative nr s) 
  TempoFold{tfVars=vars, tfPrevailing=prev} = base*multiplier
  where
    base = case s of
      Nothing -> prev
      Just name -> case M.lookup name vars of
        Nothing -> throwMine $ printf "at %s, variable '%s' is not yet defined"
                   (simpleShowLoc loc) name
        Just v  -> v
    multiplier = let NumRatio num den = nr in approxRational (num/den) 0.001

  

foldT :: TempoFold -> (Loc,Mark) -> TempoFold
-- SetVar
foldT f@TempoFold {tfVars=vs} (loc,SetVar s t) = 
  f {tfVars = M.insert s (foldLookupTempo loc t f) vs}

-- SetTempo
foldT f@TempoFold {tfEntries=entries} (loc, SetTempo t) =
  f { tfEntries = M.insert loc (FDataSetTempo tr) entries
    , tfPrevailing = tr }
  where
    tr = foldLookupTempo loc t f

-- RampBeg
foldT f@TempoFold {tfEntries=entries, tfPrevailing=prev} 
      (loc, RampBeg (NumRatio num den)) =
  f { tfEntries = M.insert loc (FDataRampBeg t) entries }
  where
    t = prev * approxRational (num/den) 0.001

-- RampEndBeg
--
--   New: ramp end/begins will have an arrival tempo and a departure tempo. 
foldT f@TempoFold {tfEntries=entries, tfPrevailing=prev}
      (loc, RampEndBeg (NumRatio num1 den1) (NumRatio num2 den2)) =
  f { tfEntries = M.insert loc (FDataRampEndBeg t1 t2) entries }
  where
    t1 = prev * approxRational (num1/den1) 0.001
    t2 = prev * approxRational (num2/den2) 0.001

-- RampEnd
foldT f@TempoFold {tfEntries=entries, tfPrevailing=prev}    
      (loc, RampEnd ratio stay) = 
  f { tfEntries = M.insert loc (FDataRampEnd endT newT) entries
    , tfPrevailing=newT }
  where
    endT = let NumRatio num den = ratio
           in prev * approxRational (num/den) 0.001
    {-
    endT = case ratioOrTempo of
      Left (NumRatio num den) -> prev * approxRational (num/den) 0.001
      Right t -> foldLookupTempo loc t f
    -}
    newT = if stay then endT else prev

-- RitAccel
foldT f@TempoFold {tfEntries=entries, tfPrevailing=prev}
      (loc, RitAccel) =
  f { tfEntries = M.insert loc (FDataRitAccel prev) entries }



----------------------------------------------------------------------
----------------------------------------------------------------------
--          generate list of slices and find tempo for 
--                         each one

timeMapFromSliceLocs :: Double -> Map Int TimeSig -> Map Loc FData ->  
                        RelTimeMap
timeMapFromSliceLocs ratio timeSigs entries = 
    RelTimeMap m2
  where
    m2 :: Map Loc Double
    m2 = M.insert finalLoc 0 m
    m :: Map Loc Double
    m = M.fromList . map g . allSliceLocs $ timeSigs
    finalLoc :: Loc 
    finalLoc = Loc (length timeSigs+1) 1
    g :: Loc -> (Loc,Double)
    g l@(Loc msr _) = (l, (60/tempoDouble)*(4/denomDouble)/(spb*ratio))
      where
        t = tempoFromFData timeSigs l entries
        tempoDouble = fromRational t
        spb = fromIntegral configSlicesPerBeat
        denomDouble = fromIntegral . tsDenom . fromJust . M.lookup msr $ 
                      timeSigs
        -- t is quarters per minute
        -- 1/t is minutes per quarter
        -- 60/t is seconds per quarter
        -- 4/denominator is quarters per beat
        -- 60/t * (4/denominator) is seconds per beat
        -- 60/t * (4/denominator) / spb is seconds per slice


-- tempoFromFData
--
-- Given a Loc, find out the absolute tempo at that Loc. This is used to find
-- the tempo at slice locs.
tempoFromFData :: Map Int TimeSig -> Loc -> Map Loc FData -> Rational
tempoFromFData timeSigs loc entries = case (directTempo,rampData) of
    -- these are the major cases. either a tempo is directly specified, or
    -- there is a ramp situation. if no appropriate prior entry exists, then
    -- we use a default tempo of 60
    (Just t,_)                        -> t
    -- ramp data: don't I need ending tempo? Oh I look it up below in
    -- computeRampTempo.
    (_, Just (locBeg,t,ritAccelFlag)) -> computeRampTempo locBeg t ritAccelFlag
    (Nothing, Nothing)                -> 60
  where
    prior = M.lookupLE loc entries 

    -- directTempo: we look up entry just prior to 'loc' which is the loc we
    -- are computing the current tempo of. these are the cases in which the
    -- prior FData entry indicates the beginning of a flat tempo curve so we
    -- can read the tempo directly off the value in the FData. Note that
    -- the actual Loc of prior entry is irrelevant
    directTempo = case prior of
      Just (_,FDataSetTempo  t) -> Just t
      Just (_,FDataRampEnd _ t) -> Just t
      _                         -> Nothing

    -- rampData: this is the case that prior FData indicates start of a ramp.
    -- we collect the Loc at which the prior entry occurred, the tempo
    -- indicated there, and a flag to indicate if it was a RitAccel. The way
    -- the following tempo is determined is different from RitAccel. Note that
    -- we are looking for Locs LE 'loc' which means any entry *at* 'loc' will
    -- be the one affecting it
    rampData = case prior of
      Just (l,FDataRampBeg      t) -> Just (l,t,False)
      Just (l,FDataRampEndBeg _ t) -> Just (l,t,False)
      Just (l,FDataRitAccel     t) -> Just (l,t,True) 
      _                            -> Nothing
    computeRampTempo locRampBeg tempoBeg ritAccelFlag =
        scale 0 locInBeats locEndInBeats tempoBeg tempoEnd
      where
        locInBeats = locDiff timeSigs locRampBeg loc
        locEndInBeats = locDiff timeSigs locRampBeg locEnd
        -- this is a ramp situation, must find the end of the ramp and the
        -- tempo at the end. two major cases: with rit accel flag and not.
        (locEnd,tempoEnd) 
          -- with ritaccel flag, then the next entry must be a SetTempo or
          -- RampBeg. i.e. we can't be in middle of a rmap
          | ritAccelFlag = case M.lookupGT loc entries of
                             Just (l,FDataSetTempo t) -> (l,t)
                             Just (l,FDataRampBeg t)  -> (l,t)
                             _ -> throwMine $ printf ("rit/accel not "++
                               "followed by SetTempo or RampBeg at %s")
                               (simpleShowLoc loc)
          -- with not in ritaccel, then we must be in the middle of a
          -- ramp. next entry must be RampEndBeg or RampEnd
          | otherwise    = case M.lookupGT loc entries of
                             Just (l,FDataRampEndBeg t _) -> (l,t)
                             Just (l,FDataRampEnd t _)    -> (l,t)
                             _ -> throwMine $ printf ("ramp begin not "++
                               "followed by RampEndBeg or RampEnd at"++
                               " %s") (simpleShowLoc loc)




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
        spb = fromIntegral configSlicesPerBeat
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
    

