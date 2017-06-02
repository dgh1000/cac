
module Translation.Tempo where

import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Data.Ratio
import Data.Maybe
import Data.List(tails)
import Score.ScoreData
import Translation.TranslationData
import Translation.TimeMap
import Common.CommonData
import Common.CommonUtil
import Util.Math(scale)
import Util.Exception
import Util.Map

xxxx

data TempoFold = TempoFold
  { tfVars     ::   Map String Rational
  , tfTempos   :: Map Loc Rational
  , tfRampEnds :: Map Loc Rational
  }


data RampData = RampData Loc Rational Loc Rational  
  -- ^ <beg loc> <beg tempo> <end loc> <end tempo>


----------------------------------------------------------------------
----------------------------------------------------------------------

--
--
--
initialTimeMap :: Double -> Score -> RelTimeMap
initialTimeMap ratio score = 
    RelTimeMap . 
    M.insert (Loc (length timeSigs + 1) 1) 0 .
    applyRamps timeSigs ramps . M.fromList . map toSlice $ locs
  where
    (tempos,ramps) = temposRamps . reduceToTempoMarks . scMarks $ score
    timeSigs = scTimeSigs score
    s = fromIntegral configSlicesPerBeat 
    msrLocs :: (Int,TimeSig) -> [(Loc,Int)]
    msrLocs (msrNum,TimeSig numer denom) = 
      take (numer*configSlicesPerBeat) $ 
        map (\b -> (Loc msrNum b, denom)) [1,1+1%s..]
    locs :: [(Loc,Int)]
    locs = concatMap msrLocs . M.toAscList $ timeSigs
    toSlice :: (Loc,Int) -> (Loc,Double)
    toSlice (l@(Loc msr _),denom) = (l,dur)
      where
        tempo = case M.lookupLE l tempos of
          Nothing -> throwMine $ 
                     printf "Error: no tempo marking prior to loc %s"
                     (simpleShowLoc l)
          Just (_,t)  -> t
        durBeat = 4 * 60 / (fromRational tempo) / (fromIntegral denom)
        dur  = durBeat / fromIntegral configSlicesPerBeat / ratio


reduceToTempoMarks :: Map Loc [Mark] -> Map Loc Mark
reduceToTempoMarks = M.mapMaybeWithKey reduceLoc
  where
    reduceLoc :: Loc -> [Mark] -> Maybe Mark
    reduceLoc loc ms = case mapMaybe maybeTempoMark ms of
      []  -> Nothing
      [m] -> Just m
      _   -> throwMine $ printf "more than one tempo-related mark at %s"
             (simpleShowLoc loc)


maybeTempoMark m@SetTempo {} = Just m
maybeTempoMark m@SetVar   {} = Just m
maybeTempoMark m@RampBeg  {} = Just m
maybeTempoMark m@RampEnd  {} = Just m
maybeTempoMark RitAccel      = Just RitAccel
maybeTempoMark WMark         = Just WMark
maybeTempoMark _             = Nothing


temposRamps :: Map Loc Mark -> (Map Loc Rational,[RampData])
temposRamps ms = (tempos,computeRamps tempos ends ms)
  where
    (tempos,ends) = initialTempos ms



----------------------------------------------------------------------
----------------------------------------------------------------------
--        initial tempos (ignoring ramps at this point)


initialTempos :: Map Loc Mark -> (Map Loc Rational,Map Loc Rational)
initialTempos ms = (tempos,ends)
  where 
    TempoFold _ tempos ends = 
      foldl foldTempo (TempoFold M.empty M.empty M.empty) . M.toAscList $ ms


--
-- We fold, ignoring ramps and non-tempo marks
--
foldTempo :: TempoFold -> (Loc,Mark) -> TempoFold
-- SetTempo
foldTempo (TempoFold vars tempos ends) (loc,SetTempo t) = 
    TempoFold (M.insert "prev" tr vars) (M.insert loc tr tempos) ends
  where
    tr = lookupTempo loc t vars
foldTempo (TempoFold vars tempos end) (loc,SetVar s t) =
    TempoFold (M.insert s tr vars) tempos end
  where
    tr = lookupTempo loc t vars
foldTempo (TempoFold vars tempos ends) (loc,RampBeg t)
  | not (okayForRampBeg t) = 
      throwMine $ printf ("ramp beg at %s must have tempo relative to " ++
      "prevailing tempo, not a variable or absolute") (simpleShowLoc loc)
  | otherwise = TempoFold vars (M.insert loc tr tempos) ends
  where
    tr = lookupTempo loc t vars
foldTempo (TempoFold vars tempos ends) (loc,RampEnd t flag) =
    if flag 
      then TempoFold vars tempos newEnds
      else TempoFold (M.insert "prev" tr vars) (M.insert loc tr tempos) newEnds
  where
    tr = lookupTempo loc t vars
    newEnds = M.insert loc tr ends
foldTempo (TempoFold vars tempos ends) (loc,RitAccel) = 
  case M.lookup "prev" vars of
    Nothing -> throwMine $ printf ("rit or accel mark occurs where there "++
               "is no prevailing tempo defined, at %s") (simpleShowLoc loc)
    Just t  -> TempoFold vars (M.insert loc t tempos) ends
foldTempo f _ = f  


okayForRampBeg :: Tempo -> Bool
okayForRampBeg (TempoAbs _)               = False
okayForRampBeg (TempoRelative _ (Just _)) = False
okayForRampBeg _                          = True



lookupTempo :: Loc -> Tempo -> Map String Rational -> Rational
lookupTempo loc (TempoAbs i) _ = fromIntegral i
lookupTempo loc (TempoRelative nr s) vars = base*multiplier
  where
    base = case s of
      Nothing -> case M.lookup "prev" vars of
        Nothing -> throwMine $ printf ("in relative tempo at %s, no prior "++
                   "prevailing tempo") (simpleShowLoc loc)
        Just t  -> t
      Just name -> case M.lookup name vars of
        Nothing -> throwMine $ printf "at %s, variable '%s' is undefined"
                   (simpleShowLoc loc) name
        Just v  -> v
    multiplier = case nr of 
      Just (NumRatio num den) -> approxRational (num/den) 0.001
      Nothing                 -> 1
  


----------------------------------------------------------------------
----------------------------------------------------------------------
--             computing ramp data and processing ramps


computeRamps :: Map Loc Rational -> Map Loc Rational -> Map Loc Mark -> 
                [RampData]
computeRamps tempos ends marks = 
    mapMaybe g . tails . filter isRelated . M.toAscList $ marks
  where
    isRelated (_, SetTempo  _)  = True
    isRelated (_, RampBeg   _)  = True
    isRelated (_, RampEnd _ _)  = True
    isRelated (_, RitAccel   )  = True
    isRelated _                 = False
    g :: [(Loc,Mark)] -> Maybe RampData
    g ((loc1,RitAccel):remain) = case remain of
      (loc2,SetTempo _):_ -> 
         Just $ RampData loc1 (fromJust . M.lookup loc1 $ tempos)
                         loc2 (fromJust . M.lookup loc2 $ tempos)
      _ -> throwMine $ printf ("rit or accel mark not immediately followed"++
           " by tempo set mark, at %s") (simpleShowLoc loc1)
    g ((loc1,RampBeg _):remain) = case remain of
      (loc2,RampEnd _ _):_ -> 
         Just $ RampData loc1 (fromJust . M.lookup loc1 $ tempos)
                         loc2 (fromJust . M.lookup loc2 $ ends  )
      _ -> throwMine $ printf ("ramp beg not immediately followed by ramp "++
           "end at %s") (simpleShowLoc loc1)
    g _ = Nothing    


applyRamps :: Map Int TimeSig -> [RampData] -> Map Loc Double -> Map Loc Double
applyRamps timeSigs ramps m = foldr (applyRamp timeSigs) m ramps


--  at beginning we multiply segments by 1, and at end by t2/t1.
--  
-- 
applyRamp :: Map Int TimeSig -> RampData -> Map Loc Double -> Map Loc Double
applyRamp timeSigs (RampData loc1 t1 loc2 t2) tm = mMapRange g loc1 loc2 tm
  where
    rampLength = fromRational $ locDiff timeSigs loc1 loc2
    finalRatio = fromRational $ t1/t2
    g :: Loc -> Double -> Double
    g loc d = d * scale 0 x rampLength 1 finalRatio
      where
        x = fromRational $ locDiff timeSigs loc1 loc
