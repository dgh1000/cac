
module Translation.Time where

import qualified Data.Map as M
import Data.Ratio
import Text.Printf
import Translation.TranslationData
import Score.ScoreData
import Common.CommonExport
import Util.Exception
xxxx
----------------------------------------------------------------------
----------------------------------------------------------------------
--                    time lookup 


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

{-
lookupTime  loc (AbsTimeMap timeMap) = case M.splitLookup loc timeMap of
    -- the map has values of type (Rational,Double)
    (    _,Just t,    _)    -> t
    (lower,     _,upper)    -> tBeg + (tEnd-tBeg) * r
      where
        (locBeg,beatSpan,tBeg) = case M.maxViewWithKey lower of
          Just ((k,(r,d)),_) -> (k,r,d)
          Nothing -> throwMine $ printf ("in lookupTime, passed Loc %s " ++
                     "and found there was no lower entry in the time map")
                     (simpleShowLoc loc)
        tEnd = case M.minViewWithKey upper of
          Just ((_,(_,d)),_) -> d
          Nothing -> throwMine $ printf ("in lookupTime, passed Loc %s " ++
                     "and found there was no upper entry in the time map")
                     (simpleShowLoc loc)
        (Loc _ beatBeg) = locBeg
        (Loc _ beatTarget) = loc
        r = fromRational $ (beatTarget - beatBeg) / beatSpan
-}


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





