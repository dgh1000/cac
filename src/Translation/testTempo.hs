{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map as M
import Data.Map(Map)
import Data.Ratio
import Text.Printf
import Translation.TimeMap
import Translation.ShowTranslation
import Util.Showable
import Util.Map
import Common.CommonData
import Common.CommonUtil
import Score.ScoreData




instance Showable TempoFold where
  showi (TempoFold vars tempos ends) = Component "" False 
    [ Component "vars" True (map showi . M.toAscList $ vars)
    , Component "tempos" True (map showi . M.toAscList $ tempos)
    , Component "ends" True (showMap1 show show ends) ]

type Var = (String,Rational)
instance Showable Var where
  showi (s,r) = SingleLine $ printf "%5s: %s" s (show r)

type LocTempo = (Loc,Rational)
instance Showable LocTempo where
  showi (l,t) = SingleLine $ printf "%s: %s" (simpleShowLoc l) (show t)


type TemposRamps = (Map Loc Rational,[RampData])
instance Showable TemposRamps where
  showi (tempos,ramps) = Component "" False [stempos,sramps]
    where
      stempos = Component "tempos" True (showMap1 show show tempos)
      sramps  = Component "ramps"  True (map showRamp ramps )
      showRamp (RampData loc1 tempo1 loc2 tempo2) = SingleLine $
        printf "%s %15s %s %15s" (simpleShowLoc loc1) (show tempo1)
                                 (simpleShowLoc loc2) (show tempo2)


----------------------------------------------------------------------
----------------------------------------------------------------------

ms1 = listToLMap ms1l
ms1l :: [(Loc,Mark)]
ms1l = 
  [ (Loc  1 1, SetTempo (TempoAbs 90))
  , (Loc  2 1, SetVar "a" (TempoAbs 120))
  , (Loc  3 1, SetTempo $ TempoRelative (NumRatio 1 2) (Just "a"))
  , (Loc  4 1, SetTempo $ TempoRelative (NumRatio 1 1) (Just "a"))
  , (Loc  5 1, RampBeg  $ NumRatio 1 2)
  , (Loc  6 1, RampEnd (Left $ NumRatio 1 3) False)
  , (Loc  7 1, SetTempo $ TempoRelative (NumRatio 1 1) Nothing)
  , (Loc  8 1, RampBeg  $ NumRatio 1 1)
  , (Loc  9 1, RampEnd (Left $ NumRatio 2 1) True)
  , (Loc 10 1, SetTempo $ TempoRelative (NumRatio 1 1) Nothing)
  , (Loc 11 1, RitAccel)
  , (Loc 12 1, SetTempo $ (TempoAbs 50))
  ]

ms2 = listToLMap 
  [ (Loc 1 1, SetTempo $ TempoRelative (NumRatio 1 1) Nothing)
  , (Loc 5 1, SetTempo $ TempoRelative (NumRatio 1 1) (Just "b")) 
 ]

ms3 = listToLMap
  [ (Loc 1 1, SetTempo $ TempoRelative (NumRatio 1 1) Nothing) ]

timeSigs1 = M.fromList $ zip [1,2..15] (repeat $ TimeSig 4 4)

ms4 = listToLMap
  [ (Loc 1 1, SetTempo (TempoAbs 60))
  , (Loc 2 (1+1%48), RampBeg $ (NumRatio 1 1))
  , (Loc 3 (1+1%48), RampEnd  (Left $ NumRatio 2 1) False)
  ]

timeSigs2 = M.fromList $ zip [1..4] (repeat $ TimeSig 4 4)

tf :: [(Loc,Mark)] -> IO ()
tf = putStrLn . showiToString . showi . 
     foldl foldTempo (TempoFold M.empty M.empty M.empty)

f = writeFile "out.txt" . showiToString . showi . initialTimeMap 1 timeSigs2

