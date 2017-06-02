{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Util.Showable
import Score.ScoreData
import Translation.TimeMap
import Translation.TranslationData
import Common.CommonData
import Common.CommonUtil

{-
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


type FoldOutput = Map Loc Rational
instance Showable FoldOutput where
  showi m = Component "" True (showMap1 simpleShowLoc show m)
-}

instance Showable RelTimeMap where
  -- Map Loc Double
  showi (RelTimeMap m) = Component "" False (map g $ M.toAscList m)
    where
      g :: (Loc,Double) -> ShowItem
      g (loc,x) = SingleLine $ printf "%s %8.3f" (showLoc2 loc) x


----------------------------------------------------------------------
----------------------------------------------------------------------

ms1 = M.fromList 
  [ (Loc  1 1, [SetTempo 10])
  , (Loc  1 2, [Pause (Left 1)])
  , (Loc  1 3, [RampBeg  (NumRatio 1 2)])
  , (Loc  2 1, [RampEnd  (NumRatio 1 4)])
  , (Loc  2 3, [RampBeg  (NumRatio 1 1)])
  , (Loc  3 1, [RampEndBeg (NumRatio 1 2) (NumRatio 1 1)])
  , (Loc  3 3, [RampEnd (NumRatio 1 3), SetTempo 20])
  , (Loc  4 1, [RitAccel])
  , (Loc  4 3, [SetTempo 5])
  ]

timeSigs = M.fromList
  [ (1, TimeSig 4 4)
  , (2, TimeSig 4 4)
  , (3, TimeSig 4 4)
  , (4, TimeSig 4 4)
  ]


main = do
  let m = computeBaseTimeMap 1 timeSigs ms1
  writeFile "timemap.txt" $ showiToString $ showi m

{-
t :: Map Loc Mark -> IO ()
t m = do
  let (tempos,ends) = initialTempos m
  putStrLn $ (showiToString $ showi tempos) ++ (showiToString $ showi ends)

r m = do
  let (tempos,ends) = initialTempos m
      ramps = computeRamps tempos ends m
      s = showiToString . showi $ (tempos,ramps)
  putStrLn s
-}
