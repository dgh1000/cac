

import qualified Data.Set as S
import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Data.Ratio
import Data.Maybe
import Common.CommonData
import Common.CommonUtil
import Translation.TranslationData
import Translation.Warp
import Score.ScoreData

spb = fromIntegral configSlicesPerBeat  :: Integer


timeSigs = M.fromList $ zip [1..] [ TimeSig 4 8
                                  , TimeSig 4 8
                                  , TimeSig 4 4
                                  , TimeSig 4 4
                                  , TimeSig 4 4
                                  , TimeSig 4 4 ]

sliceDur = 1 :: Double

baseMap = M.fromList $ concatMap doMsr [1..length timeSigs]
  where
    doMsr n = concatMap doBeat [1..numer]
      where 
        numer = tsNumer . fromJust . M.lookup n $ timeSigs
        doBeat b = map doSlice [0..spb-1]  
          where
            doSlice i = ( Loc n (fromIntegral b + (i%spb))
                        , sliceDur )


showTimeMap :: RelTimeMap -> String
showTimeMap (RelTimeMap tm) = concatMap (\(l,d) -> printf "%s %8.4f\n" 
                                                   (showLoc2 l) d) .
                              M.toAscList $ tm


----------------------------------------------------------------------
----------------------------------------------------------------------

s1 = roundToSlice timeSigs (Loc 4 (3+47%48))

----------------------------------------------------------------------
----------------------------------------------------------------------


ws = S.fromList [ Loc 1 1
                , Loc 3 1
                , Loc 3 (1+3%48)
                , Loc 3 (3+3%48)
                ]

wLoc1 = Loc 2 1

-- how many slices? 8 beats * spb. want to adjust each one by A=0.1
-- seconds. 0.1 = beat dur is spb seconds.

warp1 =   (wLoc1, Warp True (Just $ Loc 1 1) (Just $ Loc 3 1)    1     0.2)


warp1_2 = (wLoc1, Warp True (Just $ Loc 1 1) (Just $ Loc 3 1)    1    (0.2))

warp1_3 = (wLoc1, Warp True (Just $ Loc 1 1) Nothing             1    (0.2))

warp1_4 = (wLoc1, Warp True Nothing          (Just $ Loc 3 1) (-1)    (0.2))


wLoc2 = Loc 3 2

warp2_1 = (wLoc2, Warp True (Just $ Loc 3 (1+3%24))
                            (Just $ Loc 3 (3+3%24)) (-1) 0.5)

wLoc3 = Loc 3 (1+1%12)

warp3_1 = (wLoc3, Warp True (Just $ Loc 3 (1+3%48))
                            (Just $ Loc 3 (3+3%48)) (-1) 0.5)



----------------------------------------------------------------------
----------------------------------------------------------------------

wLoc5 = Loc 3 1

absWarp1 = (wLoc5, AbsWarp (Loc 2 3) 2.0)

----------------------------------------------------------------------
----------------------------------------------------------------------

run w = writeFile "out.txt" . 
  showTimeMap . oneWarp timeSigs w $ RelTimeMap baseMap


runAbs w = writeFile "out.txt" .
  showTimeMap . oneAbsWarp timeSigs w $ RelTimeMap baseMap

main = writeFile "out.txt" . showTimeMap $ RelTimeMap baseMap

----------------------------------------------------------------------

testPause = 
  writeFile "out.txt" . showTimeMap . applyOnePause timeSigs 
  (Loc 2 (4+47%48),Pause $ Right 1) . RelTimeMap $ baseMap
