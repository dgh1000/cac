
import qualified Data.Map as M
import Text.Printf
import Translation.Curves
import Translation.TranslationData
import Common.CommonData
import Common.CommonUtil
import Util.Showable
import Data.Ratio



{-

ts = M.fromList [ (1, TimeSig 4 4)
                , (2, TimeSig 4 4)
                , (3, TimeSig 4 8)
                , (4, TimeSig 4 4)
                ]

m1 = M.fromList [ (Loc 1 1, (Loc 4 4,(0,10))) ]

m2 = M.fromList [ (Loc 2 1, (Loc 3 1,(5, 5))) ]

m3 = M.fromList [ (Loc 3 2, (Loc 3 3, (2,2))) ]

main = putStrLn $ showCurve $ merge ts [m1,m2,m3]

-}


c1 = OneCurve (M.fromList [(1, Seg 2 0 10)]) []
c2 = OneCurve (M.fromList [(3, Seg 4 10 20)]) []
cs = Curve [c1,c2]

main = do
  putStrLn $ showiToString $ showCurve cs
  print $ curveLookup (5) cs
