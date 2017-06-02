import qualified Data.Map as M
import qualified Data.Set as S
import Score.ScoreData
import Common.CommonData
import Translation.LoudnessFunc
import Translation.Curves


dyns1 = M.fromList [ (Loc 2 1, [SimpleDyn 3 1] )
                   , (Loc 6 4, [SimpleDyn 5 1] ) ]


hps1  = M.fromList [ (Loc 2 1, Hairpin Crescendo (Loc 6 3)) ]

trueEnds1 = M.fromList [ (Loc 7 1, [])]



s = Staff "staff1" dyns1 hps1 M.empty M.empty trueEnds1 S.empty M.empty
          M.empty


main = putStrLn $ showCurve $ hpDynOneStaff s
