{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Score.ParseWords2
import Score.ScoreData2
import Common.CommonExport
import Common.CommonUtil
import Util.Parse
import Util.Map
import Util.Showable
import Data.Maybe
import Data.Map(Map)
import Data.Ratio

timeSigs_1 = M.fromList
  [ (1, TimeSig 4 4)
  , (2, TimeSig 4 4) 
  , (3, TimeSig 4 4)
  , (4, TimeSig 4 4)
  , (5, TimeSig 4 4)
  , (6, TimeSig 4 4)
  , (7, TimeSig 4 4)
  , (8, TimeSig 4 4)
  , (9, TimeSig 4 4)
  ]


data_1 = listToLMap 
  [ (Loc 1 1, "a=100"    )  
  , (Loc 1 (1+1%2), "w"   )
  , (Loc 1 2, "<1:4<"     )
  , (Loc 1 (2+1%2), "b=2" )
  , (Loc 1 3, ">1:20>"    )
  , (Loc 1 (3+1%2), "<<1.1b")
  , (Loc 1 4, "<1:20"     )
  , (Loc 2 1, ">+:20>"    )
  , (Loc 2 2, "<3:2;:8"   )
  , (Loc 2 (2+1%2), "1:2b>>")
  , (Loc 2 3, "<3:2;1:2"  )
  , (Loc 2 4, "<:4"       )
  , (Loc 2 (4+1%2), "w"   )
  , (Loc 3 1, "T=100"    )
  , (Loc 3 2, "T=3:2a"    )
  , (Loc 3 3, "T=3:2"     )
  , (Loc 3 4, "$a"        )
  , (Loc 4 1, "arp@5"     )
  , (Loc 4 2, "stac@5")
  , (Loc 4 3, "ps@5")
  , (Loc 4 4, "ps:15")
  , (Loc 4 (4+1%2), "ps1:4")
  , (Loc 5 1, "a=100")
  , (Loc 5 2, "a=3:2b")
  , (Loc 5 3, "rit.")
  , (Loc 5 4, "accel.")
  , (Loc 6 1, "pat:foo")
  , (Loc 6 2, "wl{<+1:2;:/1<}")
  , (Loc 6 3, "wl!16")
  , (Loc 6 4, "wl!2")
  , (Loc 7 1, "1--" )
  , (Loc 7 2, "1:2--")
  , (Loc 7 3, "--2--")
  , (Loc 7 4, "=2:3=")
  , (Loc 8 1, "--2:3|4:5--")
  , (Loc 8 2, "=4:5")
  , (Loc 8 3, "=4:5")
  , (Loc 8 4, "<>.70/.50")
  , (Loc 9 1, "<>")
  , (Loc 9 2, "><.70/.50")
  , (Loc 9 3, "><")
  ]


{-

okay we ran into problem using our percent scheme, numbers like 50 are perfect
for beats per minute

-}


type L1 = (Loc,Map String [Mark])

instance Showable L1 where
  showi (loc,m) = Component (showLoc2 loc) True (map showi $ M.toAscList m)


type M1 = (String,[Mark])

instance Showable M1 where
  showi (name,ms) = Component name True (map (SingleLine . show) ms)


test :: Map Loc [String] -> IO ()
test words = do
  let marks  = computeWordMarks $ M.fromList [("foo",words)]
      -- output = concatMap doLoc $ M.toAscList marks
      -- doLoc :: (Loc,[Mark]) -> String
      -- doLoc (loc,ms) = printf "%s\n" (showLoc2 loc) ++ concatMap doMark ms
      -- doMark m = printf "    %s\n" (show m)
  putStrLn . showiToString $
    Component "foo" True (map showi $ M.toAscList marks)


main = test data_1
    

main2 :: Double
main2 = read ".5"
