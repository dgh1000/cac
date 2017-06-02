import Translation.Trill

import Text.Printf
import Score.ScoreData


main = putStrLn . unlines . 
  map (\(t1,t2,s) -> printf "%8.3f %8.3f %s" t1 t2 (show s)) $ t


t = trillTimes 5 10 (TrillShape Upper [(0,5),(9,5)] Upper) 10 12
