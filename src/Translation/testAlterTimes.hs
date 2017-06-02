import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf
import Data.Set(Set)
import Data.Map(Map)


import Midi.MidiData
import Translation.AlterTimes


testMap = M.fromList [ (60, S.fromList [0,1,2])
                     , (61, S.fromList [0]    )
                     ]


t1 = concatMap f outs
  where
    mLegato = Nothing
    mSepSame = Just 0.5
    pitch = 60
    o = OnOff [("x",(1,2.0))]

    OnOff outs = doTime testMap mLegato mSepSame pitch o
    f :: (String,(Double,Double)) -> String
    f (s,(b,e)) = printf "%8.3f %8.3f   %s\n" b e s
    
main = putStrLn t1
