
import qualified Data.Map as M
import Data.Map(Map)
import Util.Map

d1 :: Map Int (Map String [Double])
d1 = M.fromList
  [ (1, M.fromList [ ("foo", [1,2,3]) ])
  , (2, M.fromList [ ("monkee", [-1,-2,0]) ])
  ]

m :: Double -> Maybe Char
m x | x < 0 = Just 'c'
    | otherwise = Nothing

d2 = M.map (lMapMaybe m) d1

