{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map as M
import Data.Map(Map)
import Util.Map
import Util.Showable




instance (Show k, Showable a, Ord k) => Showable (Map k a) where
  showi m = Component "map" True (map g $ M.toAscList m)
    where
      g (x,y) = Component (show x) True [showi y]
  

type Ints = [Int]

instance Showable Ints where
  showi xs = SingleLine $ show xs
  



x1_0 :: Map Double [Int]
x1_0 = M.fromList [ (3.0, [1,2,3])
                  , (6.0, [4,5,6])
                  ]

x1_1 = M.fromList [ (11.0,[11]) ]

x1 :: Map String (Map Double [Int])
x1 = M.fromList [ ("s0", x1_0)
                , ("s1", x1_1)
                ]

x2 = flipMap x1

main = putStrLn . showiString $ x2
