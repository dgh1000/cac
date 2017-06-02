
import Data.Monoid

data T = T (Maybe Int) (Maybe Int) (Maybe Int)

combine x Nothing = x
combine _ (Just x) = Just x


f :: (Maybe Int, Maybe Int, Maybe Int) -> T -> (Maybe Int, Maybe Int, Maybe Int)
f (a,b,c) (T ma mb mc) 
      = (a `combine` ma, b `combine` mb, c `combine` mc)

d = [ T (Just  1)   Nothing  (Just  3)
    , T   Nothing (Just  2)    Nothing
    , T (Just  4)   Nothing    Nothing
    ]

x = scanl f (Nothing,Nothing,Nothing) d

a1 = Just 3 :: Maybe Int
a3 = Nothing :: Maybe Int

a2 = let (Just x) = a3 in x
a4 = let x:_ = [] in x

