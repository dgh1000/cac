import Util.Map
import qualified Data.Map as M
import Data.Monoid
import Data.Ratio
import Control.Arrow


m1 = M.fromList [(1,"ab"),(2,"a")]
m2 = M.mapMaybe (getFirst . mconcat . map (First . isA)) m1


isA 'a' = Just 'a'
isA _ = Nothing

isB 'b' = Just 'b'
isB _ = Nothing

x1 :: Rational
x1 = fromIntegral $ round $ 7%3

data XXX = XXX Int Int
         deriving(Show)

x2 :: Int -> (Int,Int)
x2 = (+1) &&& (+2)

x3 :: Int -> XXX
x3 = (uncurry XXX) . x2