
import Control.Arrow
import qualified Data.List as L
import Data.Ratio

f :: Int -> Maybe Int
f 3 = Nothing
f x = Just x

g = ('a',3) :: (Char,Int)

h = []
i = zip h (L.tails h)

x10 = [1,2]

x11 = L.tails x10

scale :: Fractional a => a -> a -> a -> a -> a -> a
scale x1 x2 x3 y1 y3 = y1 + (y3-y1)*(x2-x1)/(x3-x1)

x100 = scale (0::Double) 2 10 10 20

x101 = scale (0 :: Rational) 2 10 10 20
