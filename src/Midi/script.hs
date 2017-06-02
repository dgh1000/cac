import Data.Bits
import qualified Data.List as L
import Text.Printf

(x,y) = L.partition (\x -> x < 5) [5,4,6, 3]

x1 = 0x9c
x2 = 0x8c :: Int

t1 = x1 .&. 0xef :: Int



t2 = printf "%x" t1 :: String

t3 = x2 .&. 0xf0 == 0x80

t4 = [[], [1]] :: [[Int]]
t5 = concat . map (take 1) $ t4

t6 = takeWhile (==3) []

