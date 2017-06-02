
import qualified Data.List as L

a10 = [1,2,3,4,5,6]
a11 = zip a10 (drop 1 . L.tails $ a10)

a12 = zip [1] (drop 1 . L.tails $ [1])
