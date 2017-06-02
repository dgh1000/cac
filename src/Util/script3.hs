
import Data.Array.IArray

t1 :: Array Int Int
t1 = array (0,13) (zip [0..13] (repeat 0))

t2 (p1,p2) = if d<=13 then d else fh d
      where
        d = if p2>p1 then p2-p1 else p1-p2
        -- fh x: we know that x is an interval of at least an octave.
        -- find x mod 13, and if that is < 2 then add 12
        fh x = if m >= 2 then m else m + 12
          where m = x `mod` 12

t3 = accum (+) t1 [(4,1)]
t4 = accum (+) t3 [(4,3)]



