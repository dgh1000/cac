import Data.Monoid

t1 = [Nothing,Just 2,Just 4] :: [Maybe Int]

t2 = (getFirst . mconcat . map First) t1

t3 = "pq"
t4 = "0q"

