-- read a Hetro file and replace frequency values with the proper 
-- fundamental and harmonics

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary
import Data.Int
import System.IO
import Control.Monad
import Util.MiscCsound.HetroFormat.Hetro

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (y:[]) = [y]
everyOther (x:y:rem) = x : everyOther rem

interlace :: [a] -> [a] -> [a]
interlace [] xs = xs
interlace xs [] = xs
interlace (x:xs) (y:ys) = x : y : interlace xs ys

replaceRowValues :: HetroRow -> Int16 -> HetroRow
replaceRowValues (HetroRow x ys) v = HetroRow x newYs
  where
    times = everyOther ys
    newYs = interlace times (replicate (length times) v)

main = do
  -- config
  let inputFile = "pno-C4.het"
      outputFile = "pno-C4-fixed.het"
      freqs = [262,524..]
  
  h <- openBinaryFile inputFile ReadMode
  HetroData n rows <- readHetFile h
  let ampRows = everyOther rows
      freqRows = everyOther (tail rows)
      replacedFreqRows = zipWith replaceRowValues freqRows freqs
      newRows = interlace ampRows replacedFreqRows
      newData = HetroData n newRows
  -- putStrLn $ showSomeHetData newData
  h2 <- openBinaryFile outputFile WriteMode
  writeHetFile h2 newData
  hClose h
  hClose h2
  

  -- print $ map fst stuff


t1 = interlace [1,2] [3,4]
t2 = everyOther $ tail [1,2,3,4,5,6]
