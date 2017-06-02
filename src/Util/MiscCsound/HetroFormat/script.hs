import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary
import Data.Int
import System.IO
import Control.Monad

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (y:[]) = []
everyOther (x:y:rem) = x : everyOther rem

interlace :: [a] -> [a] -> [a]
interlace [] xs = xs
interlace xs [] = xs
interlace (x:xs) (y:ys) = x : y : interlace xs ys

-- Int16:: number of partials
data HetroOutput = HetroOutput Int16 [HetroRow]

data HetroRow = HetroRow Int16 [Int16]

replaceValues :: HetroRow -> [Int16] -> HetroRow
replaceValues (HetroRow x ys) = HetroRow x 

x :: Get (Int16,Int16)
x = do
  i <- get
  i2 <- get
  return (i,i2)

y :: Int -> Get [Int16]
y n = do
  if n <= 0
    then return []
    else do
       i <- get
       is <- y (n-1)
       return $ i : is




oneRowData :: Get [Int16]
oneRowData = do
  n <- getWord16le
  let n2 = fromIntegral n :: Int16
  if n == 32767
    then return []
    else do
      liftM (n2 :) oneRowData

oneRow :: Get HetroRow
oneRow = do
  pre <- getWord16le
  let pre2 = fromIntegral pre
  rest <- oneRowData
  return $ HetroRow pre2 rest

hetFile :: Get [(Int16,[Int16])]
hetFile = do
  n <- getWord16le
  let n2 = fromIntegral n :: Int
  replicateM n2 oneRow

main = do
  h <- openBinaryFile "example.het" ReadMode
  buf <- BL.hGetContents h
  print $ runGet (y 100) buf


main2 = do
  h <- openBinaryFile "example.het" ReadMode
  buf <- BL.hGetContents h
  print $ BL.length buf

main3 = do
  h <- openBinaryFile "example.het" ReadMode
  buf <- BL.hGetContents h
  let stuff = runGet hetFile buf
  print $ take 50 . snd . (!! 13) $ stuff
  -- print $ map fst stuff
