module Util.MiscCsound.HetroFormat.Hetro where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Data.Int
import System.IO
import Control.Monad

-- Creating a "hetro" file (to be read by adsyn):
--
--  Choose number of partials.
--  Create that many amplitude rows and that many frequency rows.
--     Interleave ampl and freq rows; ampl go first.
--     ampl rows have -1 tag
--     freq rows have -2 tag
--  Within a row, time (in ms) and values alternate


-- Int16:: number of partials
-- [HetroRow]
data HetroData = HetroData Int16 [HetroRow]

-- Int16   :: -1 if amp breakpoints, -2 if freq breakpoints
-- [Int16] :: <time1> <value1> <time2> <value2> etc.
data HetroRow = HetroRow Int16 [Int16]

----------------------------------------------------------------------
----------------------------------------------------------------------
--  getting
----------------------------------------------------------------------
----------------------------------------------------------------------

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

hetData :: Get HetroData
hetData = do
  n <- getWord16le
  let n2 = fromIntegral n :: Int
  rows <- replicateM n2 oneRow
  return $ HetroData (fromIntegral n) rows

readHetFile :: Handle -> IO HetroData
readHetFile h = do
  -- h <- openBinaryFile s ReadMode
  buf <- BL.hGetContents h
  let stuff = runGet hetData buf
  return stuff


----------------------------------------------------------------------
----------------------------------------------------------------------
-- putting
----------------------------------------------------------------------
----------------------------------------------------------------------
putOneRowData :: [Int16] -> Put
putOneRowData vs = do
  mapM_ (putWord16le . fromIntegral) vs
  putWord16le 32767
  
putOneRow :: HetroRow -> Put
putOneRow (HetroRow typ values) = do
  putWord16le . fromIntegral $ typ
  putOneRowData values

putHetData :: HetroData -> Put
putHetData (HetroData n rows) = do
  putWord16le . fromIntegral $ n
  mapM_ putOneRow rows

writeHetFile :: Handle -> HetroData -> IO ()
writeHetFile h d = do
  BL.hPutStr h $ runPut (putHetData d)

showSomeHetData :: HetroData -> String
showSomeHetData (HetroData n rows) = nStr ++ row 0 ++ row 1 ++ row 2 ++ row 3
  where
    nStr = "n partials: " ++ show n ++ "\n"
    row i = "type: " ++ show typ ++ ", " 
            ++ concatMap (\v -> show v ++ " ") (take 8 vs) ++ "\n"
      where
        HetroRow typ vs = rows !! i
    