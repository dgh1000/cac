
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import System.Directory
import Data.List
import Util.Math
import GenPvsCsd

-- Map <time> (Map <freq> <ampl>)
type FileData = Map Int (Map Float Float)
--[Map <time> <ampl>] :: partials starting at 1
type PartialData = [Map Int Float]

readTableValues :: String -> IO [Float]
readTableValues s = do
  b <- readFile s
  let bs = lines b
      lenLine = head . drop 1 $ bs
      n = read (drop 6 lenLine) :: Int
      valueLines = take n . drop 25 $ bs
  return $ map read valueLines

oneFilePair :: String -> (String,String) -> IO (Int,Map Float Float)
oneFilePair dir (ffile,afile) = do
  fvalues <- readTableValues (dir ++ "/" ++ ffile)
  avalues <- readTableValues (dir ++ "/" ++ afile)
  let t = read . take 6 . drop 1 $ ffile
  return (t, M.fromList $ zip fvalues avalues)


readFileData :: [(String,String)] -> String -> IO FileData
readFileData filenames dir = do
  -- contents :: [(Int,Map Float Float)]
  contents <- mapM (oneFilePair dir) filenames
  return $ M.fromList contents
  
-- computePartial
--   Inputs
--     Float: fundamental
--     FileData
--     Int: partial #
--   output
--   Map Int Float :: Map <p
computePartial :: Float -> FileData -> Int -> Map Int Float
computePartial f fd n = M.fromList l
  where
    low  = (f * fromIntegral n) - f/2
    high = (f * fromIntegral n) + f/2
    -- [(Int,Float)]
    -- [(<time>, <strength of partial>)]
    l = map (\(t,m) -> (t, computeStrength m)) $ M.toList fd
    computeStrength :: Map Float Float -> Float
    computeStrength = sum . M.elems . M.filterWithKey filtFn
    filtFn k _ = low < k && k < high

-- computeSampleTimes
--    These are computed as number of audio samples rather than
--    seconds.
computeSampleTimes :: Float -> Float -> Map Int Float -> [Float]
computeSampleTimes outSampMs sampRate m =
  [1, 1 + spacingInAudioSamples .. fromIntegral maxTimeS]
  where
    spacingInAudioSamples = sampRate * outSampMs / 1000
    (maxTimeS,_) = M.findMax m
    
computeTableSize :: Int -> Int
computeTableSize nOutSamples = 
    2 ^ (ceiling $ logBase 2 (fromIntegral nOutSamples))

lookupCurveValue :: Map Int Float -> Float -> Float
lookupCurveValue map x =
    case M.splitLookup (round x) map of
      (_,Just y,_) -> y
      (low,_,high) 
          | M.null low  && M.null high -> error "boo"
          | M.null low -> let (_,v) = M.findMin high in v
          | M.null high -> let (_,v) = M.findMax low in v
          | otherwise -> 
              let (t1,v1) = M.findMax low
                  (t2,v2) = M.findMin high
              in scale (fromIntegral t1) x (fromIntegral t2) v1 v2

computeTables :: Float -> Float -> [Map Int Float] -> (Int,String)
computeTables outSampMs sampRate strengths =
  (tableSize, concat $ zipWith computeTable [1..] strengths)
  where
  sampleTimes = computeSampleTimes outSampMs sampRate (head strengths)
  tableSize = computeTableSize (length sampleTimes)
  computeTable :: Int -> Map Int Float -> String
  computeTable n curve = 
    printf "f%d 0 %d -2 %s\n" n tableSize valueStrings
    where
      valueStrings = concatMap printOneValue values
      values = map (lookupCurveValue curve) sampleTimes
      printOneValue :: Float -> String
      printOneValue v = printf " %.4f" v

main = do
  -- config
  let dir = "c:/Temp/csound/tables"
      fundamental = 262
      nParts = 40
      sampRate = 44100
      msPerEnvSamp = 10


  c <- getDirectoryContents "c:/Temp/csound/tables"
  let afiles = filter ((== 'a') . head) c
      ffiles = filter ((== 'f') . head) c
      filenames = sort $ zip ffiles afiles
  -- read files into Map Int (Map Float Float)
  -- fileData :: FileData
  fileData <- readFileData filenames dir
  --    a map of sample time to a map of freq to amp values
  -- for each harmonic, for all time, find sum of nearby amps
  --   this goes in Map Int (Map Int Float)
  -- ps :: [Map Int Float]
  let ps = show (M.size $ head ps') `trace` ps'
        where  ps' = map (computePartial fundamental fileData) [1..nParts]
  -- write tables for each harmonic giving strength over time
  --   as f statements, with the amplitude interpolated between 
  --   points to match msPerEnvSample
  let (tlen,tablesCode) = show tlen' `trace` (tlen',tablesCode')
          where (tlen',tablesCode') = computeTables msPerEnvSamp sampRate ps
  let csdCode = genCsd nParts fundamental msPerEnvSamp tlen tablesCode
  writeFile "playback.csd" csdCode
