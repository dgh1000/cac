
import System.IO
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import System.Directory
import Data.List
import Util.Math
import GenPvsCsd
import Util.MiscCsound.HetroFormat.Hetro

-- Map <time> (Map <freq> <ampl>)
type PvsDump = Map Int (Map Float Float)
--[Map <time> <ampl>] :: partials starting at 1
type PartialCurve = [Map Int Float]

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


readPvsDump :: [(String,String)] -> String -> IO PvsDump
readPvsDump filenames dir = do
  -- contents :: [(Int,Map Float Float)]
  contents <- mapM (oneFilePair dir) filenames
  return $ M.fromList contents
  
-- computePartial
--   Inputs
--     Float: fundamental
--     PvsDump
--     Int: partial #
--   output
--   Map Int Float :: Map <p
computePartial :: Float -> PvsDump -> Int -> Map Int Float
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


-- computeHetroData 
--  [Map Int Float]  ::  curve for each partial: x values are in audio
--                        sample rate
--  Float            ::  fundamental
--  Float            ::  sampling rate
--  Float            ::  duration
computeHetroData :: [Map Int Float] -> Float -> Float -> Float -> HetroData
computeHetroData ps fundamental sampRate dur = HetroData (fromIntegral n) rows
  where
    n = length ps
    rows = concat $ zipWith makeAmpFreqRows ps [1..]
    makeAmpFreqRows :: Map Int Float -> Int -> [HetroRow]
    makeAmpFreqRows curve partial = [amplRow, freqRow]
      where
        freqRow = makeConstantFreqRow (fundamental * fromIntegral partial) dur
        amplRow = HetroRow (-1) (concatMap onePoint $ M.toList curve)
        onePoint (x,y) = [round (1000*fromIntegral x/sampRate), round y]
    makeConstantFreqRow f dur = HetroRow (-2) 
        [0, round f, round $ dur*1000, round f]
    

{-
main = do
  -- config
  let inputDir = "c:/Temp/csound/tables"
      fundamental = 262
      nParts = 40
      sampRate = 44100
      msPerEnvSamp = 5    -- milliseconds between sample points on the
                          --  envelope giving partial strength
      outputFile = "c4.het"



  c <- getDirectoryContents inputDir
  let afiles = filter ((== 'a') . head) c
      ffiles = filter ((== 'f') . head) c
      filenames = sort $ zip ffiles afiles
  -- read files into Map Int (Map Float Float)
  -- fileData :: PvsDump
  fileData <- readPvsDump filenames inputDir
  --    a map of sample time to a map of freq to amp values
  -- for each harmonic, for all time, find sum of nearby amps
  --   this goes in Map Int (Map Int Float)
  -- ps :: [Map Int Float]
  let ps = map (computePartial fundamental fileData) [1..nParts]
      dur = (/ sampRate) . fromIntegral . fst . M.findMax . head $ ps
  -- write tables for each harmonic giving strength over time
  --   as f statements, with the amplitude interpolated between 
  --   points to match msPerEnvSample

  --   This was doing code gen for playback via gen-routine-defined tables
 
  -- let (tlen,tablesCode) = computeTables msPerEnvSamp sampRate ps
  -- let csdCode = genCsd nParts fundamental msPerEnvSamp tlen tablesCode
  -- writeFile "playback.csd" csdCode

  -- This section creates hetro-output-format table
  let hd = computeHetroData ps fundamental sampRate dur
  handle <- openBinaryFile outputFile WriteMode 
  writeHetFile handle hd
-}

isWavFile = (== ".wav") . takeExtension

filenameMidiPitch :: FilePath -> Int
filenameMidiPitch s = read n
  where n = dropWhile (not . isDigit) . dropExtension . s
    

main = do
  -- there is a directory with many wav files. Their midi pitches are
  -- given in the filenames.

  -- read each file and make a corresponding het file.

  --    - compute an integral frequency closest to the exact one
  --   
  --    - there is a directory to store tables. delete everything in it
  --    - make a csd that reads the wav and generates a bunch of tables
  --    - run that csd
  --    - from the tables, make a het file
  --    - name that het file with the corresponding prefix and pitch number
  --        and with the integral frequency
  -- 

  -- config
  let wavDir = "c:/Temp/csound/ariaOutput/piano"
      pvsTablesDir = "c:/Temp/csound/pvsTables"
      hetFilePrefix = "pno"
      outHetFileDir = "c:/Temp/csound/hetFiles"
      sampRate = 44100
      maxPartFreq = 15000

  c <- getDirectoryContents wavDir
  forM (filter isWavFile c) (\filename -> do

{-
    let midiPitch = filenameMidiPitch filename
        freq = midiPitchToFreq midiPitch
        roundedFreq = round freq :: Int
    removeTextFiles pvsTablesDir
    makeAnalysisCsd (wavDir ++ "/" ++ filename) freq pvsTablesDir sampRate
                    maxPartFreq
    system "csound analysis.csd"
    let hd = computeHetroData pvsTablesDir sampRate outPrefix midiPitch 
             freq roundedFreq
    let hetFilename = printf "%s_%02d_%04d.het" hetFilePrefix midiPitch 
                      roundedFreq
    handle <- openBinaryFile (outHetFileDir ++ "/" ++ hetFilename) WriteMode
    writeHetFile handle hd)
-}