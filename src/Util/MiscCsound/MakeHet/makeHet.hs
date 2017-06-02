{-# LANGUAGE BangPatterns #-}
import Control.Monad
import System
import System.IO
import System.FilePath
import Data.Char
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import System.Directory
import Data.List
import Util.Math
import Util.MiscCsound.PvsOpcodes.GenPvsCsd
import Util.MiscCsound.HetroFormat.Hetro
import Control.Exception
import qualified Data.ByteString as B

-- Map <time> (Map <freq> <ampl>)
type PvsDump = Map Int (Map Float Float)
--[Map <time> <ampl>] :: partials starting at 1
type PartialCurve = [Map Int Float]

readByteString :: Read a => B.ByteString -> a
readByteString = read . map (chr . fromIntegral) . B.unpack

readTableValues :: String -> IO [Float]
readTableValues s = do
  b <- B.readFile s
  let bs = B.split (fromIntegral . ord $ '\n') b
      lenLine = bs !! 1
      n = readByteString (B.drop 6 lenLine) :: Int
      valueLines = take n . drop 25 $ bs
  return $ map readByteString valueLines

oneFilePair :: String -> (String,String) -> IO (Int,Map Float Float)
oneFilePair dir (ffile,afile) = do
  fvalues <- readTableValues (dir ++ "/" ++ ffile)
  avalues <- readTableValues (dir ++ "/" ++ afile)
  let t = read . take 6 . drop 1 $ ffile
  return (t, M.fromList $ zip fvalues avalues)


readPvsFiles :: [(String,String)] -> String -> IO PvsDump
readPvsFiles filenames dir = do
  -- contents :: [(Int,Map Float Float)]
  contents <- mapM (oneFilePair dir) filenames
  return $ M.fromList contents
  
-- computePartial
--   Inputs
--     Float: fundamental
--     PvsDump
--     Int: partial #
--   output
--   Map Int Float :: Map <partial number> <strength>
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

readPvsDump :: String -> IO PvsDump
readPvsDump pvsDumpDir = do
  c <- getDirectoryContents pvsDumpDir
  let afiles = filter ((== 'a') . head) c
      ffiles = filter ((== 'f') . head) c
      filenames = sort $ zip ffiles afiles
  -- read files into Map Int (Map Float Float)
  -- fileData :: PvsDump
  readPvsFiles filenames pvsDumpDir
  

-- computeSizes
-- Inputs
--   Float :: fundamental
--   Float :: sample rate
-- Output
--   (<fftsize>,<overlap>,<window size>,<table size>,<number of bins>)
computeSizes :: Float -> Float -> (Int,Int,Int,Int,Int)  
computeSizes fund sampRate = (fftsize,overlap,winsize,tabsize,nbins)
  where
    fftsize = max (256::Int) $ 2 * round (sampRate/fund)
    overlap = fftsize `div` 4 :: Int
    winsize = fftsize
    nbins = 1 + fftsize `div` 2
    tabsize = 2 ^ (ceiling $ logBase 2 (fromIntegral nbins))

makeAnalysisCsd :: Float -> Float -> Float -> String -> String -> IO String
makeAnalysisCsd fund sampRate dur wavFilename tableDir = do
  -- compute fftsize, overlap, and winsize
  let (fftsize,overlap,winsize,tabsize,nbins) = computeSizes fund sampRate
  buf <- readFile "analysisCsdTemplate.csd"
  return $ printf buf (printf "fftsize:%d\n" fftsize `trace` fftsize)
           overlap winsize tabsize wavFilename tableDir 
           tableDir dur

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

isTxtFile = (== ".txt") . takeExtension

filenameMidiPitch :: FilePath -> Int
filenameMidiPitch s = read n
  where n = dropWhile (not . isDigit) . dropExtension $ s

removeTextFiles dir = do    
  c <- getDirectoryContents dir
  let cf = filter isTxtFile c
  putStrLn $ printf "Removing %d text files.\n" (length cf)
  forM_ cf (\filename ->
    removeFile $ dir ++ "/" ++ filename)
  
computeNumPartials :: Float -> Int
computeNumPartials freq = round $ 15000 / freq


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
      pvsTablesDir = "c:/Temp/csound/tables"
      hetFilePrefix = "pno"
      outHetFileDir = "c:/Temp/csound/hetFiles"
      sampRate = 44100
      maxPartFreq = 15000
      dur = 4.0              -- duration of analysis and duration of resulting
                             -- het file
      haskellCodeFilename = "haskell.hs"

  c <- getDirectoryContents wavDir
  let filesData :: [(String,Int,Float,Int)]
      filesData = map g (filter isWavFile c)
      g filename = (filename, midiPitch, freq, roundedFreq)
        where
          midiPitch = filenameMidiPitch filename
          freq = midiPitchToFreq midiPitch
          roundedFreq = round freq :: Int
  forM_ (take 3 filesData) (\(filename,midiPitch,fund,roundedFund) -> do
    removeTextFiles pvsTablesDir
    csd <- makeAnalysisCsd fund sampRate dur (wavDir ++ "/" ++ filename) 
           pvsTablesDir
    let fn = replaceExtension filename ".csd"
    writeFile "analysis.csd" csd
    system "csound analysis.csd"
    pvsDump <- readPvsDump pvsTablesDir
    let nParts = computeNumPartials fund 
        partCurves :: [Map Int Float]
        partCurves = map (computePartial fund pvsDump) [1..nParts]
        dur = (/ sampRate) . fromIntegral . fst . M.findMax . head $ partCurves
        hetData = computeHetroData partCurves fund sampRate dur
        hetFileName = printf "%s/%s%2d.het" outHetFileDir hetFilePrefix 
                      midiPitch
    handle <- openBinaryFile hetFileName WriteMode
    writeHetFile handle hetData
    hClose handle)
  -- write some haskell code that can map midi pitch to the actual and
  -- rounded frequencies
  let codeString = unlines $ comment ++ map oneCodeLine filesData
      oneCodeLine (_,midiPitch,freq,roundedFreq) = 
        printf "   , (%d,(%f,%d))" midiPitch freq roundedFreq
      comment = [ "-- [(midiPitch,(actualFund,roundedFundInHetFile)]"
                , "-- the prefix of het files is '" ++ hetFilePrefix ++ "'" ]
  writeFile haskellCodeFilename codeString


-- intervals
--   aural training: skip 1
--   written: none
--   keyboard: none
-- scales
--    aural training: skip 7
--    written: skip 7
--    keyboard: skip 7
-- chords
--    aural training: skip none
--    written: none
--    keyboard: none
-- rhythmic dictation
--    none
-- melodic dictation: skip 11
-- harmonic dictation: skil 10