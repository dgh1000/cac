
import System.Random
import Text.Printf
import Util.Math
import Control.Monad

{-
oneFTable :: Int -> Int -> Double -> String
oneFTable size pitch maxFreq = 
  printf "git%d ftgen %d, 0, %d, 10" pitch pitch size ++ harmonics ++ "\n"
  where
    maxHarmonic = floor $ maxFreq / fund
    fund = midiPitchToCps pitch
    -- 
    -- scale the exponent on the amplitude ratio so that
    --   27 maps   to expMin -- the least rate of decrease of the harmonics
    --   4186 maps to expMax
    -- in a logarithmic way
    --
    expMin = 1.25  -- for low notes
    expMax = 1.35  -- for high notes
    exponent = exp $ scale (log 27) (log fund) (log 4186) (log 1.2) (log 1.3)
    ampl h  = 1 / (fromIntegral h ** exponent) :: Double
    harmonics = concatMap (\h -> printf ",%.5f" (ampl h)) [1..maxHarmonic]


-- Given the approximate strength of a partial, compute a random actual
-- strength and phase. Actual strength will be between strength/ratio and
-- strength*ratio. Phase will be 0 to 359.
onePartial :: Double -> Double -> Bool -> IO (Double,Double)
onePartial ratio strength phaseFlag = do
  let minStr = strength/ratio
      maxStr = strength*ratio
  actualStr <- getStdRandom $ randomR (minStr,maxStr)
  phase     <- if phaseFlag 
                 then getStdRandom $ randomR (0, 359)
                 else return 0
  return (actualStr,phase)

onePartial2 :: Double -> Int -> Int -> (Double,Double)
onePartial2 strength n maxPartials = 
  (strength, fromIntegral n/ fromIntegral maxPartials)


allPartials :: Double -> Double -> IO [(Int,Double,Double)]
allPartials fund maxFreq = do
  let g n = do
        let f = fromIntegral n * fund
            strength = 1 / fromIntegral n ** 1.1
        (actualStrength,phase) <- 
          if f < 2000
            then onePartial 1.1 strength False
            else onePartial 1.5 strength True
        return (n,actualStrength,phase)
      nPartials = floor $ maxFreq / fund
  forM [1..nPartials] g


allPartialsV2 :: Double -> Double -> IO [(Int,Double,Double)]
allPartialsV2 fund maxFreq = do
  let g n = do
        let f = fromIntegral n * fund
            strength = 1 / fromIntegral n
        zeroFlag <- getStdRandom $ randomR (0,1::Double)
        let str | f <= 4000      = strength
                | zeroFlag < 0.3 = 0
                | otherwise      = strength
        (actualStrength,phase) <- 
          if f < 2000
            then onePartial 1.1 str False
            else onePartial 1.5 str True
        return (n,actualStrength,phase)
      nPartials = floor $ maxFreq / fund
  forM [1..nPartials] g


allPartialsV3 :: Double -> Double -> IO [(Int,Double,Double)]
allPartialsV3 fund maxFreq = do
  let nPartials = floor $ maxFreq / fund
      g n = do
        let (s,p) = onePartial2 (1/ fromIntegral n) n nPartials
        lowFlag <- getStdRandom $ randomR (0,1::Double)
        let freq = fromIntegral n * fund
        let actualStrength | freq < 1000  = s
                           | lowFlag < 0.5 = s/3
                           | otherwise = s
        return (n, actualStrength, p)
  mapM g [1..nPartials] 


allPartialsV4 :: Double -> Double -> IO [(Int,Double,Double)]
allPartialsV4 fund maxFreq = map g [1..nPartials]
  where
    strengths = neatStrengths nPartials
    nPartials = floor $ maxFreq / fund
    g n = let (stren,phs) = onePartial2 stren n nPartials
          in  (n,stren,phs)



oneF11Table :: Int -> Int -> Double -> IO String
oneF11Table size pitch maxFreq = do
  let fund = midiPitchToCps pitch
      toString :: (Int,Double,Double) -> String
      toString (n,strength,phase) = printf ",%d,%.5f,%.1f" n strength (360* phase)
  let ps = allPartialsV4 fund maxFreq
  return $ printf "git%d ftgen %d, 0, %d, -9 %s\n"
           pitch pitch size (concatMap toString ps)


main2 = do
  let maxFreq   = 10000
      size = 32769 -- 32769, 65537, 262145
  
  writeFile "tables.txt" $concatMap (\p -> oneFTable size p maxFreq) 
                [24,36..120]


-}

----------------------------------------------------------------------
-- compute waveform


waveform :: Int -> [(Int,Double,Double)] -> [(Int,Double)]
waveform nPoints partials = 
    zip [1..] $ foldr1 (zipWith (+)) (map oneSine partials)
  where
    oneSine :: (Int,Double,Double) -> [Double]
    oneSine (partialNumber,strength,phase) = map g [0.. nPoints-1]
      where
        g :: Int -> Double
        g idx = strength * (sin $ 2*pi*phaseOut)
          where
            phaseOut = fromIntegral partialNumber *
                       (phase + fromIntegral idx / fromIntegral nPoints)

{-

  phase by partial number. we want phaseOut to start at phase 

-}


----------------------------------------------------------------------


midiPitchToCps :: Int -> Double
midiPitchToCps p = 440 * 2 ** ((fromIntegral p - 69)/12)


----------------------------------------------------------------------

----------------------------------------------------------------------
--  computePartials1: uses varying exponential decay of partial strength


partialStrengths1 :: Int -> Double -> Double -> [Double]
partialStrengths1 nPartials initialDecay endDecay = scanl g 1.0 [1..] 
  where
    g :: Double -> Int -> Double
    g strengthIn n = r * strengthIn
      where
        r = exp $ scale 1 (fromIntegral n) (fromIntegral nPartials) 
                        (log initialDecay) (log endDecay)


computePartials1 :: Double -> Double -> IO [(Int,Double,Double)]
computePartials1 freq bandwidth = do
  let begDecay      = 0.75
      endDecay      = 0.9999


      nPartials     = floor $ bandwidth / freq
      g :: Int -> Double -> (Int,Double,Double)
      g n strength = (n,strength,phase)
        where
          phase = fromIntegral n / fromIntegral nPartials 
      x :: [Int]
      x = [1..nPartials]
      y :: [Double]
      y = partialStrengths1 nPartials begDecay endDecay
      z :: [(Int,Double,Double)]
      z = zipWith g x y
  return z -- $ zipWith g [1.. nPartials]       
           --         (partialStrengths1 nPartials begDecay endDecay)


----------------------------------------------------------------------
-- computePartials2: formula like 1/(n/2)**1.2, with flag for spreading phase
--   linearally by partial number


computePartials2 :: Bool -> Double -> Double -> IO [(Int,Double,Double)]
computePartials2 phaseFlag freq bandwidth = do
  let nPartials = floor $ bandwidth / freq
      strengthN n = 1/fromIntegral n ** 1.6
      phaseN n | phaseFlag = fromIntegral n / fromIntegral nPartials
               | otherwise = 0
  return $ map (\n -> (n, strengthN n, phaseN n)) [1..nPartials::Int]



----------------------------------------------------------------------
-- computePartials3: formula like 1/(n/2)**1.2, with flag for spreading phase
--   logarithmally by frequency


computePartials3 :: Bool -> Double -> Double -> IO [(Int,Double,Double)]
computePartials3 phaseFlag freq bandwidth = do
  let nPartials = floor $ bandwidth / freq
      strengthN n = 1/fromIntegral n ** 1.1
      phaseN n 
        -- spread so that log of harmonic number is spread between 
        -- 
        | phaseFlag = 
            let nd = fromIntegral nPartials
                ratio = fromIntegral n / nd
            in  scale 0 (log ratio) (log nd) 0 360
        | otherwise = 0
  return $ map (\n -> (n, strengthN n, phaseN n)) [1..nPartials::Int]



----------------------------------------------------------------------

computeTable :: Int -> Int -> [(Int,Double,Double)] -> String
computeTable size pitch ps = printf "git%d ftgen %d,0,%d,-9 %s\n"
                             pitch pitch size (concatMap g ps)
  where
    g :: (Int,Double,Double) -> String
    g (n,strength,phase) = printf ",%d,%.5f,%.1f  " n strength (360*phase)


main = do
  -- configuration
  let bandwidth  = 15000
      size       = 16385 -- 8192 16385 32769, 65537, 262145
      pitches    = [36,48..96] :: [Int]
      method     = computePartials2 True
      plotFreq   = 32

      
      oneTable :: Int -> IO String
      oneTable p = do
        ps <- method (midiPitchToCps p) bandwidth
        return $ computeTable size p ps
  tables <- forM pitches oneTable
  plotPartials <- method plotFreq bandwidth
  let onePartialPlotLine :: (Int,Double,Double) -> String
      onePartialPlotLine (n,strength,_) = printf "%d %f\n" n strength
      partialsPlotText = concatMap onePartialPlotLine  plotPartials
      oneWaveformLine :: (Int,Double) -> String
      oneWaveformLine (pt,signal) = printf "%d %f\n" pt signal 
      waveformText = concatMap oneWaveformLine (waveform 1000 plotPartials)
  writeFile "waveform.txt" waveformText
  writeFile "partialsPlot.txt" partialsPlotText
  writeFile "tables.txt" $ concat tables
  writeFile "ftnumbers.txt" $ concatMap (\n -> printf ", %d" n) pitches

