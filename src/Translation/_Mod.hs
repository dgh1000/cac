module Translation.Mod where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Writer
import Data.Array.Unboxed
import Common.CommonExport
import Translation.TranslationData
import Translation.TimeMap(lookupTime)
import Translation.Dynamics(toD)
import Translation.Lookup
import Translation.TranslationUtil
import Midi.MidiData



-- half-width of smoothing window in number of control points
config_smoothWindow = 2 :: Int


writeMod :: Loc -> Loc -> String -> Tr EventsRecord
writeMod beg end staffName = do
  c <- iModConfig `liftM` getInstrTr staffName
  case c of
    Nothing -> return $ ModEventsRecord staffName []
    Just config@(ModConfig dest contrs valueFn spacing lead) -> do
      Just loudFn <- M.lookup 1 `liftM` getLoudnessFuncsTr staffName
      tm          <- getTimeMapTr staffName
      let tBeg    =  lookupTime beg tm
          tEnd    =  lookupTime end tm
          (nIndices,idxToTime) = computeIndices spacing tBeg tEnd
      return . 
        ModEventsRecord staffName .
        concatMap (\(idx,loudness) -> 
          makeModMidi staffName config (toMs $ idxToTime idx) loudness) .
        assocs . smoothArray config_smoothWindow $
        mkLoudArray (toD loudFn) nIndices idxToTime

-- this looks like takes a function to compute the MIDI control value. /n/. p;
mkLoudArray :: (Double -> Double) -> Int -> (Int -> Double) -> 
               UArray Int Double
mkLoudArray loudFn n idxToTime = listArray (0,n-1) 
  [loudFn (idxToTime i) | i <- [0..n-1]]


toMs x = round $ 1000*x

smoothArray :: Int -> UArray Int Double -> UArray Int Double
smoothArray halfWindow arrayIn = listArray (lo,hi) . map g $ [lo..hi]
  where
    (lo,hi) = bounds arrayIn
    g i = (/nSum) . sum . map (arrayIn!) $ [loSum..hiSum]
      where
        loSum = max lo (i-halfWindow)
        hiSum = min hi (i+halfWindow)
        nSum = fromIntegral (hiSum-loSum+1) :: Double


computeIndices :: Double -> Double -> Double -> (Int,Int -> Double)
computeIndices spacing tBeg tEnd = (n,toTime)
  where
    n :: Int
    n = round $ (tEnd - tBeg)/spacing
    toTime x = fromIntegral x * spacing + tBeg


makeModMidi :: String -> ModConfig -> Integer -> Double -> [MidiEvent]
makeModMidi stName (ModConfig (stream,chan) contrs fn _ lead) t loudness = 
    -- 'fn loudness' will produce a list of MIDI controller data values, one
    --     for each controller number in 'contrs'
    -- 'contrs' is a list of MIDI controller numbers
    -- 
    zipWith g (fn loudness) contrs
  where
    g :: Int -> Int -> MidiEvent
    g value controller = SingleEvent t False
       (RawMidiEvent stream chan 0xb0 controller value) stName
