

module PlaybackCac.SimplePlay where

import Util.MonadStacks
import Cac.CacData
import Util.RandomState
import Playback.PlaybackUtil
import CacToCsound.CacToCsound
import CacToCsound.CacToCsoundData
import Csound.MakeCsd

playCompToCsound :: CacToCsMap -> ErrorRand Comp -> IO ()
playCompToCsound conv theComputation = do
  let masterOrcFN = "c:/Users/Mike/crit/Music/algo/haskell/" ++
                     "CacToCsound/masterOrc.csd"
      options = "-d -W -o/temp/out.wav"
  masterOrc <- readFile masterOrcFN
  rd <- freshRandData
  case runER theComputation rd of
    ((Left err,_),_) -> putStrLn err
    ((Right comp,log),_) -> do
       -- putStrLn . showItem . showi $ nodes
       putStrLn log
       case runEW (theConversion conv masterOrc options comp) of
         (Left err,_) -> putStrLn err
         (Right csd,log) -> do
           putStrLn log
           playCsd csd
           -- justWriteCsd csd
           putStrLn "Done."

theConversion :: CacToCsMap -> String -> String -> Comp -> ErrWri String
theConversion conv masterOrc options comp = do
  cnotes <- compToCsoundNotes conv comp
  makeCsd masterOrc options cnotes
