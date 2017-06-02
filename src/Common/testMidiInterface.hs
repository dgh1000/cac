
import Sound.PortMidi hiding(name,initialize)
import Control.Monad
import Common.MidiInterface
import Common.CommonData


toNote :: (Double,Int,Int) -> MidiData
toNote (t,chan,pit) = MdNote t (t+1) 0 chan pit 64



notes :: [MidiData]
notes = map toNote $ take 20 $ zip3 [0,0.25..] (cycle [1,2]) (repeat 1)

peds = [ MdCtrl 0.0 0 1 64 0  
       , MdCtrl 1.0 0 1 64 127
       , MdCtrl 2.0 0 1 64 0   ]

main = do
  putStrLn $ "start midi pipe, and PIANOTEQ. this will " ++
             "use stream 4, play notes every 0.25 seconds. 1st, 3rd, 5th" ++
             ", etc. notes will go to chan 1. Notes in-between to channel " ++
             " 2. pedal controls will be sent to chan 1 only"
  s1 <- startMidi 4 4
  case s1 of
    Left err -> putStrLn $ show err
    Right streams -> do
      t <- fromIntegral `liftM` time 
      playRawEvents streams (t+fromIntegral 200) $ peds ++ notes
      stopMidi streams

  

