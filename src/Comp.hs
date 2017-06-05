
module Comp where

import System.Random
import Sound.PortMidi
import Control.Monad
import Control.Monad.State
import Data.Map
import Data.Maybe
import Midi.MidiData
import Midi.Interface
import Util.Exception
import Cac

-- hierarchical


data Beat = Beat Rational (Maybe Beat)

{-
data Note2 = Note2
  { nTBeg :: Beat
  , nTEnd :: Beat
  , nDest :: (Int,Int)
  , nPit  :: Int
  }
  deriving(Show)
-}  

data Note = Note
  { nTBeg :: Double
  , nTEnd :: Double
  , nDest :: (Int,Int)
  , nPit  :: Int
  }
  deriving(Show)

data Comp = Comp
  { cNotes :: Map Double Note
  }


-- generate random notes within Rio

-- where should I go from here?
--
--   generate compositions via search
--
--   what algorithms do I need other than search? is there any other way I plan to generate them?
--
--   utilities that will be needed? like export to Sibelius, import from Sibelius? well it will
--   be music XML
--
--   make Qt browser: we don't know yet if we need that
--
--   to export to MusicXml we need to have a time model: beats of a measure: 

genRandomNotes :: Rio Note
genRandomNotes = do
  rioChoose [Note 0 1 (0,1) 60]


g = do
  gen <- newStdGen
  out <- evalStateT genRandomNotes gen
  print out


{-
data MidiShort = MidiShort
  { msTime   :: Double
  , msStream :: Int
  , msChan   :: Int
  , msStatus :: Int
  , msData1  :: Int
  , msData2  :: Int
  }
-}

toMidi :: Note -> [Short]
toMidi (Note tBeg tEnd (str,chan) pit) = [ Short tBeg str (chan+0x90-1) pit 64
                                         , Short tEnd str (chan+0x80-1) pit 64 ]

                                 

main = do
  result <- promptForMidi
  case result of
    Left err -> print err
    Right str -> putStrLn "success!"

main2 :: IO ()
main2 = do
  dev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing dev) (throwMine "MidiPipe Input 3 is not present")
  eStreams <- startMidi (fromJust dev) (fromJust dev+ 0 )
  case eStreams of
    Left err -> putStrLn $ show err
    Right streams -> do
      let notes = [Note 1 3 (0,1) 60]
          shorts = concatMap toMidi notes
      t <- time
      playRawEvents streams (fromIntegral t+200) shorts
      allOff streams
      stopMidi streams
      return ()
      
testRio :: Rio Int
testRio = return 3

main3 :: IO ()
main3 = do
  gen <- newStdGen
  out <- runStateT testRio gen
  print out
  
  
