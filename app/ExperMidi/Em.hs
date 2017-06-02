
import Control.Concurrent
import System.IO
import Foreign.C.Types
import Sound.PortMidi
import Midi.Interface
import Midi.MidiExport

{-
main = loop


loop = do
  putStr "Enter q, m<port number>:"
  hFlush stdout
  li <- getLine
  case li of
    'm' : rs -> 
       case reads rs of
         [] -> do { putStrLn "Something wrong in port number." ; loop  }
         (i,_):_ -> do { topLevel i ; loop } -- XXXXX
    "q" -> return ()
    _ -> loop

topLevel :: Int -> IO ()
topLevel deviceNum = realTimeMidiRun loop deviceNum

data LoopContext = LoopContext
  { lcStream :: PMStream
  , lcTid :: Maybe ThreadId }

instance MidiLoopContext LoopContext where
  mkInitMidiLoopContext stream = LoopContext stream Nothing

loop :: LoopContext -> IO ()
loop con = do
  putStr "<enter to play>" 
  hFlush stdout
  getLine
-}

-- (time and dur in ms)
-- time, dur, pitch, maybe keyswitch, chan
type NoteTuple = (Integer,Integer,Int,Maybe Int,Int) 

run deviceNum whichNoteList = do
  initialize
  result <- openOutput deviceNum 0
  case result of
    Left stream -> do
      tid <- sendEvts whichNoteList stream
      putStr "Press enter to quit:"
      hFlush stdout
      getLine
      killThread tid
      allNotesOffAllChans stream
      close stream
      terminate
      return ()
    Right err -> do
      putStrLn  $ "Error opening midi device.\n" ++ show err

sendEvts :: Int -> PMStream -> IO ThreadId
sendEvts whichNoteList stream = do
  let nl = noteLists !! whichNoteList
      mes = mkMidiEventList nl
      rmes = mkRawEvents 100 100 mes
  beginTime <- time
  forkIO $ 
    playRawEvents stream (beginTime + 200) rmes



mkMidiEventList :: [NoteTuple] -> [MidiEvent]
mkMidiEventList nl = map (\(a,b,c,d,e) -> mkNoteEvent a b c d e) nl

mkNoteEvent :: Integer -> Integer -> Int -> Maybe Int -> Int -> MidiEvent
mkNoteEvent time dur pit mKeySwitch chan =
  NoteEvent (MidiTime time)
            (RawMidiEvent chan 0x90 pit 64)
            (RawMidiEvent chan 0x80 pit 64)
            (MidiTime (time+dur))
            keySwitch
  where
    keySwitch = case mKeySwitch of
      Nothing -> []
      Just x -> [RawMidiEvent chan 0x90 x 64]

noteLists = [noteList0,noteList1,noteList2,noteList3,noteList4]

-- middle C to midi channel 1
noteList0 :: [NoteTuple] 
 -- [(time, dur, pitch, ks, chan)]
-- tuba
noteList0 = 
  [ (200, 1000, 60, Just 79, 1)]

-- tbone
noteList1 = 
  [ (200, 4000, 48, Just 89, 2)]

-- trumpet
noteList2 = 
  [ (200, 4000, 60, Just 27, 3)]


-- trumpet
noteList3 = 
  [ (200, 4000, 72, Just 29, 4)]
  

noteList4 = 
  [ (200, 4000, 60, Just 74, 1)
  , (200, 4000, 48, Just 86, 2)
  , (200, 4000, 60, Just 29, 3)
  , (200, 4000, 72, Just 24, 4) ]
