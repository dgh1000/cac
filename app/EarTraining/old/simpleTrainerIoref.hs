-- Ear-training program which allows step forward and backward 
-- from vertical to vertical.
--
--     Note: commands require press of key and enter.
--
-- To run: 
--     Edit this file to set configuration
--     On command line, trainer1.hs
--     Or in ghci, main
-- 
-- Cursor starts at first beat of begin msr (or closest one)
--
-- f            move cursor forward
-- b            move cursor back
-- p            play vertical at cursor
-- p<N>         play N verticals starting from cursor
-- q            quit
;;
-- Playing only some voices: -- Add [Int] to input line
-- Example
--   p2 [1]     Play 2 verticals, bottom voice only
--   p2 [1,2]   Play 2 verticals, two bottom voices
--   p2 [3]     Play 2 verticals, third voice from bass only
--
-- (Do not put spaces in the [Int] list. Only one space is allowed
--  between integer after p, and list.)
--

-- Installed library imports
import System.IO
import Control.Concurrent
import Debug.Trace
import Data.IORef
import Data.Ratio
import Text.XML.MusicXML hiding (String,State,StateT)
import Text.XML.MusicXML.Partwise 
import Sound.PortMidi
-- My imports
import MusDoc.MusDoc
import MusDoc.Processing
import MusDoc.FromXML
import Midi.FromMusDoc
import Util.FileUtil
import qualified PlayData as PlayData
import PlayData(PlayData)

data Command = Forward
             | Backward
             | Play Int [Int]
             | Quit
               deriving (Show)

main = do
  
  -- Configuration
  let dir = "c:/Mike/Music/algo/compositions/finale/2010/"
      midiDev = 10
      beginMsr = 1
  
  initialize
  result1 <- openOutput midiDev 1000
  case result1 of 
    Left stream -> do
  
      putStrLn $ "Looking in directory \n  " ++ dir
      filename <- mostRecentFile dir "xml"
      putStrLn $ "Using file\n  " ++ filename
      result <- read_FILE read_MusicXML_Partwise filename 
      if isOK result 
         then run stream beginMsr $ (toMidi . fromXML . fromOK) result
         else print $ fromError result
      close stream
    Right err -> do {print err; return err}
  terminate
    
         
-- Kick things off. Needs
--     Int: midi device number
--     Int: begin msr
--     MusDoc: the document
run :: PMStream -> Int -> [PMEvent] -> IO ()
run stream beginMsr doc = do
  -- Open midi device
  {-
  initialize
  result <- openOutput midiDev 1000
  case result of
    Left stream -> do 
      playIt doc stream
      close stream
    Right err -> do print err
                    return err
  terminate
  -}
  playIt doc stream
  return ()
  
{-
initialPlayData :: Int -> MusDoc -> PMStream -> (PlayData,Int)
initialPlayData msr doc str = (PlayData.PlayData verts len doc str,cur)
  where -- 'verts' is [Loc]
        verts = computeVertLocs doc
        len = length verts
        after = dropWhile (\x -> x < Loc msr (1%1)) verts
        cur = if null after
              then error "no loc found consistent with initial msr"
              else len - length after
-}

        
parse :: String -> Either String Command
parse s = case s of
  [] -> Left "Empty command"
  "q" -> Right Quit
  "f" -> Right Forward
  "b" -> Right Backward
  'p':remainder -> parseP remainder
  _   -> Left "Unrecognized command"          

  
parseP :: String -> Either String Command
parseP s = 
  case s of 
    [] -> Right $ Play 1 []
    _  -> case words s of 
            [] -> Left "Bad command, ends with whitespace but no args"
            [p1] -> Right $ Play (read p1) []
            [p1,p2] -> Right $ Play (read p1) (read p2)
            _       -> Left "Bad command, too many args"

playIt :: [PMEvent] -> PMStream -> IO ()
playIt evts stream = do
  -- let evts = toMidi doc
  sendMidi stream evts 1000 1000

sendMidi :: PMStream -> [PMEvent] -> Int -> Int -> IO ()
sendMidi stream evts beginBuf dieAway = do 
  ts' <- time
  let ts = ("\ncur time:" ++ show ts' ++ "\n") `trace` ts'
  let (theEvts,dur) = putAtCurrentTime ts evts beginBuf
  err <- writeEvents stream theEvts
  putStrLn $ "Number of events is: " ++ show (length theEvts)
  threadDelay $ fromIntegral $ 1000*(dur+fromIntegral dieAway)
  -- print err

-- Assumes evts are sorted.
putAtCurrentTime curTime evts beginBuf = (map g evts, dur)
  where g (PMEvent m ts) = PMEvent m (ts + delta + fromIntegral beginBuf)
        delta = ("\n" ++ show (delta' + fromIntegral beginBuf)) `trace` delta'
        delta' = curTime - timestamp (head evts)
        dur = timestamp (last evts) - timestamp (head evts) 


-- -----> IORef <-------

{-
, i would rather use a `Monad m => State s a -> StateT s m a' conversion that changing everything in that sizable chunk to use `StateT PlayState IO a' .. of course, it depends on how likely you think it is that that part of the program may later need to do `IO' or not)



  the point is that with something like that, if you have `deltaCursor :: Int -> State PlayState ()', and you want to call this from `IO' (where you've queried the user for a `delta', say), *without* having to change `deltaCursor' to use `StateT PlayState IO' instead of `State PlayState', then you just call `toTrans (deltaCursor delta)' instead of `deltaCursor delta' from your `StateT PlayState IO' code
-}

