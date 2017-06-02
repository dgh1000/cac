{-# LANGUAGE FlexibleInstances #-}
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
-- f<optional int>   move cursor forward
-- b<optional int>   move cursor back
-- p                 play vertical at cursor
-- p<N>              play N verticals starting from cursor
-- q                 quit
-- j<n>              jump to beginning of measure n 

-- Playing only some voices: -- Add string of single digits
-- Example
--   p2 1      Play 2 verticals, bottom voice only
--   p2 12     Play 2 verticals, two bottom voices
--   p2 3      Play 2 verticals, third voice from bass only
--   p  23     Play 1 vertical, second and third voices
--
-- (Do not put spaces in the [Int] list. Only one space is allowed
--  between integer after p, and list.)
--

-- Installed library imports
import Debug.Trace
import System.IO
import Control.Concurrent
import Control.Applicative
import Data.IORef
import Data.Ratio
import Text.XML.MusicXML hiding (String,State,StateT)
import Text.XML.MusicXML.Partwise 
import Sound.PortMidi
-- My imports
import qualified MusDoc.Data as MD
import MusDoc.Data(MusDoc, Loc(Loc))
import MusDoc.Processing
import MusDoc.FromXml
import Midi.FromMusDoc
import Util.FileUtil
import Util.Env
import qualified PlayData as Pd
import PlayData(PlayData(PlayData))
import Trainer1Parse 

type PlayMonad a = StateT PlayData IO a

instance Applicative (Either String) where
  pure x = Right x
  Right g <*> Right x = Right (g x)
  Right _ <*> Left s = Left s
  Left s <*> _ = Left s

main = do
  
  -- Configuration
  let --dir = "c:/Mike/Music/algo/compositions/finale/2010/"
      dir = "c:/Mike/Music/bach_midi/sinfon"
  
  putStrLn $ "Looking in directory \n  " ++ dir
  filename <- mostRecentFile dir "xml"
  putStrLn $ "Using file\n  " ++ filename
  result <- read_FILE read_MusicXML_Partwise filename 
  if isOK result 
    then run $ (toMusDoc . fromOK) result
    else print $ fromError result
    
         
-- Kick things off. Needs
--     Int: midi device number
--     Int: begin msr
--     MusDoc: the document
run :: MusDoc -> IO ()
run doc = do
  -- Open midi device
  initialize
  midiDev <- defaultMidiDevice
  result <- openOutput midiDev 1000
  case result of
    Left stream -> do 
      
      let pd = initialPlayData doc stream
      -- 'cursorPos' is IORef Int
      cursorPos <- newIORef 0
      loop pd cursorPos
      close stream
    Right err -> do print err
                    return err
  terminate
  return ()
  
loop :: PlayData -> IORef Int -> IO ()
loop pd cursorPos = do
  p <- readIORef cursorPos
  prompt pd p
  s <- getLine
  let cmd = runParse s
  case cmd of
    Left s ->           do {print s; loop pd cursorPos}
    Right Quit ->       return ()
    Right cmd  ->       do doCmd pd cmd cursorPos
                           loop pd cursorPos

prompt :: PlayData -> Int -> IO ()
prompt pd cur = do
  let Loc msrNum beat = PlayData.verts pd !! cur
  putStrLn $ "Current cursor position: msr: " ++ show msrNum ++
           " beat: " ++ niceShowRatio beat

niceShowRatio :: Integral a => Ratio a -> String
niceShowRatio inp = show intPart ++ " " ++ fracPart
  where numer = numerator inp
        denom = denominator inp
        intPart = numer `div` denom
        frac = numer `mod` denom
        fracPart = if frac==0 then ""
                              else show frac ++ "/" ++ show denom

doCmd :: PlayData -> Command -> IORef Int -> IO ()
doCmd pd cmd cursorPos = do
  case cmd of
       Forward n->               deltaCur pd cursorPos n
       Backward n ->             deltaCur pd cursorPos (-n)
       Play n vs->               playIt pd cursorPos n vs
       Jump n   ->               seekMsr pd cursorPos n
       _        ->               return ()


seekMsr :: PlayData -> IORef Int -> Int -> IO ()
seekMsr pd cur msrNum = do
      let pred :: Loc -> Bool
          pred (Loc m _) = m < msrNum
      case span pred (PlayData.verts pd) of
            (_,[])    -> putStrLn "Cannot find msr"
            (taken,_) -> writeIORef cur $ length taken
                    
deltaCur :: PlayData -> IORef Int -> Int -> IO ()
deltaCur pd cursorPos delta = do
  pos <- readIORef cursorPos
  let PlayData.PlayData { PlayData.len = len } = pd                   
  writeIORef cursorPos $ max 0 (min (pos+delta) (len-1))
  

initialPlayData :: MusDoc -> PMStream -> PlayData
initialPlayData doc str = PlayData verts len doc str
  where -- 'verts' is [Loc]
        verts = computeVertLocs doc
        len = length verts
  
-- Play composition.
--   PlayData        State needed to read composition.
--   IORef Int       Cursor position
--   Int             Count of verticals to play.
--   [Int]           Midi channels to include (if this is an empty 
--                    list, use all channels)
playIt :: PlayData -> IORef Int -> Int -> [Int] -> IO ()
playIt (PlayData verts len docIn stream) cursorPos count voices = do
  pos1 <- readIORef cursorPos
  let pos2 = min (pos1+count-1) (len-1)
      loc1 = verts !! pos1
      loc2 = verts !! pos2
      firstSplit = splitNotes loc1 docIn
      secondSplit = splitNotes (minExtendBeyond loc1 loc2 firstSplit) 
                    firstSplit
      doc1 = filterDoc loc1 loc2 secondSplit
      evts = toMidi $ case voices of
                       [] -> doc1
                       vs -> filterMidiChans vs doc1
  case evts of
       [] -> putStrLn "Nothing to play."
       es -> sendMidi stream es 200 1000

-- Look at all notes occurring between give locations loc1 and loc2.
-- Consider any that extend in location beyond loc2, and find the one
-- that extends the least in location beyond loc2. Return its end location.
minExtendBeyond :: Loc -> Loc -> MusDoc -> Loc
minExtendBeyond loc1 loc2 doc = minimum ends 
  where ends = (filter (> loc2) . map MD.end . docNotes . 
                 filterDoc loc1 loc2) doc

sendMidi :: PMStream -> [PMEvent] -> Int -> Int -> IO ()
sendMidi stream evts beginBuf dieAway = do 
  ts <- time
  let (theEvts,dur) = putAtCurrentTime ts evts beginBuf
  err <- writeEvents stream theEvts
  threadDelay $ fromIntegral $ 1000*(dur+fromIntegral dieAway)
  -- print err

-- Assumes evts are sorted.
putAtCurrentTime curTime evts beginBuf = (map g evts, dur)
  where g (PMEvent m ts) = PMEvent m (ts + delta + fromIntegral beginBuf)
        delta = curTime - timestamp (head evts)
        dur = timestamp (last evts) - timestamp (head evts)


