{-# LANGUAGE FlexibleInstances #-}

-- The purpose of this file: 
--   modifying trainer1 so that 'c' arguments refer to midi channel
--
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

-- Playing only some midi channels: -- Add string of single digits
-- Example
--   p2 c1      Play 2 verticals, bottom voice only
--   p2 c12     Play 2 verticals, two bottom voices
--   p2 c3      Play 2 verticals, third voice from bass only
--   p  c23     Play 1 vertical, second and third voices
--
--

-- Installed library imports
import Debug.Trace
import System.IO
import Control.Applicative
import Control.Monad.State
import Data.IORef
import Data.Ratio
import Text.XML.MusicXML hiding (String,State,StateT)
import Text.XML.MusicXML.Partwise 
import Sound.PortMidi
-- My imports
import qualified MusDoc.Data as MD
import MusDoc.Data(MusDoc, Loc(Loc), Note(Note))
import MusDoc.Processing
import MusDoc.FromXml
import Midi.FromMusDoc
import Midi.Operate
import Util.FileUtil
import Util.Env
import PlayState(PlayState(PlayState))
import qualified PlayState as PS
import Trainer2Parse 

type PlayMonad a = StateT PlayState IO a

instance Applicative (Either String) where
  pure x = Right x
  Right g <*> Right x = Right (g x)
  Right _ <*> Left s = Left s
  Left s <*> _ = Left s

main = do
  
  -- Configuration
  let --dir = "c:/Mike/Music/algo/compositions/finale/2010/"
      dir = "c:/Mike/Music/algo/compositions/finale/2010"
  
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
  print $ "midi device: " ++ show midiDev
  result <- openOutput midiDev 1000
  case result of
    Left stream -> do 
      let ps = initialPlayState doc stream
      evalStateT loop ps
      close stream
    Right err -> do 
      print err
      return err
  terminate
  return ()
  
initialPlayState :: MusDoc -> PMStream -> PlayState
initialPlayState doc str = 
  let verts = computeVertLocs doc
      len = length verts
  in PlayState verts len doc str 0

loop :: PlayMonad ()
loop = do
  prompt
  s <- liftIO getLine
  let cmd = runParse s
  case cmd of
    Left s ->           do {liftIO $ print s; loop}
    Right Quit ->       return ()
    Right cmd  ->       do doCmd cmd
                           loop


prompt :: PlayMonad ()
prompt = do
  PlayState verts _ _ _ cur <- get
  let Loc msrNum beat = verts !! cur
  liftIO $ putStrLn $ "Current cursor position: msr: " ++ show msrNum ++
                      " beat: " ++ niceShowRatio beat


niceShowRatio :: Integral a => Ratio a -> String
niceShowRatio inp = show intPart ++ " " ++ fracPart
  where numer = numerator inp
        denom = denominator inp
        intPart = numer `div` denom
        frac = numer `mod` denom
        fracPart = if frac==0 then ""
                              else show frac ++ "/" ++ show denom


doCmd :: Command -> PlayMonad ()
doCmd cmd = do
  case cmd of
       Forward n->               deltaCur n
       Backward n ->             deltaCur (-n)
       Play n vs->               playIt n vs
       Jump n   ->               seekMsr n
       _        ->               return ()


seekMsr :: Int -> PlayMonad ()
seekMsr msrNum = do
  verts <- gets PS.verts
  len <- gets PS.len
  let pred :: Loc -> Bool
      pred (Loc m _) = m < msrNum
  let l = (length . takeWhile pred) verts
  if l == len 
     then liftIO $ putStrLn "Cannot find msr"
     else modify (\ps -> ps{PS.cursor=l})     

deltaCur :: Int -> PlayMonad ()
deltaCur delta = 
  modify (\ps@PlayState{PS.len=len, PS.cursor=cursor} ->
           ps{PS.cursor = max 0 (min (cursor+delta) (len-1))})
  

-- Play composition.
--   Int             Count of verticals to play.
--   [Int]           Midi channels to include (if this is an empty 
--                    list, use all channels)
playIt :: Int -> [Int] -> PlayMonad ()
playIt count voices = do
  PlayState verts len docIn stream pos1 <- get
  let pos2 = min (pos1+count-1) (len-1)
      loc1 = verts !! pos1
      loc2 = verts !! pos2
      docOut = convenientTrim loc1 loc2 docIn
      evts = toMidi $ case voices of
        [] -> docOut
        _ -> filterMidiChans voices docOut
  case evts of
       [] -> liftIO $ putStrLn "Nothing to play."
       _  -> liftIO $ sendMidi stream evts 200 1000


-- 
-- |------------|
--     |----------|
--          |---------|



