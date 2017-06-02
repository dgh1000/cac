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
import Control.Monad.State
import System.IO
import Data.Ratio
import Text.XML.MusicXML hiding (String,State)
import Text.XML.MusicXML.Partwise 
import Sound.PortMidi
-- My imports
import MusDoc.MusDoc
import MusDoc.Processing
import MusDoc.FromXML
import Util.FileUtil

data Command = Forward
             | Backward
             | Play Int [Int]
             | Quit
               deriving (Show)

data PlayState = PlayState 
                 { playState_cur :: Int
                 , playState_verts :: [Loc]
                 , playState_len :: Int
                 , playState_doc :: MusDoc
                 }

run :: Int -> MusDoc -> PlayState
run beginMsr doc =
  let ps = initialPlayState beginMsr doc
  in execState testMonad ps 


playState_update_cur :: Int -> PlayState -> PlayState
playState_update_cur i p = p {playState_cur=i}

testMonad :: State PlayState ()
testMonad = do
  cur <- gets playState_cur
  len <- gets playState_len
  let newCur = if cur < len-1
               then cur+1
               else cur
  modify (playState_update_cur newCur)
  

initialPlayState :: Int -> MusDoc -> PlayState
initialPlayState msr doc = PlayState cur verts len doc
  where -- 'verts' is [Loc]
        verts = computeVertLocs doc
        len = length verts
        before = takeWhile (\x -> x < Loc msr (1%1)) verts
        cur = length before 
        