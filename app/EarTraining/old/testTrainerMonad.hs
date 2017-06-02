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
import Text.XML.MusicXML hiding (String,State,StateT)
import Text.XML.MusicXML.Partwise 
import Sound.PortMidi
-- My imports
import MusDoc.MusDoc
import MusDoc.Processing
import MusDoc.FromXML
import Util.FileUtil
import qualified PlayState as PlayState
import PlayState(PlayState)

data Command = Forward
             | Backward
             | Play Int [Int]
             | Quit
               deriving (Show)

data PlayData = PlayData
                { verts :: [Loc]
                , len :: Int
                , doc :: MusDoc }

main = do
  
  -- Configuration
  let dir = "c:/Mike/Music/algo/compositions/finale/2010/"
      midiDev = 10
      beginMsr = 1
  
  putStrLn $ "Looking in directory \n  " ++ dir
  filename <- mostRecentFile dir "xml"
  putStrLn $ "Using file\n  " ++ filename
  result <- read_FILE read_MusicXML_Partwise filename 
  if isOK result 
    then run midiDev beginMsr $ (fromXML . fromOK) result
    else print $ fromError result
    
         
-- Kick things off. Needs
--     Int: midi device number
--     Int: begin msr
--     MusDoc: the document
run :: Int -> Int -> MusDoc -> IO ()
run midiDev beginMsr doc = do
  -- Open midi device
  initialize
  result <- openOutput midiDev 1000
  case result of
    Left stream -> do 
      
      let (ps,initialCursor) = initialPlayData beginMsr doc
          -- 'ps' is PlayData 
          -- newPs = execStateT deltaCursorQuery ps
      -- 'cursorPos' is IORef Int
      cursorPos <- newIORef initialCursor
      loop ps cursorPos
      close stream
    Right err -> do print err
                    return err
  terminate
  return ()
  
loop :: PlayData -> IORef Int -> IO ()
loop pd cursorPos = do
  s <- getLine
  let cmd = parse s
  case cmd of
    Left s -> do {print s
    Left s -> do {print s; mainLoop' cur stream doc}
    Right Quit -> return ()
    Right cmd  -> do doCmd pd cmd cursorPos stream
                     mainLoop' cur' stream doc

-- -----> IORef <-------

{-
, i would rather use a `Monad m => State s a -> StateT s m a' conversion that changing everything in that sizable chunk to use `StateT PlayState IO a' .. of course, it depends on how likely you think it is that that part of the program may later need to do `IO' or not)



  the point is that with something like that, if you have `deltaCursor :: Int -> State PlayState ()', and you want to call this from `IO' (where you've queried the user for a `delta', say), *without* having to change `deltaCursor' to use `StateT PlayState IO' instead of `State PlayState', then you just call `toTrans (deltaCursor delta)' instead of `deltaCursor delta' from your `StateT PlayState IO' code
-}

{-
test :: Int -> MusDoc -> PlayState
test beginMsr doc =
  let ps = initialPlayState beginMsr doc
      newPs :: PlayState
      newPs = execState testMonad ps 
-}


{-
deltaCursorQuery :: (StateT PlayState IO) ()
deltaCursorQuery = do
  s <- liftIO readLine
  let i = read s
  (deltaCursor i)

deltaCursor :: Int -> (StateT PlayState a) ()
deltaCursor delta = modify g where
  g (PlayState.PlayState cur verts len doc) = 
    PlayState.PlayState (max 0 (min (cur+delta) (len-1))) verts len doc

-}


initialPlayState :: Int -> MusDoc -> (PlayData,Int)
initialPlayState msr doc = (PlayData verts len doc,cur)
  where -- 'verts' is [Loc]
        verts = computeVertLocs doc
        len = length verts
        after = dropWhile (\x -> x < Loc msr (1%1)) verts
        cur = if null after
              then error "no loc found consistent with initial msr"
              else len - length after
        
