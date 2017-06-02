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
import Data.Ratio
import Text.XML.MusicXML hiding (String)
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
--     Score_Partwise: the document
run :: Int -> Int -> MusDoc -> IO ()
run midiDev beginMsr doc = do
  -- Open midi device
  initialize
  result <- openOutput midiDev 1000
  case result of
    Left stream -> do 
      mainLoop beginMsr stream doc
      close stream
    Right err -> do print err
                    return err
  terminate
  return ()
  
-- Loop with commands until terminate
--   Int: first measure
--   PMStream
--   Score_Partwise
mainLoop :: Int -> PMStream -> MusDoc -> IO ()
mainLoop msr stream doc = do
  let cur = initializeCursor msr doc
  mainLoop' cur stream doc

  
-- Loop with commands until terminate.
--   Loc: current value of cursor
--   PMStream
--   Score_Partwise
mainLoop' :: Loc  -> PMStream -> MusDoc -> IO ()
mainLoop' cur stream doc = do
  s <- getLine
  let cmd = parse s
  case cmd of
    Left s -> do {print s; mainLoop' cur stream doc}
    Right Quit -> return ()
    Right cmd  -> do cur' <- doCmd cmd cur stream doc
                     mainLoop' cur' stream doc

-- Find initial position of cursor in MusDoc. This will
-- be nearest location at or beyond Loc <input measure> (1%1)
--  
--     Int :: initial measure
--     MusDoc
initializeCursor :: Int -> MusDoc -> Loc
initializeCursor msr doc = 
  let -- 'verticals' is [Loc]
      verticals = computeVertLocs doc
      -- Get rid of anything before measure 'msr'
      whatsLeft = dropWhile (\x -> x < Loc msr (1%1)) verticals
  in if null whatsLeft
        then error $ "No verticals at measure at or after msr " ++ show msr
        else head whatsLeft

-- Carry out a command.
--   Command
--   Loc: current cursor position
--   PMStream
--   MusDoc
--
-- The result is the new cursor position.
doCmd :: Command -> Loc -> PMStream -> MusDoc -> IO Loc
doCmd cmd cur _ _ = do
  putStrLn $ show cmd ++ " Cursor:" ++ show cur
  return $ Loc 1 (1%1)

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


