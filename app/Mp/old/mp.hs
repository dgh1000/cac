 
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Text.Parsec.ByteString as TPB
import qualified Text.Parsec as TP
import qualified Control.Exception as E
import Control.DeepSeq
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad
import Debug.Trace
import System.IO
import System.Random
import System.Directory
import System.Process
import System.FilePath
import System.Console.Haskeline
import Data.Map(Map)
import Data.Either
import Data.Maybe
import Data.Set(Set)
import Sound.PortMidi
import Text.XML.Light
import Text.Printf
import Midi.MidiData
import Midi.Interface
import Midi.ShowMidi
import App.Mp.MpData
-- import App.Mp.ParseConfig
import App.Mp.ParseCommandLine
import Translation.Translation
import Translation.TranslationData
import Translation.ShowTranslation
import Translation.ParseConfig
-- import Translation.MakeRaw
-- import Translation.MakePattern
import Translation.ShowTranslation
-- import Translation.Lookup(lookupInstrument)
import Util.Exception
import Util.FileUtil
import Util.Showable
import XmlDoc.ParseXml
import Score.XmlToScore
import Score.ScoreData
import Score.ShowScore

----------------------------------------------------------------------
----------------------------------------------------------------------
--              data and configuration


global_xmlInputFileName = "/Users/Mike/out.xml"
global_msInitialDelay = 20 -- milliseconds

data LoopContext = LoopContext
  { lcStreams    :: Maybe [PMStream]    -- needed if using MIDI playback.
                                        -- This will be Nothing if using
                                        -- Csound playback
  , lcThreadID   :: Maybe ThreadId
  , lcTempoRatio :: Double
  , lcTranspose  :: Int }


hardCodedDirectories = 
    [
      "/Users/Mike/Dropbox/common/trad-comp/2014/dec"
    , "/Users/Mike/Dropbox/common/trad-comp/2014/oct"
    , "/Users/Mike/Dropbox/common/trad-comp/2014/sept"
    , "/Users/Mike/Dropbox/common/trad-comp/2014/sibelius-experiments"
    , "/Users/Mike/Dropbox/common/trad-comp/2014/testing"
    , "/Users/Mike/Dropbox/common/trad-comp/2015/01-jan"
    , "/Users/Mike/Dropbox/common/trad-comp/2015/07-july"
    , "/Users/Mike/Dropbox/common/trad-comp/2015/rhythm"
    , "/Users/Mike/Dropbox/common/trad-comp/2015-2/fast"        -- wd2
    , "/Users/Mike/Dropbox/common/trad-comp/2015-2/slane"       -- wd8
    , "/Users/Mike/Dropbox/common/trad-comp/2015-2/waifie"      -- wd3
    , "/Users/Mike/Dropbox/common/trad-comp/2015-2/organ"       -- wd4
    , "/Users/Mike/Dropbox/common/trad-comp/2016-2/waifie-life" -- wd6
    , "/Users/Mike/Dropbox/common/trad-comp/2016-2/faster"      -- wd7
    , "/Users/Mike/Dropbox/common/trad-comp/2016-2/matt"      
    , "/Users/Mike/Dropbox/common/trad-comp/2016-2/simple"      
    , "/Users/Mike/Dropbox/common/trad-comp/2016-2/ql"
    , "/Users/Mike/Dropbox/common/trad-comp/2016-2/atonal"
    , "/Users/Mike/Dropbox/common/trad-comp/2016/tonal"            -- wd9
    , "/Users/Mike/Dropbox/common/trad-comp/2016/test"            -- wd9
    ]


----------------------------------------------------------------------
----------------------------------------------------------------------

{-

haskeline example:

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing     -> return ()
        Just "quit" -> return ()
        Just input  -> do outputStrLn $ "Input was: " ++ input
                          loop



-}

----------------------------------------------------------------------
----------------------------------------------------------------------


decideInputOrOutput di = if input di then "Input :" else "Output:"


showMidiDevice :: Int -> IO ()
showMidiDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (name di) 


main = do
  c <- countDevices
  mapM_ showMidiDevice [0..c-1]
  putStr "Enter midi port number or hit enter for Csound:"
  hFlush stdout
  li <- getLine
  if null li
    then do
      putStrLn "Csound version not currently working."
      -- loop (LoopContext Nothing Nothing 1.0 0) 
    else case reads li of
      []      -> do 
        putStrLn "Something wrong in port number."
        main
      (i,_):_ -> do
        putStrLn "Using MIDI output."
        enterLoopMidiVersion i i


enterLoopMidiVersion :: Int -> Int -> IO ()
enterLoopMidiVersion midiDevNumLow midiDevNumHigh = do
  initialize
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> do
      runInputT defaultSettings $
                loop (LoopContext (Just streams) Nothing 1.0 0)
      mapM_ close streams
      terminate
      return ()
    (err:_) -> putStrLn $ "Error opening output midi device.\n" ++ show err


openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Left stream -> return $ Right stream
    Right err -> return $ Left err


loop :: LoopContext -> InputT IO ()
loop contextIn = do
  s <- getInputLine "midi > "
  case s of
    Nothing -> loop contextIn
    Just l  -> do
      conOut <- liftIO $ loopIO l contextIn
      case conOut of
        Nothing -> return ()
        Just x  -> loop x
  


loopIO :: String -> LoopContext -> IO (Maybe LoopContext)
loopIO inp con = do
  parsedCmd <- (E.evaluate $ lineParse inp) `catches` allHandlersM
  case parsedCmd of
    Nothing  -> return $ Just con
    Just com -> case com of
      InpTerminateProcess ->
        case lcThreadID con of
          Nothing  -> return $ Just con
          Just tid -> do {killThread tid; allOff con; return $ Just con}
      InpQuit              -> allOff con >> return Nothing
      InpPlay range splice -> playCommand range splice con
      InpParseError s      -> do {putStrLn s; return $ Just con}
      InpShowDirs -> 
        let f :: String -> Int -> IO ()
            f x y = putStrLn $ printf "%2d: %s" y x
        in do zipWithM_ f hardCodedDirectories [0..]
              return $ Just con
      InpCwd i
        | i >= length hardCodedDirectories -> do
            putStrLn "Error:cwd index too large."
            return $ Just con
        | otherwise -> do
            let s = hardCodedDirectories !! i
            putStrLn $ "Changing directory to " ++ s
            setCurrentDirectory s
            return $ Just con
      InpChangeTempo r -> return $ Just con {lcTempoRatio = r}
      InpTranspose   t -> return $ Just con {lcTranspose  = t}


playCommand :: (Int,Maybe Int) -> Maybe Char -> LoopContext ->
               IO (Maybe LoopContext)
playCommand range splice con = do
  cName <- computeConfigName
  putStrLn $ "Using this config: " ++ takeFileName cName
  case lcThreadID con of
    Just t  -> killThread t
    Nothing -> return ()
  tid <- if isJust $ lcStreams con
           then do allOff con
                   x <- runPlay  con cName range splice `catches` allHandlersM
                   allOff con
                   return x
           else error "in mp.hs, took out Csound code"
                -- runPlayC con cName range splice `catches` allHandlersM
  return $ Just con {lcThreadID = tid}
      

{-
-- As written the code here can only return a Just value. But
-- this will be used with exception handlers that can return a Nothing
lineFetch :: String -> IO (Maybe InpCmd)
lineFetch = do
  -- putStr "(midi)---> "
  -- hFlush stdout
  -- li <- getLine
  if length li == 0
    then lineFetch
    else do  r <- E.evaluate $ lineParse li
             return (Just r)
-}


lineParse ::  String -> Maybe InpCmd
lineParse s =
  case TP.parse parseCmdLine "" s of 
    Left  err -> Just $ InpParseError (show err)
    Right cmd -> Just $ cmd


{-

  playing via csound: 

    I think we can convert non-raw midi to csound

    what single midi events do we need to consider when translating midi to
    csound?

      mod: not for piano

      pedal events: yes, not hard

      volume: yes



-}

-- runPlay
--
-- Play via realtime MIDI.
--
-- This is the portion of the play command which is split off from
-- 'playCommand' in order to catch exceptions arising herein. 
--
-- This calls 'readConvertScore' to get a list of EventsRecords.
-- 
-- Then this calls toRaws to handle the job of converting MidiEvent to times
-- and RawMidiEvent, where 'force' is used to be sure all events are fully
-- evaluated before raws are passed to forkIO.
--
runPlay :: LoopContext -> String -> (Int,Maybe Int) -> Maybe Char -> 
           IO (Maybe ThreadId)
runPlay (LoopContext (Just streams) _ tRatio transp) cName range splice = do
  rawEvts <- readConvertScore tRatio transp cName range splice
  --  REMOVED 1/2017 rawEvts <- toRaws evtsRecs
  beginTime <- fromIntegral `liftM` time 
  tid <- forkIO $
    playRawEvents streams (beginTime + 200) rawEvts
  return $ Just tid


{-

REMOVED 11/2016 as code is being refactored; don't intend to use Csound again
now that I have discovered the Arturia Moog unit. I don't have any reason this
code is correct, should I need to put it back again.

-- runPlayC
--
-- Play via csound
runPlayC :: LoopContext -> String -> (Int,Maybe Int) -> Maybe Char -> 
           IO (Maybe ThreadId)
runPlayC (LoopContext streams _ tRatio transp) cName range splice = do
  evtsRecs <- readConvertScore tRatio transp cName range splice
  tid <- forkIO $ (runCsound $ concatMap erEvts evtsRecs)
  return $ Just tid


-}


removeIgnoredStaves :: Set String -> Score -> Score
removeIgnoredStaves ignoredNames score@Score {scStaves = stavesIn} =
    score {scStaves = M.mapMaybeWithKey g stavesIn}
  where
    g name s | S.member name ignoredNames = Nothing
             | otherwise                  = Just s


readConvertScore :: Double -> Int -> String -> (Int,Maybe Int) -> Maybe Char -> 
                    IO [RawMidiEvent]
readConvertScore tRatio transp cName range splice = do
  -- ConfigFile staffConfigs ignoreNames mSusPed patStatements mTimeVar
  --   <- readConfig cName
  conf <- readConfig cName
  -- score <- removeIgnoredStaves ignoreNames `liftM` readScore
  score <- readScore

  {-
    removed 1/2017

  let plConf = PlaybackConfig staffConfigs ignoreNames mSusPed mTimeVar
               (PatternData M.empty)
               tRatio transp range splice
  -}

  gen <- newStdGen
  -- removed 1/2017 let (debugItems,evtsRecs) = scoreToMidi plConf score gen
  let (mes,raws) = toMidi conf (PlayCmd range tRatio transp) gen score

  
  -- MidiEvent dump
  putStrLn "*** MidiEvents: MidiEvents.txt ***"
  writeFile "MidiEvents.txt" . showiToString $ Component "" False
            (map showI $ L.sort mes)  
  

  {-
  -- RawMidiEvent dump
  putStrLn "*** RawMidiEvents: raws.txt ***"
  writeFile "raws.txt" . showiToString $ Component "" False
            (map showI $ L.sort raws)
  -}

  {-
  -- Debugging dump
  putStrLn "*** writing debug.txt ***"
  writeDebugItems debugItems
  -}
  
  -- final state dump
  {-
  putStrLn "*** final TrState: state.txt ***"
  writeFile "state.txt" . showiToString . showi $ finalSt
  -}

  -- score.txt dump
  putStrLn "*** writing score.txt ***"
  writeScore score

  E.evaluate $ force raws


validateStaffNames staffConfigs score
  | s1 == s2 = True
  | otherwise = 
      (printf "config staff statements:%s score part names:%s" (show s1)
      (show s2)) `trace` False
  where
    s1 = S.fromList . M.keys $ staffConfigs
    s2 = S.fromList . M.keys . scStaves $ score


-- makeInstrs
--
--   Look up function assosicated with instrument name, and partial completion
--   with unchanging input data. Why can't we look up instrument as needed in
--   Translation, so it doesn't have to be a field in PlaybackConfig? Does it
--   matter beyond just reducing the # of fields? What is the simplest way to
--   construct this?
--

{-
makeInstrs staffConfigs score
  | not (validateStaffNames staffConfigs score) =
       throwMine $ "names of staff entries in config file do not match " ++
       "names of staves in the score"
  | otherwise = M.map g staffConfigs
  where
    g sc = (lookupInstrument (stcInstrName sc)) score sc
-}

{-

toRaws :: [EventsRecord] -> IO [(Integer,RawMidiEvent)]
toRaws evts = do
  raws <- E.evaluate $ force $ mkRawEvents global_msInitialDelay evts
  let (tEnd,_) = last raws
  putStrLn $ printf "Last event at %.1f" (fromIntegral tEnd/1000::Double)
  return raws

-}


readScore :: IO Score
readScore = do
  buf <- B.readFile global_xmlInputFileName
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e


readConfig :: String -> IO ConfigFile
readConfig configFileName = do
  parsed <- TPB.parseFromFile parseConfig configFileName
  case parsed of
     Left err -> throwMine $ " in config: " ++ (show err)
     Right x  -> return x


--   computes filename, including path, of the config file to use. This
--   is computed by taking the most recent .sib file in the current directory
--   and changing its extension to .cfg
computeConfigName :: IO String
computeConfigName = do
  cwd <- getCurrentDirectory
  mostRecentSib <-
    treeFiles ((==".sib") . takeExtension) "/Users/Mike/Dropbox/music/" >>=
    mostRecentFileL 
  -- mostRecentSib <- mostRecentFileNDirs (cwd:hardCodedDirectories) "sib"
  return $ replaceExtension mostRecentSib "cfg"


allHandlersM = [Handler myHandlerM, Handler ioHandlerM]


myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing


ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing


allOff :: LoopContext -> IO ()
allOff (LoopContext (Just streams) _ _ _) = do
  mapM_ allNotesOffAllChans streams
  mapM_ pedalOffAllChans    streams


{-

writeDebugItems :: FullDebugInfo -> IO ()
writeDebugItems fdi = do
  writeFile "debug.txt" $ showIString fdi

-}

writeScore :: Score -> IO ()
writeScore score = do
  writeFile "score.txt" $ showIString score


{-
writeMidiEvents :: [EventsRecord] -> IO ()
writeMidiEvents evts = do
  let g r = isNoteEventsRecord r && erStaffName r == "Piano-staff2"
      NoteEventsRecord _ evts2 = case L.find g evts of
        Just x -> x
  writeFile "events.txt" . showiToString$(Component "" False (map showi evts2))
-}
