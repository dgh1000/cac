 
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Text.Parsec.ByteString as TPB
import qualified Text.Parsec as TP
import qualified Control.Exception as E
import qualified Sound.PortMidi as SP
import Language.Haskell.Interpreter
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
import Sound.PortMidi hiding(name)
import Text.XML.Light
import Text.Printf
import Midi.MidiData
import Midi.Interface
import Midi.ShowMidi
import App.Mp.MpData
-- import App.Mp.ParseConfig
import App.Mp.ParseCommandLine
import Instruments.ToMidi
import Instruments.InstrumentsData
import Translation.TranslationData
-- import Translation.MakeRaw
-- import Translation.MakePattern
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


----------------------------------------------------------------------
----------------------------------------------------------------------


decideInputOrOutput di = if input di then "Input :" else "Output:"


showMidiDevice :: Int -> IO ()
showMidiDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (SP.name di) 


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
      {-
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
      -}
      InpChangeTempo r -> return $ Just con {lcTempoRatio = r}
      InpTranspose   t -> return $ Just con {lcTranspose  = t}


playCommand :: (Int,Maybe Int) -> Maybe Char -> LoopContext ->
               IO (Maybe LoopContext)
playCommand range splice con = do
  case lcThreadID con of
    Just t  -> killThread t
    Nothing -> return ()
  tid <- if isJust $ lcStreams con
           then do allOff con
                   x <- runPlay con range splice `catches` allHandlersM
                   allOff con
                   return x
           else error "in mp.hs, took out Csound code"
  return $ Just con {lcThreadID = tid}
      

lineParse ::  String -> Maybe InpCmd
lineParse s =
  case TP.parse parseCmdLine "" s of 
    Left  err -> Just $ InpParseError (show err)
    Right cmd -> Just $ cmd



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
runPlay :: LoopContext -> (Int,Maybe Int) -> Maybe Char -> 
           IO (Maybe ThreadId)
runPlay (LoopContext (Just streams) _ tRatio transp) range splice = do
  rawEvts <- readConvertScore tRatio transp range splice
  beginTime <- fromIntegral `liftM` time 
  tid <- forkIO $
    playRawEvents streams (beginTime + 200) rawEvts
  return $ Just tid

{-

          1/2017: not currently used, but leaving here in case

removeIgnoredStaves :: Set String -> Score -> Score
removeIgnoredStaves ignoredNames score@Score {scStaves = stavesIn} =
    score {scStaves = M.mapMaybeWithKey g stavesIn}
  where
    g name s | S.member name ignoredNames = Nothing
             | otherwise                  = Just s

-}

readConvertScore :: Double -> Int -> (Int,Maybe Int) -> Maybe Char -> 
                    IO [RawMidiEvent]
readConvertScore tRatio transp range splice = do
  conf <- readConfig
  -- score <- removeIgnoredStaves ignoreNames `liftM` readScore
  score <- readScore
  gen <- newStdGen
  let (mes,raws) = toMidi conf (PlayCmd range tRatio transp) gen score

  {-
  -- MidiEvent dump
  putStrLn "*** MidiEvents: MidiEvents.txt ***"
  writeFile "MidiEvents.txt" . showiToString $ Component "" False
            (map showI $ L.sort mes)  
  -}

  
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


readScore :: IO Score
readScore = do
  buf <- B.readFile global_xmlInputFileName
  let topElems = onlyElems $ parseXML buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore $ parseXScore e



readConfig :: IO ConfigFile
readConfig = do
  i <- runInterpreter readConfigI
  case i of
    Left err -> throwMine $ "interpreter error: " ++ show err
    Right f  -> return f


readConfigI :: InterpreterT IO ConfigFile
readConfigI = do
  set [searchPath := ["/Users/Mike/haskell"]]
  loadModules [ "Score.ScoreData","Translation.TranslationData"
              , "Instruments.InstrUtils", "Instruments.InstrumentsData"
              , "Instruments.ConfigFile"]
  setImportsQ [ ("Prelude", Nothing), ("Data.Map", Just "M")
              , ("Data.Set", Just "S")
              , ("Score.ScoreData",Nothing)
              , ("Translation.TranslationData", Nothing)
              , ("Instruments.InstrumentsData", Nothing)
              , ("Instruments.InstrUtils", Nothing)
              , ("Instruments.ConfigFile", Nothing) ]
  buf <- liftIO $ readFile "/Users/Mike/haskell/test.hs"
  interpret buf (as :: ConfigFile)
  

{-

readConfig :: IO (ConfigFile,Maybe String)
readConfig = do
  cwd <- getCurrentDirectory
  mostRecentSib <-
    treeFiles ((==".sib") . takeExtension) "/Users/Mike/Dropbox/music/" >>=
    mostRecentFileL 
  let cName = replaceExtension mostRecentSib "cfg"
  putStrLn $ "Using this config: " ++ takeFileName cName
  parsed <- TPB.parseFromFile parseConfig cName
  returnedConfig <- case parsed of
         Left err -> throwMine $ " in config: " ++ (show err)
         Right x  -> return x
  -- see if haskell file is there
  let hName = replaceExtension mostRecentSib "hs"
  flag <- doesFileExist hName
  let returnedHs | flag      = Just hName
                 | otherwise = Nothing
  return (returnedConfig,returnedHs)
-}



{-
-- findConfig
--
--   computes filename, including path, of the config file to use. This
--   is computed by taking the most recent .sib file in the current directory
--   and changing its extension to .cfg
computeConfigNames :: IO (String,Maybe String)
computeConfigNames = do
  cwd <- getCurrentDirectory
  mostRecentSib <-
    treeFiles ((==".sib") . takeExtension) "/Users/Mike/Dropbox/music/" >>=
    mostRecentFileL 
  return $ replaceExtension mostRecentSib "cfg"
-}

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


writeScore :: Score -> IO ()
writeScore score = do
  writeFile "score.txt" $ showIString score

