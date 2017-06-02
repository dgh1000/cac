
-- this won't work: when an error occurs it bombs out of interpreterT --
-- unless we can figure out how to catch an error?


import qualified Data.Map as M
import qualified Sound.PortMidi as SP
import qualified Control.Exception as E
import qualified Data.List as L
import Control.Arrow hiding (loop)
import Control.DeepSeq
import Control.Monad.State
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Text.XML.Light
import System.IO
import System.FilePath
import System.Random
import Control.Concurrent
import Language.Haskell.Interpreter
import System.Console.Haskeline
import Sound.PortMidi hiding (initialize,name)
import Data.Array
import Data.Either
import Score.ScoreData
import Score.XmlToScore
import Score.ShowScore
import XmlDoc.ParseXml
import Instruments.InstrumentsData
import Translation.TranslationData
import Instruments.ToMidi
import Util.FileUtil
import Midi.MidiData
import Midi.Interface
import Util.Showable



data LoopContext = LoopContext
  { lcStreams :: [PMStream]
  , lcTid     :: Maybe ThreadId
  }


main = do
  c <- countDevices
  mapM_ showMidiDevice [0..c-1]
  putStr "Enter midi port number or hit enter for Csound:"
  hFlush stdout
  li <- getLine
  case reads li of
    []      -> do 
      putStrLn "Something wrong in port number."
      return ()
    (i,_):_ -> do
      result <- initMidi i i
      case result of
        Left err -> putStrLn $ "MIDI error: " ++ show err
        Right streams -> do
         runInterpreter $ do
           initInterp
           loop (LoopContext streams Nothing)
         stopMidi streams
         return ()


initInterp :: InterpreterT IO ()
initInterp = do
  set [searchPath := ["/Users/Mike/haskell"]]
  loadModules [ "Score.ScoreData"
              , "Instruments.InstrumentsData"
              , "Util.Math"
              , "Common.CommonData"
              , "Instruments.ToMidi"
              , "Translation.TranslationData"
              ]
  setImportsQ [ ("Prelude", Nothing)
              , ("Data.Map", Just "M")
              , ("Data.Set", Just "S")
              , ("Control.Monad", Nothing)
              , ("Control.Monad.Trans", Nothing)
              , ("Score.ScoreData",Nothing)
              , ("Instruments.InstrumentsData",Nothing)
              , ("Util.Math", Nothing)
              , ("Common.CommonData", Nothing)
              , ("Instruments.ToMidi", Nothing)
              , ("Translation.TranslationData", Nothing)
              ]
  

doInterp :: InterpreterT IO (Maybe RunData)
doInterp = do
  buf <- liftIO $ do configName <- computeConfigName
                     putStrLn $ "Using " ++ configName
                     readFileStrictly configName
  interpResult <- interpret buf (as :: RunData)
  case interpResult of
    Left err -> liftIO (putStrLn (showErr err)) >> return Nothing
    Right rd -> return $ Just rd

    
loop :: LoopContext -> InterpreterT IO ()
loop context = do
  l <- liftIO $ do putStr "midi > "
                   hFlush stdout
                   getLine
  case parse cmdLine "" l of
    Left err -> do
      liftIO $ putStrLn $ "COMMAND-LINE ERROR\n" ++ show err
      loop context
    Right cmd -> case cmd of
      MsrNum beg -> loop_part2 beg context >>= loop
      Quit -> return ()
      Stop -> case lcTid context of
        Nothing -> loop context
        Just i  -> liftIO (killThread i >> allOff (lcStreams context)) >>
                   loop context {lcTid = Nothing}


loop_part2 :: Int -> LoopContext -> InterpreterT IO LoopContext
loop_part2 msrB context = do
  eRunData <- doInterp
  case eRunData of
    Nothing -> return context
    Just rd -> liftIO $ do
      maybeKillMidi (lcTid context) (lcStreams context)
      loop_part3 msrB context rd

    
loop_part3 :: Int -> LoopContext -> RunData -> IO LoopContext
loop_part3 msrB (LoopContext streams mTid) (RunData metas tVar) = do
  score <- readXml
  putStrLn "Writing score.txt."
  writeFile "score.txt" . showIString $ score
  gen <- newStdGen
  let metaMap = M.fromList $ map (metaName &&& id) metas
      s = TrState score metaMap tVar gen M.empty (SMap M.empty)
          (SMap M.empty) (SMap M.empty) [] []
      (raws,finalState) = runState (toMidi msrB) s
  raws <- E.evaluate $ force raws
  tid <- launchPlayRawEvents streams raws
  return $ LoopContext streams (Just tid)


maybeKillMidi :: Maybe ThreadId -> [PMStream] -> IO ()
maybeKillMidi mTid streams = do
  case mTid of
    Just i -> killThread i
    Nothing -> return ()
  allOff streams
  


launchPlayRawEvents :: [PMStream] -> [RawMidiEvent] -> IO ThreadId
launchPlayRawEvents streams evts = do
  beginTime <- fromIntegral `liftM` time
  tid <- forkIO $ playRawEvents streams (beginTime+200) evts
  return tid
  



showErr (WontCompile errs) = concatMap (\e -> errMsg e ++ "\n") errs
showErr (NotAllowed s) = "NotAllowed: " ++ s
showErr (UnknownError s) = "UnknownError " ++ s
showErr (GhcException s) = "GhcException " ++ s

data CmdLine = MsrNum Int
             | Quit
             | Stop


cmdLine :: Parser CmdLine
cmdLine = (MsrNum <$> int) <|> quit <|> stop

int = many1 digit >>= return . read

quit = char 'q' >> eof >> return Quit

stop = char 's' >> eof >> return Stop
  

computeConfigName :: IO FilePath
computeConfigName = do
  sib <-
    treeFiles ((==".sib") . takeExtension) "/Users/Mike/Dropbox/music/" >>=
    mostRecentFileL 
  let cName = replaceExtension sib "hs"
  putStrLn $ "Using: " ++ cName
  return cName
  

computeRunData :: FilePath -> InterpreterT IO RunData
computeRunData filename = do
  set [searchPath := ["/Users/Mike/haskell"]]
  loadModules [ "Score.ScoreData"
              , "Instruments.InstrumentsData"
              , "Util.Math"
              , "Common.CommonData"
              , "Instruments.ToMidi"
              , "Translation.TranslationData"
              ]
  setImportsQ [ ("Prelude", Nothing)
              , ("Data.Map", Just "M")
              , ("Data.Set", Just "S")
              , ("Control.Monad", Nothing)
              , ("Control.Monad.Trans", Nothing)
              , ("Score.ScoreData",Nothing)
              , ("Instruments.InstrumentsData",Nothing)
              , ("Util.Math", Nothing)
              , ("Common.CommonData", Nothing)
              , ("Instruments.ToMidi", Nothing)
              , ("Translation.TranslationData", Nothing)
                
              ]
  buf <- liftIO $ readFile filename
  interpret buf (as :: RunData)


readXml :: IO Score
readXml = do
  buf <- readFileStrictly "/Users/Mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e


stopMidi :: [PMStream] -> IO ()    
stopMidi streams = do
  mapM_ close streams
  terminate
  return ()    
  

allOff :: [PMStream] -> IO ()
allOff streams = mapM_ allNotesOffAllChans streams >>
                 mapM_ pedalOffAllChans    streams


initMidi :: Int -> Int -> IO (Either PMError [PMStream])
initMidi midiDevNumLow midiDevNumHigh = do
  SP.initialize
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> return $ Right streams
    (err:_) -> return $ Left err


decideInputOrOutput di = if input di then "Input :" else "Output:"


openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Left stream -> return $ Right stream
    Right err -> return $ Left err


showMidiDevice :: Int -> IO ()
showMidiDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (SP.name di) 


