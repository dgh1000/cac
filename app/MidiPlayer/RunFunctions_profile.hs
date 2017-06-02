{- for profiling two-x-violins.sib starting at measure 11 -}

module App.MidiPlayer.RunFunctions where

import System.Random
import System.Cmd
import System.FilePath
import System.IO.Unsafe
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec.ByteString as TPB
import Text.Parsec as TP
import Data.List(sort, sortBy, null)
import qualified Data.List as L
import System.IO
import System.Process
import Sound.PortMidi
import Control.Monad
import System.Directory
import System.FilePath
import Prelude hiding (catch)
import Control.Concurrent
import Control.Exception
import Foreign.C.Types
import Util.FileUtil
import Util.Exception
import Util.Showable
import Util.Map
import Midi.MidiExport 
import Midi.Interface
import qualified Midi.MidiData as MiD
import PlaybackPI.ParseConfig( parseConfigFile )
import PlaybackPI.ParseCommandLine( parseCmdLine )
import PlaybackPI.PlaybackPIExport
import qualified PlaybackPI.PlaybackPIData as PLD
import PlaybackPI.Execute
import PlaybackPI.Directories( hardCodedDirectories )
import PlaybackPI.Snip
import Common.CommonData( Loc(..) )
import MusDoc.ShowMusDoc
import PerfDoc.PerfDocExport
import qualified PerfDoc.PerfDocData as PD
import qualified PerfDoc.BasicTiming as BT
import PerfDoc.Finalize
import qualified PerfDoc.PerfDocUtil as PU
import PerfDoc.ShowPerfDoc
import PerfDoc.FinalizeUtil
import qualified PerfDoc.FromMusDoc as FM
import qualified PerfDoc.FromSibDoc as FS
import SibDoc.ShowSibDoc
import qualified SibDoc.FromParsed as FP
import SibDoc.ParseSibDoc( parseSibDoc )
import SibDoc.SibDocData( ParsedSibDoc )
import Util.Showable
import Midi.ShowMidi
import qualified MidiPI.MidiPIData as MPID
import MidiPI.NewInstruments.InstrumentsData( configXSampleShearEarly )
import MidiPI.MasterInstruments
-- import Text.XML.HaXml.Parse
-- import Text.XML.HaXml.Types
-- import XmlDoc.ParseXml
-- import XmlDoc.XmlDocExport
-- import MusDoc.FromXmlDoc


data LoopContext = LoopContext
  { lcStreams :: [PMStream]
  , lcThreadID :: Maybe ThreadId
  , lcUsingType :: FileDataType }

data FileDataType = FDText | FDXML

data FileData = FileData 
 { fdType :: FileDataType
 , fdFilename :: String  -- name of source data, such as Sibelius text dump,
                         -- or MusicXML file
 , fdConfig :: String    -- config filename
 }



-- configuration
textMusicDataFilename = "z:/out.txt"
xmlMusicDataFilename = "z:/out.xml"


-- topLevel
--  
topLevel :: Int -> Int -> IO ()
topLevel midiDevNumLow midiDevNumHigh = do
  initialize
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> do
      loop (LoopContext streams Nothing FDText)
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


x :: IO ()
x = do
  let rs = (11, Nothing)
  

loop :: LoopContext -> IO ()
loop con = do
  parsedCmd <- lineFetch `catches` allHandlersM
  fileData <- computeFileData con
  case parsedCmd of
    Nothing -> loop con
    Just com -> case com of
      InpReadXml -> 
        do putStrLn "Switching to reading MusicXML"
           loop con { lcUsingType = FDXML }
      InpReadTextDump -> 
        do putStrLn "Switching to reading Sibelius text dump"
           loop con { lcUsingType = FDText }
      InpTerminateProcess ->
        case lcThreadID con of
          Nothing -> loop con
          Just tid -> do
            killThread tid
            mapM_ allNotesOffAllChans (lcStreams con)
            loop con
      InpQuit -> return ()
      InpPlay rs mFNameWriPerfDoc mTempoRatio mTransposition
        | not (validateMsrRangeArgs rs) ->
          do putStrLn $ "Something wrong with measure numbers. They must be "++
                      "in ascending order, and only the last one can be " ++
                      "a single number (non-range) "
             loop con
        | otherwise -> do
           case lcThreadID con of
             Just t -> killThread t
             Nothing -> return ()
           mapM_ allNotesOffAllChans (lcStreams con)
           tid <- runPlay (lcThreadID con) (lcStreams con) fileData rs 
                  mTempoRatio mTransposition mFNameWriPerfDoc 
                  `catches` allHandlersM
           loop con { lcThreadID = tid }
      InpParseError s -> do
        putStrLn s
        loop con
      InpCwd i -> do
        let s = hardCodedDirectories !! i
        putStrLn $ "Changing directory to " ++ s
        setCurrentDirectory s
        loop con
      {-
      InpDumpMusDoc -> do
        dumpMusDoc dfn `catches` allHandlers
        loop con
      -}
      InpDumpPerfDoc -> do
        dumpPerfDoc fileData `catches` allHandlers
        loop con




-- computeInitialPerfDoc
--   Looking at the source data and config files specified in the input
--   FileData, (which might specify music xml or might specify a sibelius
--   text dump), compute the initial PerfDoc
computeInitialPerfDoc :: AllConfig -> FileData -> IO PerfDoc
computeInitialPerfDoc allConfig (FileData typ source config) = case typ of
  {- FDXML -> do
    xmlDoc <- readXmlDoc source
    let musDoc = fromXmlDoc xmlDoc
    (_,allConfig) <- readConfig config
    return (FM.toPerfDoc (PLD.susPedParts allConfig) (PLD.timeDiv allConfig)
            $ musDoc)
  -}
  FDText -> do
    parsedSibDoc <- readSibDocDump source
    let sibDoc = FP.convert parsedSibDoc
    return (FS.toPerfDoc [] (PLD.timeDiv allConfig) $ sibDoc )
  _ -> error "cannot read xml files; had problem installing haxml"
  
  

-- runPlay
--
--  Read out.txt or out.xml, make a PerfDoc, translate it to MIDI events
--  with the help of data in the config file, and send it to MIDI streams.
--
--  Maybe ThreadId :: == "Just <id>" if a thread has been started
--  PMStream :: midi stream
--  FileData :: source document and config file names
--  [MsrRange] :: I think this is ranges of measures to be played
--  mTempoRatio :: if Just <ratio>,  modify the tempo by this much
--  mTransposition :: maybe transpose the result by this many half steps
--  mFNameWriPerfDoc :: maybe filename to write perf doc dump to
runPlay :: Maybe ThreadId -> [PMStream] -> FileData -> [MsrRange] -> 
           Maybe Double -> Maybe Int -> Maybe String -> IO (Maybe ThreadId)
runPlay maybeId streams fileData playRanges mTempoRatio mTransposition
        mFNameWriPerfDoc = do
  configData <- readConfig (fdConfig fileData)
  -- putStrLn (showAllConfig configData)
  initialPerfDoc <- computeInitialPerfDoc configData fileData
  let snips = computeSnipMsrRanges playRanges initialPerfDoc blankSepNeed
      pdFinalized =
        finalizePerfDoc mTempoRatio mTransposition snips initialPerfDoc
      blankSepNeed = 2
  if PD.isNullDoc pdFinalized
    then do putStrLn "Null finalized PerfDoc; skipping."
            return Nothing
    else do
      -- write PerfDoc if requested
      case mFNameWriPerfDoc of
        Nothing -> return ()
        Just s -> writeFile s (showItem . showi $ pdFinalized)
      -- compute midi events
      seed <- getStdRandom next
      let midiEvts  = toMidiEvents seed pdFinalized configData
          -- Delay caused by moving absolute start time into the past, in order
          -- to handle midi events with negative start times
          preStartDelay = 200
      rmes <- evaluate $ mkRawEvents MPID.initialEventsDelay midiEvts
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Double)
      writeFile "rmes.txt" (showRmeList rmes)
      -- beginTime <- time
      -- tid <- forkIO $ 
           playRawEvents streams (beginTime + fromIntegral preStartDelay) rmes
      --  return $ Just tid

-- computeFileData
--  Set up a data structure of type FileData describing the music data
--  filename and config file filename.
computeFileData :: LoopContext -> IO FileData
computeFileData (LoopContext _ _ typ) = do
  cfn <- computeMostRecentConfig
  return (case typ of
            FDText -> FileData typ textMusicDataFilename cfn
            FDXML -> FileData typ xmlMusicDataFilename cfn)

-- computeMostRecentConfig
--   computes filename, including path, of the config file to use. This
--   is computed by taking the most recent .sib file in the current directory
--   and changing its extension to .cfg
computeMostRecentConfig :: IO String
computeMostRecentConfig = do
  cwd <- getCurrentDirectory
  mostRecentSib <- mostRecentFile cwd "sib"
  let cfn = replaceExtension mostRecentSib "cfg"
  putStrLn $ "Using this config: " ++ takeFileName cfn
  return cfn
      

readSibDocDump :: String -> IO ParsedSibDoc
readSibDocDump fn = do
  -- b <- readFile fn
  parsed <- TPB.parseFromFile parseSibDoc fn
  case parsed of
    Left err -> throwMine $ "Error parsing Sib. dump: " ++ show err
    Right psd -> return psd
   

validateMsrRangeArgs :: [MsrRange] -> Bool
validateMsrRangeArgs rs 
  | x1 /= sort x1 = False -- throwMine "Input msr ranges not in order"
  -- | not (all isTwoNums $ take (l-1) rs) = False 
  --  -- "There is a non-range msr number before the end"
  | otherwise = True
  where
  x1 = concatMap toList rs
  l = length rs
  toList (i,(Just j)) = [i,j]
  toList (i,Nothing) = [i]
  isTwoNums = (==2) . length . toList

-- As written the code here can only return a Just value. But
-- this will be used with exception handlers that can return a Nothing
lineFetch :: IO (Maybe InpCmd)
lineFetch = do
  -- d <- getCurrentDirectory
  -- putStrLn d
  putStr "(midi)---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then lineFetch
    else do  r <- evaluate $ lineParse li
             return (Just r)

lineParse ::  String -> InpCmd
lineParse s =
  case TP.parse parseCmdLine "" s of 
    Left err -> InpParseError (show err)
    Right cmd -> cmd


-- toMidiEvents
--
-- Converts to *unsorted* list of MidiEvent
--
toMidiEvents :: Int -> PerfDoc -> AllConfig -> [MidiEvent]
toMidiEvents seed pd allConfig = 
  case L.lookup (masterFn allConfig) masterFnList of
    Nothing -> throwMine $ printf ("Config said master function was '%s';" ++ 
               " no function in the masterFnList with that name was found")
               (masterFn allConfig)
    Just fn -> fn seed pd allConfig


readConfig :: String -> IO AllConfig
readConfig configFileName = do
  --   configBuffer <- readFile configFileName
  parsed <- TPB.parseFromFile parseConfigFile configFileName
  case parsed of
     Left err -> throwMine $ "Config file parse error: " ++ (show err)
     Right cs -> return cs

{-
applyCommands :: [Command] -> PerfDoc -> PerfDoc
applyCommands cs pd = foldl step pd cs
  where
    atm = BT.deltaToAbsolute (PD.timeMap pd)
    -- mdw = mergeDirectionWords (map PD.partData . M.elems . PD.pParts $ pd)
    step :: PerfDoc -> Command -> PerfDoc
    step pd' com = execute pd' com atm
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

dumpRawEvents :: String -> [(MidiTime,RawMidiEvent)] -> 
                 IO [(MidiTime,RawMidiEvent)]
dumpRawEvents s es = do
  writeFile s (unlines $ map (\(t,e) -> printf "%s: %s" (show t)
                                        (briefShowRawMidiEvent e)) es)
  return es

allHandlers = [Handler myHandler, Handler ioHandler]

myHandler :: MyException -> IO ()
myHandler (MyException s) = putStrLn $ "Error: " ++ s

ioHandler :: IOException -> IO ()
ioHandler e = putStrLn (show e)


dumpPerfDoc filedata = do
  parsedSibDoc <- readSibDocDump (fdFilename filedata)
  let sibDoc = FP.convert parsedSibDoc
  allConfig <- readConfig (fdConfig filedata)
  let perfDoc = FS.toPerfDoc (PLD.susPedParts allConfig) 
                (PLD.timeDiv allConfig) sibDoc
      perfDoc2 = finalizePerfDoc Nothing Nothing [] $ perfDoc
  writeFile (replaceExtension (fdConfig filedata) "txt") . showItem . showi $ 
            perfDoc2
  putStrLn "Dumped PerfDoc (_WITHOUT_ applying any commands or snips [but with finalizing])"

{-
readXmlDoc fn = do
  buf <- readFile fn
  let Document _ _ elem _ = xmlParse fn buf
  return $ parseMusXml elem
-}

showAllConfig (AllConfig _ paramsMap _ _ _) = 
  concatMap g (M.toAscList paramsMap)
  where
    g :: (String,PartInstrParams) -> String
    g (docName,PartInstrParams _ pip) = printf "docName:%s\n%s" docName 
                      (concatMap h $ M.toAscList pip)
      where
        h :: (String,PIPValue) -> String
        h (paramName,value) = printf "   %s:%s\n" paramName (show value)

writeMidiEvents :: String -> [MidiEvent] -> IO ()
writeMidiEvents filename evts = do
  let s = concatMap (showItem . showi) evts
  writeFileStrictly filename s