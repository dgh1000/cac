module App.MidiPlayer.RunFunctions where

import System.Cmd
import System.FilePath
import System.IO.Unsafe
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Text.Parsec
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
import qualified PlaybackPI.PlaybackPIExport
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
import SibDoc.ShowSibDoc
import qualified PerfDoc.FromSibDoc as FS
import Util.Showable
import Midi.ShowMidi
import MidiPI.Lookup
import qualified MidiPI.MidiPIData as MPID
import MidiPI.Instruments.InstrumentsData( configXSampleShearEarly )
import qualified SibDoc.FromParsed as FP
import SibDoc.ParseSibDoc( parseSibDoc )
import SibDoc.SibDocData( ParsedSibDoc )
-- import Text.XML.HaXml.Parse
-- import Text.XML.HaXml.Types
-- import XmlDoc.ParseXml
-- import XmlDoc.XmlDocExport
-- import MusDoc.FromXmlDoc
import qualified PerfDoc.FromMusDoc as FM


{-

new version that can handle multiple midi channels

-- topLevel
--
--    Inputs:
--    Int ::  device number of first MIDI channel. This number plus one is
--            second MIDI channel, plus two is third, and so on.
-- 
topLevel

-}

-- topLevel
--  
topLevel :: Int -> IO ()
topLevel deviceNum = realTimeMidiRun loop deviceNum

-- topLevel = realTimeMidiRun loop 

data LoopContext = LoopContext
  { lcStream :: PMStream
  , lcTid :: Maybe ThreadId
  , lcUsingType :: FileDataType }

instance MidiLoopContext LoopContext where
  mkInitMidiLoopContext stream = LoopContext stream Nothing FDText

data FileDataType = FDText | FDXML

data FileData = FileData 
 { fdType :: FileDataType
 , fdFilename :: String  -- name of source data, such as Sibelius text dump,
                         -- or MusicXML file
 , fdConfig :: String    -- config filename
 }

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
        case lcTid con of
          Nothing -> loop con
          Just tid -> do
            killThread tid
            allNotesOffAllChans (lcStream con)
            loop con
      InpQuit -> return ()
      InpPlay rs mFNameWriPerfDoc mTempoRatio mTransposition
        | not (validateMsrRangeArgs rs) ->
          do putStrLn $ "Something wrong with measure numbers. They must be "++
                      "in ascending order, and only the last one can be " ++
                      "a single number (non-range) "
             loop con
        | otherwise ->
           do case lcTid con of
                 Just t -> killThread t
                 Nothing -> return ()
              allNotesOffAllChans (lcStream con)
              tid <- runPlay (lcTid con) (lcStream con) fileData rs mTempoRatio
                     mTransposition mFNameWriPerfDoc `catches` allHandlersM
              loop con { lcTid = tid }
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

{-      

THIS MIGHT BE USEFUL BUT I'M NOT SURE HOW IT IS DIFFERENT THAN loop



-- loop2 is modified version of loop, for testing purposes  
loop2 :: LoopContext -> IO ()
loop2 con = do
  parsedCmd <- lineFetch `catches` allHandlersM

  -- find most recent sib file
  cwd <- getCurrentDirectory
  mostRecentSib <- mostRecentFile cwd "sib"

  let dfn = "g:/out.txt"             -- plugin dump filename 
      --- cfn = joinPath [cwd, "current.cfg"]
      cfn = replaceExtension mostRecentSib "cfg"


  case parsedCmd of
    Nothing -> loop2 con
    Just com -> case com of
      InpTerminateProcess ->
        case lcTid con of
          Nothing -> loop2 con
          Just tid -> do
            killThread tid
            allNotesOffAllChans (lcStream con)
            loop2 con
      InpQuit -> return ()
      InpPlay rs _ mTempoRatio mTransposition
        | not (validateMsrRangeArgs rs) ->
          do putStrLn $ "Something wrong with measure numbers. They must be "++
                      "in ascending order, and only the last one can be " ++
                      "a single number (non-range) "
             loop2 con
        | otherwise ->
           do case lcTid con of
                 Just t -> killThread t
                 Nothing -> return ()
              allNotesOffAllChans (lcStream con)
              runPlay2 dfn cfn rs mTempoRatio mTransposition `catches` 
                      allHandlers
              loop2 con
      InpParseError s -> do
        putStrLn s
        loop2 con
      InpCwd i -> do
        let s = hardCodedDirectories !! i
        putStrLn $ "Changing directory to " ++ s
        setCurrentDirectory s
        loop2 con
      {-
      InpDumpMusDoc -> do
        dumpMusDoc dfn `catches` allHandlers
        loop2 con
      -}
      InpDumpPerfDoc -> do
        dumpPerfDoc dfn cfn `catches` allHandlers
        loop2 con
        
-}

{-
dumpSibDoc :: String -> IO ()
dumpSibDoc dfn = do
  parsedSibDoc <- readSibDocDump dfn
  let sibDoc = FP.convert parsedSibDoc
  let doc = MFS.fromSibDoc sibDoc
  writeFile (replaceExtension dfn "txt") $ showItem $ showi doc
  putStrLn "Sucessful dump."
-}



-- computeInitialPerfDoc
--   Looking at the source data and config files specified in the input
--   FileData, (which might specify music xml or might specify a sibelius
--   text dump), compute the initial PerfDoc
computeInitialPerfDoc :: FileData -> IO PerfDoc
computeInitialPerfDoc (FileData typ source config) = case typ of
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
    (_,allConfig) <- readConfig config
    return (FS.toPerfDoc (PLD.susPedParts allConfig) (PLD.timeDiv allConfig) $
            sibDoc )
  _ -> error "cannot read xml files; had problem installing haxml"
  
  

-- runPlay
--  Maybe ThreadId :: == "Just <id>" if a thread has been started
--  PMStream :: midi stream
--  String :: (dfn) MusicXML file name
--  String :: (cfn) config file name
--  [MsrRange] :: I think this is ranges of measures to be played
--  mTempoRatio :: if Just <ratio>,  modify the tempo by this much
--  mTransposition :: maybe transpose the result by this many half steps
--  mFNameWriPerfDoc :: maybe filename to write perf doc dump to
runPlay :: Maybe ThreadId -> PMStream -> FileData -> [MsrRange] -> 
           Maybe Double -> Maybe Int -> Maybe String -> IO (Maybe ThreadId)
runPlay maybeId stream fileData playRanges mTempoRatio mTransposition
        mFNameWriPerfDoc = do

  initialPerfDoc <- computeInitialPerfDoc fileData
  (commands,allConfig) <- readConfig (fdConfig fileData)

  {-
  parsedSibDoc <- readSibDocDump (fdFilename fileData)
  -- writeFile "dump_parsed_sibdoc.txt" (showItem . showi $ parsedSibDoc)
  let sibDoc = FP.convert parsedSibDoc
  -- writeFile "dump_sibdoc.txt" (showItem . showi $ sibDoc)
  -- xmlData <- readTheXml xfn
  (commands,allConfig) <- readConfig (fdConfig fileData)
  let perfDoc1 = 
        FS.toPerfDoc (PLD.susPedParts allConfig) (PLD.timeDiv allConfig) $
         sibDoc 
      snips = computeSnipMsrRanges playRanges perfDoc1 blankSepNeed
  -}

  let snips = computeSnipMsrRanges playRanges initialPerfDoc blankSepNeed
      pdFinalized =
        applyCommands commands .
        finalizePerfDoc mTempoRatio mTransposition snips $
        initialPerfDoc
      blankSepNeed = PLD.blankMsrs allConfig 

  if PD.isNullDoc pdFinalized
    then do putStrLn "Null finalized PerfDoc; skipping."
            return Nothing
    else do
      case mFNameWriPerfDoc of
        Nothing -> return ()
        Just s -> writeFile s (showItem . showi $ pdFinalized)

      let midiEvts = toMidiEventsHelper pdFinalized allConfig
          -- Delay caused by moving absolute start time into the past, in order
          -- to handle midi events with negative start times
          preStartDelay = 200
      rmes <- evaluate $ mkRawEvents MPID.initialEventsDelay midiEvts
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Double)
      beginTime <- time
      tid <- forkIO $ 
           playRawEvents stream (beginTime + fromIntegral preStartDelay) rmes
      return $ Just tid

toMidiEventsHelper :: PerfDoc -> AllConfig -> [MidiEvent]
toMidiEventsHelper pd allConfig = case PLD.ciaPlay allConfig of
  Just splay -> 
    "Using CIA" `trace` toMidiEventsCIA pd splay
  Nothing -> toMidiEvents pd (PLD.midiInstrMap allConfig)


-- runPlay2 is modified version of runPlay for testing purposes
runPlay2 :: String -> String -> [MsrRange] -> 
           Maybe Double -> Maybe Int -> IO ()
runPlay2 dfn cfn playRanges mTempoRatio mTransposition = do
  parsedSibDoc <- readSibDocDump dfn
  -- writeFile "dump_parsed_sibdoc.txt" (showItem . showi $ parsedSibDoc)
  let sibDoc = FP.convert parsedSibDoc
  -- writeFile "dump_sibdoc.txt" (showItem . showi $ sibDoc)
  -- xmlData <- readTheXml xfn
  (commands,allConfig) <- readConfig cfn
  let perfDoc1 = 
        FS.toPerfDoc (PLD.susPedParts allConfig) (PLD.timeDiv allConfig) $
        sibDoc 
      snips = computeSnipMsrRanges playRanges perfDoc1 blankSepNeed
      pdFinalized =
        {- applyCommands commands . -}
        {- PU.mapOverParts (computeLegatoOffsets 0.05) . -}
        {- PU.mapOverParts (computeGraceNoteOffsets 0.05 0.1) . -}
        {- PU.mapOverParts (computeRepeatedNoteOffsets 1.0 1.05) .  -}
        finalizePerfDoc mTempoRatio mTransposition snips $
        perfDoc1

      blankSepNeed = PLD.blankMsrs allConfig 
  if PD.isNullDoc pdFinalized
    then do putStrLn "Null finalized PerfDoc; skipping."
            return ()
    else do
      writeFile "f2.txt" . showItem . showi $ pdFinalized 
      {-
      let midiEvts = toMidiEvents pdFinalized (PLD.midiInstrMap allConfig)
         -- Delay caused by moving absolute start time into the past, in order
         -- to handle midi events with negative start times
          preStartDelay = 200
      writeFile "g:/me.txt" (show midiEvts)
      -}
      {-
      rmes <- evaluate $ mkRawEvents midiEvts 
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Double)
      writeFile "events.txt" (show rmes)
      -}

computeFileData :: LoopContext -> IO FileData
computeFileData (LoopContext _ _ typ) = do
  cfn <- computeMostRecentConfig
  return (case typ of
            FDText -> FileData typ "c:/out.txt" cfn
            FDXML -> FileData typ "c:/out.xml" cfn)

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
  b <- readFile fn
  case parse parseSibDoc "" b of
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
  d <- getCurrentDirectory
  putStrLn d
  putStr "(midi)---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then lineFetch
    else do  r <- evaluate $ lineParse li
             return (Just r)

lineParse ::  String -> InpCmd
lineParse s =
  case parse parseCmdLine "" s of 
    Left err -> InpParseError (show err)
    Right cmd -> cmd


-- toMidiEvents
--
-- Converts to *unsorted* list of MidiEvent
toMidiEvents :: PerfDoc -> InstrMap -> [MidiEvent]
toMidiEvents pd instrMap = 
  concatMap midiOnePart . M.toList . PD.parts $ pd
  where
    midiOnePart (pid, part) = 
      case lookupInstr pid instrMap of
        Nothing -> 
            printf "No instrm or instrc for part ID %s found.. skipping" pid
                       `trace` []
        -- here it skips calling the "translation to midi function" if there
        --   are no notes in the part, because otherwise the translation
        --   function will try to generate a loudness curve anyway
        Just (ip, fn) -> if PD.isNullPart part then [] else  fn ip part

-- toMidiEventsCIA
--   version of toMidiEvents that uses a complex I.A., which is an
--   instrument that works on an entire PerfDoc rather than one part at a time
-- 
toMidiEventsCIA :: PerfDoc -> CiaPlay -> [MidiEvent]
toMidiEventsCIA pdoc play = fn play pdoc 
  where
    fn = lookupCIA (PLD.cpFunName play)

    
  
{-
lookupWithPercussion :: Bool -> PartId -> InstrMap ->Maybe (InstrPlay, InstrFn)
lookupWithPercussion percFlag pid imap =
  case lookupInstr pid imap of
    Nothing -> Nothing
    Just (ip, fn) -> 
      if percFlag then (ip, g(PlD.ipChan ip)) else (ip, fn)
-}  



readConfig :: String -> IO ([Command],AllConfig)
readConfig configFileName = do
  configBuffer <- readFile configFileName
  case parse parseConfigFile "" configBuffer of
     Left err -> throwMine $ "Config file parse error: " ++ (show err)
     Right cs -> return cs

applyCommands :: [Command] -> PerfDoc -> PerfDoc
applyCommands cs pd = foldl step pd cs
  where
    atm = BT.deltaToAbsolute (PD.timeMap pd)
    -- mdw = mergeDirectionWords (map PD.partData . M.elems . PD.pParts $ pd)
    step :: PerfDoc -> Command -> PerfDoc
    step pd' com = execute pd' com atm

{-
mergeDirectionWords :: [Part] -> Map DirectionWords [Loc]
mergeDirectionWords parts = reverseLMap u
  where
    wordMaps :: [Map Loc [DirectionWords]]
    wordMaps = map MD.directionWords parts
    u = M.unionsWith (++) wordMaps
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
  (_,allConfig) <- readConfig (fdConfig filedata)
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

