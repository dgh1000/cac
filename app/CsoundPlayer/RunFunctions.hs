module App.CsoundPlayer.RunFunctions where
{-


-}

import Data.List(sort)
import Prelude hiding(catch)
import Control.Exception
import qualified Data.List as L
import Control.Monad
import System
import System.IO
import System.Directory
import System.Time
import System.CPUTime
import System.FilePath
import System.Process
import System.Exit
-- import GHC.IO.Handle
import Text.Parsec
import Data.Ratio
import qualified Data.Map as M
import Data.Map( Map )
import Data.Maybe
import Data.List( partition, sortBy )
import qualified Text.XML.MusicXML as X
import qualified Text.XML.MusicXML.Partwise as X
import qualified Text.XML.MusicXML.Score as X
import qualified Expression.Time as T
-- import qualified MusDoc.FromXml as MF
import MusDoc.ShowMusDoc
import MusDoc.Processing
-- import qualified PerfDoc.FromMusDoc as PF
import PerfDoc.PerfDocData( PerfDoc(..)
                          , AbsoluteTimeMap(..) )
import qualified PerfDoc.PerfDocData as PD
import CsoundPI.FromPerfDoc( toCsound )
import System.Environment
import Util.FileUtil
import Util.Env
import Util.Math
import Util.Showable
import Util.Map
import Util.Exception
import PerfDoc.ShowPerfDoc
{-
import MusDoc.MusDocData( MusDoc(..)
                        , DirectionWords(..)
                        , Part )
-}
import qualified MusDoc.MusDocData as MD
import Common.CommonData( Loc(..) )
import qualified PerfDoc.BasicTiming as BT
import Csound.CsoundData( CsoundConfig(..) )
import PlaybackPI.ParseConfig( parseConfigFile )
import PlaybackPI.PlaybackPIExport
import qualified PlaybackPI.PlaybackPIData as PLD
-- import App.CsoundPlayer.ParseCmdLine( parseCmdLine )
import PlaybackPI.ParseCommandLine( parseCmdLine )
import PlaybackPI.Execute
import PlaybackPI.Directories( hardCodedDirectories )
import PlaybackPI.Snip
import qualified App.CsoundPlayer.CsoundPlayerData as CPD
import PerfDoc.Finalize
import qualified SibDoc.FromParsed as FP
import qualified PerfDoc.FromSibDoc as FS
import SibDoc.ParseSibDoc( parseSibDoc )
import SibDoc.SibDocData( ParsedSibDoc(..) )


csdOptionsRealtime = "-d -odac7"
-- csdOptionsRealtime = "-d -W -oc:/Temp/test2.wav"
csdOptionsFile = "-d -W -oc:/Temp/CsoundOutput/out.wav"


topLevelLoop = do
  --cwd <- getCurrentDirectory
  --xmlFileName <- mostRecentFile cwd "xml"
  --let configFileName = replaceExtension xmlFileName "cfg"
  loop Nothing


loop :: Maybe ProcessHandle -> IO ()
loop mh = do
  parsedCmd <- lineFetch `catches` allHandlersM

  -- find most recent sib file
  cwd <- getCurrentDirectory
  mostRecentSib <- mostRecentFile cwd "sib"
  putStrLn $ "Using this sib doc: " ++ (takeFileName mostRecentSib)

  let dfn = "g:/out.txt"             -- plugin dump filename
      --- cfn = joinPath [cwd, "current.cfg"]
      cfn = replaceExtension mostRecentSib "cfg"


  {-

  -- THE FOLLOWING: case of saving XML to ram disk
  cwd <- getCurrentDirectory
  let ramDiskPath = "g:\\"
  xfn <- mostRecentFile ramDiskPath "xml"  
  let (_,xfnBase) = splitFileName xfn
      cfn = joinPath [cwd, (replaceExtension xfnBase "cfg")]
  -}

  {-
  cwd <- getCurrentDirectory
  xfn <- mostRecentFile cwd "xml"
  let cfn = replaceExtension xfn "cfg"
  -}
  case parsedCmd of
    Nothing -> loop mh
    Just com -> case com of
      InpTerminateProcess ->
        case mh of
          Nothing -> loop mh 
          Just handle -> do
            terminateProcess handle `catches` allHandlers
            loop mh 
      InpQuit -> return ()
      InpPlay rs noPlayFileName mTempoMult mTransposition
        | not (validateMsrRangeArgs rs) ->
          do putStrLn $ "Something wrong with measure numbers. They must be "++
                      "in ascending order, and only the last one can be " ++
                      "a single number (non-range) "
             loop mh
        | otherwise ->
           do mh <- runInpPlay rs noPlayFileName mTempoMult mTransposition
                    dfn cfn `catches` allHandlersM
              loop mh 
      {-
      InpDumpMusDoc -> do
        dumpMusDoc xfn `catches` allHandlers
        loop mh
      -}
      InpDumpPerfDoc -> do
        dumpPerfDoc dfn cfn `catches` allHandlers
        loop mh
      InpParseError s -> do
        putStrLn s
        loop mh 
      InpCwd i -> do
        let s = hardCodedDirectories !! i
        putStrLn $ "Changing directory to " ++ s
        setCurrentDirectory s
        loop mh
                     
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

lineFetch :: IO (Maybe InpCmd)
lineFetch = do
  d <- getCurrentDirectory
  putStrLn d
  putStr "(cound) ---> "
  hFlush stdout
  li <- getLine
  if length li == 0
    then  lineFetch
    else do
      r <- evaluate $ lineParse li
      return r


-- runInpPlay
--
--   Generate a csd and possibly play it.
--  Input:
--    
runInpPlay :: [MsrRange] -> Maybe String -> Maybe Float -> Maybe Int -> 
              String -> String -> IO (Maybe ProcessHandle)
runInpPlay playRanges noPlayFileName mtempo mTransposition sibFName 
    configFName = do
  parsedSibDoc <- readSibDocDump sibFName
  let sibDoc = FP.convert parsedSibDoc
  (commands,allConfig) <- readConfig configFName
  let perfDoc1 =
        FS.toPerfDoc (PLD.susPedParts allConfig) (PLD.timeDiv allConfig) sibDoc
      blankSepNeed = PLD.blankMsrs allConfig 
      snips = computeSnipMsrRanges playRanges perfDoc1 blankSepNeed
      pdFinalized =
        applyCommands commands .
        finalizePerfDoc mtempo mTransposition snips $ perfDoc1
  if PD.isNullDoc pdFinalized 
    then do putStrLn "Null prepared PerfDoc; skipping."
            return Nothing
    else do 
      let csoundConfig = CsoundConfig (PLD.csoundInstrMap allConfig) 
            (PLD.mixer allConfig) 
          options = case noPlayFileName of
            Nothing -> csdOptionsRealtime
            Just _ -> csdOptionsFile
      csdName <- computeCsdName noPlayFileName
      writeFile csdName =<< toCsound options pdFinalized csoundConfig 
      cwd <- getCurrentDirectory
      case noPlayFileName of
        Just _ -> return Nothing
        Nothing -> do
          h <- runProcess "c:/Program Files/Csound/bin/csound.exe" [csdName]
               (Just cwd) Nothing Nothing Nothing Nothing
          return $ Just h

{-

OLD runInpPlay that used MusDoc (MusDoc is an XML representation)

-- runInpPlay
--
--   Generate a csd and possibly play it.
--  Input:
--    
runInpPlay :: [MsrRange] -> Maybe String -> Maybe Float -> Maybe Int -> 
              String -> String -> IO (Maybe ProcessHandle)
runInpPlay playRanges noPlayFileName mtempo mTransposition xfn cfn = do
  xmlData <- readTheXml xfn
  (commands,allConfig) <- readConfig cfn
  let musDoc = MF.toMusDoc xmlData
      {- musDoc2 = case mtempo of
        Nothing -> musDoc
        Just r -> multiplyTempos musDoc r -}
      perfDoc = PF.toPerfDoc musDoc (PlD.timeDiv allConfig)
      perfDoc2 = applyCommands commands perfDoc
      blankSepNeed = PlD.blankMsrs allConfig
      snips = computeSnipMsrRanges playRanges musDoc blankSepNeed
      -- 0 is "shear early" time
      pdPrepared = preparePerfDoc 0 mtempo mTransposition snips perfDoc2 

  if PD.isNullDoc pdPrepared 
    then do putStrLn "Null prepared PerfDoc; skipping."
            return Nothing
    else do 
      let csoundConfig = CsoundConfig (PlD.csoundInstrMap allConfig) 
                     (PlD.mixer allConfig) 
          options = case noPlayFileName of
            Nothing -> csdOptionsRealtime
            Just _ -> csdOptionsFile
      csdName <- computeCsdName noPlayFileName
      writeFile csdName =<< toCsound options pdPrepared csoundConfig 
      cwd <- getCurrentDirectory
      case noPlayFileName of
        Just _ -> return Nothing
        Nothing -> do
          h <- runProcess "c:/Program Files/Csound/bin/csound.exe" [csdName]
               (Just cwd) Nothing Nothing Nothing Nothing
          return $ Just h

-}

{-
-- 
-- Inputs
--   [MsrRange] :: arguments on command-line
--   PerfDoc
--   Int  :: needed separation in # of blank measures to terminate playback
computeSnipTimeRanges :: [MsrRange] -> PerfDoc -> Int -> [(Float,Float)]
computeSnipTimeRanges playRanges pd nblanks = 
  map (\(i,j) -> (msrNumToTime atm i, msrNumToTime atm j)) snipHoles
  where
  doc = PD.pMusDoc pd
  n = numMsrs doc
  actualMsrRanges :: [(Int,Int)] 
  actualMsrRanges = map mkActualRange playRanges
  mkActualRange :: MsrRange -> (Int,Int)
  mkActualRange (i,Nothing) = (i, firstNEmptyMeasures doc nblanks i - 1)
  mkActualRange (i, Just j) = (i,j)
  snipHoles = computeSnipHoles actualMsrRanges n
  atm = BT.deltaToAbsolute (PD.timeMap pd)
-}

{-
computeSnipMsrRanges :: [MsrRange] -> MusDoc -> Int -> [(Int,Int)]
computeSnipMsrRanges playRanges doc nblanks = snipHoles
  where
    n = numMsrs doc
    explicitMsrRanges = map computeExplicitRange playRanges
    computeExplicitRange :: MsrRange -> (Int,Int)
    computeExplicitRange (i,Nothing) = (i,firstNEmptyMeasures doc nblanks i-1)
    computeExplicitRange (i, Just j) = (i,j)
    snipHoles = computeSnipHoles explicitMsrRanges n


-- computeSnipHoles
--   Given a set of ranges to INCLUDE, this routine calculated the ranges
--   of measures to EXCLUDE
-- Inputs
--   [(Int,Int)] :: the ranges of measures to play (inclusive)
--   Int         :: the number of measures in the document
--
-- Output
--  [(Int,Int)] :: the measures to snip, where the second one IS ONE PAST
--                  the last measure to snip.
computeSnipHoles :: [(Int,Int)] -> Int -> [(Int,Int)]
computeSnipHoles playRanges n = catMaybes $ map processPair pairs
  where
  addendum = [(0,0)] ++ playRanges ++ [(n+1,0)]
  pairs = zip addendum (tail addendum)
  processPair :: ((Int,Int),(Int,Int)) -> Maybe (Int,Int)
  processPair ((_,x),(y,_)) = 
    if (x+1) == y 
    then Nothing
    else Just (x+1,y)


-- last measure is 
-- (,0) [(2,3) (7,9) (13,15)] (<number+1>,_)
--  snip from 1(beg) to 2(beg)
--       from 3(beg) to 7(beg)
-}

msrNumToTime :: AbsoluteTimeMap -> Int -> Float
msrNumToTime atm num = BT.locToTime "lkj[897" atm (Loc num 1)
  

{-
-- computeBegEndLocs
--
--  A function useful for processing the command line. The cmd line will
--  have 0, 1, or 2 measure #'s given
--   0 numbers are given: means play entire doc. 
--         So compute beg & end locs for entire doc
--   1 number is given: means play from that msr # to the next empty msr
--   2 numbers are given: means play from first msr # to second #, INCLUSIVE
computeBegEndLocs :: MusDoc -> [Int] -> Int -> (Loc,Loc)
computeBegEndLocs totalMsrs [] nblanks = (Loc 1 1, Loc (totalMsrs + 1) 1)
computeBegEndLocs doc [f] nblanks = (Loc f 1, Loc mn 1)
  where mn = firstNEmptyMeasures doc nblanks f
computeBegEndLocs doc [f,l] _ = (Loc f 1, Loc (l+1) 1)
-}

{-

computeBegEnd :: Maybe Loc -> Maybe Loc -> Maybe Loc -> Maybe Loc -> 
                 Maybe (Loc,Loc)
computeBegEnd b1 e1 b2 e2 
  | isJust bm && isJust em = Just (fromJust bm, fromJust em)
  | otherwise = Nothing
  where
    bm = computeMaybeLocs b1 b2
    em = computeMaybeLocs e1 e2

computeMaybeLocs :: Maybe Loc -> Maybe Loc -> Maybe Loc
computeMaybeLocs (Just l) _ = Just l
computeMaybeLocs Nothing l2 = l2

-}

lineParse ::  String -> Maybe InpCmd
lineParse s =
  case parse parseCmdLine "" s of 
    Left err -> Just $ InpParseError (show err)
    Right cmd -> Just $ cmd

{-

-- compHelp
--
--  String:: filename of XML
--  String:: filename of config
--  Maybe (Loc,Loc) :: if present, trim the document
--  RunMode :: indicate a csd for realtime play vs. for rendering to file
compHelp :: String -> String -> Maybe (Loc,Loc) -> RunMode -> IO String
compHelp xmlFileName configFileName mll rm = do
  -- xmlFileName <- computeXmlFileName
  parsed <- readTheXml xmlFileName
  let doc = MF.toMusDoc parsed
      docFilt = case mll of
        Nothing -> doc
        Just (l1,l2) -> snipDoc l1 l2 doc
      perfDoc = PF.toPerfDoc docFilt
  (commands,instrmaps,mixer) <- readConfig configFileName
  let perfDoc2 = applyCommands commands perfDoc 
      options = case rm of
        RealtimeMode -> csdOptionsRealtime
        FileMode ->     csdOptionsFile
  toCsound options perfDoc2 instrmaps mixer

-}


applyCommands :: [Command] -> PerfDoc -> PerfDoc
applyCommands cs pd = foldl step pd cs
  where
    atm = BT.deltaToAbsolute (PD.timeMap pd)
    -- mdw = mergeDirectionWords (map PD.partData . M.elems . PD.pParts $ pd)
    step :: PerfDoc -> Command -> PerfDoc
    step pd' com = execute pd' com {- mdw -} atm
    

{-
computeXmlFileName :: IO String
computeXmlFileName = do
  pathResult <- defaultXmlPath
  let path = case pathResult of
        Just s -> s
        Nothing -> error "env variable DEFAULT_XML_PATH not defined"
  mostRecentFile path "xml"

computeXmlFileNameCwd :: IO String
computeXmlFileNameCwd = do
  pathResult <- getCurrentDirectory
  mostRecentFile pathResult "xml"

--
-- Result:
--   X.Score_Partwise:  the parsed XML
--   String:            the XML full file path and name (and extension)
readTheXml :: String -> IO X.Score_Partwise
readTheXml fileName = do
  parsed <- X.read_FILE X.read_MusicXML_Partwise fileName
  if X.isOK parsed
     then return $ X.fromOK parsed
     else error $ X.fromError parsed
-}

{-
dumpMusDoc :: String -> IO ()
dumpMusDoc xfn = do
  xml <- readTheXml xfn
  let doc = MF.toMusDoc xml
  writeFile (replaceExtension xfn "txt") $ showItem $ showi doc
  putStrLn "Sucessful dump."
-}

{-

  XML version of dumpPerfDoc

dumpPerfDoc :: String -> String -> IO ()
dumpPerfDoc xfn cfn = do
  xml <- readTheXml xfn
  let doc = MF.toMusDoc xml
  (commands,allConfig) <- readConfig cfn
  let perfDoc = PF.toPerfDoc doc (PlD.timeDiv allConfig)
  writeFile (replaceExtension xfn "txt") $ showItem $ showi perfDoc
  putStrLn "Dumped PerfDoc (_WITHOUT_ applying any commands)"
-}

dumpPerfDoc dfn cfn = do
  parsedSibDoc <- readSibDocDump dfn
  let sibDoc = FP.convert parsedSibDoc
  (_,allConfig) <- readConfig cfn
  let perfDoc = FS.toPerfDoc (PLD.susPedParts allConfig) 
                (PLD.timeDiv allConfig) sibDoc
      perfDoc2 = finalizePerfDoc Nothing Nothing [] $ perfDoc
  writeFile (replaceExtension cfn "txt") . showItem . showi $ perfDoc2
  putStrLn "Dumped PerfDoc (_WITHOUT_ applying any commands or snips [but with finalizing])"

readConfig :: String -> IO ([Command],AllConfig)
readConfig configFileName = do
  configBuffer <- readFile configFileName
  case parse parseConfigFile "" configBuffer of
     Left err -> throwMine $ "Parse error: " ++ (show err)
     Right cs -> return cs
 

{-
mergeDirectionWords :: [Part] -> Map DirectionWords [Loc]
mergeDirectionWords parts = reverseLMap u
  where
    wordMaps :: [Map Loc [DirectionWords]]
    wordMaps = map MD.directionWords parts
    u = M.unionsWith (++) wordMaps
-}

computeCsdName :: Maybe String -> IO String
computeCsdName (Just s) = return s
computeCsdName Nothing = do
  let possibleNames = map (\d -> "out" ++ show d ++ ".csd") [1..9]
  cwd <- getCurrentDirectory
  fs <- getDirectoryContents cwd
  -- pns:: [(FilePath, Maybe ClockTime)]
  pns <- mapM (checkPossibleName fs) possibleNames
  let (pns1,pns2) = partition (\(_,mc) -> isNothing mc) pns
  case pns1 of
    x:_       -> return $ fst x
    otherwise ->
      let pns2s = sortBy (\(_,jt1) (_,jt2) -> compare (fromJust jt1)
                                              (fromJust jt2)) pns2
      in return $ fst $ head pns2s
     
  

checkPossibleName :: [FilePath] -> FilePath -> IO (FilePath, Maybe ClockTime)
checkPossibleName contents possibleName =
  if any (== possibleName) contents
  then do
    t <- getModificationTime possibleName
    return $ (possibleName, Just t)
  else return $ (possibleName, Nothing)


{-
testComputeCsdName = do
  fcs <- computeCsdName
  putStrLn fcs
  -- mapM_ (\(x,y) -> putStrLn (show x ++ "\n" ++ show y ++ "\n")) fcs



testCheckPossibleName = do
  cwd <- getCurrentDirectory
  fs <- getDirectoryContents cwd
  (fp, mct) <- checkPossibleName fs "out.csd"
  putStrLn $ show fp
  putStrLn $ show mct
-}

readSibDocDump :: String -> IO ParsedSibDoc
readSibDocDump fn = do
  b <- readFile fn
  case parse parseSibDoc "" b of
    Left err -> throwMine $ "Error parsing Sib. dump: " ++ show err
    Right psd -> return psd
   

--------------------------------------------------------------------
--          Exception handlers
--------------------------------------------------------------------

allHandlersM = [Handler myHandlerM, Handler ioHandlerM]

myHandlerM :: MyException -> IO (Maybe a)
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s
  return Nothing

ioHandlerM :: IOException -> IO (Maybe a)
ioHandlerM e = do
  putStrLn (show e)
  return Nothing

{-
someExceptionM :: SomeException -> IO (Maybe a)
someExceptionM e = do
  putStrLn (show e)
  return Nothing
-}

allHandlers = [Handler myHandler, Handler ioHandler]

myHandler :: MyException -> IO ()
myHandler (MyException s) = putStrLn $ "Error: " ++ s

ioHandler :: IOException -> IO ()
ioHandler e = putStrLn (show e)

