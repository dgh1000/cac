{-

This module is kind of disorganized but the main useful function here
is 'compHelp' which goes about computing a CSD file.

Also 'lineParse' is first cut at a function that parses cmd-line arguments

-}


import System
import System.FilePath
import System.Process
import System.Exit
import Text.Parsec
import Data.Ratio
import qualified Data.Map as M
import Data.Map( Map )
import Data.Maybe
import qualified Text.XML.MusicXML as X
import qualified Text.XML.MusicXML.Partwise as X
import qualified Text.XML.MusicXML.Score as X
import qualified Expression.Time as T
import qualified MusDoc.FromXml as MF
import MusDoc.ShowMusDoc
import MusDoc.Processing
import qualified PerfDoc.FromMusDoc as PF
import PerfDoc.PerfDocData( PerfDoc )
import qualified PerfDoc.PerfDocData as PD
import Csound.FromPerfDoc( toCsound )
import System.Environment
import Util.FileUtil
import Util.Env
import Util.Math
import Util.Showable
import Util.Map
import PerfDoc.ShowPerfDoc
import MusDoc.Data( MusDoc(..)
                  , Loc(..) 
                  , DirectionWords(..) )
import qualified MusDoc.Data as MD
import qualified PerfDoc.BasicTiming as BT
import Csound.CsoundData( InstrMap
                        , Mixer(..) )
import App.CsoundPlayer.ParseConfig( parseConfigFile )
import App.CsoundPlayer.ParseCmdLine( parseCmdLine )
import App.CsoundPlayer.Execute
import App.CsoundPlayer.CsoundPlayerData( Command(..) 
                                        , RunMode(..) )


csdOptionsRealtime = "-d -odac\n"
csdOptionsFile = "-d -W -oc:\\Temp\\test.wav\n"

{-
--   select a portion of the XML document using command-line args
--         to specify start?end measure/beat
main :: IO ()
main = do
  args <- getArgs
  case args of
    [m1,b1,m2,b2] ->
      let loc1 = Loc (read m1) (readBeat b1)
          loc2 = Loc (read m2) (readBeat b2)
      in do 
        csdText <- compHelp (Just (loc1,loc2)) RealtimeMode
        writeFile "out.csd" csdText
    _       -> error "Should be four args"
-}

main = loop Nothing Nothing



data InpData = InpQuit
             | InpPlay (Maybe Loc) (Maybe Loc)  -- begin loc, end loc
             | InpParseErr String


loop :: Maybe Loc -> Maybe Loc -> IO ()
loop currBeg currEnd = do
  li <- getLine
  case lineParse li of
    InpQuit -> return ()
    InpParseErr s -> do
      putStrLn s
      loop currBeg currEnd
    InpPlay newBeg newEnd ->
      case computeBegEnd newBeg newEnd currBeg currEnd of
        Nothing -> do
          putStrLn "error either beg or end not available"
          loop currBeg currEnd
        Just (b,e) -> do
          csdText <- compHelp (Just (b,e)) RealtimeMode
          writeFile "out.csd" csdText
          putStrLn "Successful creation of csd."
          loop (Just b) (Just e)

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

lineParse ::  String -> InpData
lineParse s
  | head s == 'q' = InpQuit
  | head s == 'p' = 
    case parse parseCmdLine "" (tail s) of
      Left err -> InpParseErr $ show err
      Right (mayBeg,mayEnd) -> InpPlay mayBeg mayEnd

-- ghci entry point, when using only a section of the document
maini :: Int -> Rational -> Int -> Rational -> IO ()
maini m1 b1 m2 b2 = 
  let loc1 = Loc m1 b1
      loc2 = Loc m2 b2
  in do
    csdText <- compHelp (Just (loc1,loc2)) RealtimeMode
    writeFile "out.csd" csdText


-- ghci entry point, when playing whole document
maina :: IO ()
maina = do
  csdText <- compHelp Nothing RealtimeMode
  writeFile "out.csd" csdText

{-
-- COMPUTE AND RUN
--
runr :: IO ()
runr = do
  compOpt csdOptionsRealtime
  code <- system("csound out.csd") 
  case code of
    ExitSuccess -> return ()
    ExitFailure e -> putStrLn "error in running process"


-- COMPUTE CSD
--  for realtime playback: call compOpt with options for realtime
compr :: IO ()
compr = compOpt csdOptionsRealtime

-- COMPUTE CSD
--   fo file render: call compOpt with options for file render
compf :: IO ()
compf = compOpt csdOptionsFile
-}

{-
compCsd :: RunMode -> IO String

compTrimCsd :: Int -> Rational -> Int -> Rational -> RunMode -> IO String
compTrimCsd m1 b1 m2 b2 rm = do
  pathResult <- defaultXmlPath
  let path = case pathResult of 
               Just s -> s
               Nothing -> error "env variable DEFAULT_XML_PATH not defined"
  filepath <- mostRecentFile path "xml"
  parsed <- readXml filepath  
  if X.isOK parsed
    then do
      let doc = MF.toMusDoc $ X.fromOK parsed
          -- First make a PerfDoc derived directly from MusDoc without needing
          -- any algorithms to modify the performance
          docFilt = snipDoc (Loc m1 b1) (Loc m2 b2) doc
          pdoc = PF.toPerfDoc docFilt
          cfname = replaceExtension filepath "cfg"
      (commands,instrmaps,mixer) <- readConfig cfname
      let atm = BT.deltaToAbsolute (PD.timeMap pdoc)
          md = PD.musDoc pdoc
          mdw = mergeDirectionWords md
          pdoc2 = foldl step pdoc commands
            where step :: PerfDoc -> Command -> PerfDoc
                  step pd com = execute pd com mdw atm
          options = case rm of
            RealtimeMode -> csdOptionsRealtime
            FileMode     -> csdOptionsFile
      toCsound options pdoc2 instrmaps mixer
    else putStrLn $ X.fromError parsed
-}

{-
readBeat :: String -> Rational
readBeat s = (read s) % 1
-}

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
  (commands,instrmaps,mixer) <- readConfig xmlFileName
  let perfDoc2 = applyCommands commands perfDoc 
      options = case rm of
        RealtimeMode -> csdOptionsRealtime
        FileMode ->     csdOptionsFile
  toCsound options perfDoc2 instrmaps mixer



applyCommands :: [Command] -> PerfDoc -> PerfDoc
applyCommands cs pd = foldl step pd cs
  where
    atm = BT.deltaToAbsolute (PD.timeMap pd)
    md = PD.musDoc pd
    mdw = mergeDirectionWords md
    step :: PerfDoc -> Command -> PerfDoc
    step pd' com = execute pd' com mdw atm
    

{-
  pathResult <- defaultXmlPath
  let path = case pathResult of 
               Just s -> s
               Nothing -> error "env variable DEFAULT_XML_PATH not defined"
  filepath <- mostRecentFile path "xml"
  parsed <- readXml filepath  
  if X.isOK parsed
    then do
      let doc = MF.toMusDoc $ X.fromOK parsed
          -- First make a PerfDoc derived directly from MusDoc without needing
          -- any algorithms to modify the performance
          docFilt = snipDoc (Loc m1 b1) (Loc m2 b2) doc
          pdoc = PF.toPerfDoc docFilt
          cfname = replaceExtension filepath "cfg"
      (commands,instrmaps,mixer) <- readConfig cfname
      let atm = BT.deltaToAbsolute (PD.timeMap pdoc)
          md = PD.musDoc pdoc
          mdw = mergeDirectionWords md
          pdoc2 = foldl step pdoc commands
            where step :: PerfDoc -> Command -> PerfDoc
                  step pd com = execute pd com mdw atm
          options = case rm of
            RealtimeMode -> csdOptionsRealtime
            FileMode     -> csdOptionsFile
      toCsound options pdoc2 instrmaps mixer
    else putStrLn $ X.fromError parsed
-}

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

{-
-- compOpt
--
--   Make csound code
--
--   Will read the newest XML file at the path DEFAULT_XML_PATH
--     and convert it to csound
--
--   
--   String:  options to include in CSD file
compOpt :: String -> IO ()
compOpt opts = do
  pathResult <- defaultXmlPath
  let path = case pathResult of 
               Just s -> s
               Nothing -> error "env variable DEFAULT_XML_PATH not defined"
  filepath <- mostRecentFile path "xml"
  parsed <- readXml filepath  
  if X.isOK parsed
    then do
      let doc = MF.toMusDoc $ X.fromOK parsed
          -- First make a PerfDoc derived directly from MusDoc without needing
          -- any algorithms to modify the performance
          pdoc = PF.toPerfDoc doc
          cfname = replaceExtension filepath "cfg"
      (commands,instrmaps,mixer) <- readConfig cfname
      let atm = BT.deltaToAbsolute (PD.timeMap pdoc)
          md = PD.musDoc pdoc
          mdw = mergeDirectionWords md
          pdoc2 = foldl step pdoc commands
            where step :: PerfDoc -> Command -> PerfDoc
                  step pd com = execute pd com mdw atm
      csdText <- toCsound opts pdoc2 instrmaps mixer
      writeFile "out.txt" (showItem (showi doc))
      writeFile "out.csd" csdText
    else putStrLn $ X.fromError parsed
-}

{-
readXml :: FilePath -> IO (X.Result X.Score_Partwise)
readXml s =  X.read_FILE  X.read_MusicXML_Partwise s
-} 

readConfig :: String -> IO ([Command],[InstrMap],Mixer)
readConfig configFileName = do
  -- let configFileName = replaceExtension xmlFileName "cfg"
  configBuffer <- readFile configFileName
  case parse parseConfigFile "" configBuffer of
     Left err -> error $ "Parse error: " ++ (show err)
     Right cs -> return cs
 

{-
doSomeExpressiveStuff :: PerfDoc -> PerfDoc
doSomeExpressiveStuff pd = T.warpTime pd (Loc 1 (1%1)) (Loc 2 (1%1)) c
    where c = Curve 0 1 [(0,1), (0.5,0.75), (1,1)]
-}

mergeDirectionWords :: MusDoc -> Map DirectionWords [Loc]
mergeDirectionWords md = reverseLMap u
  where
    maps = map (\(_,(_,part)) -> MD.directionWords part) $
           M.toList (MD.parts md)
    u = M.unionsWith (++) maps

