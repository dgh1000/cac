{-
cp2.hs
  first experiment with end-to-end test
    - reading XML
    - reading an expression-config
    - generating csound

-}

import System
import System.FilePath
import System.Process
import System.Exit
import Text.Parsec
import Data.Ratio
import qualified Data.Map as M
import Data.Map( Map )
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
import App.CsoundPlayer.ParseConfig( parseConfigFile )
import App.CsoundPlayer.Execute
import App.CsoundPlayer.ConfigData( Command(..) 
                                  , InstrMap(..)
                                  , Mixer(..) )


csdOptionsRealtime = "-d -odac\n"
csdOptionsFile = "-d -W -oc:\\Temp\\test.wav\n"

--   select a portion of the XML document using command-line args
--         and compute CSD
cmds :: IO ()
cmds = do
  args <- getArgs
  case args of
    [m1,b1,m2,b2] -> 
      compCsd (read m1) (read b1) (read m2) (read b2)
    _       -> error "Should be four args"


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



compCsd :: Int -> Rational -> Int -> Rational -> IO ()
compCsd m1 b1 m2 b2 = do
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
      csdText <- toCsound csdOptionsRealtime pdoc2 instrmaps mixer
      writeFile "out.txt" (showItem (showi doc))
      writeFile "out.csd" csdText
    else putStrLn $ X.fromError parsed

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

readXml :: FilePath -> IO (X.Result X.Score_Partwise)
readXml s =  X.read_FILE  X.read_MusicXML_Partwise s
 
readConfig :: String -> IO ([Command],[InstrMap],Mixer)
readConfig fp = do
  configBuffer <- readFile fp
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
