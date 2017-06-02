-- standard library import
import System
import System.FilePath
-- my own library import
import Util.FileUtil
import App.CsoundPlayer.RunFunctions( compHelp )
import App.CsoundPlayer.CsoundPlayerData

main = do
  cwd <- getCurrentDirectory
  xmlFileName <- mostRecentFile cwd "xml"
  configFileName = replaceExtension xmlFileName "cfg"
  -- args <- getArgs
  compHelp xmlFileName configFileName Nothing RealtimeMode
 