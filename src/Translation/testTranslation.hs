
import System.Random
import Data.List as L
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import Translation.Translation
import Translation.TranslationData
import XmlDoc.ParseXml
import Text.XML.Light
import Score.XmlToScore
import Score.ScoreData
import Score.ShowScore
import Util.FileUtil
import Util.Showable
import Instruments.Piano(piano)
import Translation.ShowTranslation


readXml :: IO Score
readXml = do
  buf <- readFileStrictly "c:/users/mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e

instrConfig1 = StaffConfig
  { icStaffName = "Piano-staff1"
  , icInstr     = piano
  , icMidiDests = M.fromList [("1",(0,1))]
  , icParams    = M.empty
  , icSusPed    = SusPedNotUsed
  }

instrConfig2 = StaffConfig
  { icStaffName = "Piano-staff2"
  , icInstr     = piano
  , icMidiDests = M.fromList [("1",(0,2))]
  , icParams    = M.empty
  , icSusPed    = SusPedNotUsed
  }

playbackConfig begMsr endMsr = PlaybackConfig
  { pcStaffConfigs = M.fromList [ ("Piano-staff1", instrConfig1)
                                , ("Piano-staff2", instrConfig2) ]
  , pcSusPed       = Nothing
  , pcTempoChange  = Nothing
  , pcMsrRange     = (begMsr, endMsr)
  }

initialState score gen begMsr endMsr = TrState 
  { tsPlaybackConfig  = playbackConfig begMsr endMsr
  , tsScore           = score
  , tsTimeMaps        = M.empty
  , tsLoudnessCurves  = M.empty
  , tsLoudnessCurveDs = M.empty
  , tsRandomGen       = gen
  }

main = do
  let begMsr  =  6
      endMsr  =  Nothing
  score       <- readXml
  gen         <- getStdGen
  let st      = initialState score gen begMsr endMsr
      ((_,finalSt),output) = runWriter (runStateT scoreToMidi st)
      records =  [r | r@OutputRecord{} <- output]
  writeFile "out.txt"  $ 
    (showItem . showi $ finalSt) ++ concatMap (showItem . showi) output
  writeFile "score.txt" $ showItem . showi $ score

