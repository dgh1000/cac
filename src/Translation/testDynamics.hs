
import qualified Data.Map as M
import Text.Printf
import qualified Data.List as L
import Text.XML.Light
import XmlDoc.ParseXml
import Score.XmlToScore
import Score.ShowScore
import Score.ScoreExport
import Translation.Dynamics
import Util.FileUtil
import Util.Showable

readXml :: IO Score
readXml = do
  buf <- readFileStrictly "c:/users/mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e



-- test computing dynamic/hairpin curve        
main = do
  score <- readXml
  let loudCurs = M.map computeDynHairpinLoudCur (scStaves score)
  writeFile "out.txt" . concatMap  (showItem . showi) . M.toList $ loudCurs


-- test computing accent loudness curve
main2 = do
  score <- readXml
  let toCurve name = (name, computeAccentLoudCur1 score 1.0 name)
      curves = M.fromList . map toCurve . M.keys . scStaves $ score
  writeFile "out.txt" . concatMap (showItem . showi) . M.toList $ curves
  