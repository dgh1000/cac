import Data.List
import Text.XML.Light
import XmlDoc.ShowXmlDoc
import XmlDoc.ParseXml
import Util.Showable
import Util.Exception

xmlFile = "c:/users/mike/out.xml"

dump = do
  buf <- readFile xmlFile
  let cs = parseXML buf
      e = case find 
        (\x -> "score-partwise"==(qName . elName $ x)) (onlyElems cs) of
          Nothing -> throwMine "No score-partwise element found"
          Just x -> x
  writeFile "dump.txt" (showItem . showi . ShowSelectiveElement $ e)

main = do
  buf <- readFile xmlFile
  let cs = parseXML buf
      e = case find 
        (\x -> "score-partwise"==(qName . elName $ x)) (onlyElems cs) of
          Nothing -> throwMine "No score-partwise element found"
          Just x -> x
  writeFile "out.txt" (showItem . showi . parseXScore $ e)

