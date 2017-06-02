
import Data.Ratio
import Data.List 
import Text.XML.Light
import XmlDoc.ParseXml
import Util.Showable
import XmlDoc.ShowXmlDoc
import XmlDoc.ToIXmlDoc
import XmlDoc.ParseXml


xmlFile = "c:/users/mike/out.xml"

main = do 
  buf <- readFile xmlFile
  let cs = parseXML buf
      e = case find 
        (\x -> "score-partwise" == (qName . elName $ x)) (onlyElems cs) of
          Just x -> x
  writeFile "out.txt" (showItem . showi . toIXmlDoc . parseXScore $ e)


r :: Rational -> Double
r = fromRational