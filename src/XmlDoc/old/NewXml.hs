module XmlDoc.NewXml where


import Data.List
import Data.Monoid
import Data.Maybe
import Text.Printf
import Text.XML.Light
import Util.Showable
import XmlDoc.ShowXml
import Util.Exception
import XmlDoc.NewXmlData

parseScore :: [Content] -> XScore
parseScore cs = 
  case find ((== "score-partwise") . qName . elName) (onlyElems cs) of
    Nothing -> throwMine "no score-partwise element found in parsing XML"
    Just e -> toScore e
  where
    toScore :: Element -> XScore
    toScore e = XScore (parsePartList e) (parseParts e)


parsePartList :: Element -> XPartList
parsePartList e = case findElement (simpleName "part-list") e of
  Nothing -> throwMine "no part-list element found in parsing XML"
  Just x -> XPartList  x


parseParts :: Element -> [XPart]
parseParts e = map toPart . findElements (simpleName "part") $ e
  where
    toPart :: Element -> XPart
    toPart e = XPart i e
      where
        i = case findAttr (simpleName "id") e of {Just x -> x}


filterContentDepth :: Int -> Content -> Maybe Content
filterContentDepth 0 _ = Nothing
filterContentDepth d (Elem e) = Just $ Elem (e {elContent = newCs})
  where
    newCs = mapMaybe (filterContentDepth (d-1)) . elContent $ e
filterContentDepth _ t@(Text _) = Just t
filterContentDepth _ c@(CRef _) = Just c


report :: [Content] -> String
report cs = printf "nElem:%d nText:%d nCRef:%d" nElem nText nCRef
  where
    nElem = length [() | Elem _ <- cs]
    nText = length [() | Text _ <- cs]
    nCRef = length [() | CRef _ <- cs]


reportContents :: [Content] -> ShowItem
reportContents cs = Component "Contents" True (map showi cs)


main = do
  b <- readFile "music.xml"
  let f = mapMaybe (filterContentDepth 2) $ parseXML b
      o = showItem . reportContents $ f
  writeFile "xml.txt" o


main2 = do
  b <- readFile "c:/users/mike/crit/music/compositions/2015/waifie/waifie-01-B.xml"
  let cs = parseXML b
      score = parseScore cs
  writeFile "xml.txt" . showItem . showi $ score
