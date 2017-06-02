module XmlDoc.NewXmlData where

import Text.XML.Light

data XScore = XScore XPartList [XPart]

data XPartList = XPartList
  { xplElem :: Element }

data XPart = XPart 
  { xpId :: String
  , xpElem :: Element }

simpleName :: String -> QName
simpleName s = QName s Nothing Nothing

