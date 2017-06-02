{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Text.Printf
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.XmlContent
import Util.Showable


instance Showable (Element i) where
  showi (Elem name attrs contents) = 
    Component (printf "Element name:%s" (show name)) True [sAttrs,sContents]
    where
      sAttrs = Component "Attributes:" True (map showi attrs)
      sContents = Component "Contents:" True (map showi contents)

instance Showable Attribute where
  showi (name,value) = SingleLine (printf "name:%s value:%s" 
                       (show name) (show value))

instance Showable (Content i) where
  showi (CElem elem _) = showi elem
  showi (CString _ dat _) = SingleLine $ "CString:" ++ show dat
  showi (CRef _ _) = SingleLine $ "CRef (don't know how to display)"
  showi (CMisc _ _) = SingleLine $ "CMisc (don't know how to display)"

main = do
  let filename = "c:/Users/Mike/crit/Music/compositions/2012/mus321/" ++
                 "class-final-2012-11-06.xml"
  buf <- readFile filename
  let Document _ _ elem _ = xmlParse filename buf
  writeFile "out.txt" (showItem . showi $ elem)
 
