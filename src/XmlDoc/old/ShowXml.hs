{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module XmlDoc.ShowXml where

import Text.Printf
import Text.XML.Light
import Data.Maybe
import Util.Showable
import XmlDoc.NewXmlData

data ShowSelectiveElement = ShowSelectiveElement Element

data ShowSimpleElement = ShowSimpleElement Element

instance Showable Content where
  showi (Elem e) = Component "Content cons Elem" True [showi e]
  showi (Text t) = Component "Content cons Text" True [showi t]
  showi (CRef s) = SingleLine $ "Content cons CRef: " ++ s

instance Showable Element where
  showi e = Component "Element:" True [name,attribs,contents]
    where
      name = SingleLine $ "elname:" ++ (qName . elName $ e)
      attribs = Component "Attr:" True (map showi . elAttribs $ e)
      contents = Component "Content:" True (map showi . elContent $ e)

instance Showable ShowSimpleElement where
  showi (ShowSimpleElement e) = SingleLine . elementToLine $ e

elementToLine :: Element -> String
elementToLine e = printf "%s %s" (qName . elName $ e) concatAttrs
  where
    f :: Attr -> Maybe String
    f a = case (qName.attrKey$a,attrVal a) of
      ("default-x",_) -> Nothing
      ("default-y",_) -> Nothing
      ("font-family",_) -> Nothing
      ("font-size",_) -> Nothing
      ("font-weight",_) -> Nothing
      ("font-style",_) -> Nothing
      ("color",_) -> Nothing
      ("justify",_) -> Nothing
      ("valign",_) -> Nothing
      ("print-object",_) -> Nothing
      (n,v) -> Just $ printf " [%s:%s]" n v
    concatAttrs = concat . mapMaybe f . elAttribs $ e

instance Showable ShowSelectiveElement where
  showi (ShowSelectiveElement e) = case qName . elName $ e of
    "attributes" -> c2
    "part-group" -> c2
    "score-part" -> c2
    "measure" -> c2
    "note" -> c2
    "direction" -> c2
    "direction-type" -> c2
    "voice" -> t
    "staff" -> t
    "words" -> t
    "clef" -> c2
    "divisions" -> t
    "key" -> c2
    "time" -> c2
    "staves" -> t
    "dynamics" -> c2
    "metronome" -> c2
    "beat-unit" -> t
    "per-minute" -> t
    "other-direction" -> t
    "part-name" -> t
    _ -> SingleLine . elementToLine $ e
    where
      -- c1 = Component (elementToLine e) True 
      --   (map (showi . ShowSimpleElement) (filterChildren (const True) e))
      c2 = Component (elementToLine e) True 
        (map (showi . ShowSelectiveElement) (filterChildren (const True) e))
      t = SingleLine $ printf "%s '%s'" (elementToLine e) (strContent e)
      


instance Showable Attr where
  showi a = SingleLine $ printf "name:%s value:%s" (qName . attrKey $ a)
            (attrVal a)

instance Showable CData where
  showi c = SingleLine $ printf "cdData:%s" (take 50 . cdData $ c)

{-
instance Showable XScore where
  showi (XScore pList parts) = Component "XScore:" True
    [showi pList, showi parts]

type ManyXParts = [XPart]
instance Showable ManyXParts where
  showi ps = Component "XParts:" True (map showi ps)

instance Showable XPart where
  showi (XPart pid e) = Component (printf "XPart %s:" pid) True
    (map (showi . ShowSelectiveElement) . filterChildren (const True) $ e)

instance Showable XPartList where
  showi (XPartList e) = Component "XPartList:" True
    (map (showi . ShowSelectiveElement) . filterChildren (const True) $ e)
-}
