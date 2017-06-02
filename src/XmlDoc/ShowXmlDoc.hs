{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module XmlDoc.ShowXmlDoc where


import Text.Printf
import Text.XML.Light
import Data.Maybe
import qualified Data.Map as M
import Data.Map(Map)
import Util.Showable
import XmlDoc.XmlDocData
import Common.CommonExport


instance Showable XScore where
  showi doc = Component "XScore" False [sPartInfos,sParts]
    where
      sPartInfos = Component "part infos:" True 
        (map showi . M.toList . xPartInfos $ doc)
      sParts = Component "Parts:" True (map showi . M.toList . xParts $ doc)

type IdXPartInfo = (String,XPartInfo)

instance Showable IdXPartInfo where
  showi (id_,XPartInfo name) = SingleLine $ printf "XPartInfo: Id:%s Name:%s"
                               id_ name

type IdXPart = (String,XPart)

instance Showable IdXPart where
  showi (id_,XPart msrs) = Component (printf "Part '%s':" id_) True 
                           (map showi msrs)

instance Showable XMsr where
  showi (XMsr i attr datas) = Component 
    (printf "Msr number:%d %s" i sAttr) True (map showi datas)
    where
      sAttr = case attr of 
        Nothing -> "No time/divisions attributes."
        Just a -> printf "dpq:%s beats:%s beat-type:%s" (show $ xaDpq a)
          (show $ xaNumer a) (show $ xaDenom a)

instance Showable XMsrData where
  showi (XMDNote (XNRest dur ch v s nots)) = 
    Component (printf "rest: dur:%d chord:%s voice:%s staff:%s" dur (show ch) 
                       (show v) (show s)) True (map showi nots)
  showi (XMDNote (XNNote dur isGra ch v st pit tieStart tieStop nots _)) = 
    Component (printf ("note dur:%d stp:%s alt:%d oct:%d tie:%s %s") 
               dur (xStep pit)
               (xAlter pit) (xOctave pit) (show tieStart) (show tieStop))
               True ([secondLine] ++ (map showi nots))
      where
        secondLine = SingleLine 
                     (printf "   grace:%s chord:%s voice:%s staff:%s" 
                     (show isGra) (show ch) (show v) (show st))
  showi (XMDDirection dir offset mVoice mStaff) = 
      SingleLine (printf "%s %s %s %s" (show dir) m voice staff)
    where
      m = case offset of 
        Nothing -> ""
        Just off -> printf "offset:%d" off
      voice = case mVoice of
        Nothing -> ""
        Just v -> printf "voice:%d" v
      staff = case mStaff of {Nothing -> ""; Just s -> "staff:" ++ (show s)}
  showi (XMDBackup dur) = SingleLine (printf "backup: %d" dur)
  showi (XMDForward dur) = SingleLine (printf "forward: %d" dur)
  showi (XMDOther s) = SingleLine (printf "other: %s" s)

instance Showable XNotation where
  showi (XNSlur s l) = SingleLine $ printf "Slur: %s %s" s ml
    where
      ml = case l of 
        Nothing -> ""
        Just i -> printf "slur number:%d" i
  showi (XNArticulations a) = Component "Articulations:" True (map showi a)
  showi (XNOrnaments a) = Component "Ornaments:" True (map showi a)
  showi XNFermata = SingleLine "Fermata"
  showi XNArpeggiate = SingleLine "Arpeggiate"
  showi (XNTechnical ts) = Component "Technical notations:" True (map showi ts)

instance Showable XTechnical where
  showi OpenString = SingleLine "open-string"

instance Showable XArticulation where
  showi XAStaccato = SingleLine "Staccato"
  showi XAStaccatissimo = SingleLine "Staccatissimo"
  showi XAAccent = SingleLine "Accent"
  showi XAStrongAccent = SingleLine "Strong accent"
  showi XATenuto = SingleLine "Tenuto"

instance Showable XOrnament where
  showi o = SingleLine (show o)

type XMsrDataAtLoc = (Loc,[XMsrData])
instance Showable XMsrDataAtLoc where
  showi (l,d) = Component (simpleShowLoc l) True (map showi d)

----------------------------------------------------------------------
----------------------------------------------------------------------
--                Text.XML.Light

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
    "score-partwise" -> c2
    "part" -> c2
    "part-list" -> c2
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
      c2 = Component (elementToLine e) True 
        (map (showi . ShowSelectiveElement) (filterChildren (const True) e))
      t = SingleLine $ printf "%s '%s'" (elementToLine e) (strContent e)
      tNoText = SingleLine (elementToLine e)
      


instance Showable Attr where
  showi a = SingleLine $ printf "name:%s value:%s" (qName . attrKey $ a)
            (attrVal a)

instance Showable CData where
  showi c = SingleLine $ printf "cdData:%s" (take 50 . cdData $ c)


----------------------------------------------------------------------
----------------------------------------------------------------------
--              IXmlDoc

{-
instance Showable IXmlDoc where
  showi (IXmlDoc mis parts) = Component "IXmlDoc" False [sMis,sParts]
    where
      sMis = Component "IXMsrInfo" True (map showi . M.toList $ mis)
      sParts = Component "IXParts" True (map showi . M.toList $ parts)

type PairIXMsrInfo = (Int,IXMsrInfo)
instance Showable PairIXMsrInfo where
  showi (idx,IXMsrInfo dpq numer denom) 
     = SingleLine $ printf "%3d: dpq:%4d numer:%2d denom:%2d" 
       idx dpq numer denom


type PairIXPart = (String,IXPart)
instance Showable PairIXPart where
  showi (name, IXPart msrDatas notes) = Component name True [sMsrDatas,sNotes]
    where 
      sMsrDatas = Component "XMsrData" True (map showi (M.toList msrDatas))
      sNotes = Component "XNotes" True (map showi (M.toList notes))
-}



{-
type XNoteAtLoc = (Loc,[XNote])
instance Showable XNoteAtLoc where
  showi (l,ns) = Component (simpleShowLoc l) True (map showi ns)

instance Showable XNote where
  -- exhausted cases here, because there is no definition for showi on the
  -- XNRest constructor, means we expected to show XNotes only inside 
  -- IXScores
  showi (XNNote _ endLoc isGrace _ voice staff pitch tieStart tieStop
        notations notehead) 
    = Component (printf "XNNote endLoc:%s voice:%s staff:%s"
      (simpleShowLoc endLoc) (show voice) (show staff)) True [l1,l2,ns]
    where
      showMaybeNotehead Nothing = "Nothing"
      showMaybeNotehead (Just n) = xnhType n 
      l1 = SingleLine $ printf "isGrace:%s step:%s alter:%d octave:%d"
           (show isGrace) (xStep pitch) (xAlter pitch) (xOctave pitch)
      l2 = SingleLine $ printf "tieStart:%s tieStop:%s notehead:%s" 
           (show tieStart) (show tieStop) (showMaybeNotehead notehead)
      ns = Component "Notations:" True (map showi notations)
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--               testing

type TestPartMap = Map String TestPart
instance Showable TestPartMap where
  showi m = Component "All parts" False (map showi . M.toAscList $ m)

type PairTestPart = (String,TestPart)
instance Showable PairTestPart where
  showi (pid,TestPart msrDatas) = Component pid True 
    (map showi . M.toAscList $ msrDatas)
