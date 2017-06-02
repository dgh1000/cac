module XmlDoc.ParseXml where

import Debug.Trace
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.List as L
import Text.Printf
import Text.XML.HaXml.Types
import Text.XML.HaXml.XmlContent
import XmlDoc.XmlDocData
import XmlDoc.XmlUtil
import Util.Exception

parseMusXml :: Element i -> XmlDoc
parseMusXml elem =
  XmlDoc { xPartInfos = pPartList elem
         , xParts = pAllPart elem }


-- Input elem is score-partwise.
pPartList :: Element i -> Map String XPartInfo
pPartList elemIn = M.fromList $ processManyElems g e1
  where
    e1 = lookupEnforce1Elem "part-list" elemIn
    g :: Element i -> Maybe (String,XPartInfo)
    g elem = if elemName elem == "score-part"
      then Just $ pScorePart elem
      else Nothing


-- input elem is 'score-part'
pScorePart :: Element i -> (String,XPartInfo)
pScorePart elem = (id_, XPartInfo c)
  where
    id_ = lookupEnforceAttr "id" elem
    c = getEnforce1StringContent (lookupEnforce1Elem "part-name" elem)
    
pAllPart :: Element i -> Map String XPart
pAllPart elemIn = M.fromList (processManyElems g elemIn)
  where
    g elem2 | elemName elem2 /= "part" = Nothing
            | otherwise = Just $ pPart elem2

pPart :: Element i -> (String,XPart)
pPart elemIn = (id_, XPart msrs)
  where
    id_ = lookupEnforceAttr "id" elemIn
    xmsrs = lookupElems "measure" elemIn
    msrs = map pMsr xmsrs
    -- g elem2 = RMsr $ getEnforceIntAttr "number" elem2

pMsr :: Element i -> XMsr 
pMsr elemIn = XMsr num a ds
  where
    num = getEnforceIntAttr "number" elemIn
    a = case lookupElem "attributes" elemIn of
      Nothing -> Nothing
      Just e -> pMsrAttr e
    ds = catMaybes . map pMsrData . lookupElemsContent $ elemIn
    {-
    ds = processManyElems g elemIn
    g e2
      | n == "direction" = Nothing
      | n == "note" = Just . XMDNote . pXNote $ e2
      | otherwise = Nothing
      where n = elemName e2
    -}

pMsrAttr :: Element i -> Maybe XMsrAttr
pMsrAttr elemIn 
  | isNothing divs && isNothing beats && isNothing beatType = Nothing
  | otherwise = Just $ XMsrAttr divs beats beatType
  where
    divs = fmap getEnforce1IntContent (lookupElem "divisions" elemIn)
    (beats,beatType) = case lookupElem "time" elemIn of
      Nothing -> (Nothing,Nothing)
      Just e ->
        ( case lookupElem "beats" e of
            Nothing -> Nothing
            Just b -> Just $ getEnforce1IntContent b
        , case lookupElem "beat-type" e of
            Nothing -> Nothing
            Just b -> Just $ getEnforce1IntContent b)

pMsrData :: Element i -> Maybe XMsrData
pMsrData elemIn
  | n == "note" = Just . XMDNote . pXNote $ elemIn
  | n == "direction" = pDirection elemIn
  | n == "backup" = pBackup elemIn
  | n == "forward" = pForward elemIn
  | n == "print" = Just $ XMDOther n
  | n == "attributes" = Just $ XMDOther n
  | n == "barline" = Just $ XMDOther n
  | otherwise = (printf ("warning; unknown element in measure: '%s'")
                n) `trace` (Just $ XMDOther n)
  where
    n = elemName elemIn 

pBackup :: Element i -> Maybe XMsrData
pBackup elemIn =
  Just . XMDBackup . getEnforce1IntContent . lookupEnforce1Elem "duration" $ 
       elemIn

pForward :: Element i -> Maybe XMsrData
pForward elemIn =
  Just . XMDForward . getEnforce1IntContent . lookupEnforce1Elem "duration" $ 
       elemIn

pDirection :: Element i -> Maybe XMsrData
pDirection elemIn 
  | dtype == "metronome" 
     = Just $ XMDDirection (pMetronome dcontent) mOffset mvoice
  | dtype == "words" 
     = Just $ XMDDirection (pWords dcontent) mOffset mvoice
  | dtype == "dynamics" 
     = Just $ XMDDirection (pDynamics dcontent) mOffset mvoice
  | dtype == "wedge" 
     = Just $ XMDDirection (pWedge dcontent) mOffset mvoice
  | dtype == "pedal" 
     = Just $ XMDDirection (pPedal dcontent) mOffset mvoice
  | dtype == "other-direction" 
     = Just $ XMDDirection (pOtherDirection dcontent) mOffset mvoice
  | otherwise = (printf "warning: direction element I don't know about:%s"
     (debugReport elemIn)) `trace` Nothing
  where
    dcontent = pDirectionHelp elemIn
    mvoice = case lookupEnforceZeroOrOneElem "voice" elemIn of
      Nothing -> Nothing
      Just e -> Just $ getEnforce1IntContent e
    dtype = elemName dcontent
    mOffset = fmap getEnforce1IntContent (lookupElem "offset" elemIn)


-- pDirectionHelp
--   Input element is 'direction'. This enforces it has one 
--   <direction-type> element; looks at child elements of that;
--   enforces there is only one, and returns 
--   <single element that is content of direction-type>
pDirectionHelp :: Element i -> Element i
pDirectionHelp elemIn 
  | length es /= 1 = throwMine $ printf ("in pDirectionHelp, in '%s' elem, "++
      "expected to find just one child elem: report:") (elemName elemIn)
  | otherwise = head es
  where
    dtype = lookupEnforce1Elem "direction-type" elemIn
    es = lookupElemsContent dtype

pOtherDirection elemIn = XDOtherDirection s
  where
    s = getEnforce1StringContent elemIn
      

pPedal :: Element i -> XDirection
pPedal elem = XDPedal b t
  where
  t = case lookupAttr "type" elem of
    Nothing -> throwMine $ "direction-type of 'pedal' found without a 'type'"++
               " attribute"
    Just s -> s

  b = case lookupAttr "line" elem of
    Nothing -> throwMine $ "direction-type of 'pedal' found without a 'line'"++
               " attribute" 
    Just "yes" -> True
    Just _ -> False

pWedge :: Element i -> XDirection
pWedge elemIn 
  | numberDefinedSomewhere = "warning: wedge with number, probably okay"
                             `trace` XDWedge a
  | otherwise = XDWedge a
  where
    a = case lookupAttr "type" elemIn of
      Nothing -> throwMine "098243"
      Just s -> s
    numberDefinedSomewhere = isJust (lookupAttr "number" elemIn)

pDynamics :: Element i -> XDirection
pDynamics elemIn = XDDynamics name
  where
    name = case lookupElemsContent elemIn of
      [] -> throwMine "klj432"
      [e] -> elemName e
      _ -> throwMine "jlk4378"

pWords :: Element i -> XDirection
pWords elemIn = XDWords (getEnforce1StringContent elemIn)

pMetronome :: Element i -> XDirection
pMetronome elemIn = XDMetronome bu pm
  where
    bu = case lookupElems "beat-unit" elemIn of
      [] -> throwMine (printf ("In pMetronome, didn't find 'beat-unit' " ++
        "element.%s") (debugReport elemIn))
      [e] -> getEnforce1StringContent e
      es -> "Warning in pMetronome, just taking first beat-unit" `trace` 
        (getEnforce1StringContent $ head es)
    pm = case lookupElems "per-minute" elemIn of
      [] -> "Warning in pMetronome, no per-minute" `trace` Nothing
      [e] -> Just $ getEnforce1IntContent e
      _ -> throwMine "jkl;908"


pXNote :: Element i -> XNote
pXNote elemIn
  | isRest = XNRest duration ch maybeVoice notations
  | otherwise = XNNote duration isGrace ch maybeVoice pitch tieStart tieStop
                notations notehead
  where
    isRest = case lookupElem "rest" elemIn of
      Nothing -> False
      Just _ -> True
    isGrace = case lookupElem "grace" elemIn of
      Nothing -> False
      Just _ -> True
    duration = case lookupElem "duration" elemIn of
      Nothing -> if isGrace
        then 0
        else throwMine ("in pXNote, no 'duration' elem in 'note' and it is"++
                        " not a grace note.")
      Just e -> getEnforce1IntContent e
    ch = case lookupElem "chord" elemIn of 
      Nothing -> False
      Just _ -> True
    pitch = pXPitch $ lookupEnforce1Elem "pitch" elemIn
    maybeVoice = fmap getEnforce1IntContent (lookupElem "voice" elemIn)
    tieStart = pTie "start" elemIn 
    tieStop = pTie "stop" elemIn
    notehead = case lookupEnforceZeroOrOneElem "notehead" elemIn of
       Nothing -> Nothing
       Just nh -> Just $ XNotehead (getEnforce1StringContent nh)
    notations = case lookupEnforceZeroOrOneElem "notations" elemIn of 
      Nothing -> []
      Just ns -> pNotations ns
                  
-- returns True if the input element has a <tie> child element and
-- its type attribute equals typ_. 
pTie :: String -> Element i -> Bool
pTie typ_ elemIn = 
  {-
  case lookupElem "tie" elemIn of
    Nothing -> False
    Just e -> case lookupAttr "type" e of
      Nothing -> throwMine "098zxczcx"
      Just s -> s == typ_
  -}
  case lookupElems "tie" elemIn of
      [] -> False
      xs -> any isTie xs
  where
    isTie e = case lookupAttr "type" e of
      Nothing -> throwMine "jklfds98"
      Just s -> s == typ_

pXPitch :: Element i -> XPitch
pXPitch elemIn = XPitch step alter octave
  where
    step = getEnforce1StringContent (lookupEnforce1Elem "step" elemIn)
    octave = getEnforce1IntContent (lookupEnforce1Elem "octave" elemIn)
    alter = case lookupElem "alter" elemIn of
      Nothing -> 0
      Just e -> getEnforce1IntContent e

pNotations :: Element i -> [XNotation]
pNotations elemIn =processManyElems g elemIn
  where
    g e2
      -- not using <tied> notation; rather using <tie> as child of <note>
      | n == "tied" = Nothing -- Just . XNTied . lookupEnforceAttr "type" $ e2
      | n == "slur" = Just $ pSlur e2
      | n == "articulations" = Just . XNArticulations . pArticulations $ e2
      | n == "ornaments" = Just . XNOrnaments . pOrnaments $ e2
      | n == "tuplet" = Nothing
      | n == "fermata" = Just XNFermata
      | n == "technical" = Just . XNTechnical . pTechnical $ e2
      | n == "arpeggiate" = Just XNArpeggiate
      | otherwise = (printf "warning, unknown notations element:'%s'" n)
         `trace` Nothing 
      where n = elemName e2

pSlur :: Element i -> XNotation
pSlur elemIn = XNSlur (lookupEnforceAttr "type" elemIn) mLevel
  where
    mLevel = lookupIntAttr "number" elemIn

pTechnical = processManyElems g
  where
    g elem
      | n == "open-string" = Just OpenString
      | otherwise = (printf "Warning, unknown technical notation '%s'" n) 
          `trace` Nothing
      where
        n = elemName elem

pArticulations = processManyElems g
  where
    g elem 
      | n == "staccato" = Just XAStaccato
      | n == "staccatissimo" = Just XAStaccatissimo
      | n == "accent" = Just XAAccent
      | n == "strong-accent" = Just XAStrongAccent
      | n == "tenuto" = Just XATenuto
      | otherwise = (printf "Warning, unknown articulation '%s'" n) `trace`
         Nothing
      where
        n = elemName elem

pOrnaments = processManyElems g
  where
    g elem
      | n == "tremolo" = Just $ pTremolo elem
      | n == "trill-mark" = Just TrillMark
      | otherwise = Nothing
      where
        n = elemName elem

pTremolo elem = Tremolo typ nBars
  where
    typ = case lookupEnforceAttr "type" elem of
      "single" -> TremoloSingle
      "start" -> TremoloStart
      "stop" -> TremoloStop
    nBars = getEnforce1IntContent elem
