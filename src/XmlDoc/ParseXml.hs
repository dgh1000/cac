module XmlDoc.ParseXml where

import Debug.Trace
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.List as L
import Text.Printf
import Text.XML.Light
import XmlDoc.XmlDocData
import Util.Exception
import Common.CommonData

allChildren = filterChildren (const True)


myFindChildren :: String -> Element -> [Element]
myFindChildren s = findChildren (simpleName s)

myFindChild :: String -> Element -> Maybe Element
myFindChild s = findChild (simpleName s)

--
myFindAttr s = findAttr (simpleName s)

myElName = qName . elName

simpleName :: String -> QName
simpleName s = QName s Nothing Nothing

parseXScore :: Element -> XScore
parseXScore e = XScore { xPartInfos = partInfos
                       , xParts = M.mapKeysWith err g . parseParts $ e }
  where
    err _ _ = throwMine "two part names the same"
    partInfos = parsePartList e
    g pid = case M.lookup pid partInfos of {Just (XPartInfo name) -> name}
    

parsePartList :: Element -> Map String XPartInfo
parsePartList e = M.fromList . map parseScorePart . 
                  myFindChildren "score-part" $ pl
  where
    pl = case myFindChild "part-list" e of {Just x -> x}
    

-- input element is 'score-part'. We need its attr 'id' and the text value
-- of its child 'part-name'
parseScorePart :: Element -> (String,XPartInfo)
parseScorePart e = case myFindChild "part-name" e of
  Just epn -> (id_, XPartInfo $ strContent epn)
  where
    id_ = case myFindAttr "id" e of {Just x -> x}


parseParts :: Element -> Map String XPart
parseParts = M.fromList . map g . myFindChildren "part"
  where
    -- g operates on elements of name 'part' which have many children of name
    -- 'measure'
    g :: Element -> (String,XPart)
    g e = (id_,XPart ms)
      where
        id_ = case myFindAttr "id" e of {Just x -> x}
        ms = map parseMsr . myFindChildren "measure" $ e

parseMsr :: Element -> XMsr
parseMsr eMsr = XMsr num_ attrs datas
  where
    num_ = case myFindAttr "number" eMsr of
      Just n -> case reads n of (i,_):_ -> i
    attrs = myFindChild "attributes" eMsr >>= parseMsrAttr
    datas = mapMaybe parseMsrData . filterChildren (const True) $ eMsr
    
parseMsrData emd = case (qName . elName $ emd) of
  "note"       -> Just . XMDNote . parseXNote $ emd
  "direction"  -> parseDirection emd
  "backup"     -> Just $ parseBackup emd
  "forward"    -> Just $ parseForward emd
  "print"      -> Nothing
  "attributes" -> Nothing
  "barline"    -> Nothing
  name         -> throwMine $ printf ("in XML parsing, unrecognized " ++
                  "element '%s' in measure element") name

parseBackup e = case myFindChild "duration" e of
  Just d -> XMDBackup (toInt $ strContent d)
  where
    toInt s = case reads s of {(i,_):_ -> i}

parseForward e = case myFindChild "duration" e of
  Just d -> XMDForward (toInt $ strContent d)
  where
    toInt s = case reads s of {(i,_):_ -> i}

parseXNote :: Element -> XNote
parseXNote eNote
  | isRest = XNRest duration chord maybeVoice maybeStaff notations
  | otherwise = XNNote duration isGrace chord maybeVoice maybeStaff 
                pitch tieStart tieStop notations notehead
  where
    toInt1 s = case reads s of {(i,_):_ -> i}
    isRest = maybe False (const True) $ myFindChild "rest" eNote
    isGrace = maybe False (const True) $ myFindChild "grace" eNote
    duration = case myFindChild "duration" eNote of
      -- exhausted case here means "duration" was supposed to be present in xml
      Nothing | isGrace -> 0
      Just d -> toInt1 . strContent $ d
    chord = maybe False (const True) $ myFindChild "chord" eNote
    pitch = case myFindChild "pitch" eNote of
      Just p -> parseXPitch p
    maybeVoice = fmap (toInt1 . strContent) . myFindChild "voice" $ eNote
    maybeStaff = fmap (toInt1 . strContent) . myFindChild "staff" $ eNote
    tieStart = not . null . filterChildren isTieStart $ eNote
    tieStop = not . null . filterChildren isTieStop $ eNote
    isTieStart e = isTie e && (case myFindAttr "type" e of 
                                 Just t -> t == "start"
                                 Nothing -> False)
    isTieStop e = isTie e && (case myFindAttr "type" e of 
                                 Just t -> t == "stop"
                                 Nothing -> False)
    isTie e = (qName . elName $ e) == "tie"
    notehead = fmap (XNotehead . strContent) . myFindChild "notehead" $ eNote
    notations = case myFindChild "notations" eNote of
      Nothing -> []
      Just ns -> parseNotations ns

parseMsrAttr :: Element -> Maybe XMsrAttr
parseMsrAttr eMsrAttr
  | isNothing divs && isNothing beats && isNothing beatType = Nothing
  | otherwise = Just $ XMsrAttr divs beats beatType
  where
    toInt1 s = case reads s of {(i,_):_ -> i}
    divs = fmap (toInt1 . strContent) . myFindChild "divisions" $ eMsrAttr
    beats = myFindChild "time" eMsrAttr >>= 
            myFindChild "beats" >>= Just . toInt1 . strContent
    beatType = myFindChild "time" eMsrAttr >>=
               myFindChild "beat-type" >>= Just . toInt1 . strContent

    
parseXPitch :: Element -> XPitch
parseXPitch ePitch = XPitch step alter octave
  where
    toInt1 s = case reads s of {(i,_):_ -> i}
    step = case myFindChild "step" ePitch of {Just s -> strContent s}
    octave = case myFindChild "octave" ePitch of
      Just s -> toInt1 . strContent $ s
    alter = case myFindChild "alter" ePitch of
      Nothing -> 0
      Just s -> toInt1 . strContent $ s


parseNotations :: Element -> [XNotation]
parseNotations = mapMaybe g . allChildren
  where
    g e = case qName . elName $ e of
      "tied"          -> Nothing
      "slur"          -> Just . parseSlur $ e
      "articulations" -> Just . XNArticulations . parseArticulations $ e
      "tuplet"        -> Nothing
      "ornaments"     -> Just . XNOrnaments . parseOrnaments $ e
      "fermata"       -> Just XNFermata
      "arpeggiate"    -> Just XNArpeggiate -- xxxx
      "technical"     -> Just . XNTechnical . parseTechnical $ e
      _ -> printf "Warning, unknown notations sub-element '%s'"
                  (qName . elName $ e) `trace` Nothing


parseArticulations = mapMaybe g . allChildren
  where
    g elem = case qName . elName $ elem of
      "staccato"         -> Just XAStaccato
      "staccatissimo"    -> Just XAStaccatissimo
      "accent"           -> Just XAAccent
      "strong-accent"    -> Just XAStrongAccent
      "tenuto"           -> Just XATenuto
      "detached-legato"  -> Just XADetachedLegato
      _ -> printf "Warning, unknown articulation '%s'" (qName . elName $ elem)
           `trace` Nothing



parseOrnaments = mapMaybe g . allChildren
  where
    g elem = case qName . elName $ elem of
      "tremolo" -> Just . parseTremolo $ elem
      "trill-mark" -> Just TrillMark
      _            -> printf "Warning: unrecognized ornament '%s'"
                      (qName . elName $ elem) `trace` Nothing

parseTremolo elem = Tremolo typ nBars
  where
    toInt1 s = case reads s of {(i,_):_ -> i}
    typ = case myFindAttr "type" elem of
      Just "single" -> TremoloSingle
      Just "start" -> TremoloStart
      Just "stop" -> TremoloStop
    nBars = toInt1 . strContent $ elem



parseTechnical = mapMaybe g . allChildren
  where
    g elem = case n of
      "open-string" -> Just XTOpenString
      "down-bow"    -> Just XTDownBow
      "up-bow"      -> Just XTUpBow
      _ -> printf "Warning: unknown technical notation '%s'" n `trace` Nothing
      where
        n = qName . elName $ elem

parseSlur :: Element -> XNotation
parseSlur s = XNSlur type_ mLevel
  where
    type_ = case myFindAttr "type" s of {Just s -> s}
    -- is there no attribute "number"? what is an attribute?
    mLevel = case myFindAttr "number" s of
      Just s -> case reads s of {(i,_):_ -> Just i}
      Nothing -> Nothing



parseDirection :: Element -> Maybe XMsrData
parseDirection eDir = fmap (\x -> XMDDirection x mOffset mVoice mStaff) xd 
  where
    xd :: Maybe XDirection
    xd = case myElName eDirType of
      "metronome"       -> Just $ parseMetronome eDirType
      "words"           -> Just $ XDWords (strContent eDirType) defaulty
        where defaulty = myFindAttr "default-y" eDirType >>= 
                \s -> case reads s of {(i,_):_ -> Just i}
      "dynamics"        -> Just $ parseDynamics eDirType
      "wedge"           -> Just $ parseWedge eDirType
      "pedal"           -> Just $ parsePedal eDirType
      "octave-shift"    -> Just $ parseOctaveShift eDirType
      "other-direction" -> Just . XDOtherDirection . strContent $ eDirType 
      t -> printf "Warning: unknonwn direction element type '%s'" t `trace`
        Nothing
    toInt1 s = case reads s of {(i,_):_ -> i}
    maybeInt1 s = myFindChild s eDir >>= Just . toInt1 . strContent
    eDirType = parseDirectionType eDir
    mVoice = maybeInt1 "voice"
    mStaff = maybeInt1 "staff"
    mOffset = maybeInt1 "offset"


-- parseDirectionType
--
--   Input element is 'direction'. This enforces it has one 
--   <direction-type> element; looks at child elements of that;
--   enforces there is only one, and returns 
--   <single element that is content of direction-type>
parseDirectionType :: Element -> Element
parseDirectionType e = case myFindChild "direction-type" e of
  Nothing -> throwMine "in parseDirectionType, no direction-type subelem"
  Just dirType -> case allChildren dirType of
    []  -> throwMine "parseDirectionType 1"
    [x] -> x
    xs | all (\e -> myElName e == "words") xs -> 
           ("Warning, multiple <words> elements found as " ++
            "sub-elements of <direction-type>; will only use first one '" ++
            (strContent . head $ xs) ++ "'") `trace` head xs



parseMetronome :: Element -> XDirection
parseMetronome e = case met of
    Just x  -> x
    Nothing -> throwMine "in parsing XML, malformed metronome tag"
  where
    readInt1 s = case reads s of {(i,_):_ -> i}
    met = do
      unit <- myFindChild "beat-unit"  e >>= Just . strContent
      pmin <- myFindChild "per-minute" e >>= Just . readInt1 . strContent
      return $ XDMetronome unit pmin

parseDynamics e = case allChildren e of {[d] -> XDDynamics . myElName $ d}

parseWedge :: Element -> XDirection
parseWedge e = case myFindAttr "type" e of 
  Just "crescendo"  -> XDWedge WedgeCresc
  Just "diminuendo" -> XDWedge WedgeDim
  Just "stop"       -> XDWedge WedgeStop
  

parsePedal :: Element -> XDirection
parsePedal e = case myFindAttr "type" e of
  Just "start"   -> XDPedal PedalStart  line
  Just "stop"    -> XDPedal PedalStop   line
  Just "change"  -> XDPedal PedalChange line
  where
    line = case myFindAttr "line" e of 
      Just "yes" -> True
      Just "no"  -> False


parseOctaveShift :: Element -> XDirection
parseOctaveShift e = XDOctaveShift upDown
  where
    octs =   case myFindAttr "size" e of
      Just "8"    -> 1
      Just "15"   -> 2
    upDown = case myFindAttr "type" e of
      Just "up"   -> -octs
      Just "down" ->  octs
      Just "stop" ->  0

{-
    step = getEnforce1StringContent (lookupEnforce1Elem "step" elemIn)
    octave = getEnforce1IntContent (lookupEnforce1Elem "octave" elemIn)
    alter = case lookupElem "alter" elemIn of
      Nothing -> 0
      Just e -> getEnforce1IntContent e
-}

{-
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









-}
