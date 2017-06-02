
module Instruments.Q where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Map(Map)
import Data.Maybe
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Score.ScoreData
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Common.CommonUtil
import Instruments.AlterTimes
import Util.Math
import Util.Exception


-- notes have destinations. the algorithms can vary quite a bit. perhaps
-- shouldn't try to make it too general. would like to have a general Q
-- instrument. actually it's not Q. a note can be directed to destinations,
-- use various velocity curves, etc. what is in common here? shorten based on
-- staccato? nominal times and pitches? not even pitches!
--
-- how should we handle fact that notes can split into multiple destinations?
-- that seems like common infrastructure.
--
-- okay so why do we want to preserve TrNote? what is it good for? oh,
-- converting to shorts. well damn that's easy

velCurveBasic = VelCurve [(0.4,10),(8.6,127)]


{-

qCello dest =
  QData { qdInitVol    = 126
        , qdDest       = dest
        , qdKs         = [ ("arco", 24) -- C0
                         , ("marc", 89) -- F5
                         , ("mart", 88) -- E5
                         , ("pizz", 90) -- F#5
                         ]
        , qdShortArtic = "mart"
        }

-}


{-

data QData = QData
  { qdDests       :: Map String QDest  -- (init volume) (str,chan)
  , qdArtic       :: Map String QArticDesc
  , qdStacArtic   :: String  -- articulation to use for notes marked stac.
  }

-}

----------------------------------------------------------------------
--                       viola

violaKSMap = [ ("susVib"      , 24)
             , ("nonVibRR"    , 25)
             , ("qLeg"        , 26)
             , ("susLeg"      , 27)
             , ("expLeg"      , 28)
             , ("trillHT"     , 29)
             , ("TrillWT"     , 30)
             , ("exp1"        , 31)
             , ("exp2"        , 32)
             , ("exp3"        , 33)
             , ("expVibSft"   , 34)
             , ("martRR"      , 35)
             , ("marcHardRRx2", 36)
             , ("pizzRR"      , 37)
             , ("spicRRx2"    , 38)
             , ("colLegnoRR"  , 39)
             , ("8vsSlideUp"  , 40) ]


violaArticVels = [ ("susVib"  , velCurveBasic)
                 , ("nonVibRR", velCurveBasic)
                 , ("qLeg"    , velCurveBasic) ]

makeQViola :: String -> String -> Int -> Int -> [(String,String)] ->
              Meta
makeQViola name staffN ksChan shortChan scoreAliasDefns =
    makeQ name staffN "qSoloViola" 0 (const Nothing)
    scoreAliasDefns chans violaKSMap violaArticVels "short" (ext,gap,minDur)
  where
    vShort = Just $ VelCurve [(0.4,10),(8.6,74)]
    chans =
      [ ("ks"   , Chan (0,ksChan)    "qSoloViola" "KS"      90 127 Nothing)
      , ("short", Chan (0,shortChan) "qSoloViola" "martRR" 127 127 vShort )
      ]
    ext    = 0.05
    gap    = 0.07
    minDur = 0.05


----------------------------------------------------------------------
--                       viola


celloKSMap = [ ("susVibSmooth", 24)
             , ("qLeg"        , 25)
             , ("susLeg"      , 26)
             , ("nonVib"      , 27)
             , ("expDn"       , 28)
             , ("expUp"       , 29)
             , ("dblBowExp"   , 30)
             , ("expVib"      , 31)
             , ("martUpDn"    , 87)
             , ("marc"        , 89)
             , ("pizzRR"      , 90) 
             , ("colLegnoRR"  , 91) ]

celloArticVels = [ ("susVibSmooth", velCurveBasic)]

makeQCello :: String -> String -> Int -> Int -> [(String,String)] ->
              Meta
makeQCello name staffN ksChan shortChan scoreAliasDefns =
    makeQ name staffN "qSoloCello" 0 (const Nothing)
    scoreAliasDefns chans celloKSMap celloArticVels "short" (ext,gap,minDur)
  where
    vShort = Just $ VelCurve [(0.4,10),(8.6,64)]
    chans =
      [ ("ks"   , Chan (0,ksChan)  "qSoloCello" "KS"  70 127 Nothing)
      , ("short", Chan (0,shortChan) "qSoloCello"
                                    "martUpDnMarcx6" 127 127 vShort )
      ]
    ext    = 0.05
    gap    = 0.07
    minDur = 0.05


----------------------------------------------------------------------






bassKSMap  = [ ("susVib"        , 72)
             , ("qLeg"          , 73)
             , ("susLeg"        , 74)
             , ("expLeg"        , 75)
             , ("lyrLeg"        , 76)
             , ("susNV"         , 77)
             , ("lyrical"       , 78)
             , ("exp"           , 79)
             , ("marcato"       , 80)
             , ("martele"       , 81)
             , ("pizz"          , 82)
             , ("spic"          , 83)
             , ("colLegno"      , 84) ]
  
bassArticVels = [ ("susVib", velCurveBasic)
                , ("qLeg"  , velCurveBasic)
                , ("susLeg", velCurveBasic)
                , ("sus"   , VelCurve [(0.4,10),(8.6,70)] ) ]

makeQBass :: String -> String -> Int -> Int -> Int -> [(String,String)] ->
             Meta
makeQBass name staffN ksChan shortChan tubaChan scoreAliasDefns =
    makeQ name staffN "qSoloBass" (-12) f
    scoreAliasDefns chans bassKSMap bassArticVels "short" (ext,gap,minDur)
  where
    f p | p <= 27 = Just "tuba"
        | otherwise = Nothing
    vShort = Just $ VelCurve [(0.4,10),(8.6,64)]
    chans =
      [ ("ks"   , Chan (0,ksChan)  "qSoloBass" "KS"  70 127 Nothing)
      , ("short", Chan (0,shortChan) "qSoloBass"
                                         "marcRRx3" 127 127 vShort )
      , ("tuba" , Chan (0,tubaChan) "qSoloTuba" "sus" 127 127 Nothing)
      ]
    ext    = 0.05
    gap    = 0.07
    minDur = 0.05


----------------------------------------------------------------------

    
qLookupL key lists = case lookup key lists of {Just x -> x}


makeQ :: String -> String -> String -> Int -> (Int -> Maybe String) ->
         [(String,String)] ->
         [(String,Chan)] -> [(String,Int)] -> [(String,VelCurve)] -> String ->
         (Double,Double,Double) -> Meta
makeQ name staffN axe pitchShift alterPitch scoreAliases chans ksMap articVels
  stacDestString timeAlter 
  = MetaQ
  Q { qName         = name
    , qStaffNs      = [staffN]
    , qInit         = qInit_impl
    , qRun          = qRun_impl
      
    , qStaffN       = staffN
    , qAxe          = axe
    , qAliases      = M.fromList scoreAliases
    , qChans        = M.fromList chans
    , qKSMap        = M.fromList ksMap
    , qArticVels    = M.fromList articVels
    , qStacDestId   = help_parseDestID stacDestString
    , qAlternateDest_pitch = alterPitch
    , qTimeAlter    = timeAlter
    , qPitchShift   = pitchShift

    , qTremShapes   = M.empty
    , qTrillShapes  = M.empty
    , qStacDurs     = M.empty
    , qAliasMarks   = M.empty
    , qArpDelta     = M.empty
    }


----------------------------------------------------------------------
----------------------------------------------------------------------

-- a "channel" is a stream/midi channel pair
--
-- each channel will have an "instrument" loaded: examples of instruments
--
--   pianoteq with a certain configuration loaded
--
--
--
--   a QL leap channel with a single articulation loaded: this is called a
--   "single articulation instrument"
--
--     qSoloViolin/susLeg
--
--   a QL orch channel with a particular axe's keyswitch patch loaded: this is
--   called a "keyswitch instrument"
--
--     qSoloVioin/KS (and naming a particular articulation is
--     qSoloViolinKS/KS/susLeg
--
-- an axe means "solo violin", "10 cellos", etc.: it's a collection of "single
-- artic. instruments" or "keyswitch instruments"
--
-- channels have names
--
-- a destination is the concept of (1) what channel the note goes to, (2)
-- whether a keyswitch needs to be selected, (3) whether to use a specific
-- velocity curve or the default one for that instrument

----------------------------------------------------------------------
----------------------------------------------------------------------

-- okay we need to direct certain pitches to a different destination id. we'd
-- like it to be configurable, well I guess if it's not a KS channel then it
-- just has a channel name that's it 

  
qInit_impl :: Q -> Tr Q
qInit_impl q = do
  let x :: Chan -> [TrRaw]
      x (Chan nums _ _ vol expr _) =
        [ TrRaw (qStaffN q) 0 nums 0xB0 7 vol
        , TrRaw (qStaffN q) 0 nums 0xB0 11 expr ]
  includeInitRaws (concatMap x $ M.elems $ qChans q)
  return q


qRun_impl :: Q -> Int -> Int -> Tr Q
qRun_impl q mBeg mEnd = 
  (concat `liftM` forOneStaff nominalPitsTs (MetaQ q) (qStaffN q) mBeg mEnd)
    >>= mapM (runPitchShift q)
    >>= mapM (runScoreAlias q)
    >>= mapM (runAlterDest q)
    >>= mapM (runChanNums q)
    >>= mapM (runKeyswitch q)
    >>= mapM runLoud_common
    >>= mapM (runVel q)
    >>= (let (ext,gap,minDur) = qTimeAlter q
         in alterTOff ext gap minDur)
    >>= includeNotes
    >> return q


runPitchShift :: Q -> TrNote -> Tr TrNote
runPitchShift q note = return note {tnPitch = (qPitchShift q)+tnPitch note}


runAlterDest :: Q -> TrNote -> Tr TrNote
runAlterDest q note =
  case qAlternateDest_pitch q (tnPitch note) of
    Nothing -> return note
    Just s  -> return note {tnScoreAlias = s}


runScoreAlias q note = return note {tnScoreAlias = alias}
  where
    alias = case M.lookupLE (tnLoc note) $ qLookup "0anqq" (tnStaffName note)
                 (qAliasMarks q) of
      Nothing -> throwMine $ printf ("no destination alias at or " ++
                  "before loc %s") (showLoc2 $ tnLoc note)
      Just (_,a)  -> a


runChanNums  :: Q -> TrNote -> Tr TrNote
runChanNums q note = do 
  let (chan,_) = help_lookupChanAndDestId note q
      Chan nums _ _ _ _ _ = chan
  return note {tnChanNums = nums}
  

runKeyswitch :: Q -> TrNote -> Tr TrNote
runKeyswitch q note
  | isJust num && singleOrRight =
      let modif = ModifKs (Left (-0.05)) (fromJust num)
      in return note {tnMods = modif:tnMods note}
  | otherwise = return note
  where
    singleOrRight = case note of
      TrSingle{} -> True
      TrTrill {} -> case tnIdx note of {0 -> True; _ -> False}
    num = help_lookupKeyswitch note q


help_lookupKeyswitch :: TrNote -> Q -> Maybe Int
help_lookupKeyswitch note q =
  let (chan,destID) = help_lookupChanAndDestId note q
      Chan _ chanAxe chanArtic _ _ _ = chan
      DestId _ maybeKs = destID
  in case maybeKs of
       Nothing -> Nothing
       Just (ksSpecifiedAxe,ksName) -> 
         help_lookupKeyswitch2 q chanAxe chanArtic ksSpecifiedAxe ksName


help_lookupKeyswitch2 :: Q -> Axe -> Artic -> Axe -> Artic -> Maybe Int
help_lookupKeyswitch2 q chanAxe chanArtic ksSpecifiedAxe ksArtic
  | chanArtic /= "KS" = throwMine "alkj234"
  | qAxe q /= ksSpecifiedAxe =
      throwMine $ printf ("error: " ++
      "reference to a keyswitch that's part of axe '%s', but the " ++
      "axe loaded at the channel is '%s'")
      (ksSpecifiedAxe::String) (chanAxe::String)
  | otherwise = case M.lookup ksArtic (qKSMap q) of
      Nothing -> throwMine $ printf ("error: reference to " ++
        "keyswitch '%s' but this is not in keyswitch map that is " ++
        "configured with this instrument, '%s' %s")
        (ksArtic::String) (qAxe q::String) (show $ qKSMap q)
      Just n -> Just n


help_lookupChanAndDestId :: TrNote -> Q -> (Chan,DestId)
help_lookupChanAndDestId note q = (chan,destID)
  where
    (destID,destString)
      | Staccato `elem` (cModifiers $ tnChord note) = (qStacDestId q,"STAC")
      | otherwise =
          let ds = qLookup "adx45" (tnScoreAlias note) (qAliases q)
          in case parse parseDestID "" ds of
               Left err -> throwMine $ "error while parsing "++show err
               Right x -> (x,ds)
    {-
    destString = qLookup "adx45" (tnScoreAlias q) (qAliases q)
    destID = case parse parseDestID "" destString of
      Left err -> throwMine $ "2345kj"++show err
      Right x -> x
    -}
    DestId chanName _ = destID
    chan = case M.lookup chanName (qChans q) of
      Nothing -> throwMine $ printf ("destination '%s' specifies a channel "
        ++ "name of '%s' but there is no such channel configured in "
        ++ "this Q axe of type (%s)") destString chanName (qAxe q)
      Just c -> c
    
{-

help_lookupDestID alias q = case M.lookup alias (qAliases q) of
  Nothing -> throwMine $ printf ("in looking up destination ID " ++
    "associated with score alias '%s' -- there is no such alias in "++
    "the map of aliases to destination IDs") alias
  Just destID -> destID
-}

{-
help_lookupDest alias q =
  case M.lookup destName (qDests q) of
    Nothing -> throwMine $ printf ("destination ID specifies " ++
               "destination named '%s', but this dest. is not present "++
               "in the dest. configuration") destName
    Just d -> d
    where
      destName :: String
      destName = let (x,_) = help_parseDestID alias q in x
-}

runVel :: Q -> TrNote -> Tr TrNote
runVel q note = do
  let (chan,destID) = help_lookupChanAndDestId note q
      Chan _ _ chanArtic _ _ mAlternate = chan
      DestId _ maybeKs = destID
      artic = case (chanArtic,maybeKs) of
        ("KS",Just (_,a)) -> a
        (a   ,Nothing)    -> a
      curve = case mAlternate of
        Just c  -> c
        Nothing -> case M.lookup artic (qArticVels q) of
          Just c -> c
          Nothing -> throwMine $ printf ("error: cannot find a "
            ++ "configured velocity curve for artic '%s'") (artic::String)
  return note {tnVel = lookupVel "lk2341" (tnLoud note) curve}
        
  

{-
runVelAndKeyswitch q note = do
  let (dest,artic,mAxe) = help_lookupStuff (tnScoreAlias note) q
      curve = let Dest _ _ _ _ alternate
              in case alternate of
                   Just c -> c
                   Nothing -> case M.lookup artic (qArticVels q) of
                     Nothing -> throwMine $ printf ("error: no velocity "++
                       "curve for artic '%s'") artic
                     Just c -> c
      setKs nt = if ksFlag
                     then case M.lookup artic (qKSMap q) of
                       Nothing -> throwMine $ printf ("error: no keyswitch "++
                         "number is configured for artic '%s'") artic
                       Just n -> nt {tnMods =
                                     (Modif (Left (-0.05)) n):tnMods nt}
                      else nt
      setVel note = note {tnVel = lookupVel (tnLoud note) curve}
  return $ setKs $ setVel note

-}



{-

help_getQDestInfo alias q = _
  where
    destID = help_lookupDestID alias q
    (chanName 
    dest = qLookup "1" destID (qDests q)
    


help_lookupStuff :: String -> Q -> (String,
help_lookupStuff alias q = _
  where
    dest = lookupDest 
-}  


help_parseDestID :: String -> DestId
help_parseDestID destIdString =
  case parse parseDestID "" destIdString of
    Left err -> throwMine $ printf ("error in parsing the " ++
      "destination ID '%s': %s") destIdString (show err)
    Right x ->  x


-- returns (<destination name>,
--           Maybe (<ks-specified instrument name>,<keyswitch name>))


parseDestID :: Parser DestId
parseDestID = do
  chanName <- many1 alphaNum
  mKs <- optionMaybe $ do char ':'
                          axe <- many1 alphaNum
                          char '/'
                          string "KS"
                          char '/'
                          ksName <- many1 alphaNum
                          return (axe,ksName)
  return $ DestId chanName mKs                     

{-

-- this will set the 'dest' of the note and possibly the articulation for a
-- keyswitch channel
runDest q note = note {tnDest = findDest findDestAlias}
  where
    scoreAlias = case M.lookupLE (tnLoc note) (qAliasMarks q) of
      Nothing -> throwMine $ printf ("no destination alias at or " ++
                  "before loc %s") (showLoc2 $ tnLoc note)
      Just a  -> a
    (chan,mKeyswitch) = case lookup alias (qAliases q) of
      Nothing -> throwMine $ printf ("in looking up destination ID " ++
        "associated with score alias '%s' -- there is no such alias in "++
        "the map of aliases to destination IDs") alias
      Just destID -> case parse parseDestID "" destID of
        Left err -> throwMine $ printf ("error in parsing the " ++
          "destination ID '%s'") destID
        Right (chanName,mKeyswitchNames) ->
          let chan = case lookup chanName (qDests q) of
                Just c  -> c
                Nothing -> throwMine $ ("destination ID '%s' specifies " ++
                  "channel named '%s', but this channel is not present " ++
                  "in the channel configuration") destID chanName
              mKeyswitch = lookupKeyswitch q <$> mKeyswitchName
-}






















{-

-- we can find the 'artic' easily enough. either the latest one, or the
-- staccato artic.
--
-- then we need to find the QDest associated with this artic.
--
-- the artic will have a dest alias

runDest :: Q -> TrNote -> Tr TrNote
runDest q note = do
  let articName = computeArticName note
  return x


runChannelAndKs q note = do
  let alias = findScoreAlias q note
      destID = lookupDestID q alias
      (channelName,maybe)
  in x













  
parseInstrName :: Parser (String,[String])
parseInstrName = do
  chan <- many1 alphNum
  ksComps <- option [] $ do char '-'
                            sepBy (many1 alphaNum) (char '/')
  return (chan,ksComps)
  

computeArticName :: Q -> TrNote -> String
computeArticName q note
  | isShort $ tnChord note = qStacArtic q
  | otherwise = case M.lookup (tnLoc note) (qArticMarks q) of
      Just alias -> case M.lookup alias (qScoreAliases q) of
       Just n -> n




runDest :: Q -> TrNote -> Tr TrNote
runDest q note = do
  let (_,QMidiChan _ _ d) = qArticUtil q note
  return note {tnDest = d}
 

runKs :: Q -> TrNote -> Tr TrNote
runKs q note = do
  let (QArticDescr mKeyswitch _ _,_) = qArticUtil q note
  case mKeyswitch of
    Nothing -> return note
    Just k  ->
      return note {tnMods = ModifKs (Left (-0.05)) k:tnMods note}

qArticUtil :: Q -> TrNote -> (QArticDescr,QMidiChan)
qArticUtil q note = (articDescription,chan)
  where
  (scoreAlias,articName) = qArticUtil_getArtName q note
  articDescription = case M.lookup articName (qArticDescrs q) of
    Just x -> x
    Nothing -> throwMine $ printf ("problem in Q instrument '%s': no " ++
       "articulation description for articulation named '%s', referenced " ++
       "by score alias '%s'") (qName q) articName scoreAlias
  chan = case M.lookup (qadChanName articDescription) (qChans q) of
    Just x -> x
    Nothing -> throwMine $ printf ("artic name '%s' specifies " ++
      "channel name of '%s' but this is not in the Q configuration")
      articName (qadChanName articDescription)


-- Gets the articulation name.
--
-- If not a staccato note, looks up score articulation alias, then translates
-- that to articulation name.
--
-- If it is a staccato note, returns the articulation name that is
-- pre-configured for staccato notes.
--
qArticUtil_getArtName :: Q -> TrNote -> (String,String)
qArticUtil_getArtName q note
  | isShort (tnChord note) = ("<staccato symbol>", qStacArtic q)
  | otherwise = (scoreAlias,articName)
  where
    scoreAlias = case M.lookupLE (tnLoc note) . qLookup (tnStaffName note) $
                      qArtics q of
      Nothing -> throwMine $ printf ("no articulation alias at or " ++
                 "before loc %s") (showLoc2 $ tnLoc note)
      Just (_,a) -> a
    articName = case lookup scoreAlias (qScoreAliases q) of
      Nothing -> throwMine $ printf ("Q instrument '%s' is not configured "++
        "with a articulation name for score alias '%s'") (qName q)
        scoreAlias
      Just s -> s
      

runVel :: Q -> TrNote -> Tr TrNote
runVel q note = do
  let (QArticDescr _ name curve,_) = qArticUtil q note
  return note {tnVel = lookupVel ("in Q, with channel name "++name)
                       (tnLoud note) curve}
-}
{-

qKs :: QData -> TrNote -> Tr TrNote
qKs (QData _ _ ksMap shortArtic) note = do
  result <-
      do
        VLocMap articMap <- lk $ staffData (tnStaffName note) ... "marks"
          ..."artic"
        let VString currArtic = qLookupLE (tnLoc note) articMap
        let currArticS =
              if isShort (tnChord note) then shortArtic else currArtic
        let r = lookup currArticS ksMap
        when (isNothing r)
          (throwError $ printf ("at %s, trying to use " ++
          "articulation '%s' but it's not in the keyswitch map")
          (showLoc2 $ tnLoc note) currArtic)
        return $ fromJust r
  return note {tnMods = ModifKs (Left (-0.05)) result:tnMods note} 
  
-}  
  
qLookupLE :: Ord k => k -> Map k a -> a
qLookupLE k m = case M.lookupLE k m of
  Just (_,v) -> v

qLookup :: Ord k => String -> k -> Map k a -> a
qLookup msg k m = case M.lookup k m of
  Just x -> x
  Nothing -> throwMine msg



{-

data NoteContext = NcSingle String Loc Int Chord Note
                 | NcTrill String Loc Int Chord Note Loc Int Int


data KeySwitch = KsSingle Int
               | KsTrill Int (Maybe Double) -- Maybe Double: Nothing means
                                            -- keyswitch should be triggered
                                            -- once for whole duration of
                                            -- trill, Just t means trigger it
                                            -- at a specific time


-- keyswitch can switch between patch based on score mark, patch based on
-- articulation
selectKeySwitch :: NoteContext -> I (Maybe KeySwitch)
selectKeySwitch nc@(NcSingle staffName loc vn chord note) = do
  SQConf  conf <- lk $ metaState..."config"
  SString art  <- lk $ metaState..."articulation"
  let out | isShort chord = case lkSwitch "short" of
              Just s -> Just $ KsSingle s
          | otherwise = case lkSwitch art of
              Just s -> Just $ KsSingle s
              Nothing -> printf ("don't know what keyswitch to use with " ++
                         "articulation '%s'") `trace` Nothing
  return out
    

-- we'd like to compute timing knowing which note in the trill this is.
--
-- we want to have multi-pass algorithms. does that work when we are holding a
-- state and going note by note? yes it does... that state is only to decide
-- how to allocate notes, what the dests are. that could be done on a second
-- pass, really

computeTiming :: NoteContext -> OnOff
computeTiming (NcSingle staffName loc vn chord note) = do
  t1 <- lookupTimeI staffName loc
  t2 <- lookupTimeI staffName $ nTrueEnd note
  let oo1 = x 
-}


