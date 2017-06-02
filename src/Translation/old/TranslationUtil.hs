module Translation.TranslationUtil where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace
import Common.CommonData
import Control.Arrow
import Control.Monad.State
import Data.Map(Map)
import Data.Monoid
import Data.Maybe
import Data.List(elemIndex,sort)
import Score.ScoreData
import Midi.MidiData
import Text.Printf
import Translation.TranslationData
import Translation.TimeMap
import Common.CommonUtil
import Util.Map(splitInclude,reduceLMap,lMapToList)
import Util.Exception


-- translation should import instruments, not vice versa

-- timemaps should be in 

----------------------------------------------------------------------
----------------------------------------------------------------------
--                computing single/trill/tremolo case




----------------------------------------------------------------------
----------------------------------------------------------------------
--               time- and Loc- related

onOffTr :: String -> Loc -> Loc -> Tr OnOff
onOffTr staffName l1 l2 = do
  t1 <- lookupTimeTr staffName l1
  t2 <- lookupTimeTr staffName l2
  return $ OnOff [("nominal",(t1,t2))]


noteOnOffTr :: NoteKey -> Tr OnOff
noteOnOffTr nk = onOffTr (staffNameK nk) (begLocK nk) (nkTrueEnd nk)


chordOnOffTr :: ChordKey -> Tr OnOff
chordOnOffTr ck = onOffTr (staffNameK ck) (begLocK ck) (endLocK ck)


chordOnMaxOffTr :: ChordKey -> Tr OnOff
chordOnMaxOffTr ck = onOffTr (staffNameK ck) (begLocK ck) (maxNoteEndK ck)


updateMi :: String -> (MetaInstr -> MetaInstr) -> Tr ()
updateMi miName g =
  modify (\s@TrState {tsMis = m} -> s {tsMis = M.adjust g miName m})



{-

stacTimesTr :: Double -> NoteKey -> OnOff -> Tr OnOff
stacTimesTr stacDur nk times = do
  let ck = nkCk nk
      staffName = ckStaffName $ nkCk nk
      (t1,t2) = headTimes times
  if S.member Staccato . cModifiers $ ckChord ck
     then return $ consTimes "staccato" t1 (t1+stacDur) times
     else return times

isStaccato :: NoteKey -> Tr (Maybe Double)

stacOnOff :: Double -> OnOff -> OnOff
stacOnOff stacDur times = con

-}

----------------------------------------------------------------------
----------------------------------------------------------------------
 --                 query functions

isShort :: K a => a -> Bool
isShort = (Staccato `elem`) . modifiersK


isArp :: K a => a -> Bool
isArp = (Arpeggiate `elem`) . modifiersK

----------------------------------------------------------------------
----------------------------------------------------------------------
--                time computation utilities


{-
-- Compute tBeg and tEnd from chord loc and true end loc, ignoring possible
-- truncation.
--
-- NoteKey
-- Maybe Double   :: if Just x, then extend note by x seconds unless there
--                   is a repeat. If nothing don't change note
regularTimeModel :: NoteKey -> Tr (Double,Double,TimeModel)
regularTimeModel nk = do
  (tBeg,tEnd) <- getBegEndTr nk
  st          <- getStaffTr $ getStaffName nk
  return (tBeg,tEnd,RegularModel tBeg tEnd)
-}


{-
truncateEnd :: (Double,Double) -> Times -> Times
truncateEnd (trunc,minDur) (tBeg,tEnd,model) =
  let dur  = tEnd-tBeg-trunc
      dur' = max minDur dur
  in (tBeg,tBeg+dur',RegularModel tBeg tEnd (-trunc))


truncateConditionally :: Maybe (Double,Double) -> (NoteKey -> Bool) -> 
                         NoteKey -> Times -> Tr Times
truncateConditionally Nothing _ _ x = return x
truncateConditionally (Just trunc) pred nk ts = do
  st <- getStaffTr $ getStaffName nk
  if any pred $ followingRepeats st nk
    then return $ truncateEnd trunc ts
    else return ts
-}

{-
staccatoTimeModel :: Map Loc Double -> NoteKey -> Tr (Double,Double,TimeModel)
staccatoTimeModel stacDurs nk = do
  let staffName =  getStaffName nk
  staff         <- getStaffTr staffName
  let loc       =  getChordLoc nk
      stacDur   =  snd . fromJust . M.lookupLE loc $ stacDurs
  (tBeg,tEnd)   <- getBegEndTr nk
  let dur       =  min stacDur (tEnd-tBeg)
  return (tBeg, tBeg+dur, StaccatoModel tBeg stacDur)
-}

{-
modifyStacTime :: Map Loc Double -> NoteKey -> TimeModel -> Tr TimeModel
modifyStacTime stacDurs nk tmod = do
  let loc     = getChordLoc nk
      stacDur = snd . fromJust . M.lookupLE loc $ stacDurs
      RegularModel d1 d2 actualBeg actualEnd _ d4 = tmod
      dur     = min stacDur (actualEnd-actualBeg)
  return $ RegularModel d1 d2 actualBeg (actualBeg+dur) (Just dur) d4
-}
  

{-
arpTimeModel :: Double -> Map Loc Double -> NoteKey -> 
                Tr (Double,Double,TimeModel)
arpTimeModel minDur deltas nk = do
  -- here 'delta' can be negative
  let delta = snd . fromJust . M.lookupLE (getChordLoc nk) $ deltas
      (absDelta,reverseFlag) | delta < 0 = (-delta,  True)
                             | otherwise = ( delta, False)
  (tBeg,tEnd)  <- getBegEndTr nk
  pitches <- ((if reverseFlag then reverse else id) . sort.  fromJust) 
             `liftM`
             scoreArpPitches (getChordLoc nk)
  let delay = case elemIndex (getMidiPitch nk) pitches of
        Just x -> absDelta * fromIntegral x
      newBeg = tBeg+delay
      newEnd = max tEnd (newBeg+minDur)
  return (newBeg,newEnd,ArpeggioModel tBeg delay newEnd)
-}  

{-
modifyArpTime :: Double -> Map Loc Double -> NoteKey -> TimeModel ->
                 Tr TimeModel
modifyArpTime minDur arpDeltas nk tmod = do
  let loc = getChordLoc nk
      delta = snd . fromJust . M.lookupLE loc $ arpDeltas
      (absDelta,reverseFlag) | delta < 0 = (-delta,  True)
                             | otherwise = ( delta, False)
      RegularModel d1 d2 actualBeg actualEnd d3 _ = tmod
  pitches <- ((if reverseFlag then reverse else id) . sort.  fromJust) 
             `liftM`
             scoreArpPitches (getChordLoc nk)
  let delay = case elemIndex (getMidiPitch nk) pitches of
        Just x -> absDelta * fromIntegral x
      newBeg = actualBeg + delay
      newEnd = max actualEnd (actualBeg+minDur)
  return $ RegularModel d1 d2 newBeg newEnd d3 (Just delay)
 -}
  

----------------------------------------------------------------------
----------------------------------------------------------------------

tuMapLookup :: Ord k => k -> Map k a -> a
tuMapLookup k m = case M.lookup k m of {Just x -> x}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                technique text query

{-
findTechText :: [String] -> Loc -> Map Loc [Text] -> Maybe String
findTechText ids loc text =
  getFirst . mconcat . map (First . isTechText ids . snd) . 
  M.toDescList . fst . splitInclude loc $ text


isTechText :: [String] -> [Text] -> Maybe String
isTechText ids texts = L.find (`elem` ids) [s | TechniqueText s <- texts]
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                 useful maps built from Marks



-- okay so we will now have access to all the Marks (I mean on all staves) and
-- the distinction between global and local is no longer made in the struct
-- that stores them.
--
--   arpDeltas: all arp deltas, on any staff, are global
--
--   stac durs: just local to staff

-- 

{-

  not using this Mark map stuff now that I've switched to state-based
  translation (as of Dec 2016 / Jan 2017)


markMapUtils :: Score -> String -> Either String [String] ->
  ( Map Loc Double,Map Loc Double,Map Loc TrillShape, Map Loc TrillShape
  , Map Loc (Either String [String]))
markMapUtils score staffName defTech = 
  ( buildArpDeltas   0.05          allMarks
  , buildStacDurs    0.07          staffMarks
  , buildTrillShapes defTrillShape staffMarks
  , buildTremShapes  defTremShape  staffMarks
  , buildTechTextMap defTech       staffMarks)
  where
    defTrillShape = TrillShape Upper [(14,1)] Lower
    defTremShape  = TrillShape Lower [(14,1)] Upper
    allMarks :: Map Loc [Mark]
    allMarks = M.map (concat . M.elems) $ scMarks score
    staffMarks :: Map Loc [Mark]
    staffMarks = M.mapMaybe (M.lookup staffName) $ scMarks score


buildMarkSubMap :: a -> (Mark -> Maybe a) -> Map Loc [Mark] -> Map Loc a
buildMarkSubMap default_ filt = 
    M.alter alt (Loc 1 1) . M.mapMaybeWithKey g
  where
    alt Nothing  = Just default_
    alt (Just x) = Just x
    g loc elems = case mapMaybe filt elems of
      []  -> Nothing
      [x] -> Just x
      _   -> throwMine $ printf "at %s, two marks of same type" 
             (simpleShowLoc loc)


-- buildTechTextMap
--
-- Map Loc [Mark] :: staff-only Marks. Will look for InstrTechnique type Mark.
--                   If more than one InstrTechnique occurs at given Loc, will
--                   throw exception
buildTechTextMap :: Either String [String] -> Map Loc [Mark] ->
                    Map Loc (Either String [String])
buildTechTextMap default_ = buildMarkSubMap default_ m
  where
    m :: Mark -> Maybe (Either String [String])
    m (InstrTechnique x) = Just x
    m _                  = Nothing


-- buildArpDeltas
--
-- Map Loc [Mark] :: unified from all staves
buildArpDeltas :: Double -> Map Loc [Mark] -> Map Loc Double
buildArpDeltas default_ = buildMarkSubMap default_ m
  where
    m (ArpDelta f) = Just f
    m _            = Nothing


buildStacDurs :: Double -> Map Loc [Mark] -> Map Loc Double
buildStacDurs default_ = buildMarkSubMap default_ m
  where
    m (StacDur f) = Just f
    m _           = Nothing


buildTrillShapes :: TrillShape -> Map Loc [Mark] -> Map Loc TrillShape
buildTrillShapes default_ = buildMarkSubMap default_ m
  where
    m (TrillShapeMark s) = Just s
    m _                  = Nothing


buildTremShapes :: TrillShape -> Map Loc [Mark] -> Map Loc TrillShape
buildTremShapes default_ = buildMarkSubMap default_ m
  where
    m (TremShapeMark s) = Just s
    m _                 = Nothing


-}













{-

reduceMarkMap ::(Mark -> Maybe b) -> b -> Score -> Map String (Map Loc b)
reduceMarkMap g defaultB score = ms2
  where
    ms :: [(String,Map Loc [Mark])]
    ms = map (stName &&& stMarks) . M.elems . scStaves $ score
    -- h :: Map Loc [a] -> Map Loc b  -- and insert default
    h m = let m2 = reduceLMap g m
          in case M.lookup (Loc 1 1) m2 of
               Nothing -> M.insert (Loc 1 1) defaultB m2
               Just _  -> m2
    ms2 = M.map h . M.fromList $ ms


reduceMarkMapTuple :: (Mark -> Maybe b) -> Map Loc [Mark] -> [(Loc,b)]
reduceMarkMapTuple g = mapMaybe h . lMapToList
 where
   -- h :: (Loc,Mark) -> Maybe (Loc,b)
   h (l,m) = case g m of {Nothing -> Nothing; Just b -> Just (l,b)}
   
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--     configuration parameters

getParams :: [(ConfigValueType,String)] -> StaffConfig -> [ConfigValue]
getParams expected sc
  | expectedStrings /= presentStrings = throwMine errMsg
  | otherwise = map (getParamValue sc) expected
  where
    expectedStrings = S.fromList . map snd $ expected
    presentStrings  = S.fromList . M.keys . stcParams $ sc
    errMsg = printf "In staff '%s', expected these params and these only: %s"
             (stcName sc)
             (concatMap (\(_,s) -> s ++ " ") expected)


-- Lookup the value of a parameter named 's' in the configuration file (as
-- present in the staff config), while checking to see if the type in the
-- config file matches the expected type 't'
getParamValue :: StaffConfig -> (ConfigValueType,String) -> ConfigValue
getParamValue sc (t,s) = case M.lookup s (stcParams sc) of
  Just p | t == toConfigValueType p  -> p
         | otherwise -> throwMine $ printf ("in staff '%s' config, " ++
             "expected a parameter type of %s for param %s") (stcName sc)
             (show t) s


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 modifier predicates


hasModifier :: ChordModifier -> ChordKey -> Bool
hasModifier m = (m `elem`) . modifiersK


----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
----------------------------------------------------------------------
--                      Access functions
    
getStaffTr :: String -> Tr Staff
getStaffTr n = do
  score <- gets tsScore
  let Just s = M.lookup n (scStaves score)
  return s

{-
getPatternTr :: Tr PatternData
getPatternTr = do
  plConf <- gets tsPlaybackConfig
  return $ pcPattern plConf
-}

{-
getPerStaffTr :: (PerStaff -> a) -> String -> Tr a
getPerStaffTr f name = (f . fromJust . M.lookup name) `liftM` gets tsPerStaff
0-  -}

getLoudnessFuncsTr :: String -> Tr StaffLoudnessFunc
getLoudnessFuncsTr name = trMapLookup name `liftM` gets tsLoudFuncs


trMapLookup :: Ord k => k -> Map k a -> a
trMapLookup k m = case M.lookup k m of {Just a -> a}


getTimeMapTr :: String -> Tr AbsTimeMap
getTimeMapTr name = trMapLookup name `liftM` gets tsAbsTimeMaps

{-
getStaffConfigTr :: String -> Tr StaffConfig
getStaffConfigTr staffName = do
  PlaybackConfig staffConfigs _ _ _ _ _ _ _ _ <- gets tsPlaybackConfig
  let Just c = M.lookup staffName staffConfigs
  return c
-}


{-
getInstrTr :: String -> Tr Instrument
getInstrTr name = do
  plConf <- gets tsPlaybackConfig
  let Just instr = M.lookup name $ pcInstrs plConf
  return instr
-}


----------------------------------------------------------------------
----------------------------------------------------------------------

getMiForStaff :: String -> Tr MetaInstr
getMiForStaff staffName = error "implement getMiForStaff"



