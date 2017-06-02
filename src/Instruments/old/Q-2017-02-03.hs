
module Instruments.Q where

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Text.Printf
import Data.Set(Set)
import Data.Map(Map)
import Data.Either
import Score.ScoreData
import Translation.TranslationData
import Translation.TranslationUtil
import Translation.LoudnessFunc
import Translation.Trill
import Midi.MidiData
import Common.CommonData
import Common.CommonUtil
import Instruments.InstrUtils
import Instruments.InstrumentsData
import Util.Exception
import Util.Math


confDefaultStac = 0.070
confDefaultArp  = 0.07

qSoloCelloConfig = QConfig "solo cello" True True True
                   (M.fromList [ ("arco",  (20,127))
                               , ("short", (20,90))
                               , ("pizz",  (30,127))
                               , ("expr",  (20,127)) ])



qInit :: QConfig -> Elem -> MetaInstr
qInit qc e = case areElemsValid validBrck validParam e of
  Nothing -> MiQ common M.empty dests legOvlp sepSame
  Just s  -> throwMine s
  where
    validBrck  = ["staves","dests"]
    validParam = ["instr","name","legatoOverlap","sepSame"]
    name = runExc $ findParam1 "name" e
    staffNames = runExc $ findBrOfSingles "staves" e
    common = MetaInstrCommon name staffNames e (qInitTrack qc)
             (qTranslateCk qc)
             qTranslateMark
             (defaultStacMap staffNames confDefaultStac)
             confDefaultArp
             (defaultTrillMap staffNames)
             (defaultTrillMap staffNames)
    dests = qInitDests qc e
    legOvlp = runExc $ findParam1 "legatoOverlap" e
    sepSame = runExc $ findParam1 "sepSame" e
    


qInitTrack :: QConfig -> MetaInstr -> [(Int,Int,Int,Int,Int)]
qInitTrack qc q = e1 ++ e2 ++ e3
  where
    e1 = map (\(str,ch) -> (str,ch,0xb0, 7,126)) . M.elems $ qDests q
    e2 = map (\(str,ch) -> (str,ch,0xb0,11,126)) . M.elems $ qDests q
    e3 = map (\(str,ch) -> (str,ch,0xb0, 5,  0)) . M.elems $ qDests q


-- initialize dests
--
--   to consider: is there a pizz.? what is arco called?
--
qInitDests :: QConfig -> Elem -> Map String (Int,Int)
qInitDests qConf e
  | M.keysSet destsMap == S.fromList ds = destsMap
  | otherwise = throwMine $ printf ("in initializing QL '%s', dests in " ++
                "config don't match needed dests: %s") (qcName qConf) (show ds)
  where
    ds = ["arco","short","expr"] ++ if (qcHasPizz qConf) then ["pizz"] else []
    destsElem = runExc $ findBr "dests" e
    destsMap = M.map (\[x,y] -> (x,y)) . M.fromList . runExc $
               mapM (convertBracketedSizeN 2) destsElem
    

qTranslateMark :: MetaInstr -> String -> Loc -> MarkD -> Tr ()
qTranslateMark mi staffName loc (InstrTechnique e) = case e of
  Left t -> let f s@MiQ {qTechnique = m} =
                  s {qTechnique = M.insert staffName t m}
            in updateMi (cName $ miCommon mi) f


qTranslateCk :: QConfig -> MetaInstr -> ChordKey -> Tr ()
qTranslateCk qc mi ck = do
  c <- computeCase ck
  evts <- case c of
    SingleCase       -> qSingle qc mi ck
    TrillCase   pits -> qTrillTrem qc mi ck (Left  pits)
    TremoloCase pits -> qTrillTrem qc mi ck (Right pits)
  prependOutputTr evts


--
--

-- qSingle: using a modifier code that turns on legato. how do we do this?
-- first we determine if a note is under a slur. that is, the ending of the
-- note is slurred to the next note. then we extend the note.

qSingle :: QConfig -> MetaInstr -> ChordKey -> Tr [MidiEvent]
qSingle qc q ck = do
  let QDestConfig dest legatoFlag velRange = qLookupDest qc q False ck
      legOvlp | legatoFlag = qLegOvlp q
              | otherwise  = Nothing
      doNote nk@(NoteKey (ChordKey staffName loc vn _) note) = do
        -- here we determine if this note's end is slurred to any next note.
        slurFlag <- checkSlurTr nk
        t1 <- noteOnOffTr nk
        let t2 = case qcShortDestForStac qc of
              False -> case stacDur q nk of
                Nothing -> t1
                Just d  -> doStacTimes d t1
              True -> t1
            modifiers | slurFlag  = [ Modif (RtOff (-0.010)) Nothing
                                     (Left (0xb0,68,127))
                                    ]
                      | otherwise = [ Modif (RtOff (-0.010)) Nothing
                                     (Left (0xb0,68,0))
                                    ]
        -- compute loudness
        l <- loudnessAtLocTr staffName vn loc
        let v = loudToVel velRange l
        return $ NoteEvent dest t2 legOvlp (qSepSame q) (staffName,loc)
                 (midiPitch $ nPitch note) v modifiers
  mapM doNote $ chordNoteKeysK ck


-- "single case" does do multiple notes. so does trill/trem, the difference
-- being that notes are specified by 

  
-- okay what are options when doing single?
--
--   is arp being used? if so, all staff names needed
--
--   should staccato be shortened? and to what?
--
--     staccato length can be read from MetaInstrCommon, but whether we
--     shorten for staccato depends on the instrument
--
--     stacTimesTr should be rewritten not to check isShort to make behavior
--     more transparent
--
--   should legato extension be used?
--
--     if not specified in config, then the answer is always "no"
--
--     in single translation, under what cases do we use legato? if it sounds
--     good, such as piano. if it is necessary for legato connections in the
--     MIDI instrument, and if the notes are under a slur. generally we won't
--     know whether it sounds good. for now it depends on dest and
--     articulation. we never extend a short note. 
--   
--   what are velocity ranges and random delta range
--
--   what is dest?


-- what is required when doing trill/trem?
--
--   dest
--
--   whether legato extend is being used
--
--   trill/trem shape (can be looked up in MiCommon)
--
--   
--
--
-- legato means two things
--
--   off time extension
--
--   turning on legato mode in PLAY
--
-- short dest
--
--   off time extension is possible
--
--   


qTrillTrem :: QConfig -> MetaInstr -> ChordKey -> TrillTremPitches ->
              Tr [MidiEvent]
qTrillTrem qc q ck@(ChordKey staffName loc vn _) pitches = do
  t1 <- chordOnMaxOffTr ck
  let legOvlp | qcShortDestForTrill qc = Nothing
              | otherwise = qLegOvlp q
      
      QDestConfig dest _ velRange = qLookupDest qc q (isShort ck) ck
      shape = qMapLookup staffName $
              (if isLeft pitches then cTrillShape else cTremShape) $
              miCommon q
      (firstStep,ts) = trillTimes shape (onTime t1) (offTime t1)
      vel (t,_) =loudnessAtDblTr staffName vn t >>= return . loudToVel velRange
  vels <- mapM vel ts
  let mkOnOff (x,y) = OnOff [ ("trill/trem",(x,y))
                            , ("nominal",(onTime t1,offTime t1)) ]
      onOffs = map mkOnOff ts
      (psLower,psUpper) = case pitches of
        Left (x,y) -> ([x],[y])
        Right x    -> x
      (pits1,pits2) = if firstStep == Lower
                        then (psLower,psUpper)
                        else (psUpper,psLower)
      pitsList = cycle [pits1,pits2]
      mkItems pits vel o = map (\p -> ((p,vel),o)) pits
  return [TrillTremEvent dest t1 legOvlp (qSepSame q) (staffName,loc)
         (concat $ zipWith3 mkItems pitsList vels onOffs) []]


  
loudToVel :: (Int,Int) -> Double -> Int
loudToVel (minV,maxV) l =
  round $ scaleClip 1 l 8 (fromIntegral minV) (fromIntegral maxV) 


data QDestConfig = QDestConfig
  { qdDest      :: (Int ,Int)
  , qdUseLegato :: Bool
  , qdVelRange  :: (Int,Int)
  }


qLookupDest :: QConfig -> MetaInstr -> Bool -> ChordKey -> QDestConfig
qLookupDest qc q isTrill ck
  | isShort ck = QDestConfig (l "short") False (l2 "short")
  | isTrill && (qcShortDestForTrill qc) =
      QDestConfig (l "short") False (l2 "short")
  | otherwise = case M.lookup (staffNameK ck) $ qTechnique q of
      Nothing -> QDestConfig (l "arco") True (l2 "arco")
      Just t  -> case t of
        "arco"  -> QDestConfig (l "arco") True  (l2 "arco")
        "pizz." -> QDestConfig (l "pizz") False (l2 "pizz") 
        "expr"  -> QDestConfig (l "expr") True  (l2 "expr")
        s       -> throwMine $ printf ("unknown instr. technique " ++
                   "'%s' in the score at or before %s") s
                   (showLoc2 $ begLocK ck)
  where
    l  s = case M.lookup s (qDests q) of {Just x -> x}
    l2 s = case M.lookup s (qcVelRanges qc) of {Just x -> x}


qMapLookup :: Ord k => k -> Map k a -> a
qMapLookup k m = case M.lookup k m of {Just x -> x}
