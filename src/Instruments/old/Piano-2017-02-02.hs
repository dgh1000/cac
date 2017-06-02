
module Instruments.Piano where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Text.Printf
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Either
import Data.Map(Map)
import Common.CommonData
import Midi.MidiData
import Score.ScoreData
import Translation.TranslationUtil
import Translation.TranslationData
import Translation.Trill
import Translation.ValidateConfig
import Translation.LoudnessFunc
import Instruments.InstrUtils
import Instruments.InstrumentsData
import Util.Math
import Util.Exception

confDefaultStac = 0.07
confDefaultArp  = 0.05


-- config file: will specify dests somehow, need to look up config state


-- we will find the m.i. statement in the config with a name for this instance
-- and the word "piano" (name for this instance can be skipped if there is
-- only one piano? or should I manually call it 'piano'? I will need to refer
-- to it in other types of programming statements in the config... 

pianoInit :: Elem -> MetaInstr
pianoInit e = case areElemsValid validBrck validParam e of
  Nothing -> MiPiano common destsMap legatoOvlp
  Just s  -> throwMine $ "while initializing 'piano', " ++ s
  where
    validBrck  = ["staves", "dests"]
    validParam = ["instr", "name" , "velDelta", "legatoOverlap"]
    common = MetaInstrCommon name staffNames e pnoInitTrack
             pnoTranslateCk
             pnoTranslateMark
             (defaultStacMap staffNames confDefaultStac)
             confDefaultArp
             (defaultTrillMap staffNames)
             (defaultTrillMap staffNames)
    staffNames = runExc $ findBrOfSingles "staves" e
    destsElems = runExc $ findBr "dests" e
    destsMap' = M.map (\[x,y] -> (x,y)) . M.fromList . runExc $
                mapM (convertBracketedSizeN 2) destsElems
    destsMap = runExc $ withExcept ("while verifying staff names, "++)
               (mapVfyKeys staffNames destsMap')
    legatoOvlp = runExc $ findParam1 "legatoOverlap" e
    name = runExc $ findParam1 "name" e

{-
pnoValidElems (Bracketed _ _ _ elems) = case e2 of
  [] -> Nothing
  xs -> throwMine $ printf "invalid elems in config for meta-inst piano: %s"
                    (concatMap (\x -> simpleShowElem x ++ ",") xs)
  where
    (_,e2) = L.partition correct elems
    correct (Bracketed _ _ t _) = t `elem` ["staves","dests"]
    correct (Param _ _ t _) = t `elem`
      ["instr","name","velDelta","legatoOverlap"]
    correct (Single _ _ _) = False
-}

pnoInitTrack piano =
  map (\(str,ch) -> (str,ch,0xb0, 7,126)) . M.elems $ pnoDests piano


-- pnoTranslateMark
--
-- Translate a Mark. This is only called if the more general code was not able
-- to handle the Mark itself.
--
-- At this time, no Marks do anything for pianos specifically, so this code
-- does nothing.
pnoTranslateMark :: MetaInstr -> String -> Loc -> MarkD -> Tr ()
pnoTranslateMark _ _ _ m | pnoIgnoredMark m = return ()


pnoIgnoredMark :: MarkD -> Bool
pnoIgnoredMark (InstrTechnique _) = True
pnoIgnoredMark _                  = False


-- pnoTranslateCk
--
-- Translate a ChordKey into midi events and update TrState.
--
-- Note that this doesn't update the MetaInstr (a MiPiano) because no piano
-- state is ever changed in the translation of a ChordKey.
pnoTranslateCk :: MetaInstr -> ChordKey -> Tr ()
pnoTranslateCk mi ck = do
  c <- computeCase ck
  evts <- case c of
    SingleCase       -> pianoSingleCase mi ck
    TrillCase   pits -> pianoTrillTremCase mi ck (Left  pits)
    TremoloCase pits -> pianoTrillTremCase mi ck (Right pits)
  prependOutputTr evts


pianoSingleCase :: MetaInstr -> ChordKey -> Tr [MidiEvent]
pianoSingleCase piano ck = do
  let legOvlp | isShort ck = Nothing
              | otherwise  = pnoLegOvlp piano
      
      staffName = staffNameK ck
      dest = pianoMapLookup staffName (pnoDests piano)
      allStaffNames = cStaffNames $ miCommon piano
      doNote :: NoteKey -> Tr MidiEvent
      doNote nk@(NoteKey (ChordKey staffName loc vn _) note) = do
        -- compute times
        t1 <- noteOnOffTr nk

        let t2 = case stacDur piano nk of
              Nothing -> t1
              Just d  -> doStacTimes d t1
        t3 <- arpTr allStaffNames (cArpDelta $ miCommon piano) nk t2
        -- compute loudness
        l <- loudnessAtLocTr staffName vn loc
        v <- loudToVel piano 10 127 l
        return $ NoteEvent dest t3 legOvlp Nothing (staffName,loc)
                 (midiPitch $ nPitch note) v []
  mapM doNote $ chordNoteKeysK ck



-- to compute trill/trem case, we need to know
--
--   chord begin/end times
--
--   time of each sub chord
--
--   pitch(es) in each sub chord
--
--   velocity at each 



-- pianoTrillTremCase
--
--   Convert a trilled or tremolo'd chord to MidiEvent.
--
-- Inputs
--
--   'pitches' : for trill, this is Left (<lower>,<upper>)
--               for tremolo, this is Right ( [<first  chord pitches>]
--                                          , [<second chord pitches>] )
-- 
pianoTrillTremCase :: MetaInstr -> ChordKey ->
                      Either (Int,Int) ([Int],[Int]) -> Tr [MidiEvent]
pianoTrillTremCase piano ck@(ChordKey staffName loc vn _) pitches = do
  -- okay so we are going to need to know note pitches and times. and compute
  -- loudness for each one.
  t1 <- chordOnMaxOffTr ck
  let legOvlp = pnoLegOvlp piano
      shape = pianoMapLookup staffName $
              (if isLeft pitches then cTrillShape else cTremShape) $
              miCommon piano
      (firstStep,ts) = trillTimes shape (onTime t1) (offTime t1)
      -- 'vel' is a function to look up velocity at time t
      vel (t,_) = loudnessAtDblTr staffName vn t >>=
                  loudToVel piano 10 110
      dest = pianoMapLookup staffName (pnoDests piano)
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
      mkItems :: [Int] -> Int -> OnOff -> [((Int,Int),OnOff)]
      mkItems pits vel o = map (\p -> ((p,vel),o)) pits
  return [TrillTremEvent dest t1 legOvlp Nothing (staffName,loc)
          (concat $ zipWith3 mkItems pitsList vels onOffs) []]



{-  
      idxOf sn = case staffArpPitches loc staff of
        xs@(_:_) -> let ps = (if reverseFlag then reverse else id) $ sort xs
                    in  case elemIndex (getMidiPitch nk) of
                          Just i -> i
        where
          staff = mapLookupTrUtil sn $ scStaves score
-}



  
pianoMapLookup :: Ord k => k -> Map k a -> a
pianoMapLookup k m = case M.lookup k m of {Just x -> x}
  
 
-- velocity: now this depends on the "case" - different for trills
loudToVel :: MetaInstr -> Int -> Int -> Double -> Tr Int
loudToVel piano minV maxV ld = do
  let velDelta :: Int
      velDelta = runExcMsg "in piano, " .
                 findParam1 "velDelta" . cOrigConfig $ miCommon piano
      v = round $ scaleClip 1 ld 8 (fromIntegral minV) (fromIntegral maxV)
      minVR = max minV (v-velDelta)
      maxVR = min maxV (v+velDelta)
  trRandomR (minVR,maxVR)

