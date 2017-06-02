
module Instruments.Translation where

import Text.Printf
import Data.Monoid
import Data.Map(Map)
import qualified Data.Map as M
import Midi.MidiExport
import Common.CommonExport
import qualified Common.CommonUtil as CU
import Instruments.InstrumentsData
import Score.ScoreData
import Score.Access
import Score.Dynamics 
import Score.TimeMap
import Util.Exception


scoreToMidi :: Score -> Config -> Map String Instrument -> Int -> Int ->
               [MidiEvent]
scoreToMidi

scoreToMidi :: Score -> Config -> Map String Instrument -> Int -> Int -> 
               [MidiEvent]
scoreToMidi score config instruments begMsr endMsr =
  concatMap doStaff $ M.toList . scStaves $ score
  where
    doStaff (staffName,staff) = 
        concatMap (doChord tc) chordKeys ++ computeModEvts tc
      where
        instr = case M.lookup staffName instruments of {Just i -> i}
        chordKeys = filterInMsrRange score staffName begMsr endMsr
        loudCurs = computeDynHairpinLoudCur staff :
          (loudCurvesFn instr) score staffName
        loudCursD = map loudCurToD loudCurs
        tc = TranslationContext
          { tcConfig = config
          , tcInstr = instr
          , tcLoudData = LoudnessData loudCurs loudCursD
          }


filterInMsrRange :: Score -> String -> Int -> Int -> [ChordKey]
filterInMsrRange _ _ _ = error "foo"


doChord :: TranslationContext -> ChordKey -> [MidiEvent]
doChord tc chordKey 
  | not . null . chordDoubTremNotes $ chordKey =
      doTrillTremChord tc computeTremoloPitches chordKey
  | otherwise = case maybeTrill chordKey of
      Nothing  -> doSingleChord tc chordKey
      Just alt -> doTrillTremChord tc (computeTrillPitches alt) chordKey

computeTremoloPitches :: ChordKey -> ([Int],[Int])
computeTremoloPitches c = ( map getMidiPitch (chordNotes c)
                          , map getMidiPitch (chordDoubTremNotes c) )

computeTrillPitches :: Int -> ChordKey -> ([Int],[Int])
computeTrillPitches alter chordKey = case chordNotes chordKey of
  [noteKey] -> ( [computeTrillPitch alter (getPitch noteKey)]
               , [getMidiPitch noteKey] )
  xs -> throwMine $ printf ("Can't apply trill to chord with multiple notes "++
        "at %s") (simpleShowLoc . getChordLoc $ chordKey)

maybeTrill :: ChordKey -> Maybe Int
maybeTrill ck = M.lookup (getChordLoc ck) (stSymbols $ getStaff ck) >>= 
    getFirst . mconcat . map (First . isTrill)
  where
    voiceNum = getVoiceNum ck
    isTrill :: Symbol -> Maybe Int
    isTrill (Symbol s vn) | vn /= voiceNum = Nothing
                          | otherwise = case s of
                              "trill-natural" -> Just 0
                              "trill-flat"    -> Just (-1)
                              "trill-sharp"   -> Just 1

computeTrillPitch :: Int -> Pitch -> Int
computeTrillPitch upperAlter (Pitch _ step alter octave) = newMidiPitch
  where
    newStep = (step+1) `mod` 7
    newOctave = if step == 6 then octave+1 else octave
    newMidiPitch = CU.stepAlterOctToMidi newStep upperAlter newOctave


doSingleChord :: TranslationContext -> ChordKey -> [MidiEvent]
doSingleChord tc chordKey = concatMap (doSingleNote tc) (chordNotes chordKey)


doSingleNote :: TranslationContext -> NoteKey -> [MidiEvent]
doSingleNote (TranslationContext config instr loudData) noteKey =
    [NoteEvent (toMidiTime tBeg) noteOn noteOff (toMidiTime tEnd) [] False]
  where
    (LoudnessData loudCurs _) = loudData
    pit = getMidiPitch noteKey
    (tBeg,tEnd) = (timeFn instr) config noteKey
    loudValue = lookupLoudness loudCurs $ getChordLoc noteKey
    vel = (velFn instr) False loudValue
    (stream,chan) = (destFn instr) False config 
    noteOn  = RawMidiEvent stream chan 0x90 pit vel
    noteOff = RawMidiEvent stream chan 0x80 pit 64


doTrillTremChord :: TranslationContext -> (ChordKey -> ([Int],[Int])) ->
                    ChordKey -> [MidiEvent]
doTrillTremChord tc@(TranslationContext config instr _) pitFn chordKey =
    evts1 ++ evts2
  where
    timeMap = getTimeMap chordKey
    tBeg = lookupTime timeMap $ getChordLoc chordKey
    (n1:_) = chordNotes chordKey
    tEnd = lookupTime timeMap $ getTrueEnd n1
    rate = (trillTremRateFn instr) config chordKey
    (times1,times2) = computeTrillTremTimes tBeg tEnd rate
    (pitches1,pitches2) = pitFn chordKey
    begLoc = getChordLoc chordKey
    doTimes1 pitch = map (doTrillTremOneNote tc begLoc pitch) $ 
                     zip times1 (True:repeat False)
    doTimes2 pitch = map (doTrillTremOneNote tc begLoc pitch) $ 
                     zip times2 (repeat False)
    evts1 = concatMap doTimes1 pitches1
    evts2 = concatMap doTimes2 pitches2

computeTrillTremTimes :: Double -> Double -> Double -> 
                         ( [(Double,Double)], [(Double,Double)] )
computeTrillTremTimes _ _ _ = undefined


doTrillTremOneNote :: TranslationContext -> Loc -> Int -> 
                      ((Double,Double),Bool) -> MidiEvent
doTrillTremOneNote (TranslationContext config instr loudData) begLoc
    pitch
    ((tBeg,tEnd),firstFlag) 
    = NoteEvent (toMidiTime tBeg) noteOn noteOff (toMidiTime tEnd) [] False
  where
    LoudnessData loudCurs loudCursD = loudData
    loudValue = if firstFlag 
      then lookupLoudness  loudCurs   begLoc
      else lookupLoudnessD loudCursD  tBeg
    vel = (velFn instr) True loudValue
    (stream,chan) = (destFn instr) True config
    noteOn  = RawMidiEvent stream chan 0x90 pitch vel
    noteOff = RawMidiEvent stream chan 0x80 pitch 64

computeModEvts _ = error "foo"