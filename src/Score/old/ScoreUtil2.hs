
module Score.ScoreUtil where

import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Common.CommonExport
import Common.CommonUtil
import Util.Exception
import Score.ScoreData
import Data.Ratio


_followingRepeats :: Staff -> NoteKey -> [NoteKey]
_followingRepeats Staff {stChords=chs, stName=staffName} nk =
  filter (midiPitchEqual nk) .
  noteKeysAtLoc staffName (nkTrueEnd nk) $ chs


midiPitchEqual nk1 nk2 = nkMidiPitch nk1 == nkMidiPitch nk2


noteKeysAtLoc :: String -> Loc -> Map Loc (Map Int Chord) -> [NoteKey]
noteKeysAtLoc staffName loc m = case M.lookup loc m of
  Nothing -> []
  Just m2 -> concatMap (\(vn,ch) -> chordNoteKeys staffName loc vn ch) .
             M.toList $ m2


-- to construct a ChordKey, we need staff name, loc, voice number and chord
chordNoteKeys :: String -> Loc -> Int -> Chord -> [NoteKey]
chordNoteKeys staffName loc vn ch = 
  map (\(idx,n) -> NoteKey (ChordKey staffName loc vn ch) n) . M.toList $
  cNotes ch

