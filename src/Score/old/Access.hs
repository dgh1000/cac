module Score.Access where

import Data.Map(Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Score.ScoreData
import Common.CommonExport
import qualified Common.CommonData as CD

{-
instance ScoreLike Score where
  getStaves = M.toList . scStaves
-}


instance ChordLike ChordKey where
  getChordLoc  = ckChordLoc
  getVoiceNum  = ckVoiceNum
  getChordMods = S.toList . cModifiers . ckChord
  getChordEnd  = cEndLoc . ckChord
  getStaffName = ckStaffName

instance ChordLike NoteKey where
  getChordLoc  = nkChordLoc
  getVoiceNum  = nkVoiceNum
  getChordMods = S.toList . cModifiers . nkChord
  getChordEnd  = cEndLoc . nkChord
  getStaffName = nkStaffName

instance NoteLike NoteKey where
  getTrueEnd   = nTrueEnd . nkNote
  getPitch     = nPitch . nkNote
  getMidiPitch = CD.midiPitch . nPitch . nkNote

instance NoteLike Note where
  getTrueEnd   = nTrueEnd
  getPitch     = nPitch
  getMidiPitch = CD.midiPitch . nPitch


getChordNotes :: ChordKey -> [NoteKey]
getChordNotes (ChordKey staffName loc voiceNum chord) = 
  map g . M.toList $ cNotes chord
  where
    g :: (Int,Note) -> NoteKey
    g (noteIdx,note) = NoteKey staffName loc voiceNum chord noteIdx note


getDoubTremNotes :: ChordKey -> [NoteKey]
getDoubTremNotes (ChordKey staffName loc voiceNum chord) = 
  map g . M.toList $ cDoubTremNotes chord
  where
    g :: (Int,Note) -> NoteKey
    g (noteIdx,note) = NoteKey staffName loc voiceNum chord noteIdx note

getStaffChordKeys :: Score -> String -> [ChordKey]
getStaffChordKeys score staffName = concatMap doLoc . M.toList . stChords $ 
                                    staff
  where
    Just staff = M.lookup staffName . scStaves $ score
    doLoc :: (Loc,Map Int Chord) -> [ChordKey]
    doLoc (loc,chords) = map doVoice . M.toList $ chords
      where
        doVoice :: (Int,Chord) -> ChordKey
        doVoice (vn,chord) = ChordKey staffName loc vn chord

