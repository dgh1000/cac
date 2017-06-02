
module Score.ScoreData where

type PartId = String
type MsrNumber = Int
type VoiceNumber = Int

data Score = Score
  { scParts :: Map PartId Part
  , scMsrInfo :: Map MsrNumber MeasureInfo
  , scMetronomeMarks :: Map Loc Int
  , scTempoLines :: Map Loc TempoLine
  }


globalPedalEvts 

data ScoreMemo = ScoreMemo 
  { smGlobalPedalEvts :: Map Loc PedalEvt
  }

data Score2 
  { score2Score :: Score
  , score2Memo :: ScoreMemo
  , score2Parts :: Map PartId Part2 
  }


data Part = Part
  { pChords :: Map Loc (Map VoiceNumber Chord)
  , pDynamics :: Map Loc (Map VoiceNumber Dynamic)
  , pHairpins :: Map Loc Hairpin
  , pSlurs :: Map Loc Slur
  , pSymbols :: Map Loc [Symbol]
  , pPedalEvts :: Map Loc PedalEvt
  , pTechniqeText :: Map Loc [TechniqueText]
  , pExpressionText :: Map Loc [ExpressionText]
  }

data PartMemo = PartMemo
  { pmTimeMap :: Map Loc (Rational, Double)
  , pmAbsoluteTimeMap :: Map  Loc (Rational, Double)
  , pmTrueEnds :: Map Loc [NoteContext]
  , pmLoudnessCurve :: Map (Loc,Loc) (Double,Double)
  , pmMinLoc :: Loc
  , pmMaxLoc :: Loc
  , ... and much more ...
  }

data Part2 
  { part2Part :: Part 
  , part2Memo :: PartMemo
  , part2Chords :: Map Loc (Map VoiceNumber Chord2)
  }

data Chord = Chord
  { cEnd :: Loc
  , cArts :: [Articulation]
  , cSingleTrem :: Int
  , cDoubleTrem :: Int
  , cIsGrace :: Bool
  , cNotes :: [Note]
  }

data ChordMemo = Chord
  { ccTemoloOtherNotes :: [NoteContext]
  }

data Chord2
  { chord2Chord :: Chord
  , chord2Memo :: ChordMemo
  , chord2Notes [Note2]

data Note = Note
  { nPitch :: Pitch
  , nIsTied :: Bool
  , nNotehead :: Notehead
  }

data NoteMemo
  { nmFollowsTie :: Bool
  }

data Note2 = Note2
  { note2Note :: Note
  , note2Memo :: NoteMemo
  }

data PartContext = PartContext
  { pcScore :: Score2
  , pcPartId :: PartId
  }

data ChordContext = ChordContext
  { ccScore :: Score2
  , ccPartId :: PartId
  , ccChordLoc :: Loc
  , ccVoiceNumber :: Int
  }

data NoteContext = NoteContext
  { ncScore :: Score2
  , ncPartId :: String
  , ncChordLoc :: Loc
  , ncVoiceNumber :: Int
  , ncNoteIdx :: Int
  }

lookupCC :: ChordContext -> (Part2,Chord2)
lookupNC :: NoteContext -> (Part2,Chord2,Note2)

enumeratePartChords :: PartContext  -> [(ChordContext,Chord2)]
enumeratePartNotes  :: PartContext  -> [(NoteContext,Note2)]
enumerateChordNotes :: ChordContext -> [(NoteContext,Note2)]

class ChordLike a where
  begLoc :: a -> Loc
  arts :: a -> [Articulation]
  ... etc ..

instance ChordLike Chord2 where
  begLoc = chord2Chord