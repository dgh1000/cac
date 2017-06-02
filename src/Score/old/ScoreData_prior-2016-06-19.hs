module Score.ScoreData where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Common.CommonData as CD
import Data.Array(Array)
import Data.Set(Set)
import Data.Map(Map)
import Common.CommonExport


data Score = Score
  { scTimeSigs       :: Map Int TimeSig
  , scMarks          :: Map Loc [Mark]    -- merged Marks from staves
  , scStaves         :: Map String Staff
  , scUsedMsrs       :: Array Int Bool
  }

data Staff = Staff
  { stName         :: String
  , stDynamics     :: Map Loc [Dynamic]
  , stHairpins     :: Map Loc Hairpin
  , stSlurs        :: Map Loc Slur
  , stPedalEvts    :: Map Loc PedalEvt
  , stMarks        :: Map Loc [Mark]
  , stPass1Words   :: Map Loc [Pass1Word]
  , stTrueEndsMap  :: Map Loc [NoteKey]
  , stUsedMsrs     :: Set Int
  , stChords       :: Map Loc (Map Int Chord)
  }



data MacroDefn = MacroDefn String Int [MacroDefnChar]

data MacroDefnChar = MdcChar Char
                   | MdcArg Int

data MacroInstance = MacroInstance String [String]  
  -- name of macro to apply, arguments

data Pass1Word = P1MacroDefn MacroDefn
               | P1MacroInstance MacroInstance
               | P1Normal String
               -- ^ <maybe name of macro defn that was applied> <text>
               | P1Comment 
               | P1W


-- data PostMacroWord = PostMacroWord (Maybe (Loc,String)) String


data MarkWord = MwMark Mark
              | MwWarpTemp Int Int (Maybe Rational) Rational Bool
                -- ^ <left arrow: 0 if absent, -1 if left-pointing, 
                --      1 if right point> 
                --   <right arrow>
                --   <width if present>  <amount> 
                --   <true if global warp>

{-
data ParsedWord = PwMark Mark

                | PwWarpTemp Int Int (Maybe Rational) Rational Bool
                -- ^ <left arrow: 0 if absent, -1 if left-pointing, 
                --      1 if right point> 
                --   <right arrow>
                --   <width if present>  <amount> 
                --   <true if global warp>
                | PwW
                | PwComment
                  deriving(Show)
-}


data Mark = SymbolMark         String Int
          | InstrTechnique     String
          | SetTempo           Tempo
          | SetVar             String Tempo
          | SpliceMark         Char
          | ArpDelta           Double
          | StacDur            Double
          -- | WarpTemp           Int Int (Maybe Rational) Rational Bool
          | Warp               Bool (Maybe Loc) (Maybe Loc) Rational Double
            -- ^ <true means this is global warp>
            --   <left w location if left-sided>
            --   <right w location if right-sided>
            --   <direction multiplier> 
            --     ^ direction multiplier will normally be 1 or -1
            --   <delta time as number of beats>.  
          | Pause              Duration
          | RampBeg            NumRatio
          | RampEndBeg         NumRatio NumRatio
          | RampEnd            NumRatio Bool
            -- ^ if the Bool is true, then don't return to prevailing tempo
            --   after the ramp is complete but stay at new tempo
          | RitAccel
          -- | WMark
          | TrillShapeMark     TrillShape
          | TremShapeMark      TrillShape
          | PatternMark        String
          -- | Comment
            deriving(Show)


data TrillShape = TrillShape TrillStep [(Double,Int)] TrillStep
                  deriving(Show)


data TrillStep = Upper | Lower
                 deriving(Show,Eq)


data NumRatio = NumRatio Double Double
                deriving(Show)


data Duration = DurSecs  Double
              | DurBeats Rational
                 deriving(Show)


data Tempo = TempoAbs Int
           | TempoRelative NumRatio (Maybe String)
               -- <ratio>  < Just v  : ratio of variable v
               --            Nothing : relative to previous section (this is
               --                      where a * is present)
             deriving(Show)


isRampBeg (RampBeg _) = True
isRampBeg _           = False


isRampEnd (RampEnd _ _) = True
isRampEnd _             = False


isRampEndBeg (RampEndBeg _ _) = True
isRampEndBeg _                = False


isPause (Pause _) = True
isPause _         = False


isStaffWarp (Warp flag _ _ _ _) = not flag
isStaffWarp _                   = False


isGlobalWarp (Warp flag _ _ _ _) = flag
isGlobalWarp _                   = False


maybeStacDur :: Mark -> Maybe Double
maybeStacDur (StacDur d) = Just d
maybeStacDur _           = Nothing


maybeArpDelta :: Mark -> Maybe Double
maybeArpDelta (ArpDelta d) = Just d
maybeArpDelta _            = Nothing


maybePatternMark :: Mark -> Maybe String
maybePatternMark (PatternMark s) = Just s
maybePatternMark _               = Nothing

data Chord = Chord
  { cEndLoc        :: Loc
  , cModifiers     :: Set ChordModifier
  , cNotes         :: Map Int Note
  , cDoubTremNotes :: Map Int Note
  }
           deriving(Show)

data Note = Note
  { nPitch    :: Pitch
  , nIsTied   :: Bool
  , nTrueEnd  :: Loc
  , nNotehead :: Notehead
  }
          deriving(Show)

data ChordKey = ChordKey 
  { ckStaffName   :: String
  , ckChordLoc    :: Loc
  , ckVoiceNum    :: Int
  , ckChord       :: Chord
  }
              deriving(Show)

data NoteKey = NoteKey 
  { nkStaffName :: String
  , nkChordLoc  :: Loc
  , nkVoiceNum  :: Int
  , nkChord     :: Chord
  , nkNote      :: Note
  }
             deriving(Show)

data ChordModifier = SingTrem Int
                   | DoubTremStart Int
                   | DoubTremStop Int
                   | Arpeggiate
                   | Staccato
                   | Staccatissimo
                   | Tenuto
                   | Accent
                   | StrongAccent
                   | Marcato
                   | Fermata
                   | DownBow
                   | UpBow
                   | OpenString
                     deriving(Show,Eq,Ord)


data Hairpin = Hairpin { hType :: HairpinType, hEnd :: Loc }
             deriving(Show)

data HairpinType = Crescendo | Diminuendo
                 deriving(Show,Eq)

data Slur = Slur Loc
          deriving(Show)

data Text = TechniqueText String 
          | ExpressionText String
          deriving(Show)



data OctaveLine = OctaveLine Int Loc   -- <# octaves shift>, <end loc>

----------------------------------------------------------------------
--                 class ChordLike

class ChordLike a where
  getChordLoc      :: a -> Loc
  getVoiceNum      :: a -> Int
  getChordMods     :: a -> [ChordModifier]
  getChordEnd      :: a -> Loc
  getStaffName     :: a -> String
  getChordNotes    :: a -> [NoteKey]
  getDoubTremNotes :: a -> [NoteKey]
  getMaxTrueEnd    :: a -> Loc        -- get maximum true end of all notes in
                                      -- chord

instance ChordLike ChordKey where
  getChordLoc  = ckChordLoc
  getVoiceNum  = ckVoiceNum
  getChordMods = S.toList . cModifiers . ckChord
  getChordEnd  = cEndLoc . ckChord
  getStaffName = ckStaffName
  getChordNotes (ChordKey staffName loc vn chord) = 
    map (\(idx,n) -> NoteKey staffName loc vn chord n) 
    (M.toList . cNotes $ chord)
  getDoubTremNotes (ChordKey staffName loc vn chord) =
    map (\(idx,n) -> NoteKey staffName loc vn chord n) 
    (M.toList . cDoubTremNotes $ chord)
  getMaxTrueEnd k = maximum $ (map nTrueEnd . M.elems . cNotes         $ c) 
                           ++ (map nTrueEnd . M.elems . cDoubTremNotes $ c)
    where c = ckChord k


instance ChordLike NoteKey where
  getChordLoc  = nkChordLoc
  getVoiceNum  = nkVoiceNum
  getChordMods = S.toList . cModifiers . nkChord
  getChordEnd  = cEndLoc . nkChord
  getStaffName = nkStaffName
  getChordNotes (NoteKey staffName loc vn chord _) =
    map (\(idx,n) -> NoteKey staffName loc vn chord n)
    (M.toList . cNotes $ chord)
  getDoubTremNotes (NoteKey staffName loc vn chord _) =
    map (\(idx,n) -> NoteKey staffName loc vn chord n) 
    (M.toList . cDoubTremNotes $ chord)
  getMaxTrueEnd k = maximum $ (map nTrueEnd . M.elems . cNotes         $ c)
                           ++ (map nTrueEnd . M.elems . cDoubTremNotes $ c)
    where
      c = nkChord k

----------------------------------------------------------------------
--                class NoteLike

class NoteLike a where
  getTrueEnd   :: a -> Loc
  getPitch     :: a -> Pitch
  getMidiPitch :: a -> Int

instance NoteLike NoteKey where
  getTrueEnd   = nTrueEnd . nkNote
  getPitch     = nPitch . nkNote
  getMidiPitch = CD.midiPitch . nPitch . nkNote


----------------------------------------------------------------------
--                 access functions


getChordKeys_staff :: Staff -> [ChordKey]
getChordKeys_staff staff = getChordKeys_chords (stName staff) (stChords staff)


getChordKeys_chords :: String -> Map Loc (Map Int Chord) -> [ChordKey]
getChordKeys_chords name chords = concatMap doLoc . M.toList $ chords
  where
    doLoc :: (Loc,Map Int Chord) -> [ChordKey]
    doLoc (loc,chords) = map doVoice . M.toList $ chords
      where
        doVoice :: (Int,Chord) -> ChordKey
        doVoice (vn,chord) = ChordKey name loc vn chord


getNoteKeys_score :: Score -> [NoteKey]
getNoteKeys_score score = concatMap getNoteKeys_staff . 
                          M.elems . scStaves $ score


getNoteKeys_staff :: Staff -> [NoteKey]
getNoteKeys_staff = 
  concatMap (\c -> getChordNotes c ++ getDoubTremNotes c) . getChordKeys_staff
 

{-
-- If, for example, "ps10" and "ps8" occur at the same Loc, it just takes
-- one of them arbitrarily
lookupMergedTech :: String -> Loc -> Score -> Maybe Int
lookupMergedTech s loc score = 
  M.lookup s (scMergedNumTech score) >>=
  M.lookup loc >>= 
  Just . head
-}


mapOverNotes :: (Loc -> Note -> Note) -> Map Loc (Map Int Chord) ->
                Map Loc (Map Int Chord)
mapOverNotes g = M.mapWithKey doLoc
  where
    doLoc :: Loc -> Map Int Chord -> Map Int Chord
    doLoc loc = M.map (doChord loc)
    doChord :: Loc -> Chord -> Chord
    doChord loc c@Chord{cNotes = cNotesIn, cDoubTremNotes = cDoubTremNotesIn} =
      c { cNotes = M.map (g loc) cNotesIn
        , cDoubTremNotes = M.map (g loc) cDoubTremNotesIn }
