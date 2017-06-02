module Score.ScoreData where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Common.CommonData as CD
import Data.Array(Array)
import Data.Set(Set)
import Data.Map(Map)
import Common.CommonExport

data ScoreObject = SoMarks { soLoc       :: Loc
                           , soStaffName :: String
                           , soMarks     :: [Mark Double]
                           }
                 | SoCk    { soLoc       :: Loc
                           , soStaffName :: String
                           , soVn        :: Int
                           , soCk        :: ChordKey
                           }
                 deriving(Eq)


instance Ord ScoreObject where
  compare x y | soLoc x < soLoc y = LT
              | soLoc x > soLoc y = GT
              | otherwise = case (x,y) of
                  (SoMarks{},SoCk{}) -> LT
                  (SoCk{},SoMarks{}) -> GT
                  otherwise          -> EQ
                         

data Score = Score
  { scTimeSigs       :: Map Int TimeSig
  , scMarks          :: Map Loc (Map String [Mark Double])

  -- cached data related to Marks
  , scMarksByStaff   :: Map String (Map Loc [Mark Double])
  , scMarkers        :: Map String Markers
                        
  , scStaves         :: Map String Staff
  , scUsedMsrs       :: Array Int Bool
  }


data Staff = Staff
  { stName         :: String
  , stDynamics     :: Map Loc [Dynamic]
  , stHairpins     :: Map Loc Hairpin
  , stPedalEvts    :: Map Loc PedalEvt
  , stMetSymMarks  :: Map Loc [Mark Double]
  , stTrueEndsMap  :: Map Loc [NoteKey]
  , stUsedMsrs     :: Set Int
  , stSlurs        :: Map Loc Loc
  , stChords       :: Map Loc (Map Int Chord)
  }



  

{-

types of words needed for parsing under new scheme

  Pass1Word

    needed for macro processing and to remove comments.

    is there any need to store Pass1Words between calls in XmlDocToScore.hs or

whatever it is called?

  MarkWord: no longer needed

    originally this was needed because we did the following in two stages:

      evaluate variables

      find end points of warps and absolute warps

    however we will now find variables at a later point, when processing
    warps, pauses, tempo etc. inside the Translation module

  Mark

        

-}



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



data PointType = PtW | PtCrescDescr | PtDescrCresc


data WarpSide = LeftWarp | RightWarp
                deriving(Show,Eq,Ord)

type Variables = Map Loc (Map String Double)


type MarksByLoc = Map Loc (Map String [Mark Double])


type MarksByStaff = Map String (Map Loc [Mark Double])


{-

should ramp begin, end-beg, and end use NumRatio or NumVar? I don't see any
reason we can't use variables in tempo ramps. why didn't we use them
previously? I am not sure. Perhaps it had to do with method of determining
variable values, there was a fold over all marks in order including variable
setting. oh wait. the issue is that only ratios are meaningful inside
ramps. if there is a var it strongly implies absolute tempo because that's the
only thing we've used vars for up to now. that's okay, we can use vars for
other things too.

-}


-- this appears to be new Mark styles. does this include new way of indicating
-- channel? I don't see it in InstrTechnique

type MarkN = Mark NumVar

type MarkD = Mark Double


data Mark a = SymbolMark       String Int
            | InstrTechnique   (Either String [String])
            | SetTempo         a
            | SetVar           String a
            | SpliceMark       Char
            | ArpDelta         Double
            | StacDur          Double
            | DynShape         Bool a a
             -- ^ < True=<>, False=>< > <peak loc ratio> <peak loudness delta>
            | Warp             Int Int (Maybe a) a Bool
            -- ^ <left arrow: 0 if absent, -1 if left-pointing, 
            --      1 if right point> 
            --   <right arrow>
            --   <width if present>  
            --   <amount of warp>
            --   <true if global warp>
           | AbsWarp          WarpSide a
           | Pause            (Either Double a)
           | MultPause        Int a a
             -- <num pauses> <separation of pauses> <max pause in quarters>
           | RitPause         a a a a
             -- <tempo change pre-pause>
             -- <tempo change post-pause>
             -- <num quarters over which to spread out rit/accel>
             -- <num quarters to pause>
           | W
           | CrescDescr       a a
           | EndCrescDescr
           | EndDescrCresc
           | RampBeg          a
           | RampEndBeg       a a
           | RampEnd          a
           | RitAccel
           | TrillShapeMark   TrillShape
           | TremShapeMark    TrillShape
           | PatternMark      String
           deriving(Show,Eq,Ord)


{-

-- Mark
--
data Mark   = SymbolMark       String Int
            | InstrTechnique   (Either String [String])
            | SetTempo         Double
            | SpliceMark       Char
            | ArpDelta         Double
            | StacDur          Double
            | DynShape         Bool Double Double
              -- ^ < True=<>, False=>< > <peak loc ratio> <peak loudness delta>
            | Warp             Int Int (Maybe Double) Double Bool
              -- ^ <left arrow: 0 if absent, -1 if left-pointing, 
              --      1 if right point> 
              --   <right arrow>
              --   <width if present>  
              --   <amount of warp>
              --   <true if global warp>
            | AbsWarp          WarpSide Double
          | Pause            (Either Double Double)
          | MultPause        Int Double Double
            -- <num pauses> <separation of pauses> <max pause in quarters>
          | RitPause         Double Double Double Double 
            -- <tempo change pre-pause>
            -- <tempo change post-pause>
            -- <num quarters over which to spread out rit/accel>
            -- <num quarters to pause>
          | W
          | CrescDescr       Double Double
          | EndCrescDescr
          | EndDescrCresc
          | RampBeg          Double
          | RampEndBeg       Double Double
          | RampEnd          Double
          | RitAccel
          | TrillShapeMark   TrillShape
          | TremShapeMark    TrillShape
          | PatternMark      String
            deriving(Show,Eq,Ord)

-}

{-
data Mark = SymbolMark         String Int
          | InstrTechnique     String
          | SetTempo           Double
          | SetVar             String Double
          | SpliceMark         Char
          | ArpDelta           Double
          | StacDur            Double
          | DynShape           Bool Double Double
            -- ^ < True=<>, False=>< > <peak loc ratio> <peak loudness delta>
          | DynShapeEnd        Bool
            -- ^ < True=<>, False=>< >
          | Warp               Bool (Maybe Loc) (Maybe Loc) Double Double
            -- ^ <true means this is global warp>
            --   <left w location if left-sided>
            --   <right w location if right-sided>
            --   <amount of warp as number of quarters>  
            --   <direction multiplier> 
            --     ^ direction multiplier will normally be 1 or -1
          | AbsWarp            Loc Double
          | Pause              Double
            --   ^ (Right value) is number of quarters
          | MultPause Int Double
            --  <num pauses> <max pause in quarters>
          | RitPause Double Double Double Double
            --  <
          | RampBeg            NumRatio
          | RampEndBeg         NumRatio NumRatio
          | RampEnd            NumRatio
            -- ^ if the Bool is true, then don't return to prevailing tempo
            --   after the ramp is complete but stay at new tempo
          | RitAccel
          -- | WMark
          | TrillShapeMark     TrillShape
          | TremShapeMark      TrillShape
          | PatternMark        String
          
          -- | Comment
            deriving(Show)
-}


data Markers = Markers
  { maWs :: Set Loc
  , maCD :: Set Loc
  , maDC :: Set Loc
  }


-- Number of beats as number of quarters
data NumVar = NumVar Double (Maybe String)
                deriving(Show,Eq,Ord)


data TrillShape = TrillShape TrillStep [(Double,Int)] TrillStep
                  deriving(Show,Eq,Ord)


data TrillStep = Upper | Lower
                 deriving(Show,Eq,Ord)


{-
data NumRatio = NumRatio Double Double
                deriving(Show)
-}


{-
data Tempo = TempoAbs Int
           | TempoRelative NumRatio (Maybe String)
               -- <ratio>  < Just v  : ratio of variable v
               --            Nothing : relative to previous section (this is
               --                      where a * is present)
             deriving(Show)
-}


{-
isRampBeg (RampBeg _) = True
isRampBeg _           = False


isRampEnd (RampEnd _ ) = True
isRampEnd _            = False


isRampEndBeg (RampEndBeg _ _) = True
isRampEndBeg _                = False


isPause (Pause _) = True
isPause _         = False


isStaffWarp (Warp flag _ _ _ _) = not flag
isStaffWarp _                   = False


isGlobalWarp (Warp flag _ _ _ _) = flag
isGlobalWarp _                   = False


isAbsWarp (AbsWarp _ _) = True
isAbsWarp _             = False


maybeStacDur :: Mark -> Maybe Double
maybeStacDur (StacDur d) = Just d
maybeStacDur _           = Nothing


maybeArpDelta :: Mark -> Maybe Double
maybeArpDelta (ArpDelta d) = Just d
maybeArpDelta _            = Nothing


maybePatternMark :: Mark -> Maybe String
maybePatternMark (PatternMark s) = Just s
maybePatternMark _               = Nothing
-}


data Chord = Chord
  { cEndLoc        :: Loc
  , cModifiers     :: Set ChordModifier
  , cNotes         :: NSingles
  , cNotes         :: Map Int Note
  , cDoubTremNotes :: Map Int Note
  }
           deriving(Eq,Ord,Show)

data ChordTiming = ChordTiming Bool [(Int,OnOff)]
  CTSingles [(Int,OnOff)]
                 | CTTrill [(Int,OnOff)]

data ChordContent = SinglesContent (Map Int Note)
                  | TrillContent Bool Bool (Set Int)
                    -- True: trill, Fal:trem
                    -- True: start on upper, False: start on lower

data Note = Note
  { nPitch    :: Pitch
  , nIsTied   :: Bool
  , nTrueEnd  :: Loc
  , nNotehead :: Notehead
  }
          deriving(Eq,Ord,Show)

data ChordKey = ChordKey 
  { ckStaffName   :: String
  , ckChordLoc    :: Loc
  , ckVoiceNum    :: Int
  , ckContent     :: Either [Int] ((Bool,Bool),([Int],[Int]))
  -- , ckChord       :: Chord
  }
              deriving(Eq,Ord,Show)


{-
data NoteKey = NoteKey 
  { nkStaffName :: String
  , nkChordLoc  :: Loc
  , nkVoiceNum  :: Int
  , nkChord     :: Chord
  , nkNote      :: Note
  }
             deriving(Show)
-}

data NoteKey = NoteKey
  { nkCk         :: ChordKey
  , nkMidiPitch  :: Int
  -- , nkNote       :: Note
  } 



nkPitch :: NoteKey -> Pitch
nkPitch = nPitch . nkNote

nkMidiPitch :: NoteKey -> Int
nkMidiPitch = midiPitch . nPitch . nkNote

nkTrueEnd = nTrueEnd . nkNote

  

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

data Text = TechniqueText String 
          | ExpressionText String
          deriving(Show)



data OctaveLine = OctaveLine Int Loc   -- <# octaves shift>, <end loc>

----------------------------------------------------------------------
--                 class ChordLike

class K a where
  begLocK        :: a -> Loc
  endLocK        :: a -> Loc
  maxNoteEndK    :: a -> Loc -- max true end of any note in this chord
  staffNameK     :: a -> String
  voiceNumK      :: a -> Int
  modifiersK     :: a -> Set ChordModifier  
  chordEndK      :: a -> Loc
  chordNoteKeysK :: a -> [NoteKey]
  doubTremNoteKeysK :: a -> [NoteKey]


instance K ChordKey where
  begLocK = ckChordLoc
  endLocK = cEndLoc . ckChord
  maxNoteEndK ck = maximum $ (map nTrueEnd . M.elems . cNotes         $ c) 
                          ++ (map nTrueEnd . M.elems . cDoubTremNotes $ c)
    where c = ckChord ck
  staffNameK = ckStaffName
  voiceNumK = ckVoiceNum
  modifiersK = cModifiers . ckChord
  chordEndK = cEndLoc . ckChord
  chordNoteKeysK ck = map (\n -> NoteKey ck n) . M.elems . cNotes $ ckChord ck
  doubTremNoteKeysK ck =
    map (\n -> NoteKey ck n) . M.elems . cDoubTremNotes $ ckChord ck



instance K NoteKey where
  begLocK = begLocK . nkCk
  endLocK = endLocK . nkCk
  maxNoteEndK = maxNoteEndK . nkCk
  staffNameK = staffNameK . nkCk
  voiceNumK = voiceNumK . nkCk
  modifiersK = modifiersK . nkCk
  chordEndK = cEndLoc . ckChord . nkCk
  chordNoteKeysK (NoteKey ck _) =
    map (\n -> NoteKey ck n) . M.elems . cNotes $ ckChord ck
  doubTremNoteKeysK (NoteKey ck _) =
    map (\n -> NoteKey ck n) . M.elems . cDoubTremNotes $ ckChord ck



{-

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
  getChordNotes ck@(ChordKey staffName loc vn chord) = 
    map (\(_,n) -> NoteKey ck n) . M.toList $ cNotes chord
  getDoubTremNotes ck@(ChordKey staffName loc vn chord) =
    map (\(_,n) -> NoteKey ck n) . M.toList $ cDoubTremNotes $ chord
  getMaxTrueEnd k = maximum $ (map nTrueEnd . M.elems . cNotes         $ c) 
                           ++ (map nTrueEnd . M.elems . cDoubTremNotes $ c)
    where c = ckChord k


instance ChordLike NoteKey where
  getChordLoc  = ckChordLoc . nkCk
  getVoiceNum  = ckVoiceNum . nkCk
  getChordMods = S.toList . cModifiers . ckChord . nkCk
  getChordEnd  = cEndLoc . ckChord . nkCk
  getStaffName = ckStaffName . nkCk
  getChordNotes (NoteKey ck@(ChordKey _ _ _ chord) _) =
    map (\(idx,n) -> NoteKey ck n) . M.toList $ cNotes chord
  getDoubTremNotes (NoteKey ck@(ChordKey _ _ _ chord) _) =
    map (\(idx,n) -> NoteKey ck n) . M.toList $ cDoubTremNotes chord
  getMaxTrueEnd k = maximum $ (map nTrueEnd . M.elems . cNotes         $ c)
                           ++ (map nTrueEnd . M.elems . cDoubTremNotes $ c)
    where
      c = ckChord $ nkCk k

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


-}

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
  concatMap (\c -> chordNoteKeysK c ++ doubTremNoteKeysK c) . getChordKeys_staff
 

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
