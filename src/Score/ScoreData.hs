{-# LANGUAGE DeriveAnyClass,DeriveGeneric #-}
module Score.ScoreData where

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Common.CommonData as CD
import GHC.Generics hiding(Meta)
import Control.DeepSeq
import Control.Arrow
import Data.Array(Array)
import Data.Set(Set)
import Data.Map(Map)
import Common.CommonData


data ScoreObject = SoMarks { soLoc       :: Loc
                           , soStaffName :: String
                           , soMarks     :: [Mark Double]
                           }
                 | SoCk    { soLoc       :: Loc
                           , soStaffName :: String
                           , soVn        :: Int
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


emptyScore = Score M.empty M.empty M.empty M.empty M.empty
             (A.listArray (1,1) [True])


data Staff = Staff
  { stName         :: String
  , stDynamics     :: Map Loc [Dynamic]
  , stHairpins     :: Map Loc Hairpin
  , stPedalEvts    :: Map Loc PedalEvt
  , stMetSymMarks  :: Map Loc [Mark Double]
  , stMaxTrueEnd   :: Loc
  , stUsedMsrs     :: Set Int
  , stSlurs        :: Map Loc Loc
  , stBrackets     :: Map String [(Loc,Loc)]
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



data PointType = PtW | PtCrescDescr | PtDescrCresc


data WarpSide = LeftWarp | RightWarp
                deriving(Show,Eq,Ord)

type Variables = Map Loc (Map String Double)


type MarksByLoc = Map Loc (Map String [Mark Double])


type MarksByStaff = Map String (Map Loc [Mark Double])


type MarkN = Mark NumVar

type MarkD = Mark Double


data Mark a = SymbolMark       String Int
            -- | InstrTechnique   (Either String [String])
            | SetTempo         a
            | SetVar           String a
            | SpliceMark       Char
            | ArpDelta         Double
            | StacDur          Double
            | Trunc            Double
            | Extend           Double
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
           -- | RitPause         a a a a
             -- <tempo change pre-pause>
             -- <tempo change post-pause>
             -- <num quarters over which to spread out rit/accel>
             -- <num quarters to pause>
           -- | StaffAdjust      a a Double  -- <width in each direction> <amo>
                                          -- <direction>
           -- | GlobAdjust       a a Double
           | Adjust           (Maybe a) a Double Bool
             -- (width if present) amt direction <global flag>
           | AdjustMarker
           | W
           | CrescDescr       a a
           | EndCrescDescr
           | EndDescrCresc
           | RampBeg          a
           | RampEndBeg       a a
           | RampEnd          a
           | TwoModify        TempoModify TempoModify (Maybe Double)
           | RitAccel
           | TrillShapeMark   TrillShape
           | TremShapeMark    TrillShape
           | PatternMark      String
           | Artic            String
           | BracketL         String
           | BracketR         String
           deriving(Show,Eq,Ord)



data TempoModify = TmRamp      Double Double
                 | TmRampParab Double Double Bool
                   -- parabolic ramp shape. bool = true means steepest slope
                   -- at beginning, otherwise steepest slope at end
                   deriving(Show,Eq,Ord)

data Markers = Markers
  { maWs :: Set Loc
  , maCD :: Set Loc
  , maDC :: Set Loc
  , maCaret :: Set Loc
  }


-- Number of beats as number of quarters
data NumVar = NumVar Double (Maybe String)
                deriving(Show,Eq,Ord)


data TrillShape = TrillShape TrillStep [(Double,Int)] TrillStep
                  deriving(Show,Eq,Ord)


data TrillStep = Upper | Lower
                 deriving(Show,Eq,Ord)


data Chord = Chord
  { cEndLoc        :: Loc
  , cModifiers     :: Set ChordModifier
  , cNotes         :: Notes
  }
           deriving(Eq,Ord,Show,NFData,Generic)

-- data ChordContent = SinglesContent (Map Int Note)
--                  | TrillContent Bool Bool (Set Int)
                    -- True: trill, Fal:trem
                    -- True: start on upper, False: start on lower

-- okay we need

-- how do you identify this with a NoteKey? Just an Int!


data Notes = NSingles (Map Int Note)
           | NTrill Bool (Map Int Note) (Map Int Note)
             -- Bool is tremolo flag: False for trill, True for tremolo
             deriving(Eq,Ord,Show,NFData,Generic)


data PrelimChord = PrelimChord
  { prcEndLoc     :: Loc
  , prcModifiers  :: Set ChordModifier
  , prcNotes      :: [Note]
  }


data Note = Note
  { nPitch    :: Pitch
  , nIsTied   :: Bool
  , nTrueEnd  :: Loc
  , nNotehead :: Notehead
  }
          deriving(Eq,Ord,Show,NFData,Generic)


data ChordKey = ChordKey 
  { ckStaffName   :: String
  , ckChordLoc    :: Loc
  , ckVoiceNum    :: Int
  , ckContent     :: Either [Int] ([Int],[Int])
  }
              deriving(Eq,Ord,Show)


notesToContent :: Notes -> Either [Int] ([Int],[Int])
notesToContent (NSingles m) = Left $ M.keys m
notesToContent (NTrill _ m1 m2) = Right (M.keys m1,M.keys m2)


data NoteKey = NoteKey ChordKey Int
             deriving(Eq,Ord,Show)


-- Loc, voice number, pitch
data StaffNoteKey = StaffNoteKey Loc Int Int

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
                     deriving(Show,Eq,Ord,NFData,Generic)


data Hairpin = Hairpin
  { hType :: HairpinType
  , hEnd  :: Loc
  }
             deriving(Show)

data HairpinType = Crescendo | Diminuendo
                 deriving(Show,Eq)

data Text = TechniqueText String 
          | ExpressionText String
          deriving(Show)



data OctaveLine = OctaveLine Int Loc   -- <# octaves shift>, <end loc>


chordTrueEnds :: Chord -> [(Int,Loc)]
chordTrueEnds (Chord endLoc _ notes) = case notes of
  NSingles noteMap ->
    let f p = case M.lookup p noteMap of
          Just n -> nTrueEnd n
    in  map (id &&& f) $ M.keys noteMap
  NTrill _ s1 s2 ->
    let f s p = case M.lookup p s of
          Just n -> nTrueEnd n
    in (map (id &&& f s1) $ M.keys s1) ++ (map (id &&& f s2) $ M.keys s2)



mapOverNotes :: (NoteKey -> a) -> Score -> Map NoteKey a
mapOverNotes f sc =
  M.fromList . concatMap (mapOverNotes_staff f) . M.toList $ scStaves sc


mapOverNotes_staff :: (NoteKey -> a) -> (String,Staff) -> [(NoteKey,a)]
mapOverNotes_staff f (name,staff) =
  concatMap (mapOverNotes_loc f name) . M.toList $ stChords staff


mapOverNotes_loc :: (NoteKey -> a) -> String -> (Loc,Map Int Chord) ->
                    [(NoteKey,a)]
mapOverNotes_loc f staffName (loc,chords) =
  concatMap (mapOverNotes_vn f staffName loc) . M.toList $ chords

mapOverNotes_vn :: (NoteKey -> a) -> String -> Loc -> (Int,Chord) ->
                   [(NoteKey,a)]
mapOverNotes_vn f staffName loc (vn,chord) =
  map (\i -> (nk i,f $ nk i)) (pitchesOf $ cNotes chord)
  where
    
    contentOf (NSingles m) = Left $ M.keys m
    contentOf (NTrill _ xs ys) = Right (M.keys xs,M.keys ys)
    pitchesOf (NSingles m) = M.keys m
    pitchesOf (NTrill _ xs ys) = M.keys xs ++ M.keys ys
    nk i = NoteKey (ChordKey staffName loc vn (contentOf $ cNotes chord)) i

    
{-

allNoteKeys :: Score -> [NoteKey]
allNoteKeys sc = concatMap allNKs_staff . M.toList $ scStaves sc

allCKs_staff :: (String,Staff) -> [ChordKeys]
allCKs_staff (name,staff) =
  concatMap (allNKs_loc name) . M.toList $ stChords staff

allNKs_loc :: String -> (Loc,Map Int Chord) -> [NoteKey]
allNKs_loc 

-}
