{-# LANGUAGE TupleSections #-}
module Instruments.QuantumLeapDXF where

import qualified Data.Map as M
import Text.Printf
import Control.Monad
import Control.Arrow
import Data.Map(Map)
import Common.CommonData
import Score.ScoreData
import Translation.TranslationData
import Translation.TranslationUtil( markMapUtils, isShort, getParams 
                                  , hasModifier )
import Translation.TimeMap
import Translation.Dynamics
import Util.Exception

{-

we need to have some kind of mark that controls choice of channel. I think we
implemented it. built whole mcp with it yet?


truncating/extending midi events, and Quantum Leap patches

  extending

    useful for legato in sustained patches

    what happens if we use extension in a pizz or short patch? probably
    nothing harmful

  truncation

    only useful if sound is sustained, which will be marked in the NoteEvent

marking midi events as shortened

  this is to prevent extension. but we won't use extension except in cases a
  NoteEvent is marked as a legato connection, in which case we wouldn't want
  to mark it as shorted anyway. We will use "short" patches

-}



configModSpacing = 0.01
configModLead    = 0.01
configAccentSize = 1.0

type MaybeTrunc   = Maybe (Double,Double)
type TechniqueMap = Map Loc (Either String String)

data QLConfig = QLConfig
  { qcTec             :: TruncExtConfig
  , qcVelFn           :: TechniqueMap -> TranslationCase -> Double -> 
                         ChordKey -> Tr Int
  -- qcMakeLoudnessFn differs slightly from the iMakeLoudnessFn field in
  -- Instrument. it takes an extra argument, the staff.
  , qcMakeLoudnessFn  :: Staff -> AbsTimeMap -> Map Int LoudnessFunc
  , qc
  , qcModValueFn      :: Double -> Int
  , qcFixedExprIds    :: [String]
  }


quantumLeapDXF :: QLConfig -> Score -> StaffConfig -> Instrument
quantumLeapDXF 
  (QLConfig truncExtConfig velFn modifierFn qlLoudnessFn modValueFn
   fixedExprIds) score sc = 
  Instrument
  { iTimeFn          = timeFn
  , iVelFn           = velFn techText
  , iDestFn          = destFn sc techText
  , iModifierFn      = modifierFn staff
  , iMakeLoudnessFn  = qlLoudnessFn staff
  -- The field 'iTrillTremRate' is no longer part of Instrument. Instead we
  -- have iTrillShape and iTremShape. We will now use markMapUtils in
  -- TranslationUtil.hs to create 'trillShapes' and 'tremShapes'. The latter
  -- will be constructed with a default trill rate of 14 (at present; it may
  -- have changed). We no longer will use a default trill rate as part of the
  -- Quantum Leap specific instrument configuration. If this fails to sound
  -- good for all Quantum Leap instruments, then we will have do something
  -- better.
  , iTrillShape      = trillShapes
  , iTremShape       = tremShapes
  , iAlterTOff       = truncExtConfig
  , iModConfig       = Just modConfig
  , iSusPedDest      = Nothing
  , iFixedControls   = fixedExprControls ++ fixedVolControls
  }
  where
    -- do we need arpDeltas? no. don't need stacDurs.
    -- do need trillShapes, tremShapes
    -- do need techText.
    --
    -- how did we get techText below? we called buildTechTextMap. what has
    -- happened to this function? we don't even know what a TechniqueMap is. 
    -- looks like TechniqueMap is Map Loc String. 
    -- 
    -- TechniqueMap is built by looking for InstrTechnique-type Marks at each
    -- Loc. If more than one is there, we throw error. If exactly one is
    -- there, we include that in Map Loc String. this is use.
    --
    -- same with tremolo shapes and trill shapes, with respect to
    -- TrillShapeMark and TremShapeMark.
    --
    -- probably no harm in computing arp deltas and stac durs. slightly
    -- inefficent but probably not much.
    --
    (arpDeltas,stacDurs,trillShapes,tremShapes,techText) = 
      markMapUtils score (stcName sc) "arco"
    [ConfigValueInt vol] = getParams [(CVTInt,"vol")] sc
    Just staff = M.lookup (stcName sc) (scStaves score)
    modConfig = ModConfig 
      { mcDest        = lookupDest sc "DXF"
      , mcControllers = [1,11]
      , mcValueFn     = \x -> [modValueFn x, modValueFn x]
      , mcSpacing     = configModSpacing
      , mcLead        = configModLead
      }
    fixedExprControls = map ((,(11,126)) . lookupDest sc) fixedExprIds
    fixedVolControls  = map ((,( 7,vol)) . lookupDest sc) $ "DXF":fixedExprIds


-- timeFn
--
-- In Quantum leap, time is always from start of chord to end of chord. Not
-- adjusted for staccato or arpeggio. For staccato, we always use the short
-- patch. Arpeggio is not implemented.
--
timeFn noteKey = do
  (nominalBeg,nominalEnd) <- getBegEndTr noteKey
  return $ RegularModel nominalBeg nominalEnd nominalBeg nominalEnd 
           Nothing Nothing

-- velFn
--
-- velFn is provided by specific instrument in QLConfig




-- destFn
--
-- Compute a 'destination', which is a combined midi stream and channel, and
-- also look up whether this midi stream/channel contains a sound of type
-- 'sustaining', which means it needs separation between repeated notes.
--
-- For Quantum Leap instruments, the following needs to be done.
--
-- 1. Look up the technique as marked in the score (DXF, expr, arco,
--    pizz. etc.)
--
-- 2. The config file should have a midi channel entry for that technique in
--    the 'midi' substatement section. This entry will indicate a stream and
--    channel.
--
-- 3. Note that there is no convention for the order in which the different
--    techniques are assigned to channels. The user can put them in the PLAY
--    instrument however he likes, but he must then configure the .cfg file to
--    match.
--
-- 4. The final part of the 'destination', the sustained-sound flag, is
--    determined by 'destFn' by looking at the technique id. We have hardcoded
--    here that ids 'DXF' and 'expr' are sustaining, and anything else is not.
--
destFn :: StaffConfig -> TechniqueMap -> TranslationCase -> ChordKey -> 
          (Int,Int,Bool)
destFn sc techMap case_ ck = 
  let id_ = computeLocalId techMap case_ ck
      (x,y) = lookupDest sc id_
  in (x, y, id_ == "DXF" || id_ == "expr")
    

-- lookupDest
--
-- Given a "local id" which is a name that corresponds to a MIDI patch such as
-- DXF or short, lookup the stream and channel used for that local id.
--
lookupDest :: StaffConfig -> String -> (Int,Int)
lookupDest sc id_ = case M.lookup id_ $ stcMidiDests sc of
  Nothing -> throwMine $ printf ("Couldn't find the local channel '%s'"++
             " in staff config for '%s'") id_ (stcName sc)
  Just x  -> x


lookupTechnique :: TechniqueMap -> Loc -> Maybe String
lookupTechnique m loc = case M.lookupLE loc m of
  Nothing        -> Nothing
  Just (Left s)  -> Just s
  Just (Right s) -> throwMine $ printf ("in a Quanum Leap instrument, " ++
                    "the technique marking is channel list type, at loc %s")
                    (showLoc2 loc)


techToId t = case t of
  Nothing      -> "DXF"
  Just "DXF"   -> "DXF"
  Just "expr"  -> "expr"
  Just "pizz." -> "pizz"
  Just "arco"  -> "DXF"


computeLocalId :: ChordLike a => TechniqueMap -> TranslationCase -> a -> 
                  String
computeLocalId techMap case_ k =
  case case_ of
    -- isShort checks mods that are part of ChordKey or NoteKey k
    SingleCase _   | isShort k -> "short"
                   | otherwise -> techToId . lookupTechnique techMap 
                                  $ (getChordLoc k)
    TrillCase  _   -> "short"
    TremoloCase    -> "short"


{-
qAccentUtil :: Staff -> AbsTimeMap -> Map Int LoudnessFunc
qAccentUtil staff tm = M.fromList . map (\v->(v,makeVoice v)) $ [1..4]
  where
    makeVoice n = lookupLoudness tm $ computeAccentLoudCur n configAccentSize
                  staff
-}


{-
qAccentUtil :: Staff -> AbsTimeMap -> Map Int LoudnessFunc
qAccentUtil staff tm = M.fromList . map (\v -> (v, makeVoice v)) $ [1..4]
  where
    makeVoice voiceNum = mkCurve StrongAccent 2 `addLF` mkCurve Accent 1
                         `addLF` mkCurve Tenuto (-1)
      where
        mkCurve mod accentSize = lookupLoudness tm $ computeChordLoudCur
                                 (hasModifier mod) voiceNum accentSize staff
-}

