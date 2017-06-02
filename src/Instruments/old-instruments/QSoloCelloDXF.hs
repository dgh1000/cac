{-# LANGUAGE TupleSections #-}
module Instruments.QSoloCelloDXF where


import qualified Data.Map as M
import Data.Map(Map)
import Instruments.QuantumLeapDXF
import Score.ScoreData
import Translation.TranslationData
import Translation.Dynamics
import Util.Math(scaleClip)
import Common.CommonData



--  thinking, Jan 2017.

--   any ql instrument will have possibly several channels (or one keyswitch
--   channel). let's say we have several channels, for DXF, short, etc. we
--   then have a reaper config set up this way. we also have a sibelius file
--   with particular staff names. so the sib file, the config, and the reaper
--   configuration are all linked. this is not ideal because if one of them
--   changes, the whole thing will stop working. if I lose one of them, I will
--   need to recreate it.
--
--   is there any way around this, the fact that we lose work? it's hard to
--   document what's in a reaper file outside of the reaper file itself. even
--   if I say "use a particular instrument" there may be newer versions of
--   that instrument and the sound would change.
--
--   so first thing this underscores is the need to archive SOUND files when I
--   finish a composition.
--
--   now, what can be recreated if one of them is lost?
--
--     if config is lost
--
--       looking at reaper file, we can infer what instruments and
--       articulations are loaded. this may imply what the config was, unless
--       is there some ambiguity: two versions of a meta-instrument that use
--       the same articulations, or two meta-instruments
--
--     if reaper config is lost
--
--       looking at config.txt together with documentation in the
--       meta-instrument Haskell code, we can infer what instruments and
--       articulations were used
--
--  can more than one staff be used with a QL meta-instrument? there might be
--  some problems with notes that stomp on each other and that might be hard
--  to debug, so
--
--  yes it is very useful to direct several staves to the same QL
--  channels. however that means that I need to write code that gives a
--  sensible error message when notes from different staves result in a
--  conflict. 




{-

patches used:

  - DXF: note July 2016, I didn't write down before which DXF patch I used, so
      I will assume it is SVC Vib DXF ACC and see how that works out.

  - short: SVC Mart Up Dn Marc x6
  - pizz: SVC Pizz RRx3
  - expr: SVC Exp Vib

balanced:

  - short relative to DXF, 9/13/2015
  - pizz. relative to DXF, 9/13/2015
  - expr  relative to DXF, 9/13/2015

-}


-- qSoloCelloDXF
--
-- 
qSoloCelloDXF :: Score -> StaffConfig -> Instrument
qSoloCelloDXF score sc = quantumLeapDXF 
  QLConfig
  { qcTec             = TruncExtConfig
                        { tecMinDur       = 50
                        , tecMinSep       = 1
                        , tecSustainedSep = Just 50
                        , tecExtend       = Nothing
                        , tecLegExtend    = Just 10
                        , tecMaxRatio     = 1.0 }
  , qcVelFn           = velFn
  , qcMakeLoudnessFn  = makeLoudnessFn
  , qcModValueFn      = modValueFn
  , qcFixedExprIds    = ["expr", "pizz", "short"]
  } score sc


velFn :: TechniqueMap -> TranslationCase -> Double -> ChordKey -> Tr Int
velFn techMap case_ loudness k = do
  let id_ = computeLocalId techMap case_ k
      vel1 = round $ scaleClip 1.0 loudness 8.0 20 90
      vel2 = round $ scaleClip 1.0 loudness 8.0 30 127
      vel3 = round $ scaleClip 1.0 loudness 8.0 20 127
  return (case id_ of
    "DXF"   -> 64
    "short" -> vel1
    "pizz"  -> vel2
    "expr"  -> vel3)


modValueFn x = round $ scaleClip 1.0 x 8.0 20 127

makeLoudnessFn staff tm = standardModifiersToLoudFunc staff tm
