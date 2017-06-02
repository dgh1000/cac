module Instruments.QSoloViolinDXF where


import qualified Data.Map as M
import Data.Map(Map)
import Instruments.QuantumLeapDXF
import Score.ScoreData
import Translation.TranslationData
import Translation.Dynamics
import Util.Math(scaleClip)
import Common.CommonData


{-

patches used:

  - expr : SVL Exp-Leg
  - short: SVL Stac RRx4
  - pizz.: SVL Pizz RRx3


balanced:

  - expr : done 9/12/2015
  - short: done 9/12/2015
  - pizz.: done 9/12/2015

-}




qSoloViolinDXF score sc = quantumLeapDXF 
  QLConfig
  { qcRptTrunc        = Just (0.08,0.08)
  , qcVelFn           = velFn
  , qcMakeLoudnessFn  = qAccentUtil
  , qcTrillTremRate   = 12
  , qcModValueFn      = modValueFn
  , qcFixedExprIds    = ["expr", "pizz", "short"]
  } score sc


velFn :: TechniqueMap -> TranslationCase -> Double -> ChordKey -> Tr Int
velFn techMap case_ loud k = do
  let id_ = computeLocalId techMap case_ k
      vel1 | loud >= 4.0 = round $ scaleClip 4.0 loud 8.0 45 100
           | loud <  4.0 = round $ scaleClip 1.0 loud 4.0 10 44
  
      vel2 | loud >= 4.5 = round $ scaleClip 4.5 loud 8.0 55 120
           | loud <  4.5 = round $ scaleClip 1.0 loud 4.5 15 54

      vel3 | 4.5 <= loud = round $ scaleClip 4.5 loud 8.0 41 85
           | 4.5 >  loud = round $ scaleClip 1.0 loud 4.5 15 40
  return (case id_ of
    "DXF"   -> 64
    "short" -> vel1
    "pizz"  -> vel2
    "expr"  -> vel3)


modValueFn x = round $ scaleClip 1.0 x 8.0 20 127
