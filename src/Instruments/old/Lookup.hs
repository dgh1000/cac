
module Translation.Lookup where

import Text.Printf
import Util.Exception
import Score.ScoreData
import Translation.TranslationData
import Instruments.Piano         (piano         )
import Instruments.GOrgan        (gOrgan        )
import Instruments.QSoloCelloDXF (qSoloCelloDXF )
-- import Instruments.QSoloViolinDXF(qSoloViolinDXF)

instrAssocs :: [(String,Score -> StaffConfig -> Instrument)]
instrAssocs  = [ ("piano"          , piano         )
               , ("gOrgan"         , gOrgan        )
               , ("qSoloCelloDXF"  , qSoloCelloDXF ) 
               -- , ("qSoloViolinDXF" , qSoloViolinDXF)
               ]


lookupInstrument :: String -> Score -> StaffConfig -> Instrument
lookupInstrument s = case lookup s instrAssocs of
  Nothing  -> throwMine $ printf ("config specifies instrument '%s' but "++
              "this instrument is not listed in Lookup.hs") s
  Just i   -> i

