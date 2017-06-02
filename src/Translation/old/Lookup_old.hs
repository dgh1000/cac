
module Translation.Lookup where

import qualified Data.Map as M
import Control.Monad.State
import Text.Printf
import Util.Exception
import Score.ScoreData
import Translation.TranslationData

import Instruments.Piano         (piano         )
{-
import Instruments.GOrgan        (gOrgan        )
import Instruments.QSoloCelloDXF (qSoloCelloDXF )
-- import Instruments.QSoloViolinDXF(qSoloViolinDXF)
-}

instrAssocs :: [(String,Score -> StaffConfig -> Instrument)]
instrAssocs  = [ ("piano"          , piano         )
               -- , ("gOrgan"         , gOrgan        )
               -- , ("qSoloCelloDXF"  , qSoloCelloDXF ) 
               -- , ("qSoloViolinDXF" , qSoloViolinDXF)
               ]


lookupInstrument :: String -> Score -> StaffConfig -> Instrument
lookupInstrument s = case lookup s instrAssocs of
  Nothing  -> throwMine $ printf ("config specifies instrument '%s' but "++
              "this instrument is not listed in Lookup.hs") s
  Just i   -> i


-- getInstrTr
--
--  <staff name> -> Tr <Instrument>
--
getInstrTr :: String -> Tr Instrument
getInstrTr name = do
  PlaybackConfig staffConfigs _ _ _ _ _ _ _ _ <- gets tsPlaybackConfig
  score <- gets tsScore
  let staffConf = case M.lookup name staffConfigs of {Just x -> x}
  return $ lookupInstrument (stcInstrName staffConf) score staffConf


