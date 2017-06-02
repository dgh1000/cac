module Util.UtilData where

import System.Random
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

-- Curve
--   <first x value> <last x value> [<x,y pairs>]
data Curve = Curve Double Double [(Double,Double)]
           | CurveConstant Double

----------------------------------------------------------------------
----------------------------------------------------------------------
--                   MyRandomClass

class MyRandomClass a where
  getGen :: a -> StdGen
  putGen :: MonadState a m => StdGen -> m () 

----------------------------------------------------------------------
----------------------------------------------------------------------
--                    ErrorRand obsolete

data RandData = RandData
  { randStGen :: StdGen }
  deriving(Show)

type ErrorRand a = ExceptT String (WriterT String (State RandData)) a

instance MyRandomClass RandData where
  getGen d = randStGen d
  putGen g = do
    st <- get
    put $ st {randStGen=g}



-- type RandSt a = State RandData a

----------------------------------------------------------------------
----------------------------------------------------------------------
--               ErrWri obsolete

type ErrWri a = ExceptT String (Writer String) a




----------------------------------------------------------------------
----------------------------------------------------------------------
{-
data RandData_n_NodeAccum = RandData_n_NodeAccum
  { rnRandStGen :: StdGen
  , rnNodeAccum :: [[Node],Double] }

instance MyRandomClass RandData_n_NodeAccum where
  getGen = rnRandStGen
  putGen g m = m { rnRandStGen = g }
-}

----------------------------------------------------------------------
--         OKAY JUST MAKE ONE TYPE AND GET IT OVER WITH

----------------------------------------------------------------------
type Cacm a = ExceptT String (WriterT String (State RandData)) a

runCM m s = runState (runWriterT (runExceptT m)) s




