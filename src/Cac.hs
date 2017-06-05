
module Cac where

import Data.List(sortBy)
import Data.Function
import System.Random
import Control.Monad.State

type Rio = StateT StdGen IO

rioChoose :: [a] -> Rio a
rioChoose xs | null xs = error "given null list in rioChooseItem"
             | otherwise = do
                 gen  <- get
                 let (index,gen') = randomR (0::Int, length xs-1) gen
                 put gen'
                 return $ xs !! index


rioRandomR :: Random r => (r,r) -> Rio r
rioRandomR (lo,hi) = do
  g <- get
  let (value,g') = randomR (lo,hi) g
  put g'
  return value


rioShuffle :: [a] -> Rio [a]
rioShuffle xs = do
  let p x = do
        r <- rioRandomR (0,1) :: Rio Double
        return (r,x)
  pairs <- sortBy (compare `on` fst) `liftM` mapM p xs
  return $ map snd pairs

