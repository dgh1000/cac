{-# LANGUAGE FlexibleContexts #-}

module Util.RandomState 
  ( freshRandData
  , randData
  , rdRandom
  , rdRandomR
  , rdRandoms
  , rdRandomRs 
  , rdGausses
  , rdGaussesScale
  , rdGaussesScaleLimits
  , rdGauss
  , rdGaussScale
  , rdGaussScaleLimits
  , chooseRandom
  , weightedChoice
  , randomChooseList
  , randomChooseSeqs
  , mkStdGen
  , MyRandomClass
  , putGen
  , getGen ) where

import qualified Data.List as L
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Error
import System.Random
import Util.Math
import Util.UtilData




freshRandData :: IO StdGen
freshRandData = do
  seed <- randomIO
  return (mkStdGen seed)


randData s = RandData (mkStdGen s)

rdRandom :: (Random a, MyRandomClass b, MonadState b m) => m a
rdRandom = do
  st <- get
  let (a,newgen) = random (getGen st)
  put (putGen newgen st)
  return a



rdRandomR :: (Random a, MyRandomClass b, MonadState b m) => 
             a -> a -> m a
rdRandomR r1 r2 = do
  st <- get
  let (a,newgen) = randomR (r1,r2) (getGen st)
  put (putGen newgen st)
  return a

rdRandoms :: (Random a, MyRandomClass b, MonadState b m) => m [a]
rdRandoms = do
  st <- get
  let (g1,g2) = split (getGen st)
  put (putGen g2 st)
  return $ randoms g1

rdRandomRs :: (Random a, MyRandomClass b, MonadState b m) => a -> a -> m [a]
rdRandomRs  r1 r2 = do
  st <- get
  let (g1,g2) = split (getGen st)
  put (putGen g2 st)
  return $ randomRs (r1,r2) g1

----------------------------------------------------------------------
----------------------------------------------------------------------
--                gauss


add8 :: Num a => [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a] -> [a]
add8 s1 s2 s3 s4 s5 s6 s7 s8 
  | null s1 || null s2 || null s3 || null s4 || null s5 || null s6 || null s7
            || null s8 = []
  | otherwise = 
     (head s1 + head s2 + head s3 + head s4 + head s5 + head s6 + head s7
     + head s8 - 4) : add8 (tail s1) (tail s2) (tail s3) (tail s4) (tail s5)
     (tail s6) (tail s7) (tail s8)


rdGausses :: (MyRandomClass b, MonadState b m) => m [Double]
rdGausses = do
  r1 <- rdRandomRs 0 1
  r2 <- rdRandomRs 0 1
  r3 <- rdRandomRs 0 1
  r4 <- rdRandomRs 0 1
  r5 <- rdRandomRs 0 1
  r6 <- rdRandomRs 0 1
  r7 <- rdRandomRs 0 1
  r8 <- rdRandomRs 0 1
  -- let g x1 x2 x3 x4 x5 x6 x7 = (x1+x2+x3+x4+x5+x6+x7) - 3.5
  -- return $ L.zipWith7 g r1 r2 r3 r4 r5 r6 r7
  return $ add8 r1 r2 r3 r4 r5 r6 r7 r8
  

rdGaussesScale :: (MyRandomClass b, MonadState b m , MonadError String m) =>
                  Double -> Double -> m [Double]
rdGaussesScale low high = do
  xs <- rdGausses
  let d = abs (high - low)
  when (d == 0) (throwError "zero delta limits in rdGausessScale")
  when (high <= low) (throwError "high less than low in rdGausessScale")
  return $ map (\x -> (scale (-1.5) x 1.5 low high)) xs

rdGaussesScaleLimits :: (MyRandomClass b, MonadState b m, 
  MonadError String m) => Double -> Double -> m [Double]
rdGaussesScaleLimits low high = do
  xs <- rdGaussesScale low high
  let clamp x | x < low = low
              | x > high = high
              | otherwise = x
  return . map clamp $ xs


----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
--              single-value form of rdGauss

rdGauss :: (MyRandomClass b, MonadState b m, MonadError String m) => m Double
rdGauss = do
  r1 <- rdRandomR (-0.5) 0.5
  r2 <- rdRandomR (-0.5) 0.5
  r3 <- rdRandomR (-0.5) 0.5
  r4 <- rdRandomR (-0.5) 0.5
  r5 <- rdRandomR (-0.5) 0.5
  r6 <- rdRandomR (-0.5) 0.5
  r7 <- rdRandomR (-0.5) 0.5
  r8 <- rdRandomR (-0.5) 0.5
  r9 <- rdRandomR (-0.5) 0.5
  r10 <- rdRandomR (-0.5) 0.5
  r11 <- rdRandomR (-0.5) 0.5
  r12 <- rdRandomR (-0.5) 0.5
  return $ r1 + r2 + r3 + r4 +r5 + r6 + r7 + r8 + r9 +r10 +r11 +r12


--
-- scaling is set so that somewhere between 93% of values are within the 
-- limits
--
rdGaussScale low high = do
  x <- rdGauss
  when (abs(high-low)==0) (throwError "zero delta limits in rdGaussScale")
  when (high < low) (throwError "high less than low in rdGauss")
  return $ scale (-1.8) x 1.8 low high

rdGaussScaleLimits low high = do
  x <- rdGaussScale low high
  return $ min (max x low) high

----------------------------------------------------------------------
----------------------------------------------------------------------
--          other utilities



-- chooseRandom
--
--  Group by equal scores. Look at N best groups and choose one. One of the
--  inputs is an array of N weights giving the probabilisitc weight that
--  each group will be chosen. (this is usually starts with a high number
--  and goes down. Say [10,2,1]. 
--
-- [Double] : weights. Number of values here is the number of groups of
--   equal scores that will be considered.
--
-- [(Double,a)] : data with scores
--
--  ErrorRand (Double,a) : the chosen value and score
-- 
chooseRandom :: [Double] -> [(Double,a)] -> ErrorRand (Double,a)
chooseRandom weights choices = do
  when (L.null choices) (throwError "Null choice list.")
  let sorted = L.sortBy (\(y2,_) (y1,_)-> compare y1 y2 {-reversed!-} ) choices
      grouped = L.groupBy (\(x,_) (y,_) -> x == y) sorted
  idx <- weightedChoice weights
  -- if idx >= length 'grouped' we need to adjust it to be within bounds
  let idxR = min idx (length grouped - 1)
      pickedGroup = grouped !! idxR
  randomChooseList pickedGroup


-- weightedChoice
--  Given a list of N weights (unchecked condition is that they are all
--  positive, choose an Int from 0 to N-1, randomly, but each number being
--  weighted in probability.
weightedChoice :: [Double] -> ErrorRand Int
weightedChoice weights = do
    let s = sum weights
    x <- rdRandomR 0 (s - 0.00001)
    let cutoffs = drop 1 $ scanl (+) 0 weights
        remain = dropWhile (\a -> a <= x) cutoffs
    return $ length cutoffs - length remain

-- randomChooseList
--
--  randomly choose one value from a list
--
randomChooseList :: [a] -> ErrorRand a
randomChooseList xs = do
  let ll = length xs
  when (ll == 0) (throwError "in randomChooseList, passed null list")
  idx <- rdRandomR 0 (ll - 1)
  return (xs !! idx)



randomChooseSeqs :: (Random a, MyRandomClass b, MonadState b m) =>
        m [a] -> m [a] -> m [a]
randomChooseSeqs seq1 seq2 = do
  let g i v1 v2 = if i == 1 then v1 else v2
  liftM3 (zipWith3 g) (rdRandomRs 1 (2::Int)) seq1 seq2




