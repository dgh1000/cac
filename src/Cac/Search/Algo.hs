
{-# LANGUAGE BangPatterns #-}
module Cac.Search.Algo where


import qualified Data.List as L
import Data.Function
import Cac.Search




-- type RuleAndResult a b c = (Rule a b c, RuleResult c)


-- what if we track the sum of step scores ourselves? why make the instance of
-- Opt do it? where should we update the saChosen field of the StepArchive?


-- different ways of running algorithm, want to switch between them at
-- will. for instance we could do a simple full backtracking search. we could
-- do a full search to a depth of 5 to find next best step, then choose that
-- step and do a full search again. we could do a monte-carlo kind of deal,
-- search 1000 random selections to a depth of 5 or 10, to evaluate the next
-- best step. we could take the top 5 selections at each step to a depth of
-- whatever results in the number of nodes we want to explore per "next step"
-- evaluation.
--
-- Also we want to generate a set of fragments F_aud to audition.

-- terminology:
--
-- Set S_next is set of all possible next steps. we want to pick up. the
-- algorithm amounts to selecting from S_next, then again, then again until we
-- reach a complete composition. S_avail. 


-- should I define complete composition as always N steps, or is there some
-- other criteria for completeness? considering that my working method is to
-- generate a fragment F and most of the these fragments are not the end of
-- the composition, # of steps is good.
--
-- some kind of evaluation of the time as it progresses, adjusting number of
-- choices correspondingly.
--
-- we evaluate each step in C1_A by adding that step, evaluating, then
-- monte-carlo to a depth of MC_depth, using

-- should composition record the score at each step along the way? we need to
-- identify tests by name. should record include test results that were
-- Nothing, seeing as those would be pruned? It's simpler and works for now,
-- yes.



-- C1 is starting composition. We want to generate C2, which is one greater in
-- size than C1. (C1 is not necessarily size 0 or 1.)
--
-- defintions
--
--   avail(C) is set of available steps in C
--
--   MC(C,N) is set of all N-step extensions of C done with semi-random step
--   selection
--
--   max( MC(C,N) ) is max score and/or max comp of the set
--
--   step_fraction is fraction of available steps that we evaluate
--
--   monte-carlo-consider means a step that is applied during monte carlo
--   evaluation. it generally is a single step chosen randomly from what's
--   available
--
--   monte-carlo-choice means a step that is chosen from among
--   monte-carlo-considers
--
--   next-consider means any step that is being monte-carlo-eval'd
--
--   next-choice means a step that is chosen after being monte-carlo-eval'd
--
--   test-X 
--
-- find every available next step for C1.
--
-- for each step S in C1, compute C_all = map applyStep S
--
-- MC_scores = map (\c -> (MC( c, MC_depth ),c)) C_all
--
-- call recursively: pick comp in MC_scores with maximum score, picking
-- randomly from among tied comps
--


-- we want to understand how different tests interact
--
-- given the score of test X on step S, among all next-considered, the
-- percentile rank of S is next-considered-rank(X,S)
--
-- there is also next-monte-carloed-rank(X,S)
--
-- there is the rank of all tests combined next-considered-rank(comb,S)
--
-- so what we to do is note, for step S, when combined rank is high, make sure
-- that rank of each individual test is high. we note how often it happens
-- that when combined rank is high and rank of individual test is low


-- definitions
--
-- a choice made
-- 



----------------------------------------------------------------------
----------------------------------------------------------------------
--


backtrack :: Opt comp step => Int -> comp -> (Double,comp)
backtrack targetSize !c
  | oSize c >= targetSize = (oScore c, c)
  | otherwise =
      L.maximumBy (compare `on` fst) $
      [backtrack targetSize (oApply step c) | step <- oList c]


{-
backtrackR :: Opt comp step => Int -> comp -> Rio comp
backtrackR tSize !c
  | oSize c >= tSize = return c
  | otherwise = do
      let steps = oList c
      steps2 <- rioShuffle steps
      let h = case steps2 of
            x:_ -> x
      backtrackR tSize (oApply h c)
-}


backtrackR :: Opt comp step => Int -> Double -> comp -> Rio (Double,comp)
backtrackR tSize scoreIn !c
  | oSize c >= tSize = return (scoreIn, c)
  | otherwise = do
      let steps = oList c
          -- doStep :: step -> (Double,comp)
          doStep s = let c2 = oApply s c in (oScore c2, c2)
          (scoreOut,cOut) = L.maximumBy (compare `on` fst) $ map doStep steps
      backtrackR tSize scoreOut cOut


goN :: Opt comp step => Int -> comp -> Rio comp
goN n !c = do
  let goOne !cc = rioChoose (oList cc) >>= return . flip oApply cc
  foldl (>>=) (return c) (replicate n goOne)


-- what is the output? a single composition? 

backtrackR2 :: Opt comp step => OptConfig -> comp -> Rio comp
backtrackR2 oc comp
  | oSize comp == ocTargetSize oc = return comp
  | otherwise = do
      let ss = oList comp
          -- doStep :: step -> (Double,comp)
          doStep !s = do
            let c = oApply s comp
            sc <- monteCarloEval oc comp
            return (sc,c)
      cs <- mapM doStep ss
      let tops = chooseTopFrac (ocFractionNext oc) cs
      mapM_ updateRecord $ map snd tops
      c <- rioChoose $ map snd tops
      backtrackR2 oc c


chooseTopFrac :: Double -> [(Double,a)] -> [(Double,a)]
chooseTopFrac frac items = case takeWhile pred ys of
    zs@(_:_) -> map fst $ concat zs
  where
    n = length items
    xs = zip (L.reverse $ L.sortBy (compare `on` fst) items) [0..]
    -- ys :: [[((Double,a),Int)]]
    ys = L.groupBy (\x y -> fst (fst x) == fst (fst y)) xs
    -- pred :: [((Double,a),Int)] -> Bool
    pred ((_,i):_) = fromIntegral i/fromIntegral n <= frac


monteCarloEval :: Opt comp step => OptConfig -> comp
monteCarloEval _ = error "monteCarloEval"

updateRecord :: Opt c s => c -> Rio ()
updateRecord _ = error "updateRecord"
