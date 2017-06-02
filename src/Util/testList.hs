import Util.List

import Text.Printf
import qualified Data.List as L
import Common.CommonData
import Common.CommonUtil
import Util.Exception

xs = [1,1,44,55] :: [Int]
ys = [1,2,4] :: [Int]

t1 = interleaveAscendingBy compare xs ys

t20 = [4,5,6,7,8] :: [Int]
t21 = [4,5,6,6] :: [Int]
t22 = commonPrefix [1] t20

t30 = shortestDistinctPrefixes t20 t21

t40 = takeWhileWithIdx (\x n -> x /= 15 && n < 10) [10..]

test1 = updateListElem 2 'a' "efg"

test2 = alterListElem 4 (+1) [1,2,3,4]

data Dyn = Dyn Loc Double
         deriving(Show)

data T = T Loc Double
       deriving(Show)

instance LocKey Dyn where
  locKey (Dyn l _) = l

instance LocKey T where
  locKey (T l _) = l


l1 = [ Dyn (Loc 2 1) 1
     , Dyn (Loc 4 1) 1
     , Dyn (Loc 6 1) 1
     , Dyn (Loc 8 1) 1
     ]

l2 = [ T (Loc 2  1) 1
     , T (Loc 3  1) 1
     , T (Loc 4  1) 1
     , T (Loc 2  1) 1
     , T (Loc 5  1) 1
     , T (Loc 10 3) 1
     , T (Loc 12 3) 1
     ]

{-
showStepResult :: (Show a) => StepResult a -> String
showStepResult SRTooLate = "SRTooLate"
showStepResult SRNoneLeft = "SRNoneLeft"
showStepResult (SRApplies xs x) = printf "apply:%s\n%s" (show x) s
  where
    s = unlines $ map (\x -> "   " ++ show x) xs
-}

main2 = do
  let res = pairTags "foo" l1 l2
      s :: (Dyn,T) -> String
      s (Dyn l1 _,T l2 _) = printf "tag:%s item:%s\n" (simpleShowLoc l1)
                            (simpleShowLoc l2)
  putStrLn $ concatMap s res

  
  