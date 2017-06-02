import System.FilePath
import Data.Ratio
import Util.Math
import Util.Map
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.List as L
import Util.Exception
import Util.FileUtil

--data Curve = Curve Float Float [(Float,Float)]

{-
curveLookup :: Curve -> Float -> Float
curveLookup (Curve _ _ pts) = 
    where
      surrounding ((x1,y1) : (x2,y2) : remain)
          | 
-}

t1 = [5,4,3,2,1] :: [Int]
t2 s (x1 : x2 : remain) 
       | x1+x2 < s = (x1,x2)
       | otherwise = t2 s (x2 : remain)

t2 _ (_ :[]) = error "boo"
t2 _ [] = error "foo"


t3 = map rationalFloor [1%1, 3%4, 4%3]

xMin = 1
xMax = 3.5
x = 3.6
t4 = max xMin (min x xMax)

t100 = M.fromList [(1, 'a'), (3,'b'), (5, 'c')] :: Map Int Char
t101 = combineWithPrevious t100
t102 = combineWithFollowing t100

-- Check filename for the form <prefix><numbers>.<ext>
--
-- Inputs
--   String :: filename to check
--   String :: prefix that it might match
--   String :: extension that it should have
-- Output
--   Nothing:: if file doesn't meet the form
--   Just <numeric value of numbers>
avfn_help :: String -> String -> String -> Maybe Int
avfn_help name prefix ext =
  if ext == nameExt
  then
    case checkPrefix prefix nameBase of
      Nothing -> Nothing
      Just remainder -> case reads remainder of
        [] -> Nothing
        ((i,_):_) -> Just i
  else Nothing
  where
    (nameBase,nameExt) = 
        let (b,e) = splitExtension name
        in (b, drop 1 e)
    

-- checkPrefix
-- 
-- Inputs:
--   - prefix to check
--   - list to check: does this have the prefix at its beginning?
-- Outputs
--   - Nothing if the list to check doesn't have the given prefix
--   - Just the remainer of the list following the prefix
checkPrefix :: Eq a => [a] -> [a] -> Maybe [a]
checkPrefix possiblePrefix listToCheck 
  | lenPossPre == 0 || lenListToCheck == 0 = throwMine "098v"
  | lenListToCheck <= lenPossPre = Nothing
  | otherwise =
    if L.isPrefixOf possiblePrefix listToCheck 
    then Just $ drop lenPossPre listToCheck
    else Nothing
  where
    lenPossPre = length possiblePrefix
    lenListToCheck = length listToCheck

t200 = checkPrefix "sdfd" "sdf"

t201 = avfn_help "bob1001.xxt" "bob" "txt"

t202 = splitExtension "bob.txt"
