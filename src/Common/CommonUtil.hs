module Common.CommonUtil where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Common.CommonData as CD
import qualified Util.Math as UM
import Control.Monad
import Control.Monad.Except
import Debug.Trace
import Text.Printf
import Data.Ratio
import Data.Maybe
import Common.CommonData
import Data.Map( Map )
import Util.Exception

showPitch :: Pitch -> String
showPitch Pitch { CD.midiPitch = mp
                , CD.step = st
                , CD.pAlter = alt
                , CD.octave = oct } =
  printf "midiPitch:%2d step:%2d alter:%2d octave:%2d" mp st alt oct

-- stepAlterOctToMidi
--   Given step (as Int, 0 to 6), alter (as Int, -2 to 2), and octave
--   (as Int, where 4 is middle C)
stepAlterOctToMidi :: Int -> Int -> Int -> Int
stepAlterOctToMidi step alter octave = 12*(octave+1) + alter + s
  where
    s = case lookup step [(0,0),(1,2),(2,4),(3,5),(4,7),(5,9),(6,11)] of
      Just x -> x

simpleShowLoc :: Loc -> String
simpleShowLoc (Loc msrNum beat) = 
   printf "[m:%3d,%8s]" msrNum (show beat) 

showLoc2 :: Loc -> String
showLoc2 (Loc msr beat) = printf "[m:%3d, %2d+%7s]" msr beatInt 
  (show beatRemain)
  where
    beatInt :: Int
    beatInt = floor beat
    beatRemain = beat - fromIntegral beatInt


showRational :: Rational -> String
showRational r = printf "(%3d+%10s)" rInt (show rRemain)
  where
    rInt :: Int
    rInt = floor r
    rRemain = r-fromIntegral rInt

-- locAdd
--
-- Add delta beats to Loc and normalize. Will raise an exception if the
-- normalized Loc would be past the end of the TimeSig Map.
locAdd :: Map Int TimeSig -> Loc -> Rational -> Maybe Loc
locAdd ts (Loc msr beat) delta = locNormalize ts (Loc msr (beat+delta))


-- locSub
--
-- Subtract delta beats from the Loc. Return Nothing if this would end up
-- before msr 1, beat 1. Otherwise return Just <newLoc>.
locSub :: Map Int TimeSig -> Loc -> Rational -> Maybe Loc
locSub ts (Loc msr beat) delta = locNormalize ts (Loc msr (beat-delta))


-- okay so if the number of quarters into a measure is negative, then we add
-- number of quarters of previous measure and 

-- locAddQuar
--
-- Add a positive/negative number of quarters to the Loc. Return Nothing if
-- this would end up before msr 1, beat 1 or after the last measure in the
-- piece. Otherwise return Just <new loc>.
--
locAddQuar :: Map Int TimeSig -> Loc -> Rational -> Maybe Loc
locAddQuar ts (Loc msr1 beat1) delta = normalize msr1 (beat1Quars+delta)
  where
    TimeSig numer1 denom1 = case M.lookup msr1 ts of {Just x->x}
    quarsPerBeat = 4/fromIntegral denom1 :: Rational
    beat1Quars   = (beat1-1)*quarsPerBeat

    -- !!! note: if the following fails because of the case hitting Nothing:
    --
    -- we want to rewrite locAddQuar so it returns Nothing in this case
    quarsInMsr n = case M.lookup n ts of
      Just (TimeSig numer denom) -> fromIntegral numer*4/fromIntegral denom

    normalize :: Int -> Rational -> Maybe Loc
    normalize msr dQuars0 = case M.lookup msr ts of
      Nothing -> Nothing
      Just (TimeSig numer denom)
        | dQuars0 < 0         -> normalize (msr-1) (dQuars0+quarsInMsr (msr-1))
        | dQuars0 >= msrQuars -> normalize (msr+1) (dQuars0-msrQuars)
        | otherwise           -> Just $ Loc msr (1+dQuars0/quarsPerBeat)
        where
          quarsPerBeat :: Rational
          quarsPerBeat = 4/fromIntegral denom :: Rational
          msrQuars     = fromIntegral numer*quarsPerBeat

{-
    doPrevMsr  :: Int -> Rational -> Maybe Loc
    doPrevMsr msr quarsFromRight = case M.lookup msr ts of
      Nothing -> Nothing
      Just (TimeSig numer denom)
        | newQuars0 < 0 -> doPrevMsr (msr-1) (-newQuars0)
        | otherwise     -> Just $ Loc msr (newBeat0+1)
        where
          quarPerBeat = fromIntegral denom/4 :: Rational
          quarsInMsr = fromIntegral numer*quarPerBeat
          newQuars0 = quarsInMsr-quarsFromRight
          newBeat0 = newQuars0*/quarPerBeat

    doNextMsr :: Int -> Rational -> Maybe Loc
    doNextMsr msr quars0 = case M.lookup msr ts of
      Nothing -> Nothing
      Just (TimeSig numer denom)
        | quars0 >= msrNumQuars -> doNextMsr (msr+1) (quars0-msrNumQuars)
        | otherwise             -> Just $ Loc msr (quars0/quarPerBeat+1)
        where
          quarPerBeat = fromIntegral denom/4 :: Rational
          msrNumQuars = numer*quarPerBeat
-}    


-- locNormalize
--
-- Given a Loc with a beat that may not be in the range of the numerator
-- of the measure, adjust measure and beat until they are appropriate. 
--
-- Output Nothing if a valid measure and beat cannot be found.
--
-- Normalizing to "one past last measure": 
--   If this is passed a Loc of (Loc m b) where m is the number of measures
--   in Map Int TimeSig and b is one past the numerator of measure m, then
--   this routine will output Just (Loc (m+1) 1)
locNormalize :: Map Int TimeSig -> Loc -> Maybe Loc
locNormalize ts (Loc msr beat)
  | beat == 1 = if 1 <= msr && msr <= length ts + 1 
                then Just $ Loc msr beat
                else Nothing
  | beat < 1  = case (fromIntegral . tsNumer) `fmap` M.lookup (msr-1) ts of
                  Nothing -> Nothing
                  Just numer -> locNormalize ts (Loc (msr-1) (numer+beat))
  | otherwise = case (fromIntegral . tsNumer) `fmap` M.lookup msr ts of
      Nothing -> Nothing
      Just numer 
        | beat >= numer+1 -> locNormalize ts (Loc (msr+1) (beat-numer))
        | otherwise       -> Just $ Loc msr beat
    

-- locDiff
--
-- Find the number of beats between loc1 and loc2. They can be in different
-- measures. loc2 must be greater than loc1.
locDiff :: Map Int TimeSig -> Loc -> Loc -> Rational
locDiff msrInfos l1@(Loc m1 b1) l2@(Loc m2 b2)
  | l1 <= l2 = if m1 == m2 
      then b2 - b1
      else 
        let beatsLeftInMsr = (fromIntegral (n+1) - b1) 
            nextMsrStart = Loc (m1+1) 1
            n = tsNumer . fromJust . M.lookup m1 $ msrInfos
        in  beatsLeftInMsr + locDiff msrInfos nextMsrStart (Loc m2 b2)


-- locDiffQuar
--
-- Find number of quarters between loc1 and loc2, which can be in different
-- measures. loc2 must be greater than loc1.
locDiffQuar :: Map Int TimeSig -> Loc -> Loc -> Rational
locDiffQuar timeSigs loc1@(Loc m1 b1) loc2@(Loc m2 b2) | loc1 <= loc2 =
  if m1 == m2
    then (b2-b1)*(4/denomM1)
    else let qsToEndM1 = (fromIntegral (numerM1+1) - b1)*(4/denomM1)
         in  qsToEndM1 + locDiffQuar timeSigs (Loc (m1+1) 1) loc2
  where
    (numerM1,denomM1) = case M.lookup m1 timeSigs of
      Just (TimeSig x y) -> (fromIntegral x,fromIntegral y)
         
    


-- subtractTinyPartOfBeat
--
-- A hack. Hard-coded to subtract 1/8 of a beat, or if that brings
-- you to the previous measure, then the output is a location 1/8 of
-- a beat away from the end of that measure. Arbitrary somewhat.
-- Throw error if too close to the beginning.
subtractTinyPartOfBeat :: Map Int TimeSig -> Loc -> Loc
subtractTinyPartOfBeat mis (Loc msr1 beat1) =
  if newBeat < 1
    then 
      if msr1 == 1
      then throwMine "in subtractTinyPartOfBeat, too close to beginning"
      else
        let n = fromIntegral . CD.tsNumer. fromJust . M.lookup (msr1-1) $ mis
            beatNearEnd = n - 1%8 
        in  Loc (msr1-1) beatNearEnd
    else (Loc msr1 newBeat)
  where
    newBeat = beat1 - 1%8
  
 
-- subtractTinyPartOfBeat100
--
-- A hack. Hard-coded to subtract 1/100 of a beat, or if that brings
-- you to the previous measure, then the output is a location 1/8 of
-- a beat away from the end of that measure. Arbitrary somewhat.
-- Throw error if too close to the beginning.
subtractTinyPartOfBeat100 :: Map Int TimeSig -> Loc -> Loc
subtractTinyPartOfBeat100 mis (Loc msr1 beat1) =
  if newBeat < 1
    then 
      if msr1 == 1
      then throwMine "in subtractTinyPartOfBeat, too close to beginning"
      else
        let n = fromIntegral . CD.tsNumer. fromJust . M.lookup (msr1-1) $ mis
            beatNearEnd = n - 1%100
        in  Loc (msr1-1) beatNearEnd
    else (Loc msr1 newBeat)
  where
    newBeat = beat1 - 1%100
  

configLookupInt :: String -> String -> Map String CValue -> Int
configLookupInt message paramName values = 
  case M.lookup paramName values of
    Just (CVInt x) -> x
    _ -> 
      throwMine $ printf ("%s: " ++
                  "expected to find an int parameter named '%s':\n%s\n")
                  message paramName (show values)

configLookupDouble :: String -> String -> Map String CValue -> Double
configLookupDouble message paramName values = 
  case M.lookup paramName values of
    Just (CVDouble x) -> x
    _ -> 
      throwMine $ printf ("%s: " ++
                  "expected to find a double parameter named '%s':\n%s\n")
                  message paramName (show values)


configAnyOtherValues :: Map String CValue -> [String] -> Bool
configAnyOtherValues map allowed = any (\k -> not (k `elem` allowed)) 
                                   (M.keys map)



maybeParam :: ElemValueClass a => String -> Elem -> Exc (Maybe a)
maybeParam name p@(Param _ _ n cv) | n == name = case toValue p of
                                       Nothing -> throwError msg1
                                       Just x  -> return $ Just x
                                   | otherwise = return Nothing
  where
    msg1 = printf "param '%s' is wrong type" name
maybeParam _ _ = return Nothing


maybeBracketed :: String -> Elem -> Maybe [Elem]
maybeBracketed name (Bracketed _ _ n es) | name == n = Just es
                                         | otherwise = Nothing
maybeBracketed _ _ = Nothing


maybeSingle :: ElemValueClass a => Elem -> Exc (Maybe a)
maybeSingle s@(Single _ _ x) = case toValue s of
  Just v  -> return $ Just v
  Nothing -> throwError $ printf "wrong type of single"
maybeSingle _              = return Nothing


catEnforceMaybe :: [Maybe a] -> Maybe [a]
catEnforceMaybe = mconcat . map f1
  where
    f1 (Just x) = Just [x]
    f1 Nothing = Nothing
    

maybeBrOfSingles :: ElemValueClass a => String -> Elem -> Exc (Maybe [a])
maybeBrOfSingles name elem =
  case maybeBracketed name elem of
    Nothing -> return Nothing
    Just es -> withExcept (printf "while searching brckted '%s', " name ++)$ do
      ms <- catEnforceMaybe `liftM` mapM maybeSingle es
      case ms of
        Nothing -> throwError "some elem was not a single of the right type"
        Just xs -> return $ Just xs


findBrOfSingles :: ElemValueClass a => String -> Elem -> Exc [a]
findBrOfSingles name (Bracketed _ _ t elems) =
  withExcept (printf "while searching for bracketed list '%s', " name ++) $ do
    ms <- catMaybes `liftM` mapM (maybeBrOfSingles name) elems
    case ms of
      [] -> throwError "no bracketed elem of that name"
      [xs] -> return xs
      _    -> throwError "multiple bracketed elems of that name"


maybeBrNameOf _ (Single _ _ _) = Nothing
maybeBrNameOf _ (Param _ _ _ _) = Nothing
maybeBrNameOf name b@(Bracketed _ _ n _) | n == name = Just b
                                         | otherwise = Nothing
                                           

findBr :: String -> Elem -> Exc [Elem]
findBr name (Bracketed _ _ t elems) =
  withExcept (printf "while searching for bracketed list '%s', " name ++)
    (case mapMaybe (maybeBrNameOf name) elems of
       []  -> throwError "no bracketed elem of that name"
       [Bracketed _ _ _ ys] -> return ys
       _   -> throwError "multiple bracketed elems of that name")


findBrOfSingles_sizeN :: ElemValueClass a => Int -> String -> Elem ->
                                             Exc [a]
findBrOfSingles_sizeN n name b =
  withExcept (printf "while searching for bracketed list '%s' of size %d, "
                     name n ++) $ do
    xs <- findBrOfSingles name b
    case length xs of
      x | x == n    -> return xs
        | otherwise -> throwError $ printf "size is %d" x

  


find1 :: ElemValueClass a => String -> (String -> Elem -> Exc (Maybe a)) ->
         Elem -> Exc a
find1 name f (Bracketed _ _ t elems) =
  withExcept (printf "while finding elem '%s', " name ++) $ do
    l <- catMaybes `liftM` mapM (f name) elems
    case l of
      []  -> throwError $ printf "no elem named '%s'" name
      [x] -> return x
      _   -> throwError $ printf "multiple elems named '%s'" name


findParam1 :: ElemValueClass a => String -> Elem -> Exc a
findParam1 name = find1 name maybeParam


findParamMaybe :: ElemValueClass a => String -> Elem -> Exc (Maybe a)
findParamMaybe name (Bracketed _ _ _ elems) =
  (listToMaybe . catMaybes) `liftM` mapM (maybeParam name) elems


{-

findParam1L :: ElemValueClass a => String -> [Elem] -> Exc a
findParam1L name elems = case mapMaybe maybeParam1 elems of
    []  -> throwError $ printf "param '%s' missing" name
    [x] -> case toValue x of
              Nothing -> throwError $ printf "param '%s' of wrong type" name
              Just y  -> return y
    _   -> throwError $ printf "more than one param '%s'" name
  where
    maybeParam1 p@(Param _ _ n v) | n == name = Just p
                                  | otherwise = Nothing
    maybeParam1 _ = Nothing

-}

{-

maybeParam2 :: Elem -> Maybe Elem
maybeParam2 p@Param{} = Just p
maybeParam2 _         = Nothing


findBrParams :: Elem -> [Elem]
findBrParams (Bracketed _ _ _ es) = mapMaybe maybeParam2 es

-}

convertBracketedSizeN :: ElemValueClass a => Int -> Elem -> Exc (String,[a])
convertBracketedSizeN n (Bracketed _ _ name xs)
  | length xs == n = case catEnforceMaybe $ map toValue xs of
      Nothing -> throwError $
                 printf "not all elements in '%s' are the expected type" name
      Just ys -> return (name,ys)
  | otherwise = throwError $
                printf "bracketed list '%s' should have %d elems" name n


mapVfyKeys :: Ord a => [a] -> Map a b -> Exc (Map a b)
mapVfyKeys staffNames m
  | S.fromList staffNames == M.keysSet m = return m
  | otherwise = throwError "some extra or some missing"


