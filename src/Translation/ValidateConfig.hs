module Translation.ValidateConfig where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map(Map)

import Text.Printf
import Control.Monad.Except
import Data.Maybe
import Translation.TranslationData
import Common.CommonData


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


    
