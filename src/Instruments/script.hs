{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map as M
import Debug.Trace
import Data.Map(Map)
import Text.Printf
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Char
import Common.CommonData
import Util.Showable
import Util.Exception

type S = State Int


class Updatable a where
  update :: String -> (Value -> Exc Value) -> a -> Exc a
  insert :: String -> Value -> a -> Exc a


class Updatable2 a where
  update2 :: String -> (Value -> Value) -> a -> a
  


data Sd = Sd
  { s1 :: Value
  , s2 :: Value
  , s3 :: Value
  }

-- we need some kind of function to select the part we are updating

-- okay we need to represent cuves and state, those make sense to do through
-- Value. some of them are specifc to instruments and some to staves. Have two
-- value.


instance Updatable Sd where
  insert "s1" v sd =
    withExcept ("while modifying field 's1' of Sd, " ++) $ do
      return sd {s1 = v}
  update "s1" f sd = do
    let x = s1 sd
    y <- f x
    withExcept ("foo, " ++) $ return sd {s1 = y}


instance Updatable Value where
  update key f (VMap m) =
    withExcept (printf "while updating '%s', " key ++) $ do
      let value = M.lookup key m
      case value of
         Nothing -> throwError "no such key present"
         Just x  -> do
           y <- f x
           return $ VMap $ M.insert key y m
  update _ _ _ = throwError "tried to update a Value which is not a Map"
  insert key value (VMap m) =
    withExcept (printf "while inserting at '%s', " key ++) $
      return $ VMap $ M.insert key value m
  insert _ _ _ = throwError "tried to insert into a Value which is not a Map"

  
instance Updatable2 Value where
  update2 key f (VMap m) =
      let value = M.lookup key m 
      in case value of
           Nothing -> throwMine "no such key present"
           Just x  -> VMap $ M.insert key (f x) m
  update2 _ _ _ = throwMine "tried to update a Value which is not a Map"

  
  {-

updateMap f name v =
  withExc (printf "while updating field '%s', " name ++) 

updateMap2 :: (Value -> Value) -> String -> Value -> Exc Value
updateMap2 f name v = do
  let x = case v of
        VMap m -> case M.lookup name m of
        Just v -> VMap $ M.insert name (f v) m
       Nothing -> throwError "this field name does not exist"
-}

type R = ReaderT String S


f1 :: Char -> S Char
f1 c = do
  s <- get
  r <- runReaderT f2 "foo"
  return $ chr $ ord c + fromIntegral r

f2 :: R Int
f2 = do
  r <- ask
  return $ length r


main = do
  print $ runState (f1 'a') 5


data Sm = Sm (String,Value)


-- overlapping instances: why does it matter? why can't it select whatever
-- works in the given circumstance?


instance ShowItemClass Sm where
  showI (Sm (key,value)) = case showI value of
    SingleLine s -> SingleLine $ printf "%s: %s" key s
    Component t flag values -> Component key flag values


instance ShowItemClass Value where
  showI (VInt x) = SingleLine $ printf "VInt %d" x
  showI (VString s) = SingleLine $ printf "VString %s" s
  showI (VMap m) = Component "VMap" True $ map (showI . Sm) $ M.toAscList m
  showI (VDouble d) = SingleLine $ printf "VDouble %f" d

-- update two levels
--   update "key1" 


main1 = do
  -- create a data structure that has a couple levels
  let d1 = VMap $ M.fromList [("rats", VInt 8), ("best", VString "Nodin")]
      d2 = VMap $ M.fromList [("d1",d1),("some double",VDouble 1.1)]
  -- now insert into d2 to produce d3
  let d3 = runExc $ insert "some string" (VString "updated!") d2
  putStrLn $ "d2!\n" ++ showIString d2
  putStrLn $ "d3!\n" ++ showIString d3
  -- now update d2 to produce d4
  let d4 = runExc $ update "some double"
           (const $ return $ VString "not a double!") d2
  putStrLn $ "d4!\n" ++ showIString d4
  -- update d2 at two levels
  let d5 = runExc $ update "d1" (insert "best" (VDouble 3.14159)) d2
  putStrLn $ "d5!\n" ++ showIString d5


main1_err = do
  -- create a data structure that has a couple levels
  let d1 = VMap $ M.fromList [("rats", VInt 8), ("best", VString "Nodin")]
      d2 = VMap $ M.fromList [("d1",d1),("some double",VDouble 1.1)]
  -- try to update d2, but preduce error
  let d5 = runExc $ update "d1" (update "bestx"
                                 (const $ return $ VDouble 3.14159)) d2
  putStrLn $ "d5!\n" ++ showIString d5
     
  

main2 = do
  let d1 = VMap $ M.fromList [("rats", VInt 8), ("best", VString "Nodin")]
      d2 = VMap $ M.fromList [("d1",d1),("some double",VDouble 1.1)]
  let d4 = update2 "some double" (const $ VString "not a double!") d2
  putStrLn $ "d4!\n" ++ showIString d4
  let d5 = update2 "d1" (update2 "best" (const $ VString "Jacy")) d2
  putStrLn $ "d5!\n" ++ showIString d5


