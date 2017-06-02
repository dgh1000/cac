{-# LANGUAGE FlexibleInstances #-}
import Control.Applicative
import Control.Monad.Identity
--import Control.Monad.Error
--import Control.Monad.Trans.Error
import Text.Parsec.String
import Text.Parsec


{-

just call play with single vertical advance through composition, that simple
but maybe if generate too many same could be boring oh well deal with it


-}

test1 = "boox"
test2 = case test1 of
  "boo" -> 1
  "queue" -> 2
  _       -> 3
  
  
test3 :: Read a => String -> a
test3 = read

nesting :: Parser Int
nesting = do char '('
             n <- nesting
             char ')'
             m <- nesting
             return (max (n+1) m)
          Text.Parsec.<|> return 0

test4 = parseTest nesting "()"


test5 :: Integral a => a -> a
test5 x = 10 `div` x


type MyError a = Either String a


instance Applicative (Either String) where
  pure x = Right x
  Right g <*> Right x = Right (g x)
  Right _ <*> Left s = Left s
  Left s <*> _ = Left s
  

safeRead :: Read a => String -> MyError a
safeRead s = case reads s of
   []        -> Left $ "Error trying to parse '" ++ s ++ "'"
   (x,_):_ -> Right x

test10 = safeRead "s10" :: MyError Int
-- rtest10 = runIdentity $ runErrorT test10

data Something = Something Int [Int] 
               deriving(Show)

test11 = Something <$> safeRead "3" <*> safeRead "[1]"

