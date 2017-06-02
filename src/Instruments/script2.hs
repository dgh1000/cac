

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Instruments.InstrumentsData

data Key = Key String


data Test = Test
  { tT1 :: Salue
  , tT2 :: Salue
  }
  deriving(Show)


type J = State Test

data SomeData = T1 [String]
              | T2 [String]

t1 = T1 []
t2 = T2 []


(...) :: SomeData -> String -> SomeData
(...) (T1 ss) s = T1 $ ss++[s]
(...) (T2 ss) s = T2 $ ss++[s]

(===) :: SomeData -> Salue -> J ()
(===) (T1 keys) value = do
  s <- gets tT1
  let f :: [String] -> (Salue -> Salue)
      f [k] = insert k value
      f (k:ks) = adjust k $ f ks
      s2 = f keys $ s
  modify (\x -> x {tT1 = s2})


foo :: J ()
foo = do
  t1 ... "Bob" === SMap M.empty
  t1 ... "Bob" ... "Nodin" === SDouble 3.0

main = do
  let st = runState foo (Test (SMap M.empty) (SMap M.empty))
  print st
  
