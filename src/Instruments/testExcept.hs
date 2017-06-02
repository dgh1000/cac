
import Control.Monad.State
import Control.Monad.Except

data St = St Double Int
        deriving(Show)

type Ts = State St

type Ets = ExceptT String (State St)

testM :: Ets Double
testM = do
  St x y <- get
  return x


main = do
  let x = runState (runExceptT testM) (St 2.0 3)
  print x
