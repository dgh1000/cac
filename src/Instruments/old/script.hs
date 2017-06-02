
import Control.Monad.State
import Control.Monad.Writer

type W a = Writer [Int] a

data Foo = Foo Int String

type S a = StateT Foo (Writer [Int]) a

a1 :: W Int
a1 = do
  tell [1]
  tell [2]
  return 1

