import Data.Traversable
import Control.Monad
type Foo = ((,) Int)

x :: Foo String
x = (3, "george")

x2 :: (String -> [String]) -> Foo String -> [Foo String]
x2 = traverse 

x3 = traverse (\s -> [s, reverse s]) x

a2 :: (Int -> (Char,Int)) -> Maybe Int -> (Maybe (Char,Int))
a2 = fmap