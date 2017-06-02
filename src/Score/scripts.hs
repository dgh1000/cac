
import qualified Data.List as L

data Foo = FooInt  Int
         | FooBool Bool
           deriving(Eq, Show)
                   



instance Ord Foo where
  compare (FooInt  _) (FooInt  _) = EQ
  compare (FooBool _) (FooBool _) = EQ
  compare (FooInt  _) (FooBool _) = LT
  compare (FooBool _) (FooInt  _) = GT

x1 :: [(Int,Foo)]
x1 = [ ( 2, FooBool False)
     , ( 2, FooBool True )
     , ( 2, FooInt  4    )
     , ( 1, FooBool True )
     , ( 1, FooInt  3    )
     ]

x2 = L.sort x1

x3 = [1,2,3,4,5]
x4 = L.tails x3

x5 = zip x3 (drop 1 x4)