
import Control.Monad
import System.Random
import Util.RandMonad
import Util.Er


test1 :: Er Int
test1 = do
  v <- rRandom
  return v


test2 = replicateM 20 test1

main = do
  case runEr test2 3001 of
    (Left msg,_) -> putStrLn msg
    (Right list,_) -> putStrLn (show list)

----------------------------------------------------------------------

{-
test3 :: Er ([Int], [Int])
test3 = do
  rs1 <- erRandoms
  rs2 <- erRandoms
  return (take 10 rs1, take 10 rs2)



main3 = do
  case doEr test3 3001 of
    Left msg -> putStrLn msg
    Right (list1, list2) -> putStrLn (show list1 ++ "\n" ++ show list2)

-}


