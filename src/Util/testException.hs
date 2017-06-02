
import qualified Data.Array as A
import Data.Array(Array, (!))
import Data.Char
import Util.Exception

a1 = A.array (0,4) [(x, chr (ord 'a' + x)) | x <- [0..4]]

main1 = do
  let xs = [1,2,3]
      y = listLookup "foo" xs 0
  putStrLn (show y)

main2 = do
  putStrLn . show $ arrLookup "foo" a1 (5)

main3 = do
  putStrLn . show $ a1 ! 5