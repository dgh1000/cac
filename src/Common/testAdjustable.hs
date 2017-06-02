

import qualified Data.Map as M
import Common.CommonData
import Util.Showable


main1 = do
  -- create a data structure that has a couple levels
  let d1 = VMap $ M.fromList [("rats", VInt 8), ("best", VString "Nodin")]
      d2 = VMap $ M.fromList [("d1",d1),("some double",VDouble 1.1)]
  -- now insert into d2 to produce d3
  let d3 = insert "some string" (VString "adjustd!") d2
  putStrLn $ "d2!\n" ++ showIString d2
  putStrLn $ "d3!\n" ++ showIString d3
  -- now adjust d2 to produce d4
  let d4 = adjust "some double" (const $ VString "not a double!") d2
  putStrLn $ "d4!\n" ++ showIString d4
  -- adjust d2 at two levels
  let d5 = adjust "d1" (insert "best" (VDouble 3.14159)) d2
  putStrLn $ "d5!\n" ++ showIString d5


mainErr :: IO ()
mainErr = do
  let d1 = VMap $ M.fromList [("rats", VInt 8), ("best", VString "Nodin")]
      d2 = VMap $ M.fromList [("d1",d1),("some double",VDouble 1.1)]
      
  -- insert into something not a map
  -- let x = insert "foo" (VInt 3) (VDouble 1)

  -- adjust something not a map
  -- let x = adjust "foo" id (VDouble 2)

  -- adjust a key that is not present
  let x = adjust "foo" (const $ VInt 314) d2
  
  putStrLn $ showIString x
