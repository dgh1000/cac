
import Data.Map(Map)
import qualified Data.Map as M

t1 :: Map Int Float
t1 = M.fromList [(3,5),(4,-1)]
t2 :: Map Int Float
t2 = M.fromList [(3,6),(4,2)]

t3 = M.unionWith (flip (-)) t1 t2
