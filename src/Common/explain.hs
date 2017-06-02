
type Tr a = State TrState a

class Monad m => RandomState m  where
  getGen :: m StdGen
  putGen :: StdGen -> m ()


trRandomR :: (RandomState m, Random a) => (a,a) -> m a
trRandomR (lo,hi) = do
  gen <- getGen
  let (output,gen') = randomR (lo,hi) gen
  putGen gen'
  return output


trRandomRs :: (RandomState m, Random a) => (a,a) -> m [a]
trRandomRs (lo,hi) = do
  gen <- getGen
  let (g1,g2) = split gen
  putGen g2
  return $ randomRs (lo,hi) g1


data TrState = TrState
  { tsConfigFile :: ConfigFile
  , tsRandomGen  :: StdGen
    -- lots of other stuff that is useful as state
  }
