

import Control.Monad.State



type M = StateT String IO

type N = StateT Integer M

x :: N ()
x = do
  i <- get
  liftIO $ print i
  j <- lift get
  liftIO $ print j
  

main = runStateT (runStateT x 1111) "bobble" >> return ()

