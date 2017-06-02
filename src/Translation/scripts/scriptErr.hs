
import Control.Monad.Identity
import Control.Monad.Except

test :: Except String Int 
test = throwError "foo"

test2 = withExcept ("Nodin "++) test3


test3 = return 3

main = print $ runExcept test3
