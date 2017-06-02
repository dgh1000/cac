
import Score.ScoreData
import Language.Haskell.Interpreter
import qualified Data.Map as M
import Data.Array
import Instruments.Run
import Instruments.InstrumentsData
-- import Data.Typeable

{-

l :: M
l = do
  [loadModules "/Users/Mike/haskell/Hint/interp.hs"]

-}

r :: InterpreterT IO Bool
r = do
  setImports ["Prelude"]
  interpret "head [True,False]" (as ::Bool)

r2 :: InterpreterT IO RunData
r2 = do
  set [searchPath := ["/Users/Mike/haskell"]]
  loadModules [ "Score.ScoreData"
              , "Instruments.InstrumentsData"
              , "Util.Math"
              , "Common.CommonData"
              , "Instruments.ToMidi"
              , "Translation.TranslationData"
              ]
  setImportsQ [ ("Prelude", Nothing)
              , ("Data.Map", Just "M")
              , ("Data.Set", Just "S")
              , ("Control.Monad", Nothing)
              , ("Control.Monad.Trans", Nothing)
              , ("Score.ScoreData",Nothing)
              , ("Instruments.InstrumentsData",Nothing)
              , ("Util.Math", Nothing)
              , ("Common.CommonData", Nothing)
              , ("Instruments.ToMidi", Nothing)
              , ("Translation.TranslationData", Nothing)
                
              ]
  buf <- liftIO $ readFile "/Users/Mike/haskell/Instruments/M.hs"
  interpret buf (as :: RunData)


showErr (WontCompile errs) = concatMap (\e -> errMsg e ++ "\n") errs
showErr (NotAllowed s) = "NotAllowed: " ++ s
showErr (UnknownError s) = "UnknownError " ++ s
showErr (GhcException s) = "GhcException " ++ s

main = do
  x <- runInterpreter r2
  case x of
    Left err -> putStrLn $ showErr err
    Right r -> run r

