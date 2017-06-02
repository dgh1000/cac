
import Text.Parsec
import App.CsoundPlayer.ParseCmdLine

t0 = " 1 c md q"
t1 = case parse parseCmdLine "" t0 of
  Left err -> putStrLn (show err)
  Right v -> putStrLn (show v)
