import Text.Parsec
import App.CsoundPlayer.ParseConfig( parseFile )

main :: IO ()
main = do
  buf <- readFile "example.cfg"
  case parse parseFile "" buf of
    Left err -> putStrLn $ show err
    Right cs -> putStrLn (show cs)
