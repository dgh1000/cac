import System.Directory
import System.FilePath
import qualified Data.List as L

main = do
  -- Configuration
  let inDir = "c:/Temp/ariaOutput/viola"
      tableFileNamePrefix = "cello/"
      csoundBaseTableNumer = 300

  cs <- getDirectoryContents inDir
  let csf = filter isCorrectFormat cs
      isCorrectFormat s = x == "pinitial" && y == ".wav"
                          where x = take 8 s
                                y = takeExtension s
  let midiPitches = map (\s -> read (take 2 $ drop 8 s)) csf  :: [Int]
  let haskellTableCode = makeHaskellTableCode tableFileNamePrefix 
                         csoundBaseTableNumer
                         (L.sort midiPitches)
      usableKeyCode = makeHaskellUsableKeyCode (L.sort midiPitches)
  writeFile "haskell-code.hs" $ haskellTableCode ++ usableKeyCode
                         

makeHaskellTableCode :: String -> Int -> [Int] -> String
makeHaskellTableCode fnPrefix baseNum midiPitches =
  "  unlines\n" ++ concatMap oneTable midiPitches
  where
    oneTable :: Int -> String
    oneTable i = "    , \"f" ++ show (i+baseNum) ++ " 0 131072 -1 \\\"" ++ fn 
                 ++ "\\\" 0 0 1\"\n"
      where
        fn = fnPrefix ++ "p" ++ show i ++ ".wav"

makeHaskellUsableKeyCode :: [Int] -> String
makeHaskellUsableKeyCode ks = "config_usuableKeysOffsets =\n" 
                              ++ concatMap mkKey ks
  where
    mkKey :: Int -> String
    mkKey k = "  , ( " ++ show k ++ ", 5000)\n"
