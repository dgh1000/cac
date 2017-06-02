import Control.Monad
import Debug.Trace
import System.Directory
import System.FilePath
import System

main = do
  -- Configuration
  let dirName = "c:/Temp/ariaOutput/viola"
      trimBeginTime = 3.0
      trimEndTime = 4.0
  cs <- getDirectoryContents dirName
  -- filter by files that fit format
  let csf = filter isCorrectFormat cs
      isCorrectFormat s = x == "pinitial" && y == ".wav"
                          where x = take 8 s
                                y = takeExtension s
  -- for every file, generate csound code and run csound
  forM_ csf (runCsound trimBeginTime trimEndTime dirName)
  -- runCsound trimBeginTime trimEndTime dirName (head csf)



runCsound :: Float -> Float -> String -> String -> IO ()
runCsound trimBeginTime trimEndTime dirName inName = do
  -- compute output filename
  let outName = "p" ++ (take 2 . drop 8 $ inName) ++ ".wav"
  -- read main part of csd we are making
  mainCsd <- readFile "main.csd"
  -- make f statement
  let fStatement = "f1 0 131072 -1 \"" ++ dirName ++ "/" ++ inName
                   ++ "\" " ++ show trimBeginTime ++ " 0 1\n"
  -- make i statement
  let iStatement = "i1 0 " ++ show (trimEndTime-trimBeginTime) ++ " 1\n"
  -- make options
  let options = "-d -W -o" ++ outName ++ "\n"
  -- final csd
  let runCsd = csdHeader ++ options ++ mainCsd ++ fStatement ++ iStatement 
            ++ csdFooter
  writeFile "run.csd" runCsd  
  system "csound run.csd"
  return ()
  


csdHeader = "<CsoundSynthesizer>\n<CsOptions>\n"
csdFooter = "\n</CsScore>\n</CsoundSynthesizer>\n"