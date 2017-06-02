import System.Process

midiPitchToCps :: Int -> Float
midiPitchToCps p = 440 * (2 ** (((fromIntegral p)-69)/12))
    

iStatement midiPitch = "i1 " ++ "0 5 " ++ show (midiPitchToCps midiPitch)


-- Computes the proper command line to run Csound. The arguments in
-- are (1) orc name,  (2) score filename 
-- (3) string to append to the wave filename (the
-- output wav).

cmdLine :: String -> String -> String -> String
cmdLine orc sco wavSuffix = 
    "csound -d -W -os" ++ wavSuffix ++ ".wav " ++ orc ++ " " ++ sco

-- genSco: write a score file
--   Inputs:
--      Int: midi pitch number
--      String: full score file name  
genSco :: Int -> IO ()
genSco midiPitch = do
  let s = "i1 0 5 16000 " ++ show cps ++ "\n"
  writeFile "gen.sco" s
      where cps = midiPitchToCps midiPitch

--runCsound :: Int -> IO ExitCode
runCsound midiPitch = do
  genSco midiPitch
  c <- system $ cmdLine "gen.orc" "gen.sco" (show midiPitch)
  print "done"


main = 
  mapM_ runCsound [12..108]


--main = d
--mapM_ runnC
