
main = do
  let configDur = "/Mike/Music/algo/haskell/Csound/instr-config"
  
      testData = M.fromList 
        [("foo", Gen10ImitateSamplerLoadedConfig 0.1 
         [ (0.1, "8192 10 1")
         , (0.1, "8192 10 1 1 1 1 1 1")
         , (0.1, "8192 10 1")
         , (0.1, "8192 10 1 1 1 1 1 1")
         , (0.1, "8192 10 1")
         , (0.1, "8192 10 1 1 1 1 1 1")
         , (0.1, "8192 10 1") ])]
       