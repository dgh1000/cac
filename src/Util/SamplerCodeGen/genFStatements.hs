
-- Compute an f-statement
--  The table number will be same as midi key.
--  The file containing the samples will have a name something
--    like "piano1-60.wav". The portion "piano1" is the root, and
--    -60.wav is the suffix indicating this is the sample for midi key 60
--
--  Int: f-table number (i.e. midi key)
--  String: root of the sample file name
computeFStatement :: Int -> String -> String
computeFStatement n filenameBase 
    = "f" ++ show n ++ " 0 0 1 '" ++ filenameBase ++ "-" ++ show n ++ 
      ".wav' 0 0 0\n"

test = computeFStatement 60 "piano1"


