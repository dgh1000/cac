module Util.AnalyzeSamples.ConfigSpecs where

import Util.ParseStuff

dss = DataSpecSheet $ M.fromList

 [ ("simple_a", 

   [ ("soundFileDir", SomeDataTypeString)
   , ("soundFileName", SomeDataTypeString)
   , ("soundFileFundamental", SomeDataTypeFloat)
   , ("pvsDataDir", SomeDataTypeString)
   , ("samplingRate", SomeDataTypeFloat)
   , ("skip", SomeDataTypeInt)
   , ("dur",  SomeDataTypeFloat)
   , ("channel", SomeDataTypeString)
 ]
