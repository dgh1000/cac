----------------------------------------------------------------------

data Value = VDouble Double
           | VInt Int
           | VString String
           | VDest (Int,Int)
           | VMap (Map String Value)
           | VLocMap (Map Loc Value)
           | VCurve Curve
           | VTrillShape TrillShape


-- <meta instrs> <time var> 
data RunData = RunData [Meta] TimingVariation 


data SomeData = StaffState String [String]
              | MetaState  String [String]


(...) :: SomeData -> String -> SomeData
(...) (StaffState x ss) s = StaffState  x $ ss++[s]
(...) (MetaState  x ss) s = MetaState   x $ ss++[s]



staffData s = StaffState s []
metaData  s = MetaState  s []


