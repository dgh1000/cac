import Util.Math
import Util.UtilData
import MusDoc.MusDocData

t1 = intersect 2 5 4 4


t3 = toDecibels 10

t4 = [4,1,1,5,8,9] :: [Double]
t5 = toPercentile t4

t6 = Curve 0 1 [(0,0),(1,1),(2,4)]
t7 = curveLookup 1.1 t6
