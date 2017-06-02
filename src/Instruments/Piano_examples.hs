module Instruments.Piano_examples where

import qualified Data.Map as M
import Instruments.Piano
import Instruments.InstrumentsData


typical_1 staffN1 staffN2 maxSingleVel maxTrillVel =
  let staffData = M.fromList [ (staffN1, (0,1))
                             , (staffN2, (0,2)) ]

      vCurveSingle = VelCurve [ (0.45, 10)
                              , (8.55, maxSingleVel) ]


      vCurveTrill  = VelCurve [ (0.45, 10)
                              , (8.55, maxTrillVel)
                              ]

      piano1 = makePiano "piano1" staffData vCurveSingle
               vCurveTrill
  in piano1


{-

typical_2 =
  let staffData = M.fromList [ ("Solo", (0,1))
                             , ("Synth Brass", (0,2)) ]

      vCurveSingle = VelCurve [ (0.4, 10)
                              ,  (8.6, 100) ]


      vCurveTrill  = VelCurve [ (0.4, 10)
                              , (8.6, 90)
                              ]

      piano1 = let ext    = 0.1
                   gap    = 0.002
                   minDur = 0.01
               in makePiano "piano1" (ext,gap,minDur) staffData vCurveSingle
                  vCurveTrill
  in piano1

typical_3 =
  let staffData = M.fromList [ ("Keyboard", (0,1))
                             , ("Organ", (0,2)) ]

      vCurveSingle = VelCurve [ (0.4, 10)
                              ,  (8.6, 100) ]


      vCurveTrill  = VelCurve [ (0.4, 10)
                              , (8.6, 90)
                              ]

      piano1 = let ext    = 0.1
                   gap    = 0.002
                   minDur = 0.01
               in makePiano "piano1" (ext,gap,minDur) staffData vCurveSingle
                  vCurveTrill
  in piano1
-}

