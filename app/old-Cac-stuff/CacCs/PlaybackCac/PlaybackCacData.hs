module PlaybackCac.PlaybackCacData where

data PlayParams = PlayParams
  { ppLabel1 :: Maybe String
  , ppLabel2 :: Maybe String }

data InpCmd = InpQuit
            | InpPlay PlayParams
            | InpTerminateProcess
            | InpParseError String

data PlayArg = PlayArgPlay (Maybe String) (Maybe String)
             | PlayArgQuit
             | PlayArgStop
               deriving(Show)

