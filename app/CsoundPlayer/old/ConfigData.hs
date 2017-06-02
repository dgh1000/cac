module App.CsoundPlayer.ConfigData where

-- InstrMap
--
--   "instrument map" : first string is the name of the part in the XML
--   Second string is the Haskell function that handles that part.
type InstrMap = (String,String)

data ConfigStatement = ConfigInstrMap InstrMap
                     | ConfigCommand Command
                     | ConfigMixer Mixer
                       deriving(Show)



-- CommandTimehump <start label> <end table> <relative point of peak of hump>
--                 <size of peak>
data Command = CommandTimehump String String Rational Float
               deriving(Show)

data Mixer = Mixer Float Float  -- wet, dry
             deriving(Show)

data RunMode = RealtimeMode
             | FileMode