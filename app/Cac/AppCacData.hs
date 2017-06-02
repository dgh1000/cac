
module App.Cac.AppCacData where



-- String is name of test phrase
data Cmd = CmdPlay String PrevComp 
               -- <test phrase to use>, <what to play before>
         | CmdReport
         | CmdSelect String -- data file to use (not includ .dat extension)
         | CmdQuit
         deriving(Show)

data PrevComp = PrevPhrases Int
              | PrevTime Float
              | PrevWhole
                deriving(Show)

