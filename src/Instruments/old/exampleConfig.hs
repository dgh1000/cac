

data Meta = Meta
  { instr       :: String
  , name        :: String
  , staves      :: [String]
  , dests       :: [Dest]
  , spec        :: NoteSpec
  , curves = dynHp
  , init        :: Tr ([MidiEvent],MiState)
  , state       :: ScoreObject -> MiState -> MiState
  , continuous  :: Curves -> Tr [MidiEvent]
  }


-- would it be possible to write a state monad within a state monad? so we've
-- got something that updates 

dynHp :: Mi ()
dynHp = do
  let g staffName = hpDynCurveTr staffName >>= updateCurvesMi
  stavesMi >>= mapM_ g

  
-- within a state monad, get a staff, get score, could use monad transformer
-- to add additional state I guess, the meta
