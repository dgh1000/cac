
sd1 = [ ("Piano-staff1",(0,1))
      , ("Piano-staff2",(0,2))
      ]

n = Meta
  { name   = "piano"
  , init   = initPiano $ map snd sd1
  , staves = map fst sd1
  , dests  = staffDests sd1
  , timing = defaultTiming
  , alterT = extend 0.1 >>= gap 0.002 >> minDur 0.08
  , vel    = 
  }


-- this looks up a config value, what is the simplest way of expressing this?
-- we configure

regExtend :: NoteKey -> Mi ()
regExtend nk = do
  

initPiano :: [(Int,Int)] -> Tr (Map String Value,[MidiEvent])
initPiano chans = do
  let vs = M.fromList [ ("stac", VDouble 0.07)
                      , ("arp" , VDouble 0.05)
                      ]
      evts = map (\(s,c) -> MidiSingle 0 s c 0xb0 7 126) chans
  return (vs,evts)
