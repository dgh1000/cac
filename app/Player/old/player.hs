{-

so we indicate first measure and beat, and last measure and beat.
I guess. or # of measures. or just say play 'til gap.
also cache last values

to indicate which directory, communicate, environment variable maybe?
run with last config written to config file? defualt some kind of prompt? 
but that doesn't persist between sessions- set environment or windows 

fixed directory where state is written read from env? because then doesn't
matter where we run it
 
PLAYSTATE


-}

main = do
  env <- getEnvironment
  let e = lookup env "PLAYSTATE"
  when (isNothing e) (error "PLAYSTATE env var not defined")
  print e

{-

the idea is to generate clusters of related pitches and then I create a 
gesture or even create gesture via cell it would be would 

-}
