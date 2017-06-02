
import Text.Printf
import Sound.PortMidi

decideInputOrOutput di = if input di then "Input :" else "Output:"

showDevice :: Int -> IO ()
showDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (name di) 

main = do
  c <- countDevices
  mapM_ showDevice [0..c-1]

