import CommonTestingStuff

import Data.SnocList

sp : MonadWriter (SnocList String) m => CanSleep m => CanSpawn m => m Nat
sp = do
  spawn $ do
    for 5 $ do
      tellTimed "spawned, before 1000"
      sleepFor 1.seconds
      tellTimed "spawned, before 2000"
      sleepFor 2.seconds
    tellTimed "spawned, end"
  pure 127

program : MonadWriter (SnocList String) m => CanSleep m => CanSpawn m => m ()
program = do
  tellTimed "start"
  x <- sp
  tellTimed "                      trunk, returned when spawning: \{show x}"
  for 10 $ do
    tellTimed "                      trunk, before 350"
    sleepFor 350.millis
    tellTimed "                      trunk, before 750"
    sleepFor 750.millis
  tellTimed "                      trunk, end"

main : HasIO io => io ()
main = do
  putStrLn "before coop"
  traverse_ putStrLn $ execW program
