import CommonTestingStuff

delays : List FinDuration
delays = [250.millis, 600.millis, 0.seconds, 1.seconds, 4.seconds]

p : MonadWriter (SnocList String) m => CanSleep m => FinDuration -> m ()
p d = do
  sleepFor d
  tellTimed "after waiting for \{show d}"

programS : MonadWriter (SnocList String) m => CanSleep m => Coop m ()
programS = do
  tellTimed "===== usual applicative"
  for_ delays p
  tellTimed "==== end"

programC : MonadWriter (SnocList String) m => CanSleep m => Coop m ()
programC = do
  tellTimed "==== parallel applicative"
  for_ delays @{Concurrent} p
  tellTimed "==== end"

main : HasIO io => io ()
main = do
  putStrLn "before coop, delays: \{show delays}"
  traverse_ putStrLn $ execW programS
  traverse_ putStrLn $ execW programC
