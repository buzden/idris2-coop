import CommonTestingStuff

delays : List FinDuration
delays = [250.millis, 600.millis, 0.seconds, 1.seconds, 4.seconds]

p : CanSleep io => HasIO io => Integer -> FinDuration -> io Unit
p offset d = do
  sleepFor d
  printTime offset "after waiting for \{show d}"

export
main : IO Unit
main = do putStrLn "before coop, delays: \{show delays}"
          runCoop $ do
  offset <- millis
  printTime offset "===== usual applicative"
  for_ delays $ p offset
  printTime offset "==== end"

  offset <- millis
  printTime offset "==== parallel applicative"
  for_ delays @{Concurrent} $ p offset
  printTime offset "==== end"
