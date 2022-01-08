import CommonTestingStuff

sp : HasIO m => CanSleep m => CanSpawn m => (timeOffset : Integer) -> m Nat
sp offset = do
  spawn $ do
    for 5 $ do
      printTime offset "spawned, before 1000"
      sleepFor 1.seconds
      printTime offset "spawned, before 2000"
      sleepFor 2.seconds
    printTime offset "spawned, end"
  pure 127

export
main : IO Unit
main = do putStrLn "before coop"
          runCoop $ do
  offset <- millis
  printTime offset "start"
  x <- sp offset
  printTime offset "                      trunk, returned when spawning: \{show x}"
  for 10 $ do
    printTime offset "                      trunk, before 350"
    sleepFor 350.millis
    printTime offset "                      trunk, before 750"
    sleepFor 750.millis
  printTime offset "                      trunk, end"
