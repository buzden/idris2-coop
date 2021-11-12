import CommonTestingStuff

export
main : IO Unit
main = do putStrLn "before coop"
          runCoop $ do
  offset <- millis
  printTime offset "start"
  par
    (forever $ do
      printTime offset "proc 1, before 1000"
      sleepFor 1.seconds
      printTime offset "proc 1, before 2000"
      sleepFor 2.seconds)
    (forever $ do
      printTime offset "                     proc 2, before 350"
      sleepFor 350.millis
      printTime offset "                     proc 2, before 750"
      sleepFor 750.millis)
  printTime offset "end"
