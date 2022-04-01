module Program

import CommonTestingStuff

export
beforeString : String
beforeString = "before coop"

export
program : CanSpawn m => CanSleep m => PrintString m => m Unit
program = do
  offset <- currentTime
  printTime offset "start"
  spawn $ do
    for 5 $ do
      printTime offset "spawned, before 1000"
      sleepFor 1.seconds
      printTime offset "spawned, before 2000"
      sleepFor 2.seconds
    printTime offset "spawned, end"
  for 10 $ do
    printTime offset "                      trunk, before 350"
    sleepFor 350.millis
    printTime offset "                      trunk, before 750"
    sleepFor 750.millis
  printTime offset "                      trunk, end"
