module Program

import CommonTestingStuff

export
beforeString : String
beforeString = "before coop"

export
program : PrintString m => CanSleep m => Zippable m => m ()
program = do
  offset <- currentTime
  printTime offset "start"
  ignore $ zip
    (for 5 $ do
      printTime offset "proc 1, before 1000"
      sleepFor 1.seconds
      printTime offset "proc 1, before 2000"
      sleepFor 2.seconds)
    (for 10 $ do
      printTime offset "                     proc 2, before 350"
      sleepFor 350.millis
      printTime offset "                     proc 2, before 750"
      sleepFor 750.millis)
  printTime offset "end"
