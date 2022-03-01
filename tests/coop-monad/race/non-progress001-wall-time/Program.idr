module Program

import CommonTestingStuff

export
beforeString : String
beforeString = "before coop"

export
program : PrintString m => CanSleep m => Zippable m => Alternative m => m Unit
program = do
  offset <- currentTime
  printTime offset "start"

  (<|>) (sleepFor 1.seconds) $
    ignore $ zip {a=Unit} {b=Unit}
      (forever $ do
        printTime offset "proc 1, one"
        printTime offset "proc 1, another")
      (forever $ do
        printTime offset "                     proc 2, one"
        printTime offset "                     proc 2, another")

  printTime offset "end"
