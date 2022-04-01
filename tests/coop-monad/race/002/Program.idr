module Program

import CommonTestingStuff

export
beforeString : String
beforeString = "before coop"

long : PrintString m => CanSleep m => Zippable m => (offset : Time) -> m String
long offset = do
  printTime offset "long proc, first"
  for 5 $ do
    printTime offset "long proc, before 1000"
    sleepFor 1.seconds
    printTime offset "long proc, before 2000"
    sleepFor 2.seconds
  printTime offset "long proc, last"
  pure "long"

infinite : CanSleep m => m a
infinite = forever $ sleepFor 1.millis

export
program : PrintString m => CanSleep m => Zippable m => Alternative m => m Unit
program = do
  offset <- currentTime
  printTime offset "start"

  res <- long offset <|> infinite
  printTime offset "top: \{res}"

  sleepFor 1.seconds
  printTime offset "------"

  res <- infinite <|> long offset
  printTime offset "top: \{res}"

  printTime offset "end"
