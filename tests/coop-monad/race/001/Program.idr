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
  "long" <$ printTime offset "long proc, last"

short : PrintString m => CanSleep m => Zippable m => (offset : Time) -> m String
short offset = do
  printTime offset "                       short proc, first"
  for 5 $ do
    printTime offset "                       short proc, before 350"
    sleepFor 350.millis
    printTime offset "                       short proc, before 750"
    sleepFor 750.millis
  "short" <$ printTime offset "                       short proc, last"

export
program : PrintString m => CanSleep m => Zippable m => Alternative m => m Unit
program = do
  offset <- currentTime
  printTime offset "start"

  res <- long offset <|> short offset
  printTime offset "top: \{res}"

  sleepFor 1.seconds
  printTime offset "------"

  res <- short offset <|> long offset
  printTime offset "top: \{res}"

  printTime offset "end"
