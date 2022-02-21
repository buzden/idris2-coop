module Program

import CommonTestingStuff

export
beforeString : String
beforeString = "before coop"

s150 : PrintString m => CanSleep m => (offset : Time) => m String
s150 = do
  printTime offset "s150 proc, first"
  for 5 $ do
    printTime offset "s150 proc, before 1000"
    sleepFor 1.seconds
    printTime offset "s150 proc, before 2000"
    sleepFor 2.seconds
  "long" <$ printTime offset "s150 proc, last"

s55 : PrintString m => CanSleep m => (offset : Time) => m String
s55 = do
  printTime offset "                         s55 proc, first"
  for 5 $ do
    printTime offset "                         s55 proc, before 350"
    sleepFor 350.millis
    printTime offset "                         s55 proc, before 750"
    sleepFor 750.millis
  "mid" <$ printTime offset "                         s55 proc, last"

s35 : PrintString m => CanSleep m => (offset : Time) => m String
s35 = do
  printTime offset "                                               s35 proc, first"
  for 5 $ do
    printTime offset "                                               s35 proc, before 700"
    sleepFor 700.millis
  "short" <$ printTime offset "                                               s35 proc, last"

export
program : Timed m => PrintString m => Coop m Unit -- `Coop` because we don't have an interface for race yet
program = do
  printString "\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- s35 `race` (s150 `race` s55)
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------ above and below must be the same\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- (s35 `race` s150) `race` s55
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------\n------ change of the order\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- s55 `race` (s150 `race` s35)
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------ above and below must be the same\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- (s55 `race` s150) `race` s35
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------"
