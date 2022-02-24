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
program : PrintString m => CanSleep m => Alternative m => m Unit
program = do
  printString "\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- s35 <|> (s150 <|> s55)
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------ above and below must be the same\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- (s35 <|> s150) <|> s55
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------\n------ change of the order\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- s55 <|> (s150 <|> s35)
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------ above and below must be the same\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- (s55 <|> s150) <|> s35
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------"
