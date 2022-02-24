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

export
program : PrintString m => CanSleep m => Alternative m => m Unit
program = do
  printString "\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- empty <|> s150
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- s150 <|> empty
    printTime offset "top: \{show res}"

    printTime offset "end"

  printString "\n------"
