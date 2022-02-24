module Program

import CommonTestingStuff

import Data.List

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
  "short" <$ printTime offset "                         s55 proc, last"

export
program : PrintString m => CanSleep m => Zippable m => Alternative m => m Unit
program = do
  offset <- currentTime
  printTime offset "start"

  res <- zipWith (++) (s55 *> empty) s150
  printTime offset "top: \{res}"

  printTime offset "end"
