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
  "mid 1" <$ printTime offset "                         s55 proc, last"

s35 : PrintString m => CanSleep m => (offset : Time) => m String
s35 = do
  printTime offset "                                               s35 proc, first"
  for 5 $ do
    printTime offset "                                               s35 proc, before 700"
    sleepFor 700.millis
  "short" <$ printTime offset "                                               s35 proc, last"

s77 : PrintString m => CanSleep m => (offset : Time) => m String
s77 = do
  printTime offset "                                                                      s77 proc, first"
  for 5 $ do
    printTime offset "                                                                      s77 proc, before 1350"
    sleepFor 1350.millis
  sleepFor 950.millis
  "mid 2" <$ printTime offset "                                                                      s77 proc, last"

export
program : PrintString m => CanSleep m => Zippable m => Alternative m => m Unit
program = do
  offset <- currentTime
  printTime offset "start"

  res <- choiceMap
    ( \(comp, n) => choiceMap (\k => comp <&> (++ ", var #\{show k}")) $ take n [1..n] )
    [(s150, 1), (s55, 2), (s35, 3), (s77, 0)]
  printTime offset "top: \{res}"

  printTime offset "end"
