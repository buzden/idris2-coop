module Program

import CommonTestingStuff

export
beforeString : String
beforeString = "before coop"

long : PrintString m => CanSleep m => (offset : Time) => m String
long = do
  printTime offset "long proc, first"
  for 5 $ do
    printTime offset "long proc, before 1000"
    sleepFor 1.seconds
    printTime offset "long proc, before 2000"
    sleepFor 2.seconds
  "long" <$ printTime offset "long proc, last"

short : PrintString m => CanSleep m => (offset : Time) => m String
short = do
  printTime offset "short proc, first"
  for 5 $ do
    printTime offset "short proc, before 350"
    sleepFor 350.millis
    printTime offset "short proc, before 750"
    sleepFor 750.millis
  "short" <$ printTime offset "short proc, last"

zipped : PrintString m => CanSleep m => Zippable m => (offset : Time) => m String
zipped = zipWith (\x, y => "\{x}: \{show y}") a b where
  a : m String
  a = do
    printTime offset "                       zipped a, first"
    sleepFor 3500.millis
    printTime offset "                       zipped a, mid"
    sleepFor 3.seconds
    "zipped" <$ printTime offset "                       zipped a, last"

  b : m Nat
  b = do
    printTime offset "                       zipped b, first"
    6 <$ printTime offset "                       zipped b, last"

export
program : Timed m => PrintString m => Coop m Unit -- `Coop` because we don't have an interface for race yet
program = do
  offset <- currentTime
  printTime offset "start"

  res <- zipped `race` short
  printTime offset "top: \{res}"

  sleepFor 1.seconds
  printTime offset "------"

  res <- zipped `race` long
  printTime offset "top: \{res}"

  printTime offset "end"
