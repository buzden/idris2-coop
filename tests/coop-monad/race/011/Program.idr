module Program

import CommonTestingStuff

export
beforeString : String
beforeString = "before coop"

s15000 : PrintString m => CanSleep m => (offset : Time) => m String
s15000 = do
  printTime offset "s15000 proc, first"
  for 5 $ do
    printTime offset "s15000 proc, before 1000"
    sleepFor 1.seconds
    printTime offset "s15000 proc, before 2000"
    sleepFor 2.seconds
  "long" <$ printTime offset "s15000 proc, last"

s5500 : PrintString m => CanSleep m => (offset : Time) => m String
s5500 = do
  printTime offset "                         s5500 proc, first"
  for 5 $ do
    printTime offset "                         s5500 proc, before 350"
    sleepFor 350.millis
    printTime offset "                         s5500 proc, before 750"
    sleepFor 750.millis
  "short" <$ printTime offset "                         s5500 proc, last"

n3500 : PrintString m => CanSleep m => (offset : Time) => m Nat
n3500 = do
  printTime offset "                                               n3500 proc, first"
  for 5 $ do
    printTime offset "                                               n3500 proc, before 700"
    sleepFor 700.millis
  700 <$ printTime offset "                                               n3500 proc, last"

n7700 : PrintString m => CanSleep m => (offset : Time) => m Nat
n7700 = do
  printTime offset "                                                                      n7700 proc, first"
  for 5 $ do
    printTime offset "                                                                      n7700 proc, before 1350"
    sleepFor 1350.millis
  sleepFor 950.millis
  333 <$ printTime offset "                                                                      n7700 proc, last"

export
program : Timed m => PrintString m => Coop m Unit -- `Coop` because we don't have an interface for race yet
program = do
  offset <- currentTime
  printTime offset "start"

  res <- zipWith (\s, n => s ++ show n) (s15000 `race` s5500) (n3500 `race` n7700)
  printTime offset "top: \{res}"

  printTime offset "end"
