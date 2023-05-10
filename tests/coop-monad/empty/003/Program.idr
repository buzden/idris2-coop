module Program

import CommonTestingStuff

import Data.List

export
beforeString : String
beforeString = "before coop"

s150 : PrintString m => CanSleep m => (offset : Time) => Nat -> m String
s150 no = do
  printTime offset "s150 proc \{show no}, first"
  for 5 $ do
    printTime offset "s150 proc \{show no}, before 1000"
    sleepFor 1.seconds
    printTime offset "s150 proc \{show no}, before 2000"
    sleepFor 2.seconds
  "long" <$ printTime offset "s150 proc \{show no}, last"

s55 : PrintString m => CanSleep m => (offset : Time) => Nat -> m String
s55 no = do
  printTime offset "                         s55 proc \{show no}, first"
  for 5 $ do
    printTime offset "                         s55 proc \{show no}, before 350"
    sleepFor 350.millis
    printTime offset "                         s55 proc \{show no}, before 750"
    sleepFor 750.millis
  "mid 1" <$ printTime offset "                         s55 proc \{show no}, last"

s35 : PrintString m => CanSleep m => (offset : Time) => Nat -> m String
s35 no = do
  printTime offset "                                               s35 proc \{show no}, first"
  for 5 $ do
    printTime offset "                                               s35 proc \{show no}, before 700"
    sleepFor 700.millis
  "short" <$ printTime offset "                                               s35 proc \{show no}, last"

s77 : PrintString m => CanSleep m => (offset : Time) => Nat -> m String
s77 no = do
  printTime offset "                                                                      s77 proc \{show no}, first"
  for 5 $ do
    printTime offset "                                                                      s77 proc \{show no}, before 1350"
    sleepFor 1350.millis
  sleepFor 950.millis
  "mid 2" <$ printTime offset "                                                                      s77 proc \{show no}, last"

%hide Control.Relation.Nil

export
program : PrintString m => CanSleep m => Zippable m => Alternative m => m Unit
program = do
  offset <- currentTime
  printTime offset "start"

  res <- choiceMap
    ( \(comp, n) => choiceMap (\k => comp k <&> (++ ", var #\{show k}")) $ take n [1..n] )
    [(s150, 1), (s55, 2), (s35, 3), (s77, 0)]
  printTime offset "top: \{res}"

  printTime offset "end"
