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
  "short" <$ printTime offset "                         s55 proc, last"

n35 : PrintString m => CanSleep m => (offset : Time) => m Nat
n35 = do
  printTime offset "                                               n35 proc, first"
  for 5 $ do
    printTime offset "                                               n35 proc, before 700"
    sleepFor 700.millis
  35 <$ printTime offset "                                               n35 proc, last"

n77 : PrintString m => CanSleep m => (offset : Time) => m Nat
n77 = do
  printTime offset "                                                                      n77 proc, first"
  for 5 $ do
    printTime offset "                                                                      n77 proc, before 1350"
    sleepFor 1350.millis
  sleepFor 950.millis
  77 <$ printTime offset "                                                                      n77 proc, last"

prn : PrintString m => CanSleep m => (offset : Time) => Nat -> m String
prn n = do
  printTime offset "--- printing sleep, 1.45 sec ---"
  sleepFor 1.seconds
  printTime offset "\{show n}##"
  sleepFor 450.millis
  pure $ "#\{show n}#"

prs : PrintString m => CanSleep m => (offset : Time) => String -> m Nat
prs s = do
  printTime offset "--- printing sleep, 1.45 sec ---"
  sleepFor 1.seconds
  printTime offset "\{show s}$$"
  sleepFor 450.millis
  pure $ length s

export
program : PrintString m => CanSleep m => Alternative m => m Unit
program = do
  printString "\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- ((n77 <|> n35) >>= prn) <|> (s150 <|> s55)
    printTime offset "top: \{res}"

    printTime offset "end"

  printString "\n------\n"

  do
    offset <- currentTime
    printTime offset "start"

    res <- ((s150 <|> s55) >>= prs) <|> (n77 <|> (n35 *> n35))
    printTime offset "top: \{show res}"

    printTime offset "end"

  printString "\n------"
