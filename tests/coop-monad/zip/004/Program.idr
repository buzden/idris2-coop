module Program

import CommonTestingStuff

delays : List FinDuration
delays = [250.millis, 600.millis, 0.seconds, 1.seconds, 4.seconds]

p : PrintString m => CanSleep m => Time -> FinDuration -> m Unit
p offset d = do
  sleepFor d
  printTime offset "after waiting for \{show d}"

export
beforeString : String
beforeString = "before coop, delays: \{show delays}"

export
program : PrintString m => CanSleep m => ConcurrentApplicative m => m Unit
program = do
  offset <- currentTime
  printTime offset "===== usual applicative"
  for_ delays $ p offset
  printTime offset "==== end"

  offset <- currentTime
  printTime offset "==== parallel applicative"
  for_ delays @{Concurrent} $ p offset
  printTime offset "==== end"
