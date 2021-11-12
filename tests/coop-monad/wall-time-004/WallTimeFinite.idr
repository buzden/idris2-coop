import Control.Monad.Coop

import System.Clock

millis : HasIO io => io Integer
millis = cast . (.asMillis) <$> currentTime @{HasIO}

printTime : HasIO io => (offset : Integer) -> String -> io Unit
printTime offset s = putStrLn $ "[time: " ++ show (!millis - offset) ++ "] " ++ s

delays : List FinDuration
delays = [250.millis, 600.millis, 0.seconds, 1.seconds, 4.seconds]

Show FinDuration where
  show d = show d.asMillis

p : CanSleep io => HasIO io => Integer -> FinDuration -> io Unit
p offset d = do
  sleepFor d
  printTime offset "after waiting for \{show d}"

export
main : IO Unit
main = do putStrLn "before coop, delays: \{show delays}"
          runCoop $ do
  offset <- millis
  printTime offset "===== usual applicative"
  for_ delays $ p offset
  printTime offset "==== end"

  offset <- millis
  printTime offset "==== parallel applicative"
  for_ delays @{Concurrent} $ p offset
  printTime offset "==== end"
