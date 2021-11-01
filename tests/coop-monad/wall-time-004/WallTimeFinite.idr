import Control.Monad.Coop

import System.Clock

millis : HasIO io => io Integer
millis = liftIO $ composeTime <$> clockTime Monotonic where
  composeTime : Clock Monotonic -> Integer
  composeTime (MkClock secs nanos) = secs * 1000 + nanos `div` 1000000

Timed IO where
  currentTime = (.millis) . fromInteger <$> millis

printTime : HasIO io => (offset : Integer) -> String -> io Unit
printTime offset s = putStrLn $ "[time: " ++ show (!millis - offset) ++ "] " ++ s

delays : List FinDuration
delays = [250.millis, 600.millis, 0.seconds, 1.seconds, 4.seconds]

Show FinDuration where
  show d = show d.asMillis

p : DelayableFor io => HasIO io => Integer -> FinDuration -> io Unit
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
