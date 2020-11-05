import Arduino.Coop

import System.Clock

millis : HasIO io => io Integer
millis = liftIO $ composeTime <$> clockTime Monotonic where
  composeTime : Clock Monotonic -> Integer
  composeTime (MkClock secs nanos) = secs * 1000 + nanos `div` 1000000

Timed IO where
  currentTime = fromInteger <$> millis

printTime : HasIO io => (offset : Integer) -> String -> io Unit
printTime offset s = do
  t <- millis
  printLn $ "[time: " ++ show (t - offset) ++ "] " ++ s

forever : Monad m => m a -> m b
forever x = do x; forever x

export
main : IO Unit
main = do printLn "before coop"
          runCoop $ with Arduino.Time.delay do
  offset <- millis
  printTime offset "start"
  (<||>)
    (forever $ do
      printTime offset "proc 1, before 1000"
      delay 1000
      printTime offset "proc 1, before 2000"
      delay 2000)
    (forever $ do
      printTime offset "proc 2, before 350"
      delay 350
      printTime offset "proc 2, before 750"
      delay 750)

