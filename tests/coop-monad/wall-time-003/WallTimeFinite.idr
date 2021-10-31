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

export
main : IO Unit
main = do putStrLn "before coop"
          runCoop $ do
  offset <- millis
  printTime offset "start"
  res <- zipWith (++)
    (for [1, 2, 3, 4, 5] $ \i => do
      printTime offset "proc 1, before 1000, \{show i}"
      sleepFor 1.seconds
      printTime offset "proc 1, before 2000, \{show i}"
      sleepFor 2.seconds
      pure $ the Int $ i * 10)
    (for [10, 20, 30, 40, 50, 60, 70, 80, 90, 95] $ \i => do
      printTime offset "                                proc 2, before 350, \{show i}"
      sleepFor 350.millis
      printTime offset "                                proc 2, before 750, \{show i}"
      sleepFor 750.millis
      pure $ the Int $ i * 10)
  printTime offset $ "res: " ++ show res
  sleepFor 1.seconds
  printTime offset "end"
