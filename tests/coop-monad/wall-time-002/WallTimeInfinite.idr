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

forever : Monad m => m a -> m b
forever x = do ignore x; forever x

export
main : IO Unit
main = do putStrLn "before coop"
          runCoop $ do
  offset <- millis
  printTime offset "start"
  par
    (forever $ do
      printTime offset "proc 1, before 1000"
      sleepFor 1.seconds
      printTime offset "proc 1, before 2000"
      sleepFor 2.seconds)
    (forever $ do
      printTime offset "                     proc 2, before 350"
      sleepFor 350.millis
      printTime offset "                     proc 2, before 750"
      sleepFor 750.millis)
  printTime offset "end"
