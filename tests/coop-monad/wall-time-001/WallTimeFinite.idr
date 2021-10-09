import Control.Monad.Coop

import System.Clock

millis : HasIO io => io Integer
millis = liftIO $ composeTime <$> clockTime Monotonic where
  composeTime : Clock Monotonic -> Integer
  composeTime (MkClock secs nanos) = secs * 1000 + nanos `div` 1000000

Timed IO where
  currentTime = fromInteger <$> millis

printTime : HasIO io => (offset : Integer) -> String -> io Unit
printTime offset s = printLn $ "[time: " ++ show (!millis - offset) ++ "] " ++ s

for : Nat -> Monad m => m a -> m ()
for Z     _ = pure ()
for (S n) a = do a; for n a

export
main : IO Unit
main = do printLn "before coop"
          runCoop $ do
  offset <- millis
  printTime offset "start"
  (<||>)
    (for 5 $ do
      printTime offset "proc 1, before 1000"
      sleepFor 1000
      printTime offset "proc 1, before 2000"
      sleepFor 2000)
    (for 10 $ do
      printTime offset "                     proc 2, before 350"
      sleepFor 350
      printTime offset "                     proc 2, before 750"
      sleepFor 750)

