import CommonTestingStuff

import Data.SnocList

program : MonadWriter (SnocList String) m => CanSleep m => Zippable m => m ()
program = do
  tellTimed "start"
  res <- zipWith (++)
    (for [1, 2, 3, 4, 5] $ \i => do
      tellTimed "proc 1, before 1000, \{show i}"
      sleepFor 1.seconds
      tellTimed "proc 1, before 2000, \{show i}"
      sleepFor 2.seconds
      pure $ the Int $ i * 10)
    (for [10, 20, 30, 40, 50, 60, 70, 80, 90, 95] $ \i => do
      tellTimed "                                proc 2, before 350, \{show i}"
      sleepFor 350.millis
      tellTimed "                                proc 2, before 750, \{show i}"
      sleepFor 750.millis
      pure $ the Int $ i * 10)
  tellTimed $ "res: " ++ show res
  sleepFor 1.seconds
  tellTimed "end"

main : HasIO io => io ()
main = do
  putStrLn "before coop"
  traverse_ putStrLn $ execW program
