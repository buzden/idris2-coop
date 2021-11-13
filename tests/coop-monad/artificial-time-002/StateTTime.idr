import CommonTestingStuff

import Data.SnocList

program : MonadWriter (SnocList String) m => CanSleep m => Zippable m => m ()
program = do
  tellTimed "start"
  ignore $ zip
    (for 5 $ do
      tellTimed "proc 1, before 1000"
      sleepFor 1.seconds
      tellTimed "proc 1, before 2000"
      sleepFor 2.seconds)
    (for 10 $ do
      tellTimed "                     proc 2, before 350"
      sleepFor 350.millis
      tellTimed "                     proc 2, before 750"
      sleepFor 750.millis)
  tellTimed "end"

main : HasIO io => io ()
main = do
  putStrLn "before coop"
  traverse_ putStrLn $ execW program
