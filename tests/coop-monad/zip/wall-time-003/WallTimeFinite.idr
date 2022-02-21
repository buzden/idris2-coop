import CommonTestingStuff

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
