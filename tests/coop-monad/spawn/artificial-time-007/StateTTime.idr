import CommonTestingStuff

program : MonadWriter (SnocList String) m => CanSleep m => CanSpawn m => Zippable m => m ()
program = do
  tellTimed "start"
  (l, r) <- zip
    (do spawn $ do
          sleepFor 200.millis
          tellTimed "                                          spawned 1, before 500"
          sleepFor 500.millis
          tellTimed "                                          spawned 1, after 500"
        for 5 $ do
          tellTimed "proc 1, before 1000"
          sleepFor 1.seconds
          tellTimed "proc 1, before 2000"
          sleepFor 2.seconds
        pure 4)
    (do spawn $ do
          sleepFor 100.millis
          tellTimed "                                          spawned 2, before 15s"
          sleepFor 15.seconds
          tellTimed "                                          spawned 2, after 15s"
        for 10 $ do
          tellTimed "                     proc 2, before 350"
          sleepFor 350.millis
          tellTimed "                     proc 2, before 750"
          sleepFor 750.millis
        pure "right process")
  tellTimed "end, l: \{show l}, r: \{show r}"

main : HasIO io => io ()
main = do
  putStrLn "before coop"
  traverse_ putStrLn $ execW program
