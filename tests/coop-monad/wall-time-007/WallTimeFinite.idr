import CommonTestingStuff

export
main : IO Unit
main = do putStrLn "before coop"
          runCoop $ do
  offset <- millis
  printTime offset "start"
  (l, r) <- zip
    (do spawn $ do
          sleepFor 200.millis
          printTime offset "                                          spawned 1, before 500"
          sleepFor 500.millis
          printTime offset "                                          spawned 1, after 500"
        for 5 $ do
          printTime offset "proc 1, before 1000"
          sleepFor 1.seconds
          printTime offset "proc 1, before 2000"
          sleepFor 2.seconds
        pure 4)
    (do spawn $ do
          sleepFor 100.millis
          printTime offset "                                          spawned 2, before 15s"
          sleepFor 15.seconds
          printTime offset "                                          spawned 2, after 15s"
        for 10 $ do
          printTime offset "                     proc 2, before 350"
          sleepFor 350.millis
          printTime offset "                     proc 2, before 750"
          sleepFor 750.millis
        pure "right process")
  printTime offset "end, l: \{show l}, r: \{show r}"
