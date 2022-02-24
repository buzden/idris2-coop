import CommonTestingStuff

par : Applicative m => Coop m Unit -> Coop m Unit -> Coop m Unit
par = ignore .: zip

export
main : IO Unit
main = do putStrLn "before coop"
          runCoop $ do
  offset <- currentTime
  printTime offset "start"
  par
    (forever $ do
      printTime offset "proc 1, one"
      printTime offset "proc 1, another")
    (forever $ do
      printTime offset "                     proc 2, one"
      printTime offset "                     proc 2, another")
  printTime offset "end"
