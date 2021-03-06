import CommonTestingStuff

-------------------------------
--- Preparation for testing ---
-------------------------------

Timed (State $ SnocList String) where
  currentTime = (.millis) <$> length <$> get

CanSleep (State $ SnocList String) where
  sleepFor  _ = pure ()
  sleepTill _ = pure ()

append : MonadState (SnocList a) m => a -> m ()
append x = modify (:< x)

exec : Coop (State $ SnocList String) Unit -> SnocList String
exec = execState [<] . runCoop

-----------------------
--- Unit test cases ---
-----------------------

main : HasIO io => io ()
main = do
  putStrLn "test: do nothing"
  (exec $ pure ()) === [<]

  putStrLn "test: return time at the start"
  (exec $ currentTime >>= append . show) === [<"0"]

  putStrLn "test: return time at the start and then just a string"
  (exec $ currentTime >>= append . show >> append "test") === [<"0", "test"]

  putStrLn "test: consequent appends"
  (exec $ append "test1" *> append "test2") === [<"test1", "test2"]

  putStrLn "test: return time after some message"
  (exec $ append "before time" *> currentTime >>= append . show) === [<"before time", "1"]
