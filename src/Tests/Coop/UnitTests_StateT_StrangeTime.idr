import Control.Monad.Coop
import Control.Monad.State

-------------------------------
--- Preparation for testing ---
-------------------------------

Timed (State $ List String) where
  currentTime = length <$> get

-- Awfully inefficient implementation, but will work for small tests.
append : MonadState (List a) m => a -> m ()
append x = modify (++ [x])

exec : Coop (State $ List String) Unit -> List String
exec = execState [] . runCoop

(===) : (Eq a, Show a, HasIO io) => a -> a -> io ()
x === y = if x == y
            then printLn "- [ok]"
            else printLn $ "- [VIOLATION] got " ++ show x ++ " but expected " ++ show y

MonadState st m => MonadState st (Coop m) where
  get = lift get
  put = lift . put

-----------------------
--- Unit test cases ---
-----------------------

main : HasIO io => io ()
main = do
  printLn "test: do nothing"
  (exec $ pure ()) === []

  printLn "test: return time at the start"
  (exec $ currentTime >>= append . show) === ["0"]

  printLn "test: return time at the start and then just a string"
  (exec $ currentTime >>= append . show >>= \() => append "test") === ["0", "test"]

  printLn "test: consequent appends"
  (exec $ append "test1" *> append "test2") === ["test1", "test2"]

  printLn "test: return time after some message"
  (exec $ append "before time" *> currentTime >>= append . show) === ["before time", "1"]
