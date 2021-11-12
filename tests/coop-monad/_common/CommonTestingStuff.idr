module CommonTestingStuff

import public Control.Monad.Coop
import public Control.Monad.State

%default total

--- `IO` stuff ---

export
millis : HasIO io => io Integer
millis = cast . (.asMillis) <$> currentTime @{HasIO}

export
printTime : HasIO io => (offset : Integer) -> String -> io Unit
printTime offset s = putStrLn "[time: \{show $ !millis - offset}] \{s}"

--- Testing-specific stuff ---

export
(===) : (Eq a, Show a, HasIO io) => a -> a -> io ()
x === y = if x == y
            then putStrLn "- [ok]"
            else putStrLn "- [VIOLATION] got \{show x} but expected \{show y}"

--- Monadic stuff ---

export
for : Nat -> Monad m => m a -> m ()
for Z     _ = pure ()
for (S n) a = do ignore a; for n a

export covering
forever : Monad m => m a -> m b
forever x = do ignore x; forever x

--- Testing-purpose instances ---

export
Show Time where
  show t = show t.asMillis

export
Show FinDuration where
  show d = show d.asMillis

--- Liftings for `Coop` ---

export
MonadState st m => MonadState st (Coop m) where
  get = lift get
  put = lift . put
