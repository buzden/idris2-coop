module CommonTestingStuff

import public Data.SnocList

import public Control.Monad.Coop
import public Control.Monad.State
import public Control.Monad.Writer

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

--- `Writer`-related stuff ---

export
tellTimed : Timed m => MonadWriter (SnocList String) m => String -> m Unit
tellTimed str = tell $ pure "[time: \{show !currentTime}] \{str}"

export
Timed m => Monad m => Timed (WriterT w m) where
  currentTime = lift currentTime

export
CanSleep m => CanSleep (WriterT w m) where
  sleepFor  = lift . sleepFor
  sleepTill = lift . sleepTill

export covering
execW : Monoid w => (forall m. MonadWriter w m => CanSleep m => Coop m Unit) -> w
execW c = execWriter $ evalStateT 0.seconds $ runCoop c {m=StateT Time _} where

  Monad m => Timed (StateT Time m) where
    currentTime = get

  Monad m => CanSleep (StateT Time m) where
    sleepFor d = modify (+d)

--- Liftings for `Coop` ---

export
MonadState st m => MonadState st (Coop m) where
  get = lift get
  put = lift . put

export
MonadWriter w m => MonadWriter w (Coop m) where
  writer = lift . writer
  tell   = lift . tell
  listen = (>>= lift . listen . pure)
  pass   = (>>= lift . pass   . pure)
