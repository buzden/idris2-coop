module CommonTestingStuff

import public Data.SnocList

import public Control.Monad.Coop
import public Control.Monad.State
import public Control.Monad.Writer

%default total

--- Testing-purpose instances ---

export
Show Time where
  show t = show t.asMillis

export
Show FinDuration where
  show d = show d.asMillis

--- Printing stuff ---

public export
interface Monad m => PrintString m where
  printString : String -> m Unit

export
PrintString IO where
  printString = putStrLn

export
printTime : PrintString io => Timed io => (offset : Time) -> String -> io Unit
printTime offset s = printString "[time: \{show {ty=FinDuration} $ !currentTime - offset}] \{s}"

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

--- `Writer`-related stuff ---

export
Monad m => PrintString (WriterT (SnocList String) m) where
  printString = tell . pure

export
Timed m => Monad m => Timed (WriterT w m) where
  currentTime = lift currentTime

export
CanSleep m => CanSleep (WriterT w m) where
  sleepFor  = lift . sleepFor
  sleepTill = lift . sleepTill

export covering
execW : (forall m. PrintString m => CanSleep m => Coop m Unit) -> SnocList String
execW c = execWriter $ evalStateT 0.seconds $ runCoop c {m=StateT Time _} where

  Monad m => Timed (StateT Time m) where
    currentTime = get

  Monad m => CanSleep (StateT Time m) where
    sleepFor d = modify (+d)

  PrintString m => PrintString (StateT s m) where
    printString = lift . printString

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

export
PrintString m => PrintString (Coop m) where
  printString = lift . printString
