module Arduino.Time

-------------
--- Types ---
-------------

-- TODO to make this type be nicer
public export
Time : Type
Time = Nat

-----------------
--- Functions ---
-----------------

-- TODO to make something with, well, the fact that `Bits32` leaks out from the implementation

export
toMilliseconds : Time -> Bits32
toMilliseconds = cast . natToInteger

export
fromMilliseconds : Bits32 -> Time
fromMilliseconds = fromInteger . cast

------------------
--- Interfaces ---
------------------

public export
interface Timed (m : Type -> Type) where
  currentTime : m Time

public export
interface Monad m => DelayableTill (m : Type -> Type) where
  delayTill : Time -> m ()

public export
interface Monad m => DelayableFor (m : Type -> Type) where
  delay : Time -> m ()
