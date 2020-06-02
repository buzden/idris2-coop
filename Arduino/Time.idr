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

export
toMilliseconds : Time -> Int
toMilliseconds = toIntNat

export
fromMilliseconds : Int -> Time
fromMilliseconds = toNat

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
