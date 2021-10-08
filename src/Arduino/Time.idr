module Arduino.Time

-------------
--- Types ---
-------------

-- TODO to make this type be nicer
public export
Time : Type
Time = Int

-----------------
--- Functions ---
-----------------

-- TODO to make something with, well, the fact that `Bits32` leaks out from the implementation

export
toMilliseconds : Time -> Bits32
toMilliseconds = cast

export
fromMilliseconds : Bits32 -> Time
fromMilliseconds = fromInteger . cast

------------------
--- Interfaces ---
------------------

public export
interface Timed m where
  currentTime : m Time

public export
interface Monad m => DelayableTill m where
  sleepTill : Time -> m Unit

public export
interface Monad m => DelayableFor m where
  sleepFor : Time -> m Unit
