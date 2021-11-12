module System.Time

import Data.Nat

%default total

----------------------------------------------------------
--- Common stuff for absolute time and finite duration ---
----------------------------------------------------------

public export %inline
UntypedTime : Type
UntypedTime = Nat

export
interface TimeValue a where
  (.millis)  : UntypedTime -> a
  (.seconds) : UntypedTime -> a
  (.minutes) : UntypedTime -> a
  (.hours)   : UntypedTime -> a
  (.days)    : UntypedTime -> a

  -- back conversions may lose data
  (.asMillis)  : a -> UntypedTime
  (.asSeconds) : a -> UntypedTime
  (.asMinutes) : a -> UntypedTime
  (.asHours)   : a -> UntypedTime
  (.asDays)    : a -> UntypedTime

  x.seconds = (1000 * x).millis
  x.minutes = (60 * x).seconds
  x.hours   = (60 * x).minutes
  x.days    = (24 * x).hours

  x.asSeconds = divNatNZ x.asMillis  1000 SIsNonZero
  x.asMinutes = divNatNZ x.asSeconds 60   SIsNonZero
  x.asHours   = divNatNZ x.asMinutes 60   SIsNonZero
  x.asDays    = divNatNZ x.asHours   24   SIsNonZero

---------------------
--- Absolute time ---
---------------------

-- Well, absolute time is anyway a finite duration from some point in past...
namespace AbsTime

  export
  record Time where
    constructor MkTime
    millis : UntypedTime

  export
  TimeValue Time where
    (.millis)   = MkTime
    (.asMillis) = millis

  export
  Eq Time where
    (==) = (==) `on` (.asMillis)

  export
  Ord Time where
    compare = compare `on` (.asMillis)

----------------------------
--- Finite time duration ---
----------------------------

namespace FiniteDuration

  export
  record FinDuration where
    constructor MkFinDuration
    millis : UntypedTime

  export
  TimeValue FinDuration where
    (.millis) = MkFinDuration
    (.asMillis) = millis

  export
  Eq FinDuration where
    (==) = (==) `on` (.asMillis)

  export
  Ord FinDuration where
    compare = compare `on` (.asMillis)

  export
  Semigroup FinDuration where
    (<+>) = MkFinDuration .: (+) `on` (.asMillis)

  export
  Monoid FinDuration where
    neutral = MkFinDuration 0

  export
  (*) : Nat -> FinDuration -> FinDuration
  n * d = (n * d.asMillis).millis

  -- May lose data! `z * (d / z)` may be less than `d`
  export
  (/) : FinDuration -> (z : Nat) -> (0 _ : NonZero z) => FinDuration
  d / z = (divNatNZ d.asMillis z %search).millis

------------------------------------------------------------
--- Operations between absolute time and finite duration ---
------------------------------------------------------------

-- This is an absolute difference. `y + (x - y)` may be more than `x` if `x < y`
export
(-) : Time -> Time -> FinDuration
x - y = ((x `max` y).asMillis `minus` (x `min` y).asMillis).millis

export
(+) : Time -> FinDuration -> Time
t + d = (t.asMillis + d.asMillis).millis

------------------
--- Interfaces ---
------------------

public export
interface Timed m where
  currentTime : m Time

public export
interface Timed m => Monad m => CanSleep m where
  sleepTill : Time -> m Unit
  sleepTill t = sleepFor $ t - !currentTime

  sleepFor : FinDuration -> m Unit
  sleepFor d = sleepTill $ !currentTime + d
