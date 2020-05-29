module Arduino.Coop

import Arduino.Util

%default total

-------------
--- Types ---
-------------

-- TODO to make this type be nicer
public export
Millis : Type
Millis = Nat

------------------
--- Interfaces ---
------------------

public export
interface Timed (m : Type -> Type) where
  millis : m Millis

public export
interface Parallel (m : Type -> Type) where
  -- Alternative-like operator with parallel semantics and unavailable results of separate computations
  (<|>) : m a -> m b -> m ()

public export
interface DelayableTill (m : Type -> Type) where
  delayedTill : Millis -> m a -> m a
  -- TODO Maybe, swap the arguments to make this be used better as infix operator?

public export
interface DelayableFor (m : Type -> Type) where
  delayedFor : Millis -> m a -> m a

export
(Timed m, DelayableTill m, Monad m) => DelayableFor m where
  delayedFor t a = millis >>= \m => delayedTill (m + t) a

------------
--- Data ---
------------

export
data Coop : (m : Type -> Type) -> (a : Type) -> Type where
  Point       : m a -> Coop m a
  Sequential  : Coop m a -> (a -> Coop m b) -> Coop m b
  Cooperative : Coop m a -> Coop m b -> Coop m ()
  DelayedTill : Millis -> Coop m a -> Coop m a

-------------------
--- Interpreter ---
-------------------

export
runCoop : (Monad m, Timed m) => Coop m a -> m a
runCoop co = ?runCoop_rhs

-----------------------
--- Implementations ---
-----------------------

export
Timed m => Timed (Coop m) where
  millis = Point millis

infixl 4 $>

($>) : Applicative m => Coop m a -> b -> Coop m b
(Point _)        $> b = Point $ pure b
(Sequential a f) $> b = Sequential a $ \ar => f ar $> b
x                $> b = Sequential x . const . Point $ pure b

export
Applicative m => Functor (Coop m) where
  map f (Point a)           = Point (map f a)
  map f (Sequential a b)    = Sequential a $ \ar => map f $ b ar
  map f x@(Cooperative _ _) = x $> f ()
  map f (DelayedTill t a)   = DelayedTill t $ map f a

export
Applicative m => Applicative (Coop m) where
  pure    = Point . pure
  l <*> r = Sequential l $ \lf => map lf r
  -- This could be `(<*>) = Cooperative apply`, but it must be consistent with `(>>=)` definition.
  -- Consider code `doSmth *> delayedFor 100 *> doMore` comparing to `(doSmth <|> delayedFor 100) *> doMore`.
  -- Having parallel semantics for the `Applicative`'s `<*>`, those two examples above will mean the same, which seems to be unexpected.

export
Monad m => Monad (Coop m) where
  (>>=) = Sequential

export
Parallel (Coop m) where
  (<|>) = Cooperative

export
Timed m => DelayableTill (Coop m) where
  delayedTill = DelayedTill
