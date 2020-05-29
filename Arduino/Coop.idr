module Arduino.Coop

import Arduino.StateT
import Arduino.Util

import Control.Monad.Syntax

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
interface Monad m => DelayableTill (m : Type -> Type) where
  delayTill : Millis -> m ()

public export
interface Monad m => DelayableFor (m : Type -> Type) where
  delay : Millis -> m ()

export
(Timed m, DelayableTill m) => DelayableFor m where
  delay t = delayTill . (+t) =<< millis

------------
--- Data ---
------------

export
data Coop : (m : Type -> Type) -> (a : Type) -> Type where
  Point       : m a -> Coop m a
  Sequential  : Coop m a -> (a -> Coop m b) -> Coop m b
  Cooperative : Coop m a -> Coop m b -> Coop m ()
  DelayedTill : Millis -> Coop m ()

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
  map f x@(DelayedTill t)   = x $> f ()

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
(Timed m, Monad m) => DelayableTill (Coop m) where
  delayTill = DelayedTill

-------------------
--- Interpreter ---
-------------------

export covering
runCoop : (Monad m, Timed m) => Coop m a -> m ()
runCoop co = evalStateT [(_ ** co)] runLeftEvents where

  -- TODO to add time and to implement `Ord Event`
  Event : Type
  Event = (x : Type ** Coop m x)

  -- TODO to replace list with a sortedness-preserving kinda-list
  covering
  runLeftEvents : Monad m => StateT (List Event) m ()
  runLeftEvents = do (currEv::restEvs) <- the (StateT (List Event) m (List Event)) get | [] => pure ()
                     newEvs <- lift $ runEvent currEv
                     put $ restEvs ++ newEvs -- TODO to replace `++` with sortedness-preserving addition
                     runLeftEvents
    where
    runEvent : Event -> m (List Event) -- returns new events as the result of running
    runEvent (_ ** Point x)         = x $> Nil
    runEvent (_ ** Cooperative l r) = pure [(_ ** l), (_ ** r)]
    runEvent (_ ** DelayedTill _)   = pure Nil
    runEvent (_ ** Sequential x f)  = case x of
      Point y         => map (\r => [(_ ** f r)]) y
      Sequential y g  => pure [(_ ** Sequential y (g >=> f))]
      Cooperative l r => ?runEvent_sequential_rhs_3 -- TODO to wait for both subexecutions, maybe additional info is needed to be added to the state
      DelayedTill d   => ?runEvent_sequential_rhs_4
