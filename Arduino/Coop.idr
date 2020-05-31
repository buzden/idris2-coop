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

Sync : Type
Sync = Nat

data Event : (Type -> Type) -> Type where
  Ev : (t : Millis) -> Coop m x -> List (Sync, (y : Type ** Coop m y)) -> Event m

-- The following comparison is only according to the time; this will incorrectly work for sets.
-- Equally timed events with different actions are considered to be equal with `==` relation.
Eq (Event m) where
  (Ev tl _ _) == (Ev tr _ _) = tl == tr

Ord (Event m) where
  compare (Ev tl _ _) (Ev tr _ _) = tl `compare` tr

export covering
runCoop : (Monad m, Timed m) => Coop m a -> m ()
runCoop co = evalStateT [Ev !millis co []] runLeftEvents where

  -- TODO to replace list with a sortedness-preserving kinda-list
  covering
  runLeftEvents : Monad m => StateT (List $ Event m) m ()
  runLeftEvents = do (currEv::restEvs) <- the (StateT (List $ Event m) m (List $ Event m)) get | [] => pure ()
                     let Ev currEvTime _ postponed = currEv
                     currTime <- lift millis
                     when (currEvTime >= currTime) $ do
                       newEvs <- lift . runEvent currEv $ uniqueSync $ currEv::restEvs
                       let newLeftEvs = mergeSorted restEvs newEvs
                       let newLeftSyncs = syncs newLeftEvs
                       let postponedWithNoSyncLeft = filter (not . flip elem newLeftSyncs . fst) postponed
                       let awakened = map (\(_, (_ ** coop)) => Ev currEvTime coop []) postponedWithNoSyncLeft
                       put $ mergeSorted newLeftEvs awakened
                     -- TODO else wait for the `currEvTime - currTime`; or support and perform permanent tasks
                     runLeftEvents
    where
    runEvent : Event m -> Lazy Sync -> m (List $ Event m) -- returns new events as the result of running
    runEvent (Ev _ (Point x)         _) _       = x $> Nil
    runEvent (Ev t (Cooperative l r) p) _       = pure [Ev t l p, Ev t r p]
    runEvent (Ev _ (DelayedTill d)   p) _       = pure [Ev d (Point $ pure ()) p] -- this enables `p` to be run when appropriate (delayed)
    runEvent (Ev t (Sequential x f)  p) newSync = case x of
      Point y         => map (\r => [Ev t (f r) p]) y
      Sequential y g  => pure [Ev t (Sequential y $ g >=> f) p]
      DelayedTill d   => pure [Ev d (f ()) p]
      Cooperative l r => let extP = (Force newSync, (_ ** f ()))::p in pure [Ev t l extP, Ev t r extP]

    syncs : List (Event m) -> List Sync
    syncs evs = evs >>= \(Ev _ _ postponed) => map fst postponed

    uniqueSync : List (Event m) -> Sync
    uniqueSync = f . syncs where
      f [] = 0
      f ss@(t::ts) = case foldl min t ts of
        S x => x                  -- either minimal minus 1
        Z   => S $ foldl max 0 ss -- or maximal plus 1
