module Arduino.Coop

import Arduino.Util

import Control.Monad.Syntax

%default total

-------------
--- Types ---
-------------

-- TODO to make this type be nicer
public export
Time : Type
Time = Nat

------------------
--- Interfaces ---
------------------

public export
interface Timed (m : Type -> Type) where
  currentTime : m Time

public export
interface Parallel (m : Type -> Type) where
  -- Alternative-like operator with parallel semantics and unavailable results of separate computations
  (<|>) : m a -> m b -> m ()

public export
interface Monad m => DelayableTill (m : Type -> Type) where
  delayTill : Time -> m ()

public export
interface Monad m => DelayableFor (m : Type -> Type) where
  delay : Time -> m ()

export
(Timed m, DelayableTill m) => DelayableFor m where
  delay t = delayTill . (+t) =<< currentTime

------------
--- Data ---
------------

export
data Coop : (m : Type -> Type) -> (a : Type) -> Type where
  Point       : m a -> Coop m a
  Sequential  : Coop m a -> (a -> Coop m b) -> Coop m b
  Cooperative : Coop m a -> Coop m b -> Coop m ()
  DelayedTill : Time -> Coop m ()

-----------------------
--- Implementations ---
-----------------------

export
Timed m => Timed (Coop m) where
  currentTime = Point currentTime

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
  Ev : (t : Time) -> Coop m x -> List (Sync, (y : Type ** Coop m y)) -> Event m

-- The following comparison is only according to the time; this will incorrectly work for sets.
-- Equally timed events with different actions are considered to be equal with `==` relation.
[TimeOnly_EvEq] Eq (Event m) where
  (Ev tl _ _) == (Ev tr _ _) = tl == tr

[TimeOnly_EvOrd] Ord (Event m) using TimeOnly_EvEq where
  compare (Ev tl _ _) (Ev tr _ _) = tl `compare` tr

export covering
runCoop : (Monad m, Timed m) => Coop m a -> m ()
runCoop co = runLeftEvents [Ev !currentTime co []] where

  -- TODO to replace list with a sortedness-preserving kinda-list
  covering
  runLeftEvents : List $ Event m -> m ()
  runLeftEvents [] = pure ()
  runLeftEvents evs@((Ev currEvTime currCoop postponed)::restEvs) = do
    nextEvs <- if currEvTime >= !currentTime
               then do
                 let newLeftEvs = merge @{TimeOnly_EvOrd} restEvs !newEvsAfterRunningCurr
                 pure $ merge @{TimeOnly_EvOrd} newLeftEvs $ awakened newLeftEvs
               else
                 -- TODO else wait for the `currEvTime - !currentTime`; or support and perform permanent tasks
                 pure evs
    runLeftEvents nextEvs

  where
    syncs : List (Event m) -> List Sync
    syncs evs = evs >>= \(Ev _ _ postponed) => map fst postponed

    uniqueSync : Lazy Sync
    uniqueSync = case syncs evs of
      []         => 0
      ss@(t::ts) => case foldl min t ts of
        S x => x                  -- either minimal minus 1
        Z   => S $ foldl max 0 ss -- or maximal plus 1

    newEvsAfterRunningCurr : m (List $ Event m)
    newEvsAfterRunningCurr = case currCoop of
      Point x                        => x $> Nil
      Cooperative l r                => pure [Ev currEvTime l postponed, Ev currEvTime r postponed]
      DelayedTill d                  => pure [Ev d (Point $ pure ()) postponed] -- this enables postponed to be run when appropriate (delayed)
      Sequential (Point y)         f => map (\r => [Ev currEvTime (f r) postponed]) y
      Sequential (Sequential y g)  f => pure [Ev currEvTime (Sequential y $ g >=> f) postponed]
      Sequential (DelayedTill d)   f => pure [Ev d (f ()) postponed]
      Sequential (Cooperative l r) f => let extP = (Force uniqueSync, (_ ** f ()))::postponed in
                                        pure [Ev currEvTime l extP, Ev currEvTime r extP]

    awakened : (evsAfterCurr : List $ Event m) -> List $ Event m
    awakened evsAfterCurr = let newLeftSyncs = syncs evsAfterCurr in
      map (\(_, (_ ** coop)) => Ev currEvTime coop []) . filter (not . flip elem newLeftSyncs . fst) $ postponed
