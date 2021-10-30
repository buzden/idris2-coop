module Control.Monad.Coop

import public System.Time

import Data.Maybe
import Data.List
import Data.List.Lazy

import Control.Monad.Trans

%default total

------------------
--- Interfaces ---
------------------

public export
interface Parallel m where
  -- Alternative-like operator with parallel semantics and unavailable results of separate computations
  (<||>) : m Unit -> m Unit -> m Unit

------------
--- Data ---
------------

export
data Coop : (m : Type -> Type) -> (a : Type) -> Type where
  Point       : m a -> Coop m a
  Sequential  : Coop m a -> (a -> Coop m b) -> Coop m b
  Cooperative : Coop m a -> Coop m b -> Coop m Unit
  DelayedTill : Time -> Coop m Unit

--------------------------------
--- Basic creation functions ---
--------------------------------

export
atomic : m a -> Coop m a
atomic = Point

-----------------------
--- Implementations ---
-----------------------

export
Timed m => Timed (Coop m) where
  currentTime = Point currentTime

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
  l <*> r = Sequential l (<$> r)
  -- This could be `(<*>) = Cooperative apply`, but it must be consistent with `(>>=)` definition.
  -- Consider code `doSmth *> delayedFor 100 *> doMore` comparing to `(doSmth <||> delayedFor 100) *> doMore`.
  -- Having parallel semantics for the `Applicative`'s `<*>`, those two examples above will mean the same, which seems to be unexpected.

export
Monad m => Monad (Coop m) where
  (>>=) = Sequential

export
Parallel (Coop m) where
  (<||>) = Cooperative

export
Monad m => DelayableTill (Coop m) where
  sleepTill = DelayedTill

export
(Timed m, Monad m) => DelayableFor (Coop m) where
  sleepFor t = sleepTill . (+t) =<< currentTime

export
HasIO (Coop IO) where
  liftIO = atomic

export
MonadTrans Coop where
  lift = atomic

-------------------
--- Interpreter ---
-------------------

--- Data types describing discrete events ---

Sync : Type
Sync = Nat

mutual

  record Postponed (m : Type -> Type) where
    constructor Postpone
    sync : Sync
    postCtx : CoopCtx m

  record CoopCtx (m : Type -> Type) where
    constructor Ctx
    coop : Coop m a
    -- Two present postponed events with the same sync are meant to be blocking each other.
    -- Postponed event needs to be sheduled only when all events with its sync are over.
    -- `Sync` type is a comparable type and is a workaround of uncomparability of `Coop`.
    joinCont : Maybe $ Postponed m

record Event (m : Type -> Type) where
  constructor Ev
  time : Time
  ctx  : CoopCtx m

--- List of events ---

%inline
Events : (Type -> Type) -> Type
Events m = List $ Event m

-- insert an element to a sorted list producing a sorted list
insertBy : (lt : a -> a -> Bool) -> a -> List a -> List a
insertBy _  new []           = [new]
insertBy lt new orig@(x::xs) = if new `lt` x then new :: orig else x :: insertBy lt new xs

%inline
insertTimed : Event m -> Events m -> Events m
insertTimed = insertBy $ (<) `on` time

--- Syncs stuff ---

covering
syncs : Events m -> LazyList Sync
syncs [] = []
syncs (ev::evs) = syncsOfCtx ev.ctx ++ syncs evs where
  syncsOfCtx : CoopCtx m -> LazyList Sync
  syncsOfCtx = maybe [] (\pp => pp.sync :: syncsOfCtx pp.postCtx) . joinCont

%inline
isSyncPresentIn : Events m -> Sync -> Bool
isSyncPresentIn evs sy = Lazy.any (== sy) $ syncs evs

newUniqueSync : LazyList Sync -> Sync
newUniqueSync [] = Z
newUniqueSync (x::xs) = case foldrLazy (\c, (mi, ma) => (mi `min` c, ma `max` c)) (x, 0) xs of
  (S x, _) => x   -- either minimal minus 1
  (Z  , y) => S y -- or maximal plus 1

--- The run loop ---

export covering
runCoop : Timed m => Monad m => Coop m Unit -> m Unit
runCoop co = runLeftEvents [Ev !currentTime $ Ctx co Nothing] where

  -- we could have `Event m -> m (Events m -> Events m)`
  runEvent : Event m -> (rest : Events m) -> m (Events m)
  runEvent ev@(Ev _ $ Ctx {}) rest = case ev.ctx.coop of
    Point x                        => x $> restWithAwakened
    Cooperative l r                => pure $ {ctx.coop := l} ev :: {ctx.coop := r} ev :: rest
    DelayedTill d                  => pure $ {time := d, ctx.coop := Point $ pure ()} ev `insertTimed` rest
    Sequential (Point x)         f => x <&> \r => {ctx.coop := f r} ev :: rest
    Sequential (Sequential x g)  f => pure $ {ctx.coop := Sequential x $ g >=> f} ev :: rest
    Sequential (DelayedTill d)   f => pure $ {time := d, ctx.coop := f ()} ev `insertTimed` rest
    Sequential (Cooperative l r) f => let uniqueSync = newUniqueSync $ syncs $ ev::rest in
                                      let cont = Just $ Postpone uniqueSync $ {coop := f ()} ev.ctx in
                                      pure $ {ctx := Ctx l cont} ev :: {ctx := Ctx r cont} ev :: rest
    where
      restWithAwakened : Events m
      restWithAwakened = case filter (not . isSyncPresentIn rest . sync) ev.ctx.joinCont of
        Nothing => rest                                 -- no postponed event or someone else will raise this
        Just pp => {ctx := pp.postCtx} ev :: rest       -- no one that blocks is left

  runLeftEvents : Events m -> m Unit
  runLeftEvents [] = pure ()
  runLeftEvents evs@(currEv::restEvs) = do
    nextEvs <- if !currentTime >= currEv.time
                 then runEvent currEv restEvs
                 else pure evs -- TODO to wait for the `currEvTime - !currentTime`; or support and perform permanent tasks
    runLeftEvents nextEvs

------------------------------
--- Interesting properties ---
------------------------------

0 run_unlifts : (Monad m, Timed m) => (x : m ()) -> runCoop (lift x) = x

0 run_seq_dep_lin : (Monad m, Timed m) => (x : m a) -> (y : a -> Coop m ()) -> runCoop (lift x >>= y) = x >>= Coop.runCoop . y

0 run_seq_indep_lin : (Monad m, Timed m) => (x, y : Coop m ()) -> runCoop (x >>= const y) = runCoop x >>= const (runCoop y)
