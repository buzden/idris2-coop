module Control.Monad.Coop

import public System.Time

import Data.List

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

Sync : Type
Sync = Nat

mutual

  record Postponed (0 m : Type -> Type) where
    constructor Postpone
    sync : Sync
    postCtx : CoopCtx m

  record CoopCtx (0 m : Type -> Type) where
    constructor Ctx
    coop : Coop m x
    -- Two present postponed events with the same sync are meant to be blocking each other.
    -- Postponed event needs to be sheduled only when all events with its sync are over.
    -- `Sync` type is a comparable type and is a workaround of uncomparability of `Coop`.
    joinCont : Maybe $ Postponed m

record Event (0 m : Type -> Type) where
  constructor Ev
  time : Time
  ctx  : CoopCtx m

-- The following comparison is only according to the time; this will incorrectly work for sets.
-- Equally timed events with different actions are considered to be equal with `==` relation.
[TimeOnly_EvEq] Eq (Event m) where
  (==) = (==) `on` time

[TimeOnly_EvOrd] Ord (Event m) using TimeOnly_EvEq where
  compare = compare `on` time

-- insert an element to a sorted list producing a sorted list
insertOrd : Ord a => a -> List a -> List a
insertOrd new []           = [new]
insertOrd new orig@(x::xs) = if new < x then new :: orig else x :: insertOrd new xs

export covering
runCoop : (Monad m, Timed m) => Coop m Unit -> m Unit
runCoop co = runLeftEvents [Ev !currentTime $ Ctx co Nothing] where

  %inline
  Events : (Type -> Type) -> Type
  Events m = List $ Event m

  -- TODO to replace list with a sortedness-preserving kinda-list
  covering
  runLeftEvents : Events m -> m Unit
  runLeftEvents [] = pure ()
  runLeftEvents evs@(ev@(Ev currEvTime $ Ctx currCoop currJoinCont)::restEvs) = do
    nextEvs <- if !currentTime >= currEvTime
               then do
                 let newLeftEvs = merge @{TimeOnly_EvOrd} restEvs !newEvsAfterRunningCurr
                 pure $ case awakened newLeftEvs of
                          Nothing => newLeftEvs
                          Just aw => insertOrd @{TimeOnly_EvOrd} aw newLeftEvs
               else
                 -- TODO else wait for the `currEvTime - !currentTime`; or support and perform permanent tasks
                 pure evs
    runLeftEvents nextEvs

  where
    syncs : Events m -> List Sync
    syncs evs = evs >>= \ev => syncs' ev.ctx where
      syncs' : CoopCtx m -> List Sync
      syncs' = maybe [] (\pp => pp.sync :: syncs' pp.postCtx) . joinCont

    uniqueSync : Lazy Sync
    uniqueSync = case syncs evs of
      []         => 0
      ss@(t::ts) => case foldl min t ts of
        S x => x                  -- either minimal minus 1
        Z   => S $ foldl max 0 ss -- or maximal plus 1

    -- All actions of form `patterm => pure [Ev ..., ...]` can be thought as a rewriting rule upon the list of events.
    newEvsAfterRunningCurr : m (Events m)
    newEvsAfterRunningCurr = case currCoop of
      Point x                        => x $> []
      Cooperative l r                => pure [{ctx.coop := l} ev, {ctx.coop := r} ev]
      DelayedTill d                  => pure [{time := d, ctx.coop := Point $ pure ()} ev]
      Sequential (Point y)         f => map (\r => [{ctx.coop := f r} ev]) y
      Sequential (Sequential y g)  f => pure [{ctx.coop := Sequential y $ g >=> f} ev]
      Sequential (DelayedTill d)   f => pure [{time := d, ctx.coop := f ()} ev]
      Sequential (Cooperative l r) f => let cont = Just $ Postpone uniqueSync $ {coop := f ()} ev.ctx in
                                            pure [{ctx := Ctx l cont} ev, {ctx := Ctx r cont} ev]

    awakened : (evsAfterCurr : Events m) -> Maybe $ Event m
    awakened evsAfterCurr = currJoinCont >>= \pp =>
      if pp.sync `elem` syncs evsAfterCurr
        then Nothing                             -- then someone else will raise this
        else Just $ {ctx := pp.postCtx} ev       -- no one that blocks is left

------------------------------
--- Interesting properties ---
------------------------------

0 run_unlifts : (Monad m, Timed m) => (x : m ()) -> runCoop (lift x) = x

0 run_seq_dep_lin : (Monad m, Timed m) => (x : m a) -> (y : a -> Coop m ()) -> runCoop (lift x >>= y) = x >>= Coop.runCoop . y

0 run_seq_indep_lin : (Monad m, Timed m) => (x, y : Coop m ()) -> runCoop (x >>= const y) = runCoop x >>= const (runCoop y)
