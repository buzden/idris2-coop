module Control.Monad.Coop

import public System.Time

import Data.Maybe
import Data.List
import Data.List.Lazy
import Data.SortedMap

import Control.Monad.State
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

export
Applicative m => Functor (Coop m) where
  map f (Point a)           = Point (map f a)
  map f (Sequential a b)    = Sequential a $ \ar => map f $ b ar
  map f x@(Cooperative _ _) = Sequential x $ Point . pure . f
  map f x@(DelayedTill t)   = Sequential x $ Point . pure . f

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

record CoopCtx (m : Type -> Type) where
  constructor Ctx
  coop : Coop m a
  -- Two present postponed events with the same sync are meant to be blocking each other.
  -- Postponed event needs to be sheduled only when all events with its sync are over.
  -- `Sync` type is a comparable type and is a workaround of uncomparability of `Coop`.
  joinSync : Maybe Sync

record Event (m : Type -> Type) where
  constructor Ev
  time : Time
  ctx  : CoopCtx m

--- List of events ---

%inline
Events : (Type -> Type) -> Type
Events = List . Event

-- insert an element to a sorted list producing a sorted list
insertBy : (lt : a -> a -> Bool) -> a -> List a -> List a
insertBy _  new []           = [new]
insertBy lt new orig@(x::xs) = if new `lt` x then new :: orig else x :: insertBy lt new xs

%inline
insertTimed : Event m -> Events m -> Events m
insertTimed = insertBy $ (<) `on` time

--- Syncs stuff ---

record Postponed (m : Type -> Type) where
  constructor Postpone
  postCtx : CoopCtx m
  -- This postponed continuation is waining for two executions.
  -- When one of them is completed, this must be `True`.
  oneIsCompleted : Bool

Syncs : (Type -> Type) -> Type
Syncs = SortedMap Sync . Postponed

newUniqueSync : MonadState (Syncs m) f => f Sync
newUniqueSync = do
  syncs <- get
  pure $ case fst <$> leftMost syncs of
    Nothing    => Z
    Just (S x) => x                                       -- either minimal minus 1
    Just Z     => maybe Z (S . fst) $ rightMost syncs     -- or maximal plus 1

--- The run loop ---

%inline
runEvent : Monad m => MonadTrans t => Monad (t m) =>
           MonadState (Events m) (t m) =>
           MonadState (Syncs m) (t m) =>
           Event m -> t m Unit
runEvent ev@(Ev _ $ Ctx {}) = case ev.ctx.coop of
  Point x                        => lift x *> tryToAwakenPostponed
  c@(Cooperative _ _)            => modify $ (::) $ {ctx.coop := c >> pure ()} ev     -- manage as `Sequential (Cooperative _ _) _`
  c@(DelayedTill _)              => modify $ (::) $ {ctx.coop := c >> pure ()} ev     -- manage as `Sequential (DelayedTill _)   _`
  Sequential (Point x)         f => lift x >>= \r => modify $ (::) $ {ctx.coop := f r} ev
  Sequential (Sequential x g)  f => modify $ (::) $ {ctx.coop := Sequential x $ g >=> f} ev
  Sequential (DelayedTill d)   f => modify $ insertTimed $ {time := d, ctx.coop := f ()} ev
  Sequential (Cooperative l r) f => do uniqueSync <- newUniqueSync
                                       modify $ insert uniqueSync $ Postpone ({coop := f ()} ev.ctx) False
                                       modify $ \rest : Events m => {ctx := Ctx l $ Just uniqueSync} ev :: {ctx := Ctx r $ Just uniqueSync} ev :: rest
  where
    tryToAwakenPostponed : t m Unit
    tryToAwakenPostponed =
      whenJust (ev.ctx.joinSync) $ \sy => do
        syncs <- get
        whenJust (lookup sy syncs) $ \pp =>
          if pp.oneIsCompleted
          then do                                     -- no one that blocks is left
            modify $ (::) $ {ctx := pp.postCtx} ev
            put $ delete sy syncs
          else                                        -- someone else will raise this continuation
            put $ insert sy ({oneIsCompleted := True} pp) syncs

Monad m => MonadState l (StateT (l, r) m) where
  get = Builtin.fst <$> get
  put = modify . mapFst . const

Monad m => MonadState r (StateT (l, r) m) where
  get = Builtin.snd <$> get
  put = modify . mapSnd . const

export covering
runCoop : Timed m => Monad m => Coop m Unit -> m Unit
runCoop co = evalStateT ([Ev !currentTime $ Ctx co Nothing], empty) runLeftEvents {stateType=(Events m, Syncs m)} where

  runLeftEvents : MonadTrans t => Monad (t m) => MonadState (Events m) (t m) => MonadState (Syncs m) (t m) => t m Unit
  runLeftEvents = case !(get {stateType=Events _}) of
    [] => pure ()
    evs@(currEv::restEvs) => do
      if !(lift currentTime) >= currEv.time
        then put restEvs *> runEvent currEv
        else pure () -- TODO to wait for the `currEvTime - !currentTime`; or support and perform permanent tasks
      runLeftEvents

------------------------------
--- Interesting properties ---
------------------------------

0 run_unlifts : (Monad m, Timed m) => (x : m ()) -> runCoop (lift x) = x

0 run_seq_dep_lin : (Monad m, Timed m) => (x : m a) -> (y : a -> Coop m ()) -> runCoop (lift x >>= y) = x >>= Coop.runCoop . y

0 run_seq_indep_lin : (Monad m, Timed m) => (x, y : Coop m ()) -> runCoop (x >>= const y) = runCoop x >>= const (runCoop y)
