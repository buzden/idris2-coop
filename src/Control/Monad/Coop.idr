module Control.Monad.Coop

import public System.Time

import Data.Maybe
import Data.List
import Data.List1
import Data.List.Lazy
import Data.SortedMap
import public Data.Zippable

import public Control.Monad.Spawn
import Control.Monad.State
import Control.Monad.State.Tuple
import public Control.Monad.Trans

%default total

------------
--- Data ---
------------

export
data Coop : (m : Type -> Type) -> (a : Type) -> Type where
  Point       : m a -> Coop m a
  Sequential  : Coop m a -> (a -> Coop m b) -> Coop m b
  Cooperative : Coop m a -> Coop m b -> Coop m (a, b)
  DelayedTill : Time -> Coop m Unit
  Spawn       : Coop m Unit -> Coop m Unit

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
  map f x@(DelayedTill _)   = Sequential x $ Point . pure . f
  map f x@(Spawn _)         = Sequential x $ Point . pure . f

export
Applicative m => Applicative (Coop m) where
  pure    = Point . pure
  l <*> r = Sequential l (<$> r)
  -- This could be `(<*>) = Cooperative <&> uncurry apply`, but it must be consistent with `(>>=)` definition.
  -- Consider code `doSmth *> sleepFor 100 *> doMore` comparing to `(doSmth `zip` sleepFor 100) *> doMore`.
  -- Having parallel semantics for the `Applicative`'s `<*>`, those two examples above will mean the same, which seems to be unexpected.
  -- We have a special name instance `Concurrent` below for that case.

export
Applicative m => Monad (Coop m) where
  (>>=) = Sequential

export
Applicative m => Zippable (Coop m) where
  zip = Cooperative
  zipWith f = map (uncurry f) .: Cooperative

  zip3 a b c = a `Cooperative` (b `Cooperative` c)
  zipWith3 f a b c = zip3 a b c <&> \(x, y, z) => f x y z

  unzipWith f ab = (fst . f <$> ab, snd . f <$> ab)
  unzipWith3 f abc = (fst . f <$> abc, fst . snd . f <$> abc, snd . snd . f <$> abc)

export
[Concurrent] Applicative m => Applicative (Coop m) where
  pure  = Point . pure
  (<*>) = zipWith apply

export
Timed m => Applicative m => CanSleep (Coop m) where
  sleepTill = DelayedTill

export
Applicative m => CanSpawn (Coop m) where
  -- Runs the given computation in parallel with the monadic continuation.
  -- In contrast with `zip`, the continuations executes immediately, without waiting to the end of spawned computation.
  -- Spawned computation will continue to work (if it needs) even if continuation has ended.
  -- For example, running the following code
  --
  -- ```idris
  -- x : HasIO m => Coop m Nat
  -- x = do
  --   spawn $ do
  --     sleepFor 4.seconds
  --     putStrLn "spawned"
  --   putStrLn "main"
  --   pure 1
  -- ```
  --
  -- will result in returning `1` as the computation result **and** printing "spawned" in four seconds after funning the whole computation `x`.
  spawn = Spawn

export
HasIO (Coop IO) where
  liftIO = Point

export
MonadTrans Coop where
  lift = Point

-------------------
--- Interpreter ---
-------------------

--- Data types describing discrete events ---

0 Sync : Type
Sync = Nat

data LeftOrRight = Left | Right

record CoopCtx (m : Type -> Type) where
  constructor Ctx
  coop : Coop m actionRetTy
  -- Two present postponed events with the same sync are meant to be blocking each other.
  -- Postponed event needs to be sheduled only when all events with its sync are over.
  -- `Sync` type is a comparable type and is a workaround of uncomparability of `Coop`.
  joinSync : Maybe (Sync, LeftOrRight)

record Event (m : Type -> Type) where
  constructor Ev
  time : Time
  ctx  : CoopCtx m

--- List of events ---

0 Events : (Type -> Type) -> Type
Events = SortedMap Time . List1 . Event

insertTimed : Event m -> Events m -> Events m
insertTimed ev evs = insert ev.time (maybe (singleton ev) (cons ev) (lookup ev.time evs)) evs

-- Must be equivalent to `insertTimed ev empty`
singleEvent : Event m -> Events m
singleEvent ev = singleton ev.time $ singleton ev

earliestEvent : Events m -> Maybe (Event m, Lazy (Events m))
earliestEvent evs = leftMost evs <&> \(t, currEv ::: restTEvs) => (currEv,) $ maybe (delete t evs) (\r => insert t r evs) $ fromList restTEvs

--- Syncs stuff ---

record Postponed (m : Type -> Type) where
  constructor Postpone
  postCtx : (contLTy, contRTy) -> CoopCtx m
  -- This postponed continuation is waiting for two executions.
  -- When one of them is completed, the result should be present in this field.
  completedHalf : Maybe completedHalfTy

0 Syncs : (Type -> Type) -> Type
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
  Point x                        => lift x >>= tryToAwakenPostponed
  Sequential (Point x)         f => lift x >>= \r => modify $ insertTimed $ {ctx.coop := f r} ev
  Sequential (Sequential x g)  f => modify $ insertTimed $ {ctx.coop := Sequential x $ g >=> f} ev
  Sequential (DelayedTill d)   f => modify $ insertTimed $ {time := d, ctx.coop := f ()} ev
  Sequential (Spawn s)         f => modify $ insertTimed ({ctx.coop := s} ev) . insertTimed ({ctx.coop := f ()} ev)
  Sequential (Cooperative l r) f => do uniqueSync <- newUniqueSync
                                       modify $ insert uniqueSync $ Postpone (\ab => {coop := f ab} ev.ctx) $ Nothing {ty=Unit}
                                       modify $ insertTimed ({ctx := Ctx l $ Just (uniqueSync, Left )} ev)
                                              . insertTimed ({ctx := Ctx r $ Just (uniqueSync, Right)} ev)
  -- The rest is meant to be non-`Sequential` and non-`Point`
  c                              => modify $ insertTimed $ {ctx.coop := c >>= pure} ev       -- manage as `Sequential _ Point`

  where
    tryToAwakenPostponed : forall a. a -> t m Unit
    tryToAwakenPostponed myHalf =
      whenJust ev.ctx.joinSync $ \(sy, iAmLOrR) => do
        syncs <- get
        whenJust (SortedMap.lookup sy syncs) $ \pp =>
          case pp.completedHalf of
            Just theirHalf => do
              let newCtx : CoopCtx m = case iAmLOrR of
                                         Left  => pp.postCtx $ believe_me (myHalf, theirHalf)
                                         Right => pp.postCtx $ believe_me (theirHalf, myHalf)
              modify $ insertTimed $ {ctx := newCtx} ev
              put $ delete sy syncs
            Nothing =>
              put $ insert sy ({completedHalf := Just myHalf} pp) syncs

export covering
runCoop : CanSleep m => Monad m => Coop m Unit -> m Unit
runCoop co = evalStateT (singleEvent $ Ev !currentTime $ Ctx co Nothing, empty) runLeftEvents {stateType=(_, Syncs m)} where

  runLeftEvents : MonadTrans t => Monad (t m) => MonadState (Events m) (t m) => MonadState (Syncs m) (t m) => t m Unit
  runLeftEvents =
    whenJust (earliestEvent !get) $ \(currEv, restEvs) => do
      if !(lift currentTime) >= currEv.time
        then put restEvs *> runEvent currEv
        else lift $ sleepTill currEv.time -- TODO to support and perform permanent tasks
      runLeftEvents
