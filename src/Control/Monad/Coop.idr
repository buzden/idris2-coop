module Control.Monad.Coop

import public System.Time

import Data.Maybe
import Data.List
import Data.List.Lazy
import Data.SortedMap
import public Data.Zippable

import public Control.Monad.Spawn
import Control.Monad.State
import Control.Monad.State.Tuple
import Control.Monad.Trans

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
  map f x@(DelayedTill t)   = Sequential x $ Point . pure . f
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

Sync : Type
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
  postCtx : (contLTy, contRTy) -> CoopCtx m
  -- This postponed continuation is waining for two executions.
  -- When one of them is completed, the result should be present in this field.
  completedHalf : Maybe completedHaftTy

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
  Point x                        => lift x >>= tryToAwakenPostponed
  c@(Cooperative _ _)            => modify $ (::) $ {ctx.coop := c >>= pure} ev       -- manage as `Sequential (Cooperative _ _) _`
  c@(DelayedTill _)              => modify $ (::) $ {ctx.coop := c >> pure ()} ev     -- manage as `Sequential (DelayedTill _)   _`
  c@(Spawn _)                    => modify $ (::) $ {ctx.coop := c >> pure ()} ev     -- manage as `Sequential (Spawn _)         _`
  Sequential (Point x)         f => lift x >>= \r => modify $ (::) $ {ctx.coop := f r} ev
  Sequential (Sequential x g)  f => modify $ (::) $ {ctx.coop := Sequential x $ g >=> f} ev
  Sequential (DelayedTill d)   f => modify $ insertTimed $ {time := d, ctx.coop := f ()} ev
  Sequential (Spawn s)         f => modify $ \rest : Events m => {ctx.coop := s} ev :: {ctx.coop := f ()} ev :: rest
  Sequential (Cooperative l r) f => do uniqueSync <- newUniqueSync
                                       modify $ insert uniqueSync $ Postpone (\ab => {coop := f ab} ev.ctx) $ Nothing {ty=Unit}
                                       modify $ \rest : Events m =>
                                                  {ctx := Ctx l $ Just (uniqueSync, Left )} ev ::
                                                  {ctx := Ctx r $ Just (uniqueSync, Right)} ev ::
                                                  rest
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
              modify $ (::) $ {ctx := newCtx} ev
              put $ delete sy syncs
            Nothing =>
              put $ insert sy ({completedHalf := Just myHalf} pp) syncs

export covering
runCoop : CanSleep m => Monad m => Coop m Unit -> m Unit
runCoop co = evalStateT ([Ev !currentTime $ Ctx co Nothing], empty) runLeftEvents {stateType=(Events m, Syncs m)} where

  runLeftEvents : MonadTrans t => Monad (t m) => MonadState (Events m) (t m) => MonadState (Syncs m) (t m) => t m Unit
  runLeftEvents = case !(get {stateType=Events _}) of
    [] => pure ()
    evs@(currEv::restEvs) => do
      if !(lift currentTime) >= currEv.time
        then put restEvs *> runEvent currEv
        else lift $ sleepTill currEv.time -- TODO to support and perform permanent tasks
      runLeftEvents
