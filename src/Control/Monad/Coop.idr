module Control.Monad.Coop

import public System.Time

import Data.List
import Data.Queue1
import Data.SortedMap
import Data.SortedSet
import public Data.Zippable

import public Control.Applicative.Concurrent

import Control.Monad.Coop.Sync
import public Control.Monad.Spawn
import Control.Monad.State
import Control.Monad.State.Tuple
import public Control.Monad.Trans
import public Control.MonadRec

%default total

------------
--- Data ---
------------

data SyncKind = Join | Race

export
data Coop : (m : Type -> Type) -> (a : Type) -> Type where
  Point       : m a -> Coop m a
  Sequential  : Coop m a -> (a -> Coop m b) -> Coop m b
  Interleaved : Coop m a -> Coop m b -> Coop m (a, b)
  Racing      : Coop m a -> Coop m a -> Coop m a
  RaceFence   : (prevRaceSync : Maybe $ Sync Race) -> Coop m Unit
  DelayedTill : Time -> Coop m Unit
  Spawn       : Coop m Unit -> Coop m Unit
  Empty       : Coop m a

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
  map f x@(Interleaved _ _) = Sequential x $ Point . pure . f
  map f x@(Racing _ _)      = Sequential x $ Point . pure . f
  map f x@(RaceFence _)     = Sequential x $ Point . pure . f
  map f x@(DelayedTill _)   = Sequential x $ Point . pure . f
  map f x@(Spawn _)         = Sequential x $ Point . pure . f
  map _ Empty               = Empty

export
Applicative m => Applicative (Coop m) where
  pure    = Point . pure
  l <*> r = Sequential l (<$> r)
  -- This could be `(<*>) = Interleaved <&> uncurry apply`, but it must be consistent with `(>>=)` definition.
  -- Consider code `doSmth *> sleepFor 100 *> doMore` comparing to `(doSmth `zip` sleepFor 100) *> doMore`.
  -- Having parallel semantics for the `Applicative`'s `<*>`, those two examples above will mean the same, which seems to be unexpected.
  -- We have a special name instance `Concurrent` below for that case.

export
race : Applicative m => Coop m a -> Coop m b -> Coop m $ Either a b
race l r = Racing (l <&> Left) (r <&> Right)

export
Applicative m => Alternative (Coop m) where
  -- `empty` computation is like an infinite computation (i.e. no computation goes *after* it and its result cannot be analysed),
  -- but in contrast, if it is the only what is left during the execution, computation simply finishes.
  empty = Empty
  l <|> r = l `Racing` r

export
Applicative m => Monad (Coop m) where
  (>>=) = Sequential

-- This implementation is like a `NonTailRec` from `MonadRec`,
-- but this is actually safe, since `>>=` returns immediately,
-- because the whole `Coop` data structure is lazy on binding.
export
Applicative m => MonadRec (Coop m) where
  tailRecM x (Access acc) st next =
    next x st `Sequential` \case
      Cont seed2 prf vst => tailRecM seed2 (acc seed2 prf) vst next
      Done vres          => Point $ pure vres

export
Applicative m => Zippable (Coop m) where
  zip = Interleaved
  zipWith f = map (uncurry f) .: Interleaved

  zip3 a b c = a `Interleaved` (b `Interleaved` c)
  zipWith3 f a b c = zip3 a b c <&> \(x, y, z) => f x y z

  unzipWith f ab = (fst . f <$> ab, snd . f <$> ab)
  unzipWith3 f abc = (fst . f <$> abc, fst . snd . f <$> abc, snd . snd . f <$> abc)

[Conc] Applicative m => Applicative (Coop m) where
  pure  = Point . pure
  (<*>) = zipWith apply

export
Applicative m => ConcurrentApplicative (Coop m) where
  Concurrent = Conc

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

export covering
forever : Monad m => m a -> m b
forever x = ignore x >> forever x

-------------------
--- Interpreter ---
-------------------

--- Data types describing discrete events ---

data LeftOrRight = Left | Right

record Event (m : Type -> Type) where
  constructor Ev
  time : Time
  coop : Coop m actionRetTy
  -- Two present postponed events with the same sync are meant to be blocking each other.
  -- Postponed event needs to be sheduled only when all events with its sync are over.
  -- `Sync` type is a comparable type and is a workaround of uncomparability of `Coop`.
  joinSync : Maybe (Sync Join, LeftOrRight)
  raceSync : Maybe $ Sync Race

--- List of events ---

0 Events : (Type -> Type) -> Type
Events = SortedMap Time . Queue1 . Event

insertTimed : Event m -> Events m -> Events m
insertTimed ev evs = insert ev.time (maybe (singleton ev) (add ev) (lookup ev.time evs)) evs

-- Must be equivalent to `insertTimed ev empty`
singleEvent : Event m -> Events m
singleEvent ev = singleton ev.time $ singleton ev

addEvents : MonadState (Events m) n => Event m -> List (Event m -> Event m) -> n Unit
addEvents ev = modify . foldl (\acc, modF => acc . insertTimed (modF ev)) id

-- Psrticular case for `addEvents ev [modF]`
addEvent : MonadState (Events m) n => Event m -> (Event m -> Event m) -> n Unit
addEvent ev modF = modify $ insertTimed $ modF ev

-- Psrticular case for `addEvents ev [modF1, modF2]`
addEvent2 : MonadState (Events m) n => Event m -> (Event m -> Event m) -> (Event m -> Event m) -> n Unit
addEvent2 ev modF1 modF2 = modify $ insertTimed (modF1 ev) . insertTimed (modF2 ev)

earliestEvent : Events m -> Maybe (Event m, Lazy (Events m))
earliestEvent evs = leftMost evs <&> \(t, tEvs) =>
  let (currEv, restTEvs) = remove tEvs in
  (currEv,) $ maybe (delete t evs) (\r => insert t r evs) restTEvs

filterEvents : (Event m -> Bool) -> Events m -> Events m
filterEvents f = fromList . mapMaybe (\(t, evs) => (t,) <$> filter f evs) . SortedMap.toList

--- Join synchronisation stuff ---

record Postponed (m : Type -> Type) where
  constructor Postpone
  postCoop : (contLTy, contRTy) -> Coop m contRetTy
  postJoinSync : Maybe (Sync Join, LeftOrRight)
  -- This postponed continuation is waiting for two executions.
  -- When one of them is completed, the result should be present in this field.
  completedHalf : Maybe completedHalfTy

0 JoinSyncs : (Type -> Type) -> Type
JoinSyncs = SortedMap (Sync Join) . Postponed

--- Race synchronisation stuff ---

-- Map from one race sync to all child syncs (i.e. those which are cancelled when a series with the parent sync finished)
0 RaceSyncs : Type
RaceSyncs = SortedMap (Sync Race) $ List $ Sync Race

transitiveLookup : Foldable f => Ord a => SortedMap a (f a) -> a -> SortedSet a
transitiveLookup mp x = let x1 = singleton x in go x1 x1 where
  go : (curr : SortedSet a) -> (new : SortedSet a) -> SortedSet a
  go curr new = if null new then curr else do
    let allNexts = fromList $ SortedSet.toList new >>= maybe [] toList . flip SortedMap.lookup mp
    let nextNew = allNexts `difference` curr
    assert_total $ go (curr `union` nextNew) nextNew -- first argument is growing and has maximum bound (all `a` in the `mp`)

--- The run loop ---

%inline
runEvent : Monad m => MonadTrans t => Monad (t m) =>
           MonadState (Events m) (t m) =>
           MonadState (JoinSyncs m) (t m) =>
           MonadState RaceSyncs (t m) =>
           Event m -> t m Unit
runEvent ev = case ev.coop of
  Point x          => lift x >>= awakePostponed
  Sequential lhs f => case lhs of
    Point x         => lift x >>= \r => addEvent ev {coop := f r}
    Sequential x g  => addEvent ev {coop := Sequential x $ g >=> f}
    DelayedTill d   => addEvent ev {time := d, coop := f ()}
    Spawn s         => addEvent2 ev {coop := s, joinSync := Nothing} {coop := f ()}
    Interleaved l r => do uniqueSync <- newUniqueSync <$> get
                          modify $ insert uniqueSync $ Postpone f ev.joinSync $ Nothing {ty=Unit}
                          addEvent2 ev
                            {coop := l, joinSync := Just (uniqueSync, Left )}
                            {coop := r, joinSync := Just (uniqueSync, Right)}
    RaceFence prevS => finishRaces *> addEvent ev {coop := f (), raceSync := prevS}
    Racing Empty r  => addEvent ev {coop := r >>= f}
    Racing l Empty  => addEvent ev {coop := l >>= f}
    Racing l r      => do uniqueSync <- newUniqueSync <$> get
                          modify $ insert uniqueSync [] -- to prevent generation of the same sync
                          whenJust ev.raceSync $ \parent => modify $ merge $ singleton parent [uniqueSync]
                          addEvent2 ev
                            {coop := l >>= (RaceFence ev.raceSync *>) . f, raceSync := Just uniqueSync}
                            {coop := r >>= (RaceFence ev.raceSync *>) . f, raceSync := Just uniqueSync}
    Empty           => pure ()
  nonSeqNonPoint   => addEvent ev {coop := nonSeqNonPoint >>= pure}       -- manage as `Sequential _ Point`

  where

    awakePostponed : forall a. a -> t m Unit
    awakePostponed myHalf =
      whenJust ev.joinSync $ \(sy, iAmLOrR) => do
        syncs <- get
        whenJust (SortedMap.lookup sy syncs) $ \pp =>
          case pp.completedHalf of
            Just theirHalf => do
              let awakenCoop = pp.postCoop $ case iAmLOrR of
                    Left  => believe_me (myHalf, theirHalf)
                    Right => believe_me (theirHalf, myHalf)
              addEvent ev {coop := awakenCoop, joinSync := pp.postJoinSync}
              put $ delete sy syncs
            Nothing =>
              put $ insert sy ({completedHalf := Just myHalf} pp) syncs

    finishRaces : t m Unit
    finishRaces = whenJust ev.raceSync $ \currRaceSync => do
      raceSyncs <- get
      let syncsToRemove = transitiveLookup raceSyncs currRaceSync
      modify $ filterEvents $ maybe True (not . flip contains syncsToRemove) . raceSync
      put $ foldl (flip SortedMap.delete) raceSyncs syncsToRemove

export covering
runCoop : MonadRec m => CanSleep m => Coop m Unit -> m Unit
runCoop co = do
  let initEvents = singleEvent $ Ev !currentTime co Nothing Nothing
      initJoinSyncs : JoinSyncs m = empty
      initRaceSyncs : RaceSyncs = empty
  evalStateT (initEvents, initJoinSyncs, initRaceSyncs) runLeftEvents where

  covering WellFounded () Equal where wellFounded = wellFounded

  runLeftEvents : MonadTrans t => MonadRec (t m) =>
                  MonadState (Events m) (t m) =>
                  MonadState (JoinSyncs m) (t m) =>
                  MonadState RaceSyncs (t m) =>
                  t m Unit
  runLeftEvents = trWellFounded () () $ \(), () => do
    case earliestEvent !get of
      Nothing => pure $ Done ()
      Just (currEv, restEvs) => do
        currTime <- lift currentTime
        if currTime >= currEv.time
          then put restEvs *> runEvent ({time := currTime} currEv)
          else lift $ sleepTill currEv.time -- TODO to support and perform permanent tasks
        pure $ Cont () Refl ()
