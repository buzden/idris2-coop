module Arduino.StateT

import Arduino.Util

%default total

----------------------
--- Data structure ---
----------------------

export
data StateT : Type -> (Type -> Type) -> Type -> Type where
  MkStateT : {m : Type -> Type} -> (s -> m (a, s)) -> StateT s m a

-----------------
--- Functions ---
-----------------

export
runStateT : StateT s m a -> s -> m (a, s)
runStateT (MkStateT f) = f

export
evalStateT : Functor m => StateT s m a -> s -> m a
evalStateT = map fst .. runStateT

export
lift : Functor m => m a -> StateT s m a
lift m = MkStateT $ \s => map (\a => (a, s)) m

------------------
--- Interfaces ---
------------------

public export
interface Applicative m => ApplicativeState s (m : Type -> Type) where
  get : m s
  put : s -> m ()

export
gets : ApplicativeState s m => (s -> a) -> m a
gets f = map f get

export
modify : (Monad m, ApplicativeState s m) => (s -> s) -> m ()
modify f = get >>= put . f

export
modifyM : (Monad m, ApplicativeState s m) => (s -> m s) -> m ()
modifyM f = get >>= f >>= put

-----------------------
--- Implementations ---
-----------------------

export
Functor m => Functor (StateT s m) where
  map f (MkStateT stf) = MkStateT $ map (\(a, s) => (f a, s)) . stf

export
Monad m => Applicative (StateT s m) where
  pure = lift . pure
  (MkStateT lf) <*> (MkStateT rf) = MkStateT $ \s => do (ab, s') <- lf s
                                                        (a, s'') <- rf s'
                                                        pure (ab a, s'')
export
Monad m => Monad (StateT s m) where
  (MkStateT lf) >>= f = MkStateT $ \s => do (a, s') <- lf s
                                            let MkStateT rf = f a
                                            rf s'
export
Monad m => ApplicativeState s (StateT s m) where
  get   = MkStateT $ \s => pure (s, s)
  put s = MkStateT $ \_ => pure ((), s)
