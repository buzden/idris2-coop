module ModelTimeRunner

import Control.Monad.Coop
import Control.Monad.ST
import Control.Monad.State

%default total

export covering
execM : (forall m. CanSleep m => Coop m a) -> Maybe a
execM c = runST $ evalStateT 0.seconds wrappedToST where

  Monad m => Timed (StateT Time m) where
    currentTime = get

  Monad m => CanSleep (StateT Time m) where
    sleepFor d = modify (+d)

  %hint STMonadRec : MonadRec (ST s)
  STMonadRec = believe_me $ the (MonadRec IO) %search

  wrappedToST : StateT Time (ST st) $ Maybe a
  wrappedToST = do
    ref <- lift $ newSTRef Nothing
    runCoop $ c >>= lift . lift . writeSTRef ref . Just
    lift $ readSTRef ref
