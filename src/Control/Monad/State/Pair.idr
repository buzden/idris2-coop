module Control.Monad.State.Pair

import public Control.Monad.State

export
Monad m => MonadState l (StateT (l, r) m) where
  get = Builtin.fst <$> get
  put = modify . mapFst . const

export
Monad m => MonadState r (StateT (l, r) m) where
  get = Builtin.snd <$> get
  put = modify . mapSnd . const
