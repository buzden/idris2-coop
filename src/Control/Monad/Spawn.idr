module Control.Monad.Spawn

%default total

public export
interface Monad m => CanSpawn m where
  spawn : m Unit -> m Unit
