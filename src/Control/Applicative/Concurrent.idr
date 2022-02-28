module Control.Applicative.Concurrent

%default total

public export
interface ConcurrentApplicative m where
  Concurrent : Applicative m
