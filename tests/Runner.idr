module Runner

import BaseDir

import Test.Golden.RunnerHelper

main : IO ()
main = goldenRunner
  [ "Sequential execution"                           `atDir` "coop-monad/seq"
  , "Concurrent execution with result merging"       `atDir` "coop-monad/zip"
  , "Concurrent execution with race"                 `atDir` "coop-monad/race"
  , "Spawning"                                       `atDir` "coop-monad/spawn"
  , "Empty computation"                              `atDir` "coop-monad/empty"
  , "Documentation"                                  `atDir` "docs"
  ]
