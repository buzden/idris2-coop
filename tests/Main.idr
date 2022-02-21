module Main

import Data.String

import Test.Golden

atDir : (poolName : String) -> (dir : String) -> IO TestPool
atDir poolName dir = testsInDir dir (not . isPrefixOf "_") poolName [] Nothing

main : IO ()
main = runner
  [ !( "Sequential execution"                           `atDir` "coop-monad/seq"      )
  , !( "Concurrent execution with result merging"       `atDir` "coop-monad/zip"      )
  , !( "Spawning"                                       `atDir` "coop-monad/spawn"    )
  ]
