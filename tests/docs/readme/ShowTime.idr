module ShowTime

import Data.Nat

import System.Time

%default total

showApprox : Nat -> String
showApprox ms = show $ ms `minus` modNatNZ ms 100 %search

export
Show Time where
  show t = showApprox t.asMillis

export
Show FinDuration where
  show t = showApprox t.asMillis
