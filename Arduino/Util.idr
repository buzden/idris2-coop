module Arduino.Util

import Data.Vect
import Data.Vect.Quantifiers

%default total
%access public export

--------------------------------------------------------
--- Type-level massive disjunctions and conjunctions ---
--------------------------------------------------------

namespace Logic

  data OneOf : Vect n Type -> Type where
    MkOneOf : (idx : Fin n) -> index idx types -> OneOf types

  data AllOf : Vect n Type -> Type where
    Nil  :                               AllOf []
    (::) : {x : Type} -> x -> AllOf v -> AllOf (x::v)

----------------------------------------------
--- Additional quantifiers for collections ---
----------------------------------------------

namespace Quantif

  data AllDiffer : Vect n a -> Type where
    Nil  : AllDiffer []
    (::) : {xs : Vect n a} -> (x : a) -> {auto ev : All (Not . \u => u = x) xs} -> AllDiffer xs -> AllDiffer (x::xs)

----------------------------------
--- "Easier constructor" stuff ---
----------------------------------

-- Dependent pair `(x : a ** guarantee x)` with `auto` `guarantee` parameter in constructor.
data That : (a : Type) -> (a -> Type) -> Type where
  Bounded : (x : a) -> {auto ev : prop x} -> a `That` prop

SummonAuto : {auto a : t} -> t
SummonAuto {a} = a
