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

  -- Curry-Howard coding of a short-circuiting conjunction
  data AndThen : (l : Type) -> (r : l -> Type) -> Type where
    ShortConj : (v : l) -> (w : r v) -> l `AndThen` r

  -- Short-circuting disjunction
  data OrElse : (l : Type) -> (r : (l -> Void) -> Type) -> Type where
    MkLeft  :                      (v : l)     -> l `OrElse` r
    MkRight : (noL : l -> Void) -> (w : r noL) -> l `OrElse` r

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

Debounded : a `That` prop -> a
Debounded (Bounded x) = x

SummonAuto : {auto a : t} -> t
SummonAuto {a} = a

-------------------
--- Combinators ---
-------------------

infixr 9 .., ...

-- Beloved "blackbird" combinator
(..) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(..) = (.) . (.)

(...) : (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(...) = (.) . (..)
