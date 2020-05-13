module Arduino.Util

import Data.Vect

%default total
%access public export

--------------------------------------------------------
--- Type-level massive disjunctions and conjunctions ---
--------------------------------------------------------

data OneOf : Vect n Type -> Type where
  MkOneOf : {types : Vect n Type} -> (idx : Fin n) -> index idx types -> OneOf types

data AllOf : Vect n Type -> Type where
  Nil  :                               AllOf []
  (::) : {x : Type} -> x -> AllOf v -> AllOf (x::v)

------------------------------------------------------
--- Special dependent pairs with nice constructors ---
------------------------------------------------------

-- Dependent pair `(x : a ** guarantee x)` with `auto` `guarantee` parameter in constructor.
data BoundedWith : (a : Type) -> (a -> Type) -> Type where
  MkBounded : {guarantee : a -> Type} -> (x : a) -> {auto ev : guarantee x} -> a `BoundedWith` guarantee

-- Dependent pair `(n : Nat ** Vect n a)` with implicit size in constructor.
data KnownCountOf : Type -> Type where
  MkKnownCountOf : {n : Nat} -> Vect n a -> KnownCountOf a
