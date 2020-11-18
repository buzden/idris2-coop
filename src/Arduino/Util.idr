module Arduino.Util

import Data.Vect
import Data.Vect.Quantifiers

%default total

--------------------------------------------------------
--- Type-level massive disjunctions and conjunctions ---
--------------------------------------------------------

namespace Logic

  public export
  data OneOf : Vect n Type -> Type where
    MkOneOf : (idx : Fin n) -> index idx types -> OneOf types

  public export
  data AllOf : Vect n Type -> Type where
    Nil  :                               AllOf []
    (::) : {x : Type} -> x -> AllOf v -> AllOf (x::v)

  -- Curry-Howard coding of a short-circuiting conjunction
  public export
  data AndThen : (l : Type) -> (r : l -> Type) -> Type where
    ShortConj : (v : l) -> (w : r v) -> l `AndThen` r

  -- Short-circuting disjunction
  public export
  data OrElse : (l : Type) -> (r : (l -> Void) -> Type) -> Type where
    MkLeft  :                      (v : l)     -> l `OrElse` r
    MkRight : (noL : l -> Void) -> (w : r noL) -> l `OrElse` r

----------------------------------------------
--- Additional quantifiers for collections ---
----------------------------------------------

namespace Quantif

  public export
  data AllDiffer : Vect n a -> Type where
    Nil  : AllDiffer []
    (::) : {xs : Vect n a} -> (x : a) -> {auto ev : All (Not . \u => u = x) xs} -> AllDiffer xs -> AllDiffer (x::xs)

----------------------------------
--- "Easier constructor" stuff ---
----------------------------------

-- Dependent pair `(x : a ** guarantee x)` with `auto` `guarantee` parameter in constructor.
public export
data That : (a : Type) -> (a -> Type) -> Type where
  Bounded : (x : a) -> {auto ev : prop x} -> a `That` prop

public export
Debounded : a `That` prop -> a
Debounded (Bounded x) = x

public export
SummonAuto : {auto a : t} -> t
SummonAuto {a} = a

-------------------
--- Combinators ---
-------------------

infixr 9 ..., ....

-- Beloved "blackbird" combinator
public export
%inline
(...) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

public export
%inline
(....) : (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(....) = (.) . (...)

--- Functor stuff ---

public export
unit : Functor f => f a -> f ()
unit = map $ const ()
