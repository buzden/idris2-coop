module Data.Queue1.Properties

import Data.List
import Data.List1
import Data.SnocList
import Data.Queue1

import Syntax.WithProof

%default total

------------------
--- Properties ---
------------------

export
toList1_singleton : (x : a) -> Queue1.toList1 (singleton x) = singleton x
toList1_singleton x = Refl

export
toList1_add : (q : Queue1 a) -> (xs : List a) -> Queue1.toList1 (foldl (flip Queue1.add) q xs) === (toList1 q `appendl` xs)
toList1_add q [] with 0 (toList1 q)
  toList1_add q [] | (_:::xs) = rewrite appendNilRightNeutral xs in Refl
toList1_add (MkQueue right left@(l:::ls)) (y :: ys) = do
  rewrite toList1_add (MkQueue (right :< y) left) ys
  rewrite sym $ appendAssociative ls (right <>> [y]) ys
  rewrite chipsAsListAppend right [y]
  rewrite sym $ appendAssociative (cast right) [y] ys
  rewrite appendAssociative ls (cast right) (y::ys)
  Refl

export
toList1_filter : (f : a -> Bool) -> (q : Queue1 a) -> Queue1.toList1 <$> filter f q = filter f (Queue1.toList1 q)
toList1_filter f (MkQueue right left) = do
  rewrite toListAppendl left (cast right)
  rewrite filterAppend f (forget left) (cast right)
  rewrite fromListAppend (filter f $ forget left) (filter f $ cast right)
  case @@ fromList (filter f $ forget left) of
    (Nothing ** prfL) => do
      rewrite prfL
      rewrite toListFilter f right
      case @@ fromList (filter f $ cast right) of
        (Nothing ** prfR) => rewrite prfR in Refl
        (Just r1 ** prfR) => do
          rewrite prfR
          rewrite appendlNilRightNeutral r1
          Refl
    (Just l1 ** prfL) => do
      rewrite prfL
      rewrite toListFilter f right
      case @@ filter f (toList right) of
        ([] ** prfR) => do
          rewrite prfR
          rewrite appendlNilRightNeutral l1
          Refl
        (_::_ ** prfR) => rewrite prfR in Refl
