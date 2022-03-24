module Data.Queue1

import Data.List
import Data.List1
import Data.SnocList

%default total

-------------
--- Queue ---
-------------

-- add from right, remove from left
export
record Queue1 a where
  constructor MkQueue
  right : SnocList a
  left  : List1 a

export
singleton : a -> Queue1 a
singleton = MkQueue [<] . singleton

export
add : a -> Queue1 a -> Queue1 a
add x = { right $= (:< x) }

-- Constructs a `Queue1` which has all elements in the left part of the queue reverted
%inline
pureLeftsFromSnoc : SnocList a -> Maybe $ Queue1 a
pureLeftsFromSnoc = map (MkQueue [<]) . fromList . cast

export
remove : Queue1 a -> (a, Lazy (Maybe $ Queue1 a))
remove $ MkQueue r (head ::: l) = (head,) $ delay $ case l of
  []      => pureLeftsFromSnoc r
  (x::xs) => Just $ MkQueue r (x:::xs)

export
Functor Queue1 where
  map f = { left $= map f, right $= map f }

export
filter : (a -> Bool) -> Queue1 a -> Maybe $ Queue1 a
filter f $ MkQueue r l = do
  let filteredR = filter f r
  let Just filteredL = fromList $ filter f $ toList l
    | Nothing => pureLeftsFromSnoc filteredR
  Just $ MkQueue filteredR filteredL
