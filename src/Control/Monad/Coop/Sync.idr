module Control.Monad.Coop.Sync

import Data.SortedMap

%default total

--- Stuff for runtime syncronisation between discrete events ---

export
record Sync (0 t : k) where
  constructor Sy
  unSy : Nat

export %inline
Eq (Sync sk) where
  (==) = (==) `on` unSy

export %inline
Ord (Sync sk) where
  compare = compare `on` unSy

export
newUniqueSync : SortedMap (Sync sk) whatever -> Sync sk
newUniqueSync syncs = Sy $ case unSy . fst <$> leftMost syncs of
  Nothing    => Z
  Just (S x) => x                                              -- either minimal minus 1
  Just Z     => maybe Z (S . unSy . fst) $ rightMost syncs     -- or maximal plus 1
