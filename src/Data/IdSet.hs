--------------------------------------------------------------------------------
-- |
-- Module      : Data.IdSet
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- A set where each element is identified by a unique identifier. As this is a
-- set, it never contains duplicate elements. The 'Ord' trait is used to
-- determine element equivalence. If the values are not totally ordered, see
-- 'Data.IdHashSet' instead.
--
-- /Warning/: The size of a 'IdSet' must not exceed @maxBound::Int@. Violation
-- is /not/ checked and causes undefined behavior.
--------------------------------------------------------------------------------

module Data.IdSet
  ( -- * Types
    Identifier
  , IdSet
    -- * Construction
  , empty
  , singleton
    -- * Query
  , null
  , size
  , lookup
  , lookupKey
    -- * Update
  , insert
  , insertAll
  , filter
    -- * Traversal
  , map
    -- * Conversion
  , keys
  , elems
  , entries
  , fromList
  , toIntMap
  ) where

-- Stdlib imports
import           Prelude hiding ( null, lookup, map, filter )
import qualified Data.List as List
-- External library imports
import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy ( IntMap )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map )
import           Control.DeepSeq ( NFData ( rnf ) )
-- Local imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )


-- # Types #

-- | Uniquely identifies elements in an `IdSet`.
type Identifier = Int

-- | A list of values with identifiers
data IdSet a =
  IdSet {
    iData    :: IdList a
  , iInvMap  :: Map a Identifier
  }


-- # Construction #

-- | /O(1)/. The empty 'IdSet'.
empty :: IdSet a
empty = IdSet IdList.empty Map.empty

-- | /O(1)/. An 'IdSet' with a single element. Also returns the identifier of
-- that element.
singleton :: a -> (Identifier, IdSet a)
singleton v =
  let (i, s) = IdList.singleton v
  in (i, IdSet s (Map.singleton v i))


-- # Query #

-- | /O(1)/. Returns 'True' iff the set is empty.
null :: IdSet a -> Bool
null = IdList.null . iData

-- | /O(1)/. The size of the set.
size :: IdSet a -> Int
size = IdList.size . iData

-- | /O(log n)/. Finds the entry for the given identifier. Returns `Nothing` if
-- it does not exist.
lookup :: Identifier -> IdSet a -> Maybe a
lookup k = IdList.lookup k . iData

-- | /O(log n)/. Returns the key of the element if it is present in the set;
  -- otherwise `Nothing` is returned.
lookupKey :: Ord a => a -> IdSet a -> Maybe Identifier
lookupKey v s = v `Map.lookup` iInvMap s


-- # Update #

-- | /O(log n)/. Inserts the element into the set. Returns the element's
-- identifier and the set containing it. If the element was previously in the
-- set, its previous identifier is returned with the original set.
insert :: Ord a => a -> IdSet a -> (Identifier, IdSet a)
insert v s =
  case v `Map.lookup` iInvMap s of
    Just i  -> (i, s) -- Already exists
    Nothing ->
      let (i, iData') = IdList.append v (iData s)
          iInvMap'    = Map.insert v i (iInvMap s)
      in (i, IdSet iData' iInvMap')

-- | /O(m log n)/. Insert the elements into the set. The identifiers of the
-- entries are also returned. /m/ is the number of elements added, where /n/
-- is the size of the list.
insertAll :: Ord a => [a] -> IdSet a -> ([Identifier], IdSet a)
insertAll vs c = fromList' c vs
  where
  fromList' :: Ord a => IdSet a -> [a] -> ([Identifier], IdSet a)
  fromList' acc [] = ([], acc)
  fromList' acc (x:xs) =
    let (xI, acc')   = insert x acc
        (xsI, acc'') = fromList' acc' xs
    in (xI:xsI, acc'')

-- | /O(n log n)/. Returns the subset of the 'IdSet', which contains only the
-- elements that satisfy the predicate. As elements may be removed from the
-- set, existing elements may be moved to lower indices. The mapping from the
-- old indices to the new is returned as the first tuple element.
filter :: Ord a => ( a -> Bool ) -> IdSet a -> (IntMap Identifier, IdSet a)
filter f c =
  let entries'  = List.filter (f . snd) $ entries c
      (ids, c') = fromList (List.map snd entries')
  in (IntMap.fromList $ zip (List.map fst entries') ids, c')


-- # Traversal

-- | /O(n log n)/. Applies the function to each element in the set. As elements
-- may be removed from the set, existing elements may be moved to lower
-- indices. The mapping from the old indices to the new is returned as the first
-- tuple element.
map :: Ord b => ( a -> b ) -> IdSet a -> (IntMap Identifier, IdSet b)
map f s =
  let (newIds, s') = fromList $ fmap f $ IdList.elems (iData s)
      mapping = IntMap.fromList $ zip [0..] newIds
  in (mapping, s')


-- # Conversion #

-- | /O(n)/. Lazily produces all identifiers from the list in ascending order.
keys :: IdSet a -> [Identifier]
keys = IdList.keys . iData

-- | /O(n)/. Converts the elements in the 'IdSet' to a list.
elems :: IdSet a -> [a]
elems = IdList.elems . iData

-- | /O(n)/. Converts the elements in the 'IdSet' to a list, each with their
-- identifier.
entries :: IdSet a -> [(Identifier, a)]
entries = IdList.entries . iData

-- | /O(n log n)/. Converts the list to an 'IdSet'. The returned 'IdSet'
-- contains every elements in the provided list. A mapping from each element to
-- its assigned identifier is also returned.
fromList :: Ord a => [a] -> ([Identifier], IdSet a)
fromList xs = insertAll xs empty

-- | /O(1)/. Converts the 'IdSet' to an 'IntMap'.
toIntMap :: IdSet a -> IntMap a
toIntMap = IdList.toIntMap . iData


-- # Instances #

instance NFData a => NFData (IdSet a) where
  rnf s = rnf (iData s) `seq` rnf (iInvMap s)
