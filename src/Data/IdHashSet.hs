--------------------------------------------------------------------------------
-- |
-- Module      : Data.IdHashSet
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- A set where each element is identified by a unique identifier. As this is a
-- set, it never contains duplicate elements. The 'Hashable' trait is used to
-- determine element equivalence.
--
-- Note that modification functions are (currently) intentionally left out, as 
-- replacing existing elements may cause identifiers to collapse together.
-- For example, consider the identifiers is the following scenario:
--
-- > map (const 2) (snd $ fromList [1,2,3]) = fromList [2]
--
-- /Warning/: The size of a 'IdHashSet' must not exceed @maxBound::Int@.
-- Violation is /not/ checked and causes undefined behavior.
--------------------------------------------------------------------------------

module Data.IdHashSet
  ( -- * Types
    Identifier
  , IdHashSet
    -- * Construction
  , empty
  , singleton
    -- * Query
  , null
  , size
  , lookup
    -- * Update
  , insert
  , insertAll
    -- * Conversion
  , keys
  , elems
  , entries
  , fromList
  , toIntMap
  ) where

-- Stdlib imports
import           Prelude hiding ( null, lookup, map )
-- External library imports
import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy ( IntMap )
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict ( HashMap )
import           Data.Hashable ( Hashable )
-- Local imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList )


-- # Types #

-- | Uniquely identifies elements in an `IdHashSet`.
type Identifier = Int

-- | A list of values with identifiers
data IdHashSet a =
  IdHashSet {
    iData    :: IdList a
  , iInvMap  :: HashMap a Int
  }


-- # Construction #

-- | /O(1)/. The empty 'IdHashSet'.
empty :: IdHashSet a
empty = IdHashSet IdList.empty HashMap.empty

-- | /O(1)/. An 'IdHashSet' with a single element. Also returns the identifier
-- of that element.
singleton :: Hashable a => a -> (Identifier, IdHashSet a)
singleton v =
  let (i, s) = IdList.singleton v
  in (i, IdHashSet s (HashMap.singleton v i))


-- # Query #

-- | /O(1)/. Returns 'True' iff the set is empty.
null :: IdHashSet a -> Bool
null = IdList.null . iData

-- | /O(1)/. The size of the set.
size :: IdHashSet a -> Int
size = IdList.size . iData

-- | /O(log n)/. Finds the entry for the given identifier. Returns `Nothing` if
-- it does not exist.
lookup :: Identifier -> IdHashSet a -> Maybe a
lookup k = IdList.lookup k . iData


-- # Update #

-- | /O(log n)/. Inserts the element into the set. Returns the element's
-- identifier and the set containing it. If the element was previously in the
-- set, its previous identifier is returned with the original set.
insert :: (Eq a, Hashable a) => a -> IdHashSet a -> (Identifier, IdHashSet a)
insert v s =
  case v `HashMap.lookup` iInvMap s of
    Just i  -> (i, s) -- Already exists
    Nothing ->
      let (i, iData') = IdList.append v (iData s)
          iInvMap'    = HashMap.insert v i (iInvMap s)
      in (i, IdHashSet iData' iInvMap')

-- | /O(m log n)/. Insert the elements into the set. The identifiers of the
-- entries are also returned. /m/ is the number of elements added, where /n/
-- is the size of the list.
insertAll :: (Eq a, Hashable a) => [a] -> IdHashSet a -> ([Identifier], IdHashSet a)
insertAll vs c = fromList' c vs
  where
  fromList' :: (Eq a, Hashable a) => IdHashSet a -> [a] -> ([Identifier], IdHashSet a)
  fromList' acc [] = ([], acc)
  fromList' acc (x:xs) =
    let (xI, acc')   = insert x acc
        (xsI, acc'') = fromList' acc' xs
    in (xI:xsI, acc'')

-- # Conversion #

-- | /O(n)/. Lazily produces all identifiers from the list in ascending order.
keys :: IdHashSet a -> [Identifier]
keys = IdList.keys . iData

-- | /O(n)/. Converts the elements in the 'IdHashSet' to a list.
elems :: IdHashSet a -> [a]
elems = IdList.elems . iData

-- | /O(n)/. Converts the elements in the 'IdHashSet' to a list, each with their
-- identifier.
entries :: IdHashSet a -> [(Identifier, a)]
entries = IdList.entries . iData

-- | /O(n log n)/. Converts the list to an 'IdHashSet'. The returned 'IdHashSet'
-- contains every elements in the provided list. A mapping from each element to
-- its assigned identifier is also returned.
fromList :: (Eq a, Hashable a) => [a] -> (HashMap a Identifier, IdHashSet a)
fromList xs =
  let s = foldr (\v -> snd . insert v) empty xs
  in (iInvMap s, s)

-- | /O(1)/. Converts the 'IdHashSet' to an 'IntMap'.
toIntMap :: IdHashSet a -> IntMap a
toIntMap = IdList.toIntMap . iData
