--------------------------------------------------------------------------------
-- |
-- Module      : Data.IdList
-- Copyright   : (c) Dennis Sprokholt, 2020
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- A list where each element is identified by a unique identifier.
--
-- /Warning/: The size of a 'IdList' must not exceed @maxBound::Int@. Violation
-- is /not/ checked and causes undefined behavior.
--------------------------------------------------------------------------------

module Data.IdList
  ( -- * Types
    Identifier
  , IdList
    -- * Construction
  , empty
  , singleton
    -- * Query
  , null
  , size
  , lookup
    -- * Update
  , append
  , appendAll
  , replace
  , maybeReplace
  , update
  , maybeUpdate
    -- * Traversal
  , map
    -- * Conversion
  , keys
  , elems
  , entries
  , toIntMap
  ) where

-- Stdlib imports
import           Prelude hiding ( null, lookup, map )
import           Data.Maybe ( fromMaybe )
-- External library imports
import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy ( IntMap )


-- # Types #

-- | Uniquely identifies elements in an `IdList`.
type Identifier = Int

-- | A list of values with identifiers
data IdList a =
  IdList {
    ilData  :: IntMap a
  , ilSize  :: Int
  }


-- # Construction #

-- | /O(1)/. The empty 'IdList'.
empty :: IdList a
empty = IdList IntMap.empty 0

-- | /O(1)/. An 'IdList' with a single element. Also returns the identifier of
-- that element.
singleton :: a -> (Identifier, IdList a)
singleton v = (0, IdList (IntMap.singleton 0 v) 1)


-- # Query #

-- | /O(1)/. Returns `True` iff the list is empty.
null :: IdList a -> Bool
null c = ilSize c == 0

-- | /O(1)/. The size of the list.
size :: IdList a -> Int
size = ilSize

-- | /O(log n)/. Finds the entry for the given identifier. Returns `Nothing` if
-- it does not exist.
lookup :: Identifier -> IdList a -> Maybe a
lookup k = IntMap.lookup k . ilData


-- # Update #

-- | /O(log n)/. Adds the entry to the end of the list. The entry's identifier
-- is also returned.
append :: a -> IdList a -> (Identifier, IdList a)
append v c =
  let k = ilSize c
  in
  ( k
  , IdList { ilData = IntMap.insert k v (ilData c)
           , ilSize = k + 1
           }
  )

-- | /O(m log n)/. Adds the entries to the end of the list in order. The
-- identifiers of the entries are also returned.
-- 'm' is the number of elements added, where 'n' is the size of the list.
appendAll :: [a] -> IdList a -> ([Identifier], IdList a)
appendAll vs c = fromList' c vs
  where
  fromList' :: IdList a -> [a] -> ([Identifier], IdList a)
  fromList' acc [] = ([], acc)
  fromList' acc (x:xs) =
    let (xI, acc')   = append x acc
        (xsI, acc'') = fromList' acc' xs
    in (xI:xsI, acc'')

-- | /O(log n)/. Replaces the value for the given identifier in the list. If the
-- identifier is not present in the list, the original list is returned.
-- 
-- When knowledge of success is desired, use `maybeReplace`.
replace :: Identifier -> a -> IdList a -> IdList a
replace k v = update (const v) k

-- | /O(log n)/. Replaces the value for the given identifier in the list. If the
-- identifier is not present in the list, `Nothing` is returned.
maybeReplace :: Identifier -> a -> IdList a -> Maybe (IdList a)
maybeReplace k v = maybeUpdate (const v) k

-- | /O(log n)/. Updates the element for the identifier. If the identifier is
-- not present in the map, the original list is returned.
--
-- When knowledge of success is desired, use `maybeUpdate`.
update :: ( a -> a ) -> Identifier -> IdList a -> IdList a
update f k c =
  let ilData' = IntMap.update (Just . f) k (ilData c)
  in c { ilData = ilData' }

-- | /O(log n)/. Updates the element for the identifier. If the identifier is
-- not present in the map, `Nothing` is returned.
maybeUpdate :: ( a -> a ) -> Identifier -> IdList a -> Maybe (IdList a)
maybeUpdate f k c =
  let (oldV, ilData') = IntMap.updateLookupWithKey (\_ -> Just . f) k (ilData c)
  in oldV >> return ( c { ilData = ilData' } )


-- # Traversal #

-- | /O(n)/. Maps a function over all elements in the list.
map :: ( a -> b ) -> IdList a -> IdList b
map f c = c { ilData = IntMap.map f (ilData c) }


-- # Conversion #

-- | /O(n)/. Lazily produces all identifiers from the list in ascending order.
keys :: IdList a -> [Identifier]
keys c = [0..ilSize c - 1]

-- | /O(n)/. Converts the elements in the 'IdList' to a list in order of
-- insertion.
elems :: IdList a -> [a]
elems = IntMap.elems . ilData

-- | /O(n)/. Converts the elements in the 'IdList' to a list, each with their
-- key.
entries :: IdList a -> [(Identifier, a)]
entries = IntMap.toList . ilData

-- | /O(1)/. Converts the 'IdList' to an 'IntMap'.
toIntMap :: IdList a -> IntMap a
toIntMap = ilData


instance Functor IdList where
  fmap f c = c { ilData = fmap f (ilData c) }

instance Foldable IdList where
  foldMap f c = foldMap f (ilData c)

instance Traversable IdList where
  traverse f c = IdList <$> traverse f (ilData c) <*> pure (ilSize c)

instance Show a => Show (IdList a) where
  show = show . IntMap.toList . ilData
