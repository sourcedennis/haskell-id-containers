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
  , (++)
  , concat
  , concatMap
    -- * Traversal
  , map
  , mapWithKey
  , foldlWithKey'
  , zip
  , zipExact
  , zipWith
  , zipWithM
  , zipWithExact
  , zipWithExactM
    -- * Conversion
  , keys
  , elems
  , entries
  , toList
  , fromList
  , toIntMap
  , toVector
  ) where

-- Stdlib imports
import           Prelude
  hiding ( null, lookup, map, zipWith, zip, (++), concat, concatMap )
import qualified Data.List as List
import           Data.Maybe ( fromMaybe )
import           Data.Hashable ( Hashable (..) )
import qualified Control.Monad as Monad
import           Control.Monad.Fail ( MonadFail )
-- External library imports
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
import qualified Data.Vector as Vector
import           Data.Vector ( Vector )
import           Control.DeepSeq ( NFData ( rnf ) )


-- # Types #

-- | Uniquely identifies elements in an `IdList`.
type Identifier = Int

-- | A list of values with identifiers
data IdList a =
  IdList {
    ilData  :: !(IntMap a)
  , ilSize  :: !Int
  }


-- # Construction #

-- | /O(1)/. The empty 'IdList'.
empty :: IdList a
empty = IdList IntMap.empty 0
{-# INLINE empty #-}

-- | /O(1)/. An 'IdList' with a single element. Also returns the identifier of
-- that element.
singleton :: a -> (Identifier, IdList a)
singleton v = (0, IdList (IntMap.singleton 0 v) 1)
{-# INLINE singleton #-}


-- # Query #

-- | /O(1)/. Returns `True` iff the list is empty.
null :: IdList a -> Bool
null c = ilSize c == 0
{-# INLINE null #-}

-- | /O(1)/. The size of the list.
size :: IdList a -> Int
size = ilSize
{-# INLINE size #-}

-- | /O(log n)/. Finds the entry for the given identifier. Returns `Nothing` if
-- it does not exist.
lookup :: Identifier -> IdList a -> Maybe a
lookup k = IntMap.lookup k . ilData
{-# INLINE lookup #-}


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
{-# INLINE append #-}

-- | /O(m log n)/. Adds the entries to the end of the list in order. The
-- identifiers of the entries are also returned.
-- /m/ is the number of elements added, where /n/ is the size of the list.
appendAll :: [a] -> IdList a -> ([Identifier], IdList a)
appendAll vs c = fromList' c vs
  where
  fromList' :: IdList a -> [a] -> ([Identifier], IdList a)
  fromList' acc [] = ([], acc)
  fromList' acc (x:xs) =
    let (xI, acc')   = append x acc
        (xsI, acc'') = fromList' acc' xs
    in (xI:xsI, acc'')
{-# INLINE appendAll #-}

-- | /O(log n)/. Replaces the value for the given identifier in the list. If the
-- identifier is not present in the list, the original list is returned.
-- 
-- When knowledge of success is desired, use `maybeReplace`.
replace :: Identifier -> a -> IdList a -> IdList a
replace k v = update (const v) k
{-# INLINE replace #-}

-- | /O(log n)/. Replaces the value for the given identifier in the list. If the
-- identifier is not present in the list, `Nothing` is returned.
maybeReplace :: Identifier -> a -> IdList a -> Maybe (IdList a)
maybeReplace k v = maybeUpdate (const $ Just v) k
{-# INLINE maybeReplace #-}

-- | /O(log n)/. Updates the element for the identifier. If the identifier is
-- not present in the map, the original list is returned.
--
-- When knowledge of success is desired, use `maybeUpdate`.
update :: ( a -> a ) -> Identifier -> IdList a -> IdList a
update f k c =
  let ilData' = IntMap.update (Just . f) k (ilData c)
  in c { ilData = ilData' }
{-# INLINE update #-}

-- | /O(log n)/. Updates the element for the identifier. If the identifier is
-- not present in the map, `Nothing` is returned.
maybeUpdate :: ( a -> Maybe a ) -> Identifier -> IdList a -> Maybe (IdList a)
maybeUpdate f k c =
  do
    let d = ilData c
    v <- f =<< IntMap.lookup k d
    return $ c { ilData = IntMap.insert k v d }
{-# INLINE maybeUpdate #-}

-- | /O(n)/. Appends the lists
--
-- >>> append (fromList [4,5,7]) (fromList [9,2])
(++) :: IdList a -> IdList a -> IdList a
(++) xs ys = fromList (toList xs List.++ toList ys)
{-# INLINE (++) #-}

-- | /O(n)/. Concatenates the lists
--
-- >>> concat [fromList [4,5,7], fromList [9,2]]
concat :: [IdList a] -> IdList a
concat = fromList . List.concatMap toList
{-# INLINE concat #-}

concatMap :: ( a -> IdList b ) -> IdList a -> IdList b
concatMap f = fromList . List.concatMap (toList . f) . toList


-- # Traversal #

-- | /O(n)/. Maps a function over all elements in the list.
map :: ( a -> b ) -> IdList a -> IdList b
map f c = c { ilData = IntMap.map f (ilData c) }
{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
  #-}

-- | /O(n)/. Maps a functions over all elements in the list.
mapWithKey :: ( Identifier -> a -> b ) -> IdList a -> IdList b
mapWithKey f c = c { ilData = IntMap.mapWithKey f (ilData c) }
{-# INLINE mapWithKey #-}

-- | /O(n)/. Left-associative fold with strict function application.
foldlWithKey' :: (a -> (Identifier, b) -> a) -> a -> IdList b -> a
foldlWithKey' f a = List.foldl' f a . entries
{-# INLINE foldlWithKey' #-}

-- | /O(\min(m,n))/. Pairs the elements in two `IdList`s. If one input `IdList`
-- is shorter, excess elements are discarded.
zip :: IdList a -> IdList b -> IdList (a, b)
zip xs ys = fromList $ List.zip (toList xs) (toList ys)
{-# INLINE zip #-}

-- | /O(n)/. Pairs the elements in two `IdList`s of /equal length/. If the
-- `IdList`s are of unequal length, it fails over the `MonadFail`.
zipExact :: MonadFail m => IdList a -> IdList b -> m (IdList (a, b))
zipExact xs ys =
  if size xs == size ys then
    return $ zip xs ys
  else
    fail "Unequal lengths"
{-# INLINE zipExact #-}

-- | /O(\min(m,n))/. Applies the function to element pairs of the `IdList`s. If
-- one input `IdList` is shorter, excess elements are discarded.
zipWith :: ( a -> b -> c ) -> IdList a -> IdList b -> IdList c
zipWith f xs ys = fromList $ List.zipWith f (toList xs) (toList ys)
{-# INLINE zipWith #-}

-- | /O(\min(m,n))/. Applies the applicative function to element pairs of the
-- `IdList`s. If one input `IdList` is shorter, excess elements are discarded.
zipWithM :: Applicative m => ( a -> b -> m c ) -> IdList a -> IdList b -> m (IdList c)
zipWithM f xs ys = fromList <$> Monad.zipWithM f (toList xs) (toList ys)
{-# INLINE zipWithM #-}

-- | /O(n)/. Applies the function to element pairs of two `IdList`s of /equal
-- length/. If the `IdList`s are of unequal length, it fails over the
-- `MonadFail`.
zipWithExact :: MonadFail m => ( a -> b -> c ) -> IdList a -> IdList b -> m (IdList c)
zipWithExact f xs ys =
  if size xs == size ys then
    return $ zipWith f xs ys
  else
    fail "Unequal lengths"
{-# INLINE zipWithExact #-}

-- | /O(n)/. Applies the monadic function to element pairs of two `IdList`s of
-- /equal length/. If the `IdList`s are of unequal length, it fails over the
-- `MonadFail`.
zipWithExactM :: MonadFail m => ( a -> b -> m c ) -> IdList a -> IdList b -> m (IdList c)
zipWithExactM f xs ys =
  if size xs == size ys then
    zipWithM f xs ys
  else
    fail "Unequal lengths"
{-# INLINE zipWithExactM #-}


-- # Conversion #

-- | /O(n)/. Lazily produces all identifiers from the list in ascending order.
keys :: IdList a -> [Identifier]
keys c = [0..ilSize c - 1]
{-# INLINE keys #-}

-- | /O(n)/. Converts the elements in the 'IdList' to a list in order of
-- insertion.
elems :: IdList a -> [a]
elems = IntMap.elems . ilData
{-# INLINE elems #-}

-- | /O(n)/. Converts the elements in the 'IdList' to a list, each with their
-- key.
entries :: IdList a -> [(Identifier, a)]
entries = IntMap.toList . ilData
{-# INLINE entries #-}

-- | /O(n)/. Converts the elements in the 'IdList' to a list in order of
-- insertion.
toList :: IdList a -> [a]
toList = elems
{-# INLINE toList #-}

-- | /O(n)/. Converts the list to an 'IdList'. The returned 'IdList' contains
-- every element in the provided list.
fromList :: [a] -> IdList a
fromList xs = IdList (IntMap.fromAscList $ List.zip [0..] xs) (length xs)
{-# INLINE fromList #-}

-- | /O(1)/. Converts the 'IdList' to an 'IntMap'.
toIntMap :: IdList a -> IntMap a
toIntMap = ilData
{-# INLINE toIntMap #-}

-- | /O(1)/. Converts the 'IdList' to an 'IntMap'.
toVector :: IdList a -> Vector a
toVector = Vector.fromList . toList
{-# INLINE toVector #-}


instance Functor IdList where
  fmap f c = c { ilData = fmap f (ilData c) }

instance Foldable IdList where
  foldMap f c = foldMap f (ilData c)

instance Traversable IdList where
  traverse f c = IdList <$> traverse f (ilData c) <*> pure (ilSize c)

instance Show a => Show (IdList a) where
  show = show . IntMap.toList . ilData

instance Eq a => Eq (IdList a) where
  (==) a b = ilData a == ilData b

instance NFData a => NFData (IdList a) where
  rnf s = rnf (ilData s) `seq` rnf (ilSize s)

instance Hashable a => Hashable (IdList a) where
  hashWithSalt i = hashWithSalt i . toList
