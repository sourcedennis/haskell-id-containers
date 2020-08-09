
module Data.IdHashSetTest
  ( tests
  ) where

-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import           Test.Tasty.QuickCheck ( testProperty )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict ( HashMap )
import qualified Data.HashSet as HashSet
import           Data.HashSet ( HashSet )
import           Data.Hashable ( Hashable )
-- Local library imports
import qualified Data.IdHashSet as IdHashSet
import           Data.IdHashSet ( IdHashSet, Identifier )


tests :: TestTree
tests =
  testGroup "IdHashSet"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################


aIdx, bIdx, a2Idx :: Identifier
xs1, xs2, xs3 :: IdHashSet String
(aIdx,  xs1) = IdHashSet.singleton "A"
(bIdx,  xs2) = IdHashSet.insert "B" xs1
(a2Idx, xs3) = IdHashSet.insert "A" xs2

unitTests :: [TestTree]
unitTests =
  [ testCase "null" $
      assertBool [] (IdHashSet.null IdHashSet.empty)
  , testCase "not null" $
      assertBool [] (not $ IdHashSet.null xs1)
  , testCase "size 0" $
      assertEqual [] 0 (IdHashSet.size IdHashSet.empty)
  , testCase "size 1" $
      assertEqual [] 1 (IdHashSet.size xs1)
  , testCase "size 2" $
      assertEqual [] 2 (IdHashSet.size xs2)
  , testCase "size 2 (duplicate element)" $
      assertEqual [] 2 (IdHashSet.size xs3)
  , testCase "unique ids for unequals" $
      assertBool [] (aIdx /= bIdx)
  , testCase "equal ids for equals" $
      assertBool [] (aIdx == a2Idx)
  , testCase "lookup 0" $
      assertEqual [] (Just "A") (IdHashSet.lookup aIdx xs3)
  , testCase "lookup 1" $
      assertEqual [] (Just "B") (IdHashSet.lookup bIdx xs3)
  , testCase "lookup 2" $
      assertEqual [] (Just "A") (IdHashSet.lookup a2Idx xs3)
  , testCase "no lookup 0" $
      assertEqual [] Nothing (IdHashSet.lookup aIdx (IdHashSet.empty :: IdHashSet String))
  , testCase "no lookup 1" $
      assertEqual [] Nothing (IdHashSet.lookup bIdx xs1)
  , testCase "entries" $
      assertEqual [] [(aIdx,"A"), (bIdx,"B")] (IdHashSet.entries xs3)
  , testCase "keys" $
      assertEqual [] [aIdx,bIdx] (IdHashSet.keys xs3)
  , testCase "elems" $
      assertEqual [] ["A","B"] (IdHashSet.elems xs3)
  ]


-- #############################################################################
-- # # # # # # # # # # # # # # # Property Tests  # # # # # # # # # # # # # # # #
-- #############################################################################


quickcheckTests :: [TestTree]
quickcheckTests =
  [ testProperty "Every element gets an ID" $
      \xs -> propLengthPreserved (xs :: [String])
  , testProperty "All elements are inserted" $
      \xs -> propElementsPreserved (xs :: [String])
  , testProperty "Duplicate elements get equal IDs" $
      \xs -> propUniqueIds (xs :: [String])
  , testProperty "No IDs change" $
      \xs -> propIdsImmutable (IntSet.empty, IdHashSet.empty) (xs :: [String])
  ]
  where
  -- | Tests whether all elements are inserted, and they all get IDs
  propLengthPreserved :: (Eq a, Hashable a) => [a] -> Bool
  propLengthPreserved xs =
    let (xsIds, xsIdList) = IdHashSet.insertAll xs IdHashSet.empty
    in length xsIds == length xs && length xsIds <= length xs
  
  -- | Tests whether no elements are lost (and elements don't magically
  -- appear)
  propElementsPreserved :: (Eq a, Hashable a) => [a] -> Bool
  propElementsPreserved xs =
    let (_, xsIdList) = IdHashSet.insertAll xs IdHashSet.empty
    in HashSet.fromList (IdHashSet.elems xsIdList) == HashSet.fromList xs

  -- | Tests whether all elements get unique IDs
  propUniqueIds :: (Eq a, Hashable a) => [a] -> Bool
  propUniqueIds xs =
    let (xsIds, _) = IdHashSet.insertAll xs IdHashSet.empty
        -- Maps values to identifiers :: HashMap a (Set Identifier)
        idMap = HashMap.fromListWith IntSet.union (zip xsIds $ map IntSet.singleton xsIds)
    in HashMap.foldl' (\a v -> a && IntSet.size v == 1) True idMap

  -- | Tests whether IDs don't change with subsequent insertions
  propIdsImmutable :: (Eq a, Hashable a) => (IntSet, IdHashSet a) -> [a] -> Bool
  propIdsImmutable (prevIds, c) [] = True
  propIdsImmutable (prevIds, c) (x:xs) =
    let (i, c') = IdHashSet.insert x c
        newIds  = IntSet.insert i prevIds
    in
    IntSet.fromList (IdHashSet.keys c') == newIds && propIdsImmutable (newIds, c') xs
