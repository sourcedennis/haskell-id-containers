
module Data.IdSetTest
  ( tests
  ) where

-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import           Test.Tasty.QuickCheck ( testProperty )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict ( IntMap )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map )
import qualified Data.Set as Set
import           Data.Set ( Set )
-- Local library imports
import qualified Data.IdSet as IdSet
import           Data.IdSet ( IdSet, Identifier )


tests :: TestTree
tests =
  testGroup "IdSet"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################


aIdx, bIdx, a2Idx :: Identifier
xs1, xs2, xs3 :: IdSet String
(aIdx,  xs1) = IdSet.singleton "A"
(bIdx,  xs2) = IdSet.insert "B" xs1
(a2Idx, xs3) = IdSet.insert "A" xs2

unitTests :: [TestTree]
unitTests =
  [ testCase "null" $
      assertBool [] (IdSet.null IdSet.empty)
  , testCase "not null" $
      assertBool [] (not $ IdSet.null xs1)
  , testCase "size 0" $
      assertEqual [] 0 (IdSet.size IdSet.empty)
  , testCase "size 1" $
      assertEqual [] 1 (IdSet.size xs1)
  , testCase "size 2" $
      assertEqual [] 2 (IdSet.size xs2)
  , testCase "size 2 (duplicate element)" $
      assertEqual [] 2 (IdSet.size xs3)
  , testCase "unique ids for unequals" $
      assertBool [] (aIdx /= bIdx)
  , testCase "equal ids for equals" $
      assertBool [] (aIdx == a2Idx)
  , testCase "lookup 0" $
      assertEqual [] (Just "A") (IdSet.lookup aIdx xs3)
  , testCase "lookup 1" $
      assertEqual [] (Just "B") (IdSet.lookup bIdx xs3)
  , testCase "lookup 2" $
      assertEqual [] (Just "A") (IdSet.lookup a2Idx xs3)
  , testCase "no lookup 0" $
      assertEqual [] Nothing (IdSet.lookup aIdx (IdSet.empty :: IdSet String))
  , testCase "no lookup 1" $
      assertEqual [] Nothing (IdSet.lookup bIdx xs1)
  , testCase "map values" $
      assertEqual [] [8,3,2] (IdSet.elems $ snd $ IdSet.map (`div` 2) $ snd $ IdSet.fromList [16,6,4,17,7])
  , testCase "map keys" $ -- map (`div` 2) [16,6,4,17,7] = [8,3,2]
      assertEqual [] (IntMap.fromList [(0,0),(1,1),(2,2),(3,0),(4,1)]) (fst $ IdSet.map (`div` 2) $ snd $ IdSet.fromList [16,6,4,17,7])
  , testCase "entries" $
      assertEqual [] [(aIdx,"A"), (bIdx,"B")] (IdSet.entries xs3)
  , testCase "keys" $
      assertEqual [] [aIdx,bIdx] (IdSet.keys xs3)
  , testCase "elems" $
      assertEqual [] ["A","B"] (IdSet.elems xs3)
  , testCase "fromList keys" $
      assertEqual [] [0,1,2] (fst $ IdSet.fromList ["A","B","C"])
  , testCase "fromList values" $
      assertEqual [] [(0,"A"),(1,"B"),(2,"C")] (IdSet.entries $ snd $ IdSet.fromList ["A","B","C"])
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
      \xs -> propIdsImmutable (IntSet.empty, IdSet.empty) (xs :: [String])
  ]
  where
  -- | Tests whether all elements are inserted, and they all get IDs
  propLengthPreserved :: Ord a => [a] -> Bool
  propLengthPreserved xs =
    let (xsIds, xsIdList) = IdSet.insertAll xs IdSet.empty
    in length xsIds == length xs && length xsIds <= length xs
  
  -- | Tests whether no elements are lost (and elements don't magically
  -- appear)
  propElementsPreserved :: Ord a => [a] -> Bool
  propElementsPreserved xs =
    let (_, xsIdList) = IdSet.insertAll xs IdSet.empty
    in Set.fromList (IdSet.elems xsIdList) == Set.fromList xs

  -- | Tests whether all elements get unique IDs
  propUniqueIds :: Ord a => [a] -> Bool
  propUniqueIds xs =
    let (xsIds, _) = IdSet.insertAll xs IdSet.empty
        -- Maps values to identifiers :: HashMap a (Set Identifier)
        idMap = Map.fromListWith IntSet.union (zip xsIds $ map IntSet.singleton xsIds)
    in Map.foldl' (\a v -> a && IntSet.size v == 1) True idMap

  -- | Tests whether IDs don't change with subsequent insertions
  propIdsImmutable :: Ord a => (IntSet, IdSet a) -> [a] -> Bool
  propIdsImmutable (prevIds, c) [] = True
  propIdsImmutable (prevIds, c) (x:xs) =
    let (i, c') = IdSet.insert x c
        newIds  = IntSet.insert i prevIds
    in
    IntSet.fromList (IdSet.keys c') == newIds && propIdsImmutable (newIds, c') xs
