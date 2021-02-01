
module Data.IdListTest
  ( tests
  ) where

-- Stdlib imports
import qualified Data.Sequence as Seq
import           Data.Sequence ( Seq, (|>) )
-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import           Test.Tasty.QuickCheck ( testProperty )
import qualified Data.IntSet as IntSet
import           Data.IntSet ( IntSet )
-- Local library imports
import qualified Data.IdList as IdList
import           Data.IdList ( IdList, Identifier )


tests :: TestTree
tests =
  testGroup "IdList"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################


aIdx, bIdx, a2Idx :: Identifier
xs1, xs2, xs3 :: IdList String
(aIdx,  xs1) = IdList.singleton "A"
(bIdx,  xs2) = IdList.append "B" xs1
(a2Idx, xs3) = IdList.append "A" xs2

unitTests :: [TestTree]
unitTests =
  [ testCase "null" $
      assertBool [] (IdList.null IdList.empty)
  , testCase "not null" $
      assertBool [] (not $ IdList.null xs1)
  , testCase "size 0" $
      assertEqual [] 0 (IdList.size IdList.empty)
  , testCase "size 1" $
      assertEqual [] 1 (IdList.size xs1)
  , testCase "size 2" $
      assertEqual [] 2 (IdList.size xs2)
  , testCase "size 3" $
      assertEqual [] 3 (IdList.size xs3)
  , testCase "unique ids for unequals" $
      assertBool [] (aIdx /= bIdx)
  , testCase "unique ids for equals" $
      assertBool [] (aIdx /= a2Idx)
  , testCase "lookup 0" $
      assertEqual [] (Just "A") (IdList.lookup aIdx xs3)
  , testCase "lookup 1" $
      assertEqual [] (Just "B") (IdList.lookup bIdx xs3)
  , testCase "lookup 2" $
      assertEqual [] (Just "A") (IdList.lookup a2Idx xs3)
  , testCase "no lookup 0" $
      assertEqual [] Nothing (IdList.lookup aIdx (IdList.empty :: IdList String))
  , testCase "no lookup 1" $
      assertEqual [] Nothing (IdList.lookup bIdx xs1)
  , testCase "no lookup 2" $
      assertEqual [] Nothing (IdList.lookup a2Idx xs2)
  , testCase "foldlWithKey' keys" $
      assertEqual [] (Seq.fromList [0,1,2,3,4,5]) (IdList.foldlWithKey' (\a (i,_) -> a |> i) Seq.empty (IdList.fromList ["A","B","C","D","E","F"]))
  , testCase "foldlWithKey' values" $
      assertEqual [] (Seq.fromList ["A","B","C","D","E","F"]) (IdList.foldlWithKey' (\a (_,x) -> a |> x) Seq.empty (IdList.fromList ["A","B","C","D","E","F"]))
  , testCase "entries" $
      assertEqual [] [(aIdx,"A"), (bIdx,"B"), (a2Idx,"A")] (IdList.entries xs3)
  , testCase "keys" $
      assertEqual [] [aIdx,bIdx,a2Idx] (IdList.keys xs3)
  , testCase "elems" $
      assertEqual [] ["A","B","A"] (IdList.elems xs3)
  , testCase "fromList" $
      assertEqual [] [(0,"A"),(1,"B"),(2,"C")] (IdList.entries $ IdList.fromList ["A","B","C"])
  ]


-- #############################################################################
-- # # # # # # # # # # # # # # # Property Tests  # # # # # # # # # # # # # # # #
-- #############################################################################


quickcheckTests :: [TestTree]
quickcheckTests =
  [ testProperty "All elements are inserted" $
      \xs -> propLengthPreserved (xs :: [String])
  , testProperty "No elements are lost" $
      \xs -> propElementsPreserved (xs :: [String])
  , testProperty "All IDs are unique" $
      \xs -> propUniqueIds (xs :: [String])
  , testProperty "No IDs change" $
      \xs -> propIdsImmutable (IntSet.empty, IdList.empty) (xs :: [String])
  ]
  where
  -- | Tests whether all elements are inserted, and they all get IDs
  propLengthPreserved :: [a] -> Bool
  propLengthPreserved xs =
    let (xsIds, xsIdList) = IdList.appendAll xs IdList.empty
    in length xsIds == length xs && IdList.size xsIdList == length xs
  
  -- | Tests whether no elements are lost (and elements don't magically
  -- appear)
  propElementsPreserved :: Eq a => [a] -> Bool
  propElementsPreserved xs =
    let (_, xsIdList) = IdList.appendAll xs IdList.empty
    in IdList.elems xsIdList == xs

  -- | Tests whether all elements get unique IDs
  propUniqueIds :: [a] -> Bool
  propUniqueIds xs =
    let (xsIds, xsIdList) = IdList.appendAll xs IdList.empty
    in isAllUnique xsIds

  -- | Tests whether IDs don't change with subsequent insertions
  propIdsImmutable :: (IntSet, IdList a) -> [a] -> Bool
  propIdsImmutable (prevIds, c) [] = True
  propIdsImmutable (prevIds, c) (x:xs) =
    let (i, c') = IdList.append x c
        newIds  = IntSet.insert i prevIds
    in
    IntSet.fromList (IdList.keys c') == newIds && propIdsImmutable (newIds, c') xs


-- #############################################################################
-- # # # # # # # # # # # # # # # # # Helpers # # # # # # # # # # # # # # # # # #
-- #############################################################################


-- /O(n log n)/. Returns 'True' iff all elements in the list are unique.
isAllUnique :: [Identifier] -> Bool
isAllUnique = isAllUnique' IntSet.empty
  where
  isAllUnique' :: IntSet -> [Identifier] -> Bool
  isAllUnique' acc []     = True
  isAllUnique' acc (x:xs) =
    if x `IntSet.member` acc then
      False
    else
      isAllUnique' (IntSet.insert x acc) xs
