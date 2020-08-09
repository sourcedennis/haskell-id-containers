
-- External library imports
import Test.Tasty       ( defaultMain, testGroup )
import Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
-- Local imports
import qualified Data.IdHashSetTest as IdHashSetTest
import qualified Data.IdSetTest as IdSetTest
import qualified Data.IdListTest as IdListTest


main :: IO ()
main =
  defaultMain $
    testGroup "Modules"
      [ IdHashSetTest.tests
      , IdSetTest.tests
      , IdListTest.tests
      ]
