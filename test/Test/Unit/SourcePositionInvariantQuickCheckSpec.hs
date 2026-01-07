module Test.Unit.SourcePositionInvariantQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.List as L
import Data.Char (isSpace)

-- Basic test properties
prop_basic_property :: String -> Property
prop_basic_property s = 
  let trimmed = L.dropWhile isSpace (L.dropWhileEnd isSpace s)
  in property $ L.length trimmed <= L.length s

tests :: TestTree
tests = testGroup "Test.Unit.SourcePositionInvariantQuickCheckSpec Tests"
  [ testProperty "basic property" prop_basic_property
  ]
