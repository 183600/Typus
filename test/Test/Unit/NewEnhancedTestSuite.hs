module Test.Unit.NewEnhancedTestSuite where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), sourceLine, sourceColumn, sourcePosOffset)
import Data.Char (isSpace)

-- Simple test properties for Utils module
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content s = 
  let trimmed = trim s
      nonSpaceCount = length $ filter (not . isSpace) s
      trimmedNonSpaceCount = length $ filter (not . isSpace) trimmed
  in property $ nonSpaceCount == trimmedNonSpaceCount

prop_splitBy_roundtrip :: Char -> String -> Property
prop_splitBy_roundtrip delim s = 
  let parts = splitBy delim s
      reconstructed = concat $ map (\p -> p ++ [delim]) parts
  in property $ take (length s) reconstructed == s

-- Unit tests
test_trim_empty :: Assertion
test_trim_empty = assertEqual "trim empty string" "" (trim "")

test_trim_all_spaces :: Assertion
test_trim_all_spaces = assertEqual "trim all spaces" "" (trim "   ")

test_splitBy_basic :: Assertion
test_splitBy_basic = assertEqual "splitBy basic" ["a", "b", "c"] (splitBy ',' "a,b,c")

test_sourcepos_creation :: Assertion
test_sourcepos_creation = do
  let pos = SourcePos 10 20 100
  assertEqual "source line" 10 (sourceLine pos)
  assertEqual "source column" 20 (sourceColumn pos)
  assertEqual "source offset" 100 (sourcePosOffset pos)

-- | Exported tests for integration
tests :: TestTree
tests = testGroup "New Enhanced Test Suite"
  [ testGroup "Utils Tests"
    [ testProperty "trim preserves content" prop_trim_preserves_content
    , testProperty "splitBy roundtrip" prop_splitBy_roundtrip
    , testCase "trim empty" test_trim_empty
    , testCase "trim all spaces" test_trim_all_spaces
    , testCase "splitBy basic" test_splitBy_basic
    ]
  , testGroup "SourceLocation Tests"
    [ testCase "SourcePos creation" test_sourcepos_creation
    ]
  ]

main :: IO ()
main = defaultMain $ testGroup "New Enhanced Test Suite"
  [ testGroup "Utils Tests"
    [ testProperty "trim preserves content" prop_trim_preserves_content
    , testProperty "splitBy roundtrip" prop_splitBy_roundtrip
    , testCase "trim empty" test_trim_empty
    , testCase "trim all spaces" test_trim_all_spaces
    , testCase "splitBy basic" test_splitBy_basic
    ]
  , testGroup "SourceLocation Tests"
    [ testCase "SourcePos creation" test_sourcepos_creation
    ]
  ]