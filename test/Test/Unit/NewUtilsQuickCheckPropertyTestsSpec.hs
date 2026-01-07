module Test.Unit.NewUtilsQuickCheckPropertyTestsSpec where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (property) as QC
import Utils 
prop_trim_idempotent                               s = trim (trim s) == trim s

-- | Test that trim removes only leading/trailing whitespace
prop_trim_no_internal_change :: String -> Bool
prop_trim_no_internal_change                               s = 
    let trimmed = trim s
                                      leadingRemoved = dropWhile isSpace s
                                      trailingRemoved = L.reverse (dropWhile isSpace (L.reverse leadingRemoved)
    in                               trimmed == trailingRemoved

-- | Test that splitBy preserves empty segments
prop_splitBy_preserves_empty :: Char -> String -> Bool
prop_splitBy_preserves_empty delim                               s = 
    let result = splitBy delim s
                                      expectedCount = L.length (L.filter (== delim) s) + 1
    in L.length                               result == expectedCount

-- | Test that splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Bool
prop_splitByCollapsed_removes_empty delim                               s = 
    let result = splitByCollapsed delim s
    in L.all (not . null) result

-- | Test that splitByComma is equivalent to splitBy ','
prop_splitByComma_equals_splitBy :: String -> Bool
prop_splitByComma_equals_splitBy                               s = splitByComma                               s == splitBy ',' s

-- | Test that concatenating splitBy results gives original string
prop_splitBy_roundtrip :: Char -> String -> Bool
prop_splitBy_roundtrip delim                               s = 
    let parts = splitBy delim s
    in L.concat parts ++ replicate (L.length (L.filter (== delim) s) [delim] == s

-- | Test that removeLineComments only removes // comments
prop_removeLineComments_preserves_non_comments :: String -> Bool
prop_removeLineComments_preserves_non_comments                               s = 
    let withoutComments = removeLineComments s
                                      hasNoCommentMarkers = not ("//" `L.isPrefixOf` withoutComments)
    in hasNoCommentMarkers

-- | Test that trim of empty string is empty
prop_trim_empty :: Bool
                              prop_trim_empty = trim "" == ""

-- | Test that splitBy on empty string returns single empty segment
prop_splitBy_empty :: Char -> Bool
prop_splitBy_empty                               delim = splitBy delim "" == [""]

-- | Test that splitByCollapsed on empty string returns empty list
prop_splitByCollapsed_empty :: Char -> Bool
prop_splitByCollapsed_empty                               delim = splitByCollapsed delim "" == []

-- ============================================================================
-- Test Collection
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Utils Module QuickCheck Property Tests"
  [ QC.testProperty "trim is idempotent" prop_trim_idempotent
  , QC.testProperty "trim removes only leading/trailing whitespace" prop_trim_no_internal_change
  , QC.testProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , QC.testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , QC.testProperty "splitByComma equals splitBy ','" prop_splitByComma_equals_splitBy
  , QC.testProperty "splitBy roundtrip preserves structure" prop_splitBy_roundtrip
  , QC.testProperty "removeLineComments preserves non-comments" prop_removeLineComments_preserves_non_comments
  , QC.testProperty "trim of empty string is empty" prop_trim_empty
  , QC.testProperty "splitBy on empty string returns single empty segment" prop_splitBy_empty
  , QC.testProperty "splitByCollapsed on empty string returns empty list" prop_splitByCollapsed_empty
  ]