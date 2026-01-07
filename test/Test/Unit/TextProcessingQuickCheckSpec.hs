module Test.Unit.TextProcessingQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, )
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import qualified Data.Text as T 
  middle <- listOf $ arbitrary `suchThat` (/= '\n')
  after <- listOf $ elements " \t\n"
  return $ before ++ middle ++ after
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- Generate strings with comma patterns
genCommaString :: Gen String
                              genCommaString = do
              parts <- listOf1 $ listOf $ arbitrary `suchThat` (/= ',')
  let withCommas = intersperse "," parts
  additionalCommas <- listOf $ return ","
  return $ L.concat withCommas ++ L.concat additionalCommas

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: trim should remove leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace                               s =
  let trimmed = trim s
                                    startsNotWithSpace = null trimmed || not (isSpace (L.head trimmed)
                                    endsNotWithSpace = null trimmed || not (isSpace (last trimmed)
  in startsNotWithSpace &&. endsNotWithSpace
  where
      (&&.) = (&&)

-- Property: trim should be idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent                               s =
  let trimmedOnce = trim s
                                    trimmedTwice = trim trimmedOnce
  in                               trimmedOnce === trimmedTwice

-- Property: splitBy should preserve empty segments
prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim                               s =
  let result = splitBy delim s
                                    rejoined = intercalate [delim] result
  in                               rejoined === s

-- Property: splitByCollapsed should remove empty segments
prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim                               s =
  let result = splitByCollapsed delim s
                                    hasNoEmpty =  L.all (not . null) result
  in property $ hasNoEmpty === True

-- Property: splitByComma should be equivalent to splitBy ','
prop_splitByComma_equivalence :: String -> Property
prop_splitByComma_equivalence                               s =
  splitByComma                               s === splitBy ',' s

-- Property: splitByCommaCollapsed should be equivalent to splitByCollapsed ','
prop_splitByCommaCollapsed_equivalence :: String -> Property
prop_splitByCommaCollapsed_equivalence                               s =
  splitByCommaCollapsed                               s === splitByCollapsed ',' s

-- Property: splitBy should be consistent with Text.split
prop_splitBy_text_consistency :: Char -> String -> Property
prop_splitBy_text_consistency delim                               s =
  splitBy delim                               s === map T.unpack (T.split (== delim) (T.pack s)

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_trim_examples :: TestTree
test_trim_examples =             testCase "trim examples" $ do
              trim "  hello  " @?= "hello"
  trim "\t\n  hello world  \n\t" @?= "hello world"
  trim "no-whitespace" @?= "no-whitespace"
  trim "   " @?= ""
  trim "" @?= ""

test_splitBy_examples :: TestTree
test_splitBy_examples =             testCase "splitBy examples" $ do
              splitBy ',' "a,b,c" @?= ["a", "b", "c"]
  splitBy ',' "a,,b" @?= ["a", "", "b"]
  splitBy ',' ",a," @?= ["", "a", ""]
  splitBy ',' "" @?= [""]
  splitBy '/' "path/to/file" @?= ["path", "to", "file"]

test_splitByCollapsed_examples :: TestTree
test_splitByCollapsed_examples =             testCase "splitByCollapsed examples" $ do
              splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
  splitByCollapsed ',' "a,,b" @?= ["a", "b"]
  splitByCollapsed ',' ",a," @?= ["a"]
  splitByCollapsed ',' "" @?= []
  splitByCollapsed '/' "path//to///file" @?= ["path", "to", "file"]

-- ============================================================================
-- Test Suite
-- ============================================================================
tests :: TestTree
tests =   testGroup "Text Processing QuickCheck Tests"
  [             testProperty "trim removes leading L.and trailing whitespace" prop_trim_removes_whitespace
  ,             testProperty "trim is idempotent" prop_trim_idempotent
  ,             testProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  ,             testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  ,             testProperty "splitByComma equivalent to splitBy ','" prop_splitByComma_equivalence
  ,             testProperty "splitByCommaCollapsed equivalent to splitByCollapsed ','" prop_splitByCommaCollapsed_equivalence
  ,             testProperty "splitBy consistent with Text.split" prop_splitBy_text_consistency
  , test_trim_examples
  , test_splitBy_examples
  , test_splitByCollapsed_examples
  ]