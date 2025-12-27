{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import Data.Char (isSpace, isControl)
import qualified Data.List as List
import Data.List (isPrefixOf, isInfixOf, sort, nub)

-- ============================================================================
-- New Cabal Test Suite for Utils Module
-- ============================================================================

-- | Test case 1: Unicode and special character handling in trim
test_trim_unicode_special_chars :: TestTree
test_trim_unicode_special_chars = testCase "trim handles unicode and special characters" $ do
    assertEqual "trim with unicode spaces" 
        "hello世界" 
        (trim "\x2000\x2001hello世界\x3000")
    assertEqual "trim with control characters"
        "test"
        (trim "\x01\x02test\x1F\x7F")

-- | Test case 2: Edge cases for splitBy with empty strings and delimiters
test_splitBy_edge_cases :: TestTree
test_splitBy_edge_cases = testCase "splitBy handles edge cases correctly" $ do
    assertEqual "splitBy on single character" 
        ["", ""] 
        (splitBy 'a' "a")
    assertEqual "splitBy on repeated delimiter"
        ["", "", ""]
        (splitBy ',', ",,")
    assertEqual "splitBy on no delimiter present"
        ["abc"]
        (splitBy ',' "abc")

-- | Test case 3: Complex comment removal scenarios
test_removeComments_complex :: TestTree
test_removeComments_complex = testCase "removeComments handles complex scenarios" $ do
    let input = unlines
            [ "code // line comment"
            , "text /* block comment */ more"
            , "str = \"// not comment /* not block */\""
            , "char = '/' /* not comment */"
            , "nested /* outer /* inner */ still outer */ end"
            ]
    let expected = unlines
            [ "code "
            , "text  more"
            , "str = \"// not comment /* not block */\""
            , "char = '/' "
            , "nested  end"
            ]
    assertEqual "complex comment removal" expected (removeComments input)

-- | Test case 4: Property test for splitBy consistency
prop_splitBy_consistency :: Char -> String -> Property
prop_splitBy_consistency delim str =
    let split = splitBy delim str
        rejoined = List.intercalate [delim] split
    in counterexample ("split: " ++ show split ++ ", rejoined: " ++ show rejoined) $
       property $ rejoined == str

-- | Test case 5: Property test for trim idempotency
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
    let trimmedOnce = trim str
        trimmedTwice = trim trimmedOnce
    in property $ trimmedOnce === trimmedTwice

-- | Test case 6: Property test for splitByCollapsed length constraint
prop_splitByCollapsed_length :: Char -> String -> Property
prop_splitByCollapsed_length delim str =
    let normal = splitBy delim str
        collapsed = splitByCollapsed delim str
    in property $ length collapsed <= length normal

-- | Test case 7: Test normalization with mixed indentation
test_normalizeIndentation_mixed :: TestTree
test_normalizeIndentation_mixed = testCase "normalizeIndentation handles mixed indentation" $ do
    let input = unlines
            [ "    line1"
            , "\tline2"
            , "        line3"
            , "\t\tline4"
            ]
    let expected = unlines
            [ "line1"
            , "  line2"
            , "    line3"
            , "\tline4"
            ]
    assertEqual "mixed indentation normalization" expected (normalizeIndentation input)

-- | Test case 8: Property test for breakOn correctness
prop_breakOn_correctness :: String -> String -> Property
prop_breakOn_correctness needle haystack =
    let result = breakOn needle haystack
        (before, after) = result
    in counterexample ("breakOn result: " ++ show result) $
       case needle of
         [] -> property $ before == "" && after == haystack
         _ -> property $ before ++ needle ++ after == haystack

-- | Test case 9: Test comment removal with escaped quotes
test_removeComments_escaped_quotes :: TestTree
test_removeComments_escaped_quotes = testCase "removeComments handles escaped quotes" $ do
    let input = "str = \"hello \\\"world\\\" // not comment\" // actual comment"
    let expected = "str = \"hello \\\"world\\\" // not comment\" "
    assertEqual "escaped quotes in comments" expected (removeComments input)

-- | Test case 10: Property test for removeComments structure preservation
prop_removeComments_preserves_structure :: String -> Property
prop_removeComments_preserves_structure str =
    let withoutComments = removeComments str
        lineCount original = length $ lines str
        lineCount processed = length $ lines withoutComments
    in property $ lineCount processed <= lineCount original

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Test Suite"
    [ testGroup "Unit Tests"
        [ test_trim_unicode_special_chars
        , test_splitBy_edge_cases
        , test_removeComments_complex
        , test_normalizeIndentation_mixed
        , test_removeComments_escaped_quotes
        ]
    , testGroup "QuickCheck Properties"
        [ fastProperty "splitBy consistency" prop_splitBy_consistency
        , fastProperty "trim idempotency" prop_trim_idempotent
        , fastProperty "splitByCollapsed length constraint" prop_splitByCollapsed_length
        , fastProperty "breakOn correctness" prop_breakOn_correctness
        , fastProperty "removeComments preserves structure" prop_removeComments_preserves_structure
        ]
    ]