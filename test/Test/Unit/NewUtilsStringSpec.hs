{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsStringSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genNonEmptyString, genStringWithSpecialChars)

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

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort)

-- Property: trim is idempotent (applying it twice gives same result as once)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: splitBy on empty string returns singleton list with empty string
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty c = splitBy c "" === [""]

-- Property: splitByCollapsed on empty string returns empty list
prop_splitByCollapsed_empty :: Char -> Property
prop_splitByCollapsed_empty c = splitByCollapsed c "" === []

-- Property: splitBy and splitByCollapsed relationship for non-empty delimiter
prop_splitBy_vs_collapsed :: Char -> String -> Property
prop_splitBy_vs_collapsed delim s = 
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in property $ collapsed == filter (not . null) normal

-- Property: removeLineComments preserves content before comment marker
prop_removeLine_comments_preserves_before :: String -> String -> Property
prop_removeLine_comments_preserves_before prefix suffix =
  let line = prefix ++ "//" ++ suffix
      result = removeLineComments line
  in property $ prefix `isPrefixOf` result

-- Property: removeLineComments handles strings with comment markers
prop_remove_line_comments_string_literals :: String -> Property
prop_remove_line_comments_string_literals content =
  let quoted = "\"" ++ content ++ "\" // comment"
      result = removeLineComments quoted
  in property $ ("\"" ++ content ++ "\" ") `isPrefixOf` result

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative multiline =
  let linesList = lines multiline
      hasMultipleLines = length linesList > 1
  in classify hasMultipleLines "multiple lines" $
     property $ 
       let normalized = normalizeIndentation multiline
           normLines = lines normalized
           originalLines = lines multiline
           -- Extract indentation patterns (count leading spaces)
           getIndent l = length $ takeWhile isSpace l
           originalIndents = map getIndent $ dropWhile (null . dropWhile isSpace) originalLines
           normIndents = map getIndent $ dropWhile (null . dropWhile isSpace) normLines
           -- Check that relative differences are preserved
           relativeDiffs orig = zipWith (-) (tail orig) (init orig)
       in length originalIndents > 1 ==> relativeDiffs originalIndents === relativeDiffs normIndents

-- Property: breakOn finds first occurrence or returns original string
prop_breakOn_behavior :: String -> String -> Property
prop_breakOn_behavior needle haystack =
  let result = breakOn needle haystack
  in if needle `isInfixOf` haystack
     then property $ 
       let (before, after) = result
       in before ++ needle `isPrefixOf` haystack
     else result === (haystack, "")

tests :: TestTree
tests =
  testGroup "New Utils String Tests"
    [ testGroup "String Properties"
        [ fastProperty "trim is idempotent" prop_trim_idempotent
        , fastProperty "splitBy on empty returns [\"\"]" prop_splitBy_empty
        , fastProperty "splitByCollapsed on empty returns []" prop_splitByCollapsed_empty
        , fastProperty "splitBy vs splitByCollapsed relationship" prop_splitBy_vs_collapsed
        ]
    , testGroup "Comment Processing"
        [ fastProperty "removeLineComments preserves content before marker" prop_remove_line_comments_preserves_before
        , fastProperty "removeLineComments handles string literals" prop_remove_line_comments_string_literals
        ]
    , testGroup "Indentation and Splitting"
        [ fastProperty "normalizeIndentation preserves relative indentation" prop_normalize_indentation_preserves_relative
        , fastProperty "breakOn behavior" prop_breakOn_behavior
        ]
    ]