{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec1 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd)
import Parser (parseTypus)

-- | 测试用例1: 字符串处理函数的边界条件
tests :: TestTree
tests = 
  testGroup "New Cabal Test 1 - String Processing Edge Cases"
    [ testCase "trim handles null bytes correctly" $ do
        let input = "\0hello\0world\0"
            expected = "hello\0world\0"
        trim input @?= expected

    , testCase "splitBy with empty delimiter creates list of characters" $ do
        let input = "abc"
            result = splitBy '\0' input
        result @?= ["a", "b", "c"]

    , testCase "removeComments handles nested block comments" $ do
        let input = "code /* outer /* inner */ still outer */ more code"
            expected = "code  still outer  more code"
        removeComments input @?= expected

    , testCase "normalizeIndentation preserves tab characters in content" $ do
        let input = "    line1\n\t\tline2\n    line3"
            result = normalizeIndentation input
            lines' = lines result
        length lines' @?= 3
        "\t\t" `isInfixOf` (lines' !! 1) @?= True

    -- QuickCheck properties
    , fastProperty "trim idempotency" prop_trim_idempotent
    , fastProperty "splitBy length property" prop_splitBy_length
    , fastProperty "removeComments preserves non-comment content" prop_removeComments_preserves_content
    , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_lines
    ]

-- QuickCheck properties

-- Property: trim is idempotent (applying twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent input =
  let trimmedOnce = trim input
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- Property: splitBy creates segments that sum to original length
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim input =
  let segments = splitBy delim input
      totalLength = sum (map length segments) + length (filter (== delim) input)
  in property $ totalLength === length input

-- Property: removeComments preserves non-comment content
prop_removeComments_preserves_content :: String -> String -> Property
prop_removeComments_preserves_content prefix suffix =
  -- Avoid strings with comment markers
  not ("/*" `isInfixOf` prefix) && not ("*/" `isInfixOf` prefix) && 
  not ("//" `isInfixOf` prefix) && not ("/*" `isInfixOf` suffix) && 
  not ("*/" `isInfixOf` suffix) && not ("//" `isInfixOf` suffix) ==>
  let content = prefix ++ "code" ++ suffix
      withComments = prefix ++ "/*comment*/" ++ "code" ++ "/*comment*/" ++ suffix
      processed = removeComments withComments
  in property $ content `isInfixOf` processed

-- Property: normalizeIndentation preserves line count
prop_normalizeIndentation_preserves_lines :: String -> Property
prop_normalizeIndentation_preserves_lines input =
  let normalized = normalizeIndentation input
      originalLines = length (lines input)
      normalizedLines = length (lines normalized)
  in property $ originalLines === normalizedLines