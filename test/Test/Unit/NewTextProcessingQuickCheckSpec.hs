{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.NewTextProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
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
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, toLower)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)

-- | 新的文本处理QuickCheck测试套件
tests :: TestTree
tests =
  testGroup "New Text Processing QuickCheck Tests"
    [ fastProperty "trim preserves non-space characters" prop_trim_preserves_content
    , fastProperty "splitBy preserves order of segments" prop_splitBy_preserves_order
    , fastProperty "splitByCollapsed removes consecutive delimiters" prop_splitByCollapsed_removes_consecutive
    , fastProperty "removeLineComments handles nested quotes correctly" prop_removeLineComments_nested_quotes
    , fastProperty "removeComments preserves string literals with comment patterns" prop_removeComments_preserves_string_literals
    , fastProperty "normalizeIndentation maintains relative indentation differences" prop_normalizeIndentation_maintains_relative
    , fastProperty "forceSingleTabIndentation converts all non-empty lines to tab format" prop_forceSingleTabIndentation_tab_format
    , fastProperty "breakOn with empty pattern returns empty prefix" prop_breakOn_empty_pattern
    , fastProperty "Complex comment removal preserves code structure" prop_complex_comment_preservation
    , fastProperty "String processing pipeline is consistent" prop_processing_pipeline_consistency
    ]

-- Property: trim preserves non-space characters
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content input =
  let trimmed = trim input
      nonSpaceOriginal = filter (not . isSpace) input
      nonSpaceTrimmed = filter (not . isSpace) trimmed
  in property $ nonSpaceTrimmed === nonSpaceOriginal

-- Property: splitBy preserves order of segments
prop_splitBy_preserves_order :: Char -> String -> Property
prop_splitBy_preserves_order delim input =
  let segments = splitBy delim input
      rejoined = Data.List.intercalate [delim] segments
  in property $ rejoined === input

-- Property: splitByCollapsed removes consecutive delimiters
prop_splitByCollapsed_removes_consecutive :: Char -> String -> Property
prop_splitByCollapsed_removes_consecutive delim input =
  let segments = splitByCollapsed delim input
      hasConsecutive = delim `elem` input && [delim, delim] `isInfixOf` input
  in classify hasConsecutive "has consecutive delimiters" $
     property $ all (not . null) segments

-- Property: removeLineComments handles nested quotes correctly
prop_removeLineComments_nested_quotes :: String -> String -> Property
prop_removeLineComments_nested_quotes code comment =
  not (any (`elem` "\"'\\") code) && not (any (`elem` "\"'\\") comment) ==>
  let input = code ++ " // comment with \"nested quotes\" // more\n" ++ code
      result = removeLineComments input
  in property $ not ("// comment" `isInfixOf` result) .&&.
     code `isInfixOf` result

-- Property: removeComments preserves string literals with comment patterns
prop_removeComments_preserves_string_literals :: String -> String -> Property
prop_removeComments_preserves_string_literals content1 content2 =
  not (any (`elem` "\"'\\") content1) && not (any (`elem` "\"'\\") content2) ==>
  let input = "var s = \"// not comment /* not block */ " ++ content1 ++ "\"\n" ++
              "var t = \"/* also not comment */ " ++ content2 ++ "\"\n" ++
              "// real comment\n" ++
              "/* real block */"
      result = removeComments input
  in property $ "// not comment /* not block */" `isInfixOf` result .&&.
     "/* also not comment */" `isInfixOf` result .&&.
     not ("// real comment" `isInfixOf` result) .&&.
     not ("/* real block */" `isInfixOf` result)

-- Property: normalizeIndentation maintains relative indentation differences
prop_normalizeIndentation_maintains_relative :: [Int] -> String -> Property
prop_normalizeIndentation_maintains_relative indentLevels content =
  not (null indentLevels) && not (any (`elem` content) "\r\n") ==>
  let maxIndent = 20
      normalizedIndents = map (`mod` maxIndent) indentLevels
      inputLines = zipWith (\level content' -> replicate level ' ' ++ content') 
                           normalizedIndents (repeat content)
      input = unlines inputLines
      result = normalizeIndentation input
      resultLines = lines result
      -- Calculate relative indentation differences
      originalDiffs = zipWith (-) (tail normalizedIndents) (normalizedIndents)
      resultIndents = map (length . takeWhile isSpace) resultLines
      resultDiffs = zipWith (-) (tail resultIndents) resultIndents
  in length resultLines === length inputLines .&&.
     (if length resultLines > 1 then resultDiffs === originalDiffs else property True)

-- Property: forceSingleTabIndentation converts all non-empty lines to tab format
prop_forceSingleTabIndentation_tab_format :: [String] -> Property
prop_forceSingleTabIndentation_tab_format lines =
  not (null lines) ==>
  let input = unlines lines
      result = forceSingleTabIndentation input
      resultLines = lines result
      nonEmptyLines = filter (not . null . trim) resultLines
  in property $ all (\line -> case line of ('\t':_) -> True; _ -> False) nonEmptyLines

-- Property: breakOn with empty pattern returns empty prefix
prop_breakOn_empty_pattern :: String -> Property
prop_breakOn_empty_pattern input =
  let (prefix, suffix) = breakOn "" input
  in property $ prefix === "" .&&. suffix === input

-- Property: Complex comment removal preserves code structure
prop_complex_comment_preservation :: String -> String -> String -> Property
prop_complex_comment_preservation before middle after =
  not (any (`elem` "\"'\\") [before, middle, after]) &&
  not (any (`isInfixOf` before) ["/*", "*/", "//"]) &&
  not (any (`isInfixOf` middle) ["/*", "*/", "//"]) &&
  not (any (`isInfixOf` after) ["/*", "*/", "//"]) ==>
  let input = before ++ "\n/* block comment\nwith multiple lines */\n" ++
              middle ++ " // line comment\n" ++
              after ++ "\n/* another block */\n"
      result = removeComments input
      resultLines = lines result
      beforeLines = lines before
      middleLines = lines middle
      afterLines = lines after
  in property $ not ("/* block comment" `isInfixOf` result) .&&.
     not ("// line comment" `isInfixOf` result) .&&.
     not ("/* another block */" `isInfixOf` result) .&&.
     before `isInfixOf` result .&&.
     middle `isInfixOf` result .&&.
     after `isInfixOf` result

-- Property: String processing pipeline is consistent
prop_processing_pipeline_consistency :: String -> Property
prop_processing_pipeline_consistency input =
  let pipeline1 = input |> trim |> removeComments |> normalizeIndentation
      pipeline2 = input |> removeComments |> trim |> normalizeIndentation
      pipeline3 = input |> normalizeIndentation |> trim |> removeComments
  in property $ pipeline1 === pipeline2 .||. pipeline2 === pipeline3 .||. pipeline1 === pipeline3

-- Helper function for pipeline composition
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- Additional edge case properties

-- Property: splitBy with Unicode delimiters
prop_splitBy_unicode :: String -> String -> Property
prop_splitBy_unicode delim input =
  not (null delim) ==>
  let unicodeInput = input ++ "测试🚀" ++ delim ++ "café naïve"
      result = splitBy (head delim) unicodeInput
  in property $ concat result `isInfixOf` unicodeInput

-- Property: removeComments handles malformed block comments gracefully
prop_removeComments_malformed_blocks :: String -> String -> Property
prop_removeComments_malformed_blocks before after =
  not (any (`elem` "\"'\\") [before, after]) &&
  not (any (`isInfixOf` before) ["/*", "*/"]) &&
  not (any (`isInfixOf` after) ["/*", "*/"]) ==>
  let input = before ++ "/* unclosed block comment\n" ++ after
      result = removeComments input
  in property $ length result <= length input .&&.
     before `isInfixOf` result

-- Property: normalizeIndentation handles mixed tabs and spaces
prop_normalizeIndentation_mixed_whitespace :: [Int] -> [Int] -> String -> Property
prop_normalizeIndentation_mixed_whitespace spacesCounts tabsCounts content =
  not (null spacesCounts) && not (null tabsCounts) &&
  all (>=0) spacesCounts && all (<10) spacesCounts &&
  all (>=0) tabsCounts && all (<5) tabsCounts &&
  not (any (`elem` content) "\r\n") ==>
  let lines' = zipWith (\s t -> replicate s ' ' ++ replicate t '\t' ++ content) 
                       spacesCounts tabsCounts
      input = unlines lines'
      result = normalizeIndentation input
      resultLines = lines result
  in property $ length resultLines === length lines' .&&.
     all (\line -> null (trim line) || not (isSpace (head line))) resultLines