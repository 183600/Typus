{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewUtilsComprehensiveSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, Arbitrary, arbitrary, oneof, elements, listOf, resize, choose)
import Data.Char (isSpace, isAlphaNum, isAlpha, isDigit, toLower, toUpper)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (sort, group, intercalate)
import qualified Data.Text as T
import Data.String (IsString)

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

-- ============================================================================
-- Arbitrary Instances L.and Generators
-- ============================================================================

-- Generate strings with various whitespace patterns
whitespaceString :: Gen String
whitespaceString = listOf $ elements " \t\n\r"

-- Generate strings with mixed content
mixedContentString :: Gen String
mixedContentString = do
  base <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " !@#$%^&*()_+-=[]{}|;':\",./<>?"
  ws1 <- whitespaceString
  ws2 <- whitespaceString
  return $ ws1 ++ base ++ ws2

-- Generate strings with comment patterns
commentString :: Gen String
commentString = do
  content <- mixedContentString
  comment <- mixedContentString
  oneof
    [ return $ content ++ " // " ++ comment
    , return $ content ++ " /* " ++ comment ++ " */ " ++ content
    , return $ "// " ++ comment ++ "\n" ++ content
    , return $ "/* " ++ comment ++ " */\n" ++ content
    ]

-- Generate strings with indentation
indentedString :: Gen Int -> Gen String
indentedString indentGen = do
  indent <- indentGen
  content <- mixedContentString
  return $ replicate indent ' ' ++ content

-- Generate multi-line strings
multiLineString :: Gen String
multiLineString = do
  numLines <- choose (1, 5)
  lines <- listOf $ mixedContentString
  return $ unlines $ take numLines lines

-- ============================================================================
-- Advanced Property Tests
-- ============================================================================

-- Property: trim removes L.all leading/trailing whitespace but preserves internal
prop_trim_comprehensive :: Property
prop_trim_comprehensive =
  forAll mixedContentString $ \content ->
  forAll whitespaceString $ \leading ->
  forAll whitespaceString $ \trailing ->
    let input = leading ++ content ++ trailing
        trimmed = trim input
        hasLeadingSpace = not (null leading) && L.any isSpace leading
        hasTrailingSpace = not (null trailing) && L.any isSpace trailing
        noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
        noTrailingSpace = null trimmed || not (isSpace (last trimmed))
        contentPreserved = content `L.isInfixOf` trimmed
    in classify hasLeadingSpace "has leading whitespace" $
       classify hasTrailingSpace "has trailing whitespace" $
       property $ noLeadingSpace .&&. noTrailingSpace .&&. contentPreserved

-- Property: splitBy preserves order L.and content
prop_splitBy_order_preservation :: Property
prop_splitBy_order_preservation =
  forAll (listOf mixedContentString) $ \parts ->
  forAll (elements $ ['\0'..'\127'] \ ['\n', '\r']) $ \delim ->
    let input = intercalate [delim] parts
        result = splitBy delim input
    in property $ result === parts

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_no_empty :: Property
prop_splitByCollapsed_no_empty =
  forAll mixedContentString $ \content ->
  forAll (elements $ ['\0'..'\127'] \ ['\n', '\r']) $ \delim ->
    let input = content ++ [delim, delim] ++ content ++ [delim] ++ [delim]
        result = splitByCollapsed delim input
    in property $ L.all (not . null) result

-- Property: removeLineComments preserves non-comment content
prop_removeLineComments_preservation :: Property
prop_removeLineComments_preservation =
  forAll mixedContentString $ \content ->
  forAll mixedContentString $ \comment ->
    let input = content ++ " // " ++ comment ++ "\n" ++ content
        result = removeLineComments input
    in not ('"' `elem` content) && not ('\'' `elem` content) ==>
       property $ content `L.isInfixOf` result .&&. not ("//" `L.isInfixOf` result)

-- Property: removeComments handles nested quotes correctly
prop_removeComments_quotes :: Property
prop_removeComments_quotes =
  forAll mixedContentString $ \content ->
  forAll mixedContentString $ \comment ->
    let input = "var s = \"// not comment " ++ comment ++ "\" // real comment\n" ++ content
        result = removeComments input
    in not ('"' `elem` comment) && not ('\\' `elem` comment) ==>
       property $ "// not comment" `L.isInfixOf` result .&&. 
                  not ("// real comment" `L.isInfixOf` result) .&&.
                  content `L.isInfixOf` result

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentation_relative_structure :: Property
prop_normalizeIndentation_relative_structure =
  forAll (choose (1, 5)) $ \numLines ->
  forAll (listOf (choose (0, 10))) $ \indents ->
  forAll (listOf mixedContentString) $ \contents ->
    let lines' = zipWith (\indent content -> replicate indent ' ' ++ content) 
                         (take numLines indents) 
                         (take numLines contents)
        input = unlines lines'
        result = normalizeIndentation input
        resultLines = lines result
        -- Check that relative indentation is preserved
        indentDiffs input = zipWith (-) (L.map (L.length . takeWhile isSpace) (L.tail lines')) 
                                      (L.map (L.length . takeWhile isSpace) lines')
        indentDiffs result = zipWith (-) (L.map (L.length . takeWhile isSpace) (L.tail resultLines)) 
                                       (L.map (L.length . takeWhile isSpace) resultLines)
    in L.length lines' > 1 ==>
       property $ indentDiffs input == indentDiffs result

-- Property: forceSingleTabIndentation enforces tab prefix
prop_forceSingleTabIndentation_tab_enforcement :: Property
prop_forceSingleTabIndentation_tab_enforcement =
  forAll multiLineString $ \input ->
    let result = forceSingleTabIndentation input
        resultLines = lines result
        nonEmptyLines = L.filter (not . null . trim) resultLines
    in property $ L.all (\line -> L.null (trim line) || L.head line == '\t') nonEmptyLines

-- Property: breakOn finds first occurrence correctly
prop_breakOn_first_occurrence :: Property
prop_breakOn_first_occurrence =
  forAll mixedContentString $ \prefix ->
  forAll mixedContentString $ \suffix ->
  forAll mixedContentString $ \pattern ->
    not (null pattern) ==>
      let input = prefix ++ pattern ++ suffix ++ pattern ++ "end"
          (before, after) = breakOn pattern input
      in property $ before === prefix ++ pattern ++ suffix .&&. after === "end"

-- Property: Complex comment removal pipeline
prop_comment_pipeline :: Property
prop_comment_pipeline =
  forAll commentString $ \input ->
    let step1 = removeLineComments input
        step2 = removeComments step1
        step3 = removeComments input  -- Direct removal
    in property $ not ("//" `L.isInfixOf` step2) .&&. 
               not ("/*" `L.isInfixOf` step2) .&&.
               not ("*/" `L.isInfixOf` step2)

-- Property: Unicode handling in L.all functions
prop_unicode_handling :: Property
prop_unicode_handling =
  let unicodeContent = "café naïve résumé 🚀 测试 こんにちは"
      unicodeComment = "üñïçødé çømméñt 🌟"
      input = unicodeContent ++ " // " ++ unicodeComment ++ "\n" ++ unicodeContent
  in property $ trim unicodeContent === unicodeContent .&&.
             splitBy ' ' unicodeContent === words unicodeContent .&&.
             unicodeContent `L.isInfixOf` removeLineComments input .&&.
             not (unicodeComment `L.isInfixOf` removeLineComments input)

-- Property: Performance with large inputs
prop_performance_large :: Property
prop_performance_large =
  forAll (choose (1, 100)) $ \multiplier ->
  forAll mixedContentString $ \base ->
    let largeContent = L.concat (replicate multiplier base)
        trimmed = trim largeContent
        split = splitBy ',' largeContent
        commentsRemoved = removeLineComments largeContent
    in property $ L.length trimmed <= L.length largeContent .&&.
               L.length split >= 1 .&&.
               L.length commentsRemoved <= L.length largeContent

-- Property: Edge cases with special characters
prop_special_characters :: Property
prop_special_characters =
  let specialChars = "\0\1\2\3\4\5\6\7\8\10\11\12\13\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31\127"
      content = "normal" ++ specialChars ++ "content"
  in property $ trim content === "normal" ++ specialChars ++ "content" .&&.
             splitBy '\0' content === ["normal", specialChars ++ "content"] .&&.
             breakOn specialChars content === ("normal", "content")

-- ============================================================================
-- Unit Tests
-- ============================================================================

-- Test trim with various whitespace combinations
test_trim_variations :: TestTree
test_trim_variations =
  testCase "Trim variations" $ do
    trim "\t  hello  world \n" @?= "hello  world"
    trim "   " @?= ""
    trim "" @?= ""
    trim "\n\n\n" @?= ""
    trim "no-whitespace" @?= "no-whitespace"

-- Test splitBy edge cases
test_splitBy_edge_cases :: TestTree
test_splitBy_edge_cases =
  testCase "SplitBy edge cases" $ do
    splitBy ',' "" @?= [""]
    splitBy ',' "a,b,c" @?= ["a", "b", "c"]
    splitBy ',' ",a,b," @?= ["", "a", "b", ""]
    splitBy ',' "a,,b" @?= ["a", "", "b"]
    splitBy ',' "abc" @?= ["abc"]

-- Test splitByCollapsed behavior
test_splitByCollapsed_behavior :: TestTree
test_splitByCollapsed_behavior =
  testCase "SplitByCollapsed behavior" $ do
    splitByCollapsed ',' "" @?= []
    splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
    splitByCollapsed ',' ",a,b," @?= ["a", "b"]
    splitByCollapsed ',' "a,,b" @?= ["a", "b"]
    splitByCollapsed ',' "abc" @?= ["abc"]

-- Test comment removal with complex strings
test_complex_comments :: TestTree
test_complex_comments =
  testCase "Complex comment removal" $ do
    let input = unlines
          [ "var s = \"// not a comment\""
          , "var c = '/' // also not a comment"
          , "// this is a real comment"
          , "var x = 42 /* block comment */"
          , "var y = \"/* not a block */\" // real comment"
          ]
        expected = unlines
          [ "var s = \"// not a comment\""
          , "var c = '/' "
          , ""
          , "var x = 42 "
          , "var y = \"/* not a block */\" "
          ]
    removeComments input @?= expected

-- Test indentation normalization
test_indentation_normalization :: TestTree
test_indentation_normalization =
  testCase "Indentation normalization" $ do
    let input = unlines
          [ "    func main() {"
          , "        fmt.Println(\"hello\")"
          , "    }"
          , ""
          ]
        expected = unlines
          [ "func main() {"
          , "    fmt.Println(\"hello\")"
          , "}"
          , ""
          ]
    normalizeIndentation input @?= expected

-- Test forceSingleTabIndentation
test_force_single_tab :: TestTree
test_force_single_tab =
  testCase "Force single tab indentation" $ do
    let input = unlines
          [ "  line1"
          , "    line2"
          , "line3"
          , ""
          ]
        expected = unlines
          [ "\tline1"
          , "\tline2"
          , "\tline3"
          , ""
          ]
    forceSingleTabIndentation input @?= expected

-- Test breakOn functionality
test_break_on_functionality :: TestTree
test_break_on_functionality =
  testCase "BreakOn functionality" $ do
    breakOn "world" "hello world test" @?= ("hello ", " test")
    breakOn "xyz" "hello world" @?= ("hello world", "")
    breakOn "" "abc" @?= ("", "abc")
    breakOn "abc" "abc" @?= ("", "")

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Utils Comprehensive Tests"
    [ testGroup "Advanced property tests"
        [ fastProperty "trim removes L.all leading/trailing whitespace but preserves internal" prop_trim_comprehensive
        , fastProperty "splitBy preserves order L.and content" prop_splitBy_order_preservation
        , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
        , fastProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preservation
        , fastProperty "removeComments handles nested quotes correctly" prop_removeComments_quotes
        , fastProperty "normalizeIndentation preserves relative structure" prop_normalizeIndentation_relative_structure
        , fastProperty "forceSingleTabIndentation enforces tab prefix" prop_forceSingleTabIndentation_tab_enforcement
        , fastProperty "breakOn finds first occurrence correctly" prop_breakOn_first_occurrence
        , fastProperty "Complex comment removal pipeline" prop_comment_pipeline
        , fastProperty "Unicode handling in L.all functions" prop_unicode_handling
        , fastProperty "Performance with large inputs" prop_performance_large
        , fastProperty "Edge cases with special characters" prop_special_characters
        ]
    , testGroup "Unit tests"
        [ test_trim_variations
        , test_splitBy_edge_cases
        , test_splitByCollapsed_behavior
        , test_complex_comments
        , test_indentation_normalization
        , test_force_single_tab
        , test_break_on_functionality
        ]
    ]