{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.UtilsStringProcessingTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements
  , vectorOf, oneof, frequency, suchThat, Positive(..)
  )

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

import Data.Char (isSpace, toLower, toUpper)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.List (sort, nub)
import qualified Data.Text as T

-- | Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
genWhitespaceString = do
  content <- listOf $ elements ['a'..'z']
  leading <- listOf $ elements " \t\n\r"
  trailing <- listOf $ elements " \t\n\r"
  return $ leading ++ content ++ trailing

-- | Generate strings with embedded comments
genCommentString :: Gen String
genCommentString = do
  code <- listOf $ elements ['a'..'z']
  comment <- listOf $ elements ['A'..'Z']
  return $ code ++ " // " ++ comment

-- | Generate strings with block comments
genBlockCommentString :: Gen String
genBlockCommentString = do
  before <- listOf $ elements ['a'..'z']
  comment <- listOf $ elements ['A'..'Z']
  after <- listOf $ elements ['a'..'z']
  return $ before ++ " /* " ++ comment ++ " */ " ++ after

-- | Generate strings with mixed indentation
genIndentedString :: Gen String
genIndentedString = do
  lines <- listOf $ do
    indent <- choose (0, 8)
    content <- listOf $ elements ['a'..'z']
    return $ replicate indent ' ' ++ content
  return $ unlines lines

-- | Generate comma-separated values
genCSVString :: Gen String
genCSVString = do
  values <- listOf $ do
    value <- listOf $ elements ['a'..'z']
    return $ value
  return $ intercalate "," values
  where
    intercalate sep [] = ""
    intercalate sep [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Property tests

-- Property: trim removes L.all leading L.and trailing whitespace
prop_trim_removes_whitespace :: Property
prop_trim_removes_whitespace =
  forAll genWhitespaceString $ \str ->
    let trimmed = trim str
        hasLeading = not (null trimmed) && isSpace (L.head trimmed)
        hasTrailing = not (null trimmed) && isSpace (last trimmed)
    in property $ not (hasLeading .||. hasTrailing)

-- Property: trim is idempotent
prop_trim_idempotent :: Property
prop_trim_idempotent =
  forAll genWhitespaceString $ \str ->
    let trimmed1 = trim str
        trimmed2 = trim trimmed1
    in property $ trimmed1 === trimmed2

-- Property: splitBy preserves empty segments
prop_splitBy_preserves_empty :: Property
prop_splitBy_preserves_empty =
  forAll (choose ('a', 'z')) $ \delim ->
  forAll (listOf $ elements [delim, 'x']) $ \str ->
    let segments = splitBy delim str
        expectedCount = L.length (L.filter (== delim) str) + 1
    in property $ L.length segments === expectedCount

-- Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_removes_empty :: Property
prop_splitByCollapsed_removes_empty =
  forAll (choose ('a', 'z')) $ \delim ->
  forAll (listOf $ elements [delim, 'x']) $ \str ->
    let segments = splitByCollapsed delim str
    in property $ L.all (not . null) segments

-- Property: splitByComma is splitBy with comma
prop_splitByComma_is_splitBy :: Property
prop_splitByComma_is_splitBy =
  forAll genCSVString $ \str ->
    splitByComma str === splitBy ',' str

-- Property: splitByCommaCollapsed is splitByCollapsed with comma
prop_splitByCommaCollapsed_is_splitByCollapsed :: Property
prop_splitByCommaCollapsed_is_splitByCollapsed =
  forAll genCSVString $ \str ->
    splitByCommaCollapsed str === splitByCollapsed ',' str

-- Property: removeLineComments removes // comments
prop_removeLineComments_removes :: Property
prop_removeLineComments_removes =
  forAll genCommentString $ \str ->
    let cleaned = removeLineComments str
    in property $ not ("//" `L.isInfixOf` cleaned)

-- Property: removeComments removes both // L.and /* */ comments
prop_removeComments_removes_both :: Property
prop_removeComments_removes_both =
  forAll genBlockCommentString $ \str ->
    let cleaned = removeComments str
    in property $ not ("/*" `L.isInfixOf` cleaned) .&&. not ("*/" `L.isInfixOf` cleaned)

-- Property: normalizeIndentation removes common prefix
prop_normalizeIndentation_removes_prefix :: Property
prop_normalizeIndentation_removes_prefix =
  forAll genIndentedString $ \str ->
    let normalized = normalizeIndentation str
        lines' = lines normalized
        nonEmptyLines = L.filter (not . null) lines'
    in if null nonEmptyLines
       then property True
       else property $ L.all (\line -> not (isPrefixOf "    " line)) nonEmptyLines

-- Property: forceSingleTabIndentation uses tabs
prop_forceSingleTabIndentation_uses_tabs :: Property
prop_forceSingleTabIndentation_uses_tabs =
  forAll genIndentedString $ \str ->
    let tabbed = forceSingleTabIndentation str
        lines' = lines tabbed
        nonEmptyLines = L.filter (not . null . trim) lines'
    in if null nonEmptyLines
       then property True
       else property $ L.all (\line -> isPrefixOf "\t" line) nonEmptyLines

-- Property: fixIndentation equals normalizeIndentation
prop_fixIndentation_equals_normalize :: Property
prop_fixIndentation_equals_normalize =
  forAll genIndentedString $ \str ->
    fixIndentation str === normalizeIndentation str

-- Property: breakOn finds first occurrence
prop_breakOn_finds_first :: Property
prop_breakOn_finds_first =
  forAll (listOf $ elements ['a'..'z']) $ \pat ->
  not (null pat) ==>
  forAll (listOf $ elements ['a'..'z']) $ \prefix ->
  forAll (listOf $ elements ['a'..'z']) $ \suffix ->
    let haystack = prefix ++ pat ++ suffix ++ pat ++ "extra"
        (before, after) = breakOn pat haystack
    in property $ before === prefix ++ pat ++ suffix .&&. after === "extra"

-- Property: breakOn handles empty pattern
prop_breakOn_empty_pattern :: Property
prop_breakOn_empty_pattern =
  forAll (listOf $ elements ['a'..'z']) $ \haystack ->
    let (before, after) = breakOn "" haystack
    in property $ before === "" .&&. after === haystack

-- Property: breakOn handles missing pattern
prop_breakOn_missing_pattern :: Property
prop_breakOn_missing_pattern =
  forAll (listOf $ elements ['a'..'z']) $ \pat ->
  forAll (listOf $ elements ['a'..'z']) $ \haystack ->
    not (null pat) && not (pat `L.isInfixOf` haystack) ==>
    let (before, after) = breakOn pat haystack
    in property $ before === haystack .&&. after === ""

-- Property: splitBy L.and join roundtrip
prop_splitBy_join_roundtrip :: Property
prop_splitBy_join_roundtrip =
  forAll (choose ('a', 'z')) $ \delim ->
  forAll (listOf $ elements ['a'..'z', delim]) $ \input ->
    let parts = splitBy delim input
        rejoined = L.concat $ intersperse [delim] parts
    in rejoined === input
  where
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x ++ sep : intersperse sep (y:xs)

-- Property: removeComments preserves non-comment content
prop_removeComments_preserves_content :: Property
prop_removeComments_preserves_content =
  forAll (listOf $ elements ['a'..'z']) $ \content ->
  forAll (listOf $ elements ['A'..'Z']) $ \comment ->
    let input = content ++ " /* " ++ comment ++ " */ " ++ content
        cleaned = removeComments input
    in property $ content `L.isInfixOf` cleaned

-- Unit tests

unit_tests :: TestTree
unit_tests = testGroup "Utils String Processing Unit Tests"
  [ testCase "trim removes leading L.and trailing whitespace" $ do
      trim "  hello  " @?= "hello"
      trim "\t\n  hello\r\n  " @?= "hello"
      trim "hello" @?= "hello"
      trim "" @?= ""
      trim "   " @?= ""

  , testCase "splitBy with various delimiters" $ do
      splitBy ',' "a,b,c" @?= ["a", "b", "c"]
      splitBy ',' "a,,b" @?= ["a", "", "b"]
      splitBy ',' ",a," @?= ["", "a", ""]
      splitBy ',' "" @?= [""]
      splitBy 'x' "axbxc" @?= ["a", "b", "c"]

  , testCase "splitByCollapsed removes empty segments" $ do
      splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
      splitByCollapsed ',' "a,,b" @?= ["a", "b"]
      splitByCollapsed ',' ",a," @?= ["a"]
      splitByCollapsed ',' "" @?= []

  , testCase "splitByComma functions" $ do
      splitByComma "a,b,c" @?= ["a", "b", "c"]
      splitByCommaCollapsed "a,,b,c" @?= ["a", "b", "c"]

  , testCase "removeLineComments" $ do
      removeLineComments "code // comment" @?= "code "
      removeLineComments "code // comment\nmore code" @?= "code \nmore code"
      removeLineComments "code // comment // another" @?= "code  "
      removeLineComments "no comment" @?= "no comment"

  , testCase "removeComments with line L.and block comments" $ do
      removeComments "code // line comment\nmore /* block */ code" @?= "code \nmore  code"
      removeComments "/* block comment */ code" @?= "  code"
      removeComments "code /* multi\nline\ncomment */ more" @?= "code  more"

  , testCase "normalizeIndentation" $ do
      normalizeIndentation "    line1\n    line2" @?= "line1\nline2"
      normalizeIndentation "  line1\n    line2\n  line3" @?= "line1\n  line2\nline3"
      normalizeIndentation "line1\nline2" @?= "line1\nline2"

  , testCase "forceSingleTabIndentation" $ do
      forceSingleTabIndentation "    line" @?= "\tline\n"
      forceSingleTabIndentation "  line\n    line2" @?= "\tline\n\tline2\n"

  , testCase "fixIndentation" $ do
      fixIndentation "    line1\n    line2" @?= "line1\nline2"
      fixIndentation "  line1\n    line2" @?= "line1\n  line2"

  , testCase "breakOn functionality" $ do
      breakOn "world" "hello world test" @?= ("hello world", " test")
      breakOn "x" "axbxc" @?= ("ax", "bxc")
      breakOn "missing" "hello world" @?= ("hello world", "")
      breakOn "" "test" @?= ("", "test")

  , testCase "edge cases" $ do
      trim "" @?= ""
      splitBy ',' "" @?= [""]
      splitByCollapsed ',' "" @?= []
      removeLineComments "" @?= ""
      removeComments "" @?= ""
      normalizeIndentation "" @?= ""
      breakOn "x" "" @?= ("", "")

  , testCase "unicode handling" $ do
      trim "  测试  " @?= "测试"
      splitBy ',' "a,测试,c" @?= ["a", "测试", "c"]
      removeLineComments "测试 // comment" @?= "测试 "

  , testCase "performance with large strings" $ do
      let largeString = replicate 10000 'a' ++ "   "
          trimmed = trim largeString
      assertBool "should handle large strings" $ L.length trimmed >= 10000

  , testCase "complex scenarios" $ do
      let complexCode = unlines
            [ "    func main() {"
            , "        // This is a comment"
            , "        x := 42 /* block comment */"
            , "        fmt.Println(x) // another comment"
            , "    }"
            ]
          processed = removeComments $ normalizeIndentation complexCode
      assertBool "should process complex code" $ 
        not ("//" `L.isInfixOf` processed) && 
        not ("/*" `L.isInfixOf` processed) &&
        not ("    " `L.isPrefixOf` processed)
  ]

-- Advanced tests

advanced_tests :: TestTree
advanced_tests = testGroup "Advanced String Processing Tests"
  [ testCase "nested comment removal" $ do
      let nested = "code /* outer /* inner */ still outer */ end"
          cleaned = removeComments nested
      assertBool "should handle nested comments" $ 
        not ("/*" `L.isInfixOf` cleaned) && 
        not ("*/" `L.isInfixOf` cleaned)

  , testCase "comments in strings" $ do
      let withStrings = "var s = \"// not a comment\" // real comment"
          cleaned = removeLineComments withStrings
      assertBool "should preserve comments in strings" $ 
        "// not a comment" `L.isInfixOf` cleaned
      assertBool "should remove real comments" $ 
        not ("// real comment" `L.isInfixOf` cleaned)

  , testCase "mixed line endings" $ do
      let mixed = "line1\r\nline2\nline3\r\nline4"
          normalized = normalizeIndentation mixed
      assertBool "should handle mixed line endings" $ 
        "line1" `L.isInfixOf` normalized &&
        "line2" `L.isInfixOf` normalized &&
        "line3" `L.isInfixOf` normalized &&
        "line4" `L.isInfixOf` normalized

  , testCase "tab L.and space mixing" $ do
      let mixedTabs = "\tline1\n    line2\n\t\tline3"
          normalized = normalizeIndentation mixedTabs
      assertBool "should normalize mixed indentation" $ 
        not (isPrefixOf "\t" normalized) &&
        not (isPrefixOf "    " normalized)

  , testCase "malformed comments" $ do
      let malformed = "code /* unclosed comment\nmore code"
          cleaned = removeComments malformed
      assertBool "should handle malformed comments gracefully" $ 
        L.length cleaned > 0

  , testCase "consecutive delimiters" $ do
      splitBy ',' "a,,,b" @?= ["a", "", "", "b"]
      splitByCollapsed ',' "a,,,b" @?= ["a", "b"]
      splitBy 'x' "axxxb" @?= ["a", "", "", "b"]
      splitByCollapsed 'x' "axxxb" @?= ["a", "b"]
  ]

-- Performance tests

performance_tests :: TestTree
performance_tests = testGroup "Performance Tests"
  [ testCase "large file processing" $ do
      let largeFile = unlines $ replicate 10000 "    line with some content // comment"
          processed = removeLineComments $ normalizeIndentation largeFile
      assertBool "should process large files" $ 
        L.length (lines processed) >= 10000

  , testCase "deep nesting" $ do
      let deepNesting = L.concat $ replicate 1000 "    "
          content = deepNesting ++ "content"
          normalized = normalizeIndentation content
      assertBool "should handle deep nesting" $ 
        not (isPrefixOf "    " normalized)

  , testCase "many small operations" $ do
      let operations = replicate 1000 "a,b,c"
          results = map splitByComma operations
      assertBool "should handle many operations" $ 
        L.length results == 1000 &&
        L.all (\res -> L.length res == 3) results
  ]

tests :: TestTree
tests = testGroup "Utils String Processing Tests"
  [ testGroup "Property Tests"
    [ fastProperty "trim removes whitespace" prop_trim_removes_whitespace
    , fastProperty "trim idempotent" prop_trim_idempotent
    , fastProperty "splitBy preserves empty" prop_splitBy_preserves_empty
    , fastProperty "splitByCollapsed removes empty" prop_splitByCollapsed_removes_empty
    , fastProperty "splitByComma is splitBy" prop_splitByComma_is_splitBy
    , fastProperty "splitByCommaCollapsed is splitByCollapsed" prop_splitByCommaCollapsed_is_splitByCollapsed
    , fastProperty "removeLineComments removes" prop_removeLineComments_removes
    , fastProperty "removeComments removes both" prop_removeComments_removes_both
    , fastProperty "normalizeIndentation removes prefix" prop_normalizeIndentation_removes_prefix
    , fastProperty "forceSingleTabIndentation uses tabs" prop_forceSingleTabIndentation_uses_tabs
    , fastProperty "fixIndentation equals normalize" prop_fixIndentation_equals_normalize
    , fastProperty "breakOn finds first" prop_breakOn_finds_first
    , fastProperty "breakOn empty pattern" prop_breakOn_empty_pattern
    , fastProperty "breakOn missing pattern" prop_breakOn_missing_pattern
    , fastProperty "splitBy join roundtrip" prop_splitBy_join_roundtrip
    , fastProperty "removeComments preserves content" prop_removeComments_preserves_content
    ]
  , unit_tests
  , advanced_tests
  , performance_tests
  ]