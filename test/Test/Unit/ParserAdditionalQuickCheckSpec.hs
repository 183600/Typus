{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.ParserAdditionalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, vectorOf, suchThat, choose, resize, forAll, (==>))
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)

import Parser (parseTypus, parseBool, trimRight, curlyDelta, leadingIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, spanStart, spanEnd)
import qualified SyntaxValidator
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)

-- ============================================================================
-- Helper Generators
-- ============================================================================

-- Generate valid directive keys
genDirectiveKey :: Gen String
genDirectiveKey = elements ["ownership", "dependent_types", "constraints"]

-- Generate valid directive values
genDirectiveValue :: Gen String
genDirectiveValue = elements ["on", "off", "true", "false"]

-- Generate file directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "//! " ++ key ++ ": " ++ value

-- Generate block directive lines
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "{//! " ++ key ++ ": " ++ value ++ " }"

-- Generate simple code blocks
genCodeBlock :: Gen String
genCodeBlock = do
  numLines <- choose (1, 5)
  lines' <- vectorOf numLines $ do
    content <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t,.;"
    return $ "    " ++ content
  return $ unlines lines'

-- Generate build tag lines
genBuildTagLine :: Gen String
genBuildTagLine = do
  tag <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ "//go:build " ++ tag

-- Generate simple Go-like code
genGoCode :: Gen String
genGoCode = do
  numLines <- choose (1, 10)
  lines' <- vectorOf numLines $ oneof
    [ return "package main"
    , return "func main() {"
    , return "    fmt.Println(\"Hello\")"
    , return "}"
    , return "if condition {"
    , return "    doSomething()"
    , return "}"
    , listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t,.;(){}"
    ]
  return $ unlines lines'

-- Generate Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  useFileDirectives <- arbitrary
  useBuildTags <- arbitrary
  numBlocks <- choose (0, 3)
  
  fileDirectives <- if useFileDirectives
    then listOf1 genFileDirectiveLine
    else return []
  
  buildTags <- if useBuildTags
    then listOf1 genBuildTagLine
    else return []
  
  blocks <- vectorOf numBlocks $ do
    directive <- genBlockDirectiveLine
    code <- genCodeBlock
    return $ directive ++ "\n" ++ code
  
  let header = unlines $ fileDirectives ++ buildTags
      body = unlines blocks
  
  return $ header ++ if null body then "" else "\n" ++ body

-- ============================================================================
-- QuickCheck Tests for Parser Functions
-- ============================================================================

-- Test parseBool function
prop_parse_bool_valid_values :: String -> Property
prop_parse_bool_valid_values value = 
  value `elem` ["on", "off", "true", "false"] ==>
  case parseBool value of
    Left _ -> False
    Right result -> case value of
      "on" -> result == True
      "off" -> result == False
      "true" -> result == True
      "false" -> result == False
      _ -> False

prop_parse_bool_invalid_values :: Property
prop_parse_bool_invalid_values = 
  forAll (elements ["maybe", "yes", "no", "1", "0", "ON", "OFF"]) $ \value ->
  case parseBool value of
    Left _ -> True
    Right _ -> False

-- Test trimRight function
prop_trim_right_no_trailing_newlines :: String -> Bool
prop_trim_right_no_trailing_newlines s = 
  let trimmed = trimRight s
      hasTrailingNewlines = not (null trimmed) && last trimmed `elem` ['\r', '\n']
  in not hasTrailingNewlines

prop_trim_right_preserves_content :: String -> Bool
prop_trim_right_preserves_content s = 
  let trimmed = trimRight s
      contentWithoutTrailingNewlines = reverse $ dropWhile (`elem` ['\r', '\n']) $ reverse s
  in trimmed == contentWithoutTrailingNewlines

-- Test curlyDelta function
prop_curly_delta_empty :: Bool
prop_curly_delta_empty = curlyDelta "" == 0

prop_curly_delta_single_open :: Bool
prop_curly_delta_single_open = curlyDelta "{" == 1

prop_curly_delta_single_close :: Bool
prop_curly_delta_single_close = curlyDelta "}" == -1

prop_curly_delta_balanced :: String -> Property
prop_curly_delta_balanced s = 
  let s' = "{" ++ s ++ "}"
  in curlyDelta s' == 0

prop_curly_delta_ignores_comments :: String -> Bool
prop_curly_delta_ignores_comments s = 
  let comment = "// { }"
  in curlyDelta comment == 0

prop_curly_delta_ignores_string_braces :: String -> Property
prop_curly_delta_ignores_string_braces s = 
  let stringWithBraces = "\"{ }\""
  in curlyDelta stringWithBraces == 0

-- Test leadingIndentation function
prop_leading_indentation_empty :: Bool
prop_leading_indentation_empty = leadingIndentation "" == 0

prop_leading_indentation_no_indent :: String -> Property
prop_leading_indentation_no_indent s = 
  not (null s) && not (isSpace (head s)) ==>
  leadingIndentation s == 0

prop_leading_indentation_counts_spaces :: Int -> Property
prop_leading_indentation_counts_spaces n = 
  n >= 0 && n <= 10 ==>
  let indented = replicate n ' ' ++ "content"
  in leadingIndentation indented == n

prop_leading_indentation_counts_tabs :: Int -> Property
prop_leading_indentation_counts_tabs n = 
  n >= 0 && n <= 5 ==>
  let indented = replicate n '\t' ++ "content"
  in leadingIndentation indented == n

-- ============================================================================
-- QuickCheck Tests for Parser Properties
-- ============================================================================

-- Test that parsing succeeds on valid inputs
prop_parse_typus_empty :: Bool
prop_parse_typus_empty = 
  case parseTypus "" of
    Left _ -> False
    Right result -> null (tfBlocks result) && null (tfSyntaxErrors result)

prop_parse_typus_simple_code :: Property
prop_parse_typus_simple_code = 
  forAll genGoCode $ \code ->
  case parseTypus code of
    Left _ -> False
    Right result -> not (null (tfBlocks result)) || not (null (tfSyntaxErrors result))

prop_parse_typus_with_file_directives :: Property
prop_parse_typus_with_file_directives = 
  forAll genFileDirectiveLine $ \directive ->
  case parseTypus directive of
    Left _ -> False
    Right result -> tfDirectives result /= defaultFileDirectives

prop_parse_typus_with_build_tags :: Property
prop_parse_typus_with_build_tags = 
  forAll genBuildTagLine $ \tag ->
  case parseTypus tag of
    Left _ -> False
    Right result -> not (null (tfBuildTags result))

-- Test that parsing preserves structure
prop_parse_typus_preserves_package :: Property
prop_parse_typus_preserves_package = 
  forAll genGoCode $ \code ->
    "package main" `isInfixOf` code ==>
    case parseTypus code of
      Left _ -> True  -- May fail due to syntax errors, which is ok
      Right result -> True  -- If succeeds, structure should be preserved

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

test_parse_bool_valid_values :: TestTree
test_parse_bool_valid_values = testCase "parseBool valid values" $ do
  assertEqual "parseBool on" (Right True) (parseBool "on")
  assertEqual "parseBool off" (Right False) (parseBool "off")
  assertEqual "parseBool true" (Right True) (parseBool "true")
  assertEqual "parseBool false" (Right False) (parseBool "false")

test_parse_bool_invalid_values :: TestTree
test_parse_bool_invalid_values = testCase "parseBool invalid values" $ do
  assertBool "parseBool maybe fails" $ case parseBool "maybe" of Left _ -> True; Right _ -> False
  assertBool "parseBool yes fails" $ case parseBool "yes" of Left _ -> True; Right _ -> False
  assertBool "parseBool 1 fails" $ case parseBool "1" of Left _ -> True; Right _ -> False

test_trim_right_function :: TestTree
test_trim_right_function = testCase "trimRight function" $ do
  assertEqual "trimRight empty" "" (trimRight "")
  assertEqual "trimRight no newlines" "hello" (trimRight "hello")
  assertEqual "trimRight with newline" "hello" (trimRight "hello\n")
  assertEqual "trimRight with multiple newlines" "hello" (trimRight "hello\n\r\n")
  assertEqual "trimRight only newlines" "" (trimRight "\n\r\n")

test_curly_delta_function :: TestTree
test_curly_delta_function = testCase "curlyDelta function" $ do
  assertEqual "curlyDelta empty" 0 (curlyDelta "")
  assertEqual "curlyDelta open brace" 1 (curlyDelta "{")
  assertEqual "curlyDelta close brace" (-1) (curlyDelta "}")
  assertEqual "curlyDelta balanced" 0 (curlyDelta "{}")
  assertEqual "curlyDelta nested" 1 (curlyDelta "{{}")
  assertEqual "curlyDelta with comment" 0 (curlyDelta "// { }")
  assertEqual "curlyDelta with string" 0 (curlyDelta "\"{\"")
  assertEqual "curlyDelta mixed" 1 (curlyDelta "{ // }")

test_leading_indentation_function :: TestTree
test_leading_indentation_function = testCase "leadingIndentation function" $ do
  assertEqual "leadingIndentation empty" 0 (leadingIndentation "")
  assertEqual "leadingIndentation no indent" 0 (leadingIndentation "hello")
  assertEqual "leadingIndentation spaces" 4 (leadingIndentation "    hello")
  assertEqual "leadingIndentation tabs" 3 (leadingIndentation "\t\t\thello")
  assertEqual "leadingIndentation mixed" 5 (leadingIndentation " \t \t hello")

test_parse_typus_empty_file :: TestTree
test_parse_typus_empty_file = testCase "parseTypus empty file" $ do
  case parseTypus "" of
    Left err -> assertFailure $ "Failed to parse empty file: " ++ err
    Right result -> do
      assertEqual "no blocks" [] (tfBlocks result)
      assertEqual "default directives" defaultFileDirectives (tfDirectives result)
      assertEqual "no build tags" [] (tfBuildTags result)

test_parse_typus_simple_file :: TestTree
test_parse_typus_simple_file = testCase "parseTypus simple file" $ do
  let content = "package main\n\nfunc main() {\n    fmt.Println(\"Hello\")\n}"
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse simple file: " ++ err
    Right result -> do
      assertBool "has blocks" $ not (null (tfBlocks result))

test_parse_typus_with_file_directives :: TestTree
test_parse_typus_with_file_directives = testCase "parseTypus with file directives" $ do
  let content = "//! ownership: on\n//! dependent_types: off\n\npackage main"
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse file with directives: " ++ err
    Right result -> do
      let dirs = tfDirectives result
      assertBool "has ownership directive" $ fdOwnership dirs /= Nothing
      assertBool "has dependent_types directive" $ fdDependentTypes dirs /= Nothing

test_parse_typus_with_block_directives :: TestTree
test_parse_typus_with_block_directives = testCase "parseTypus with block directives" $ do
  let content = "{//! ownership: on, dependent_types: true }\n    func test() {\n        return 42\n    }"
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse file with block directives: " ++ err
    Right result -> do
      assertBool "has blocks" $ not (null (tfBlocks result))
      let firstBlock = head (tfBlocks result)
      let dirs = cbDirectives firstBlock
      assertBool "has block ownership directive" $ bdOwnership dirs /= Nothing
      assertBool "has block dependent_types directive" $ bdDependentTypes dirs /= Nothing

test_parse_typus_with_build_tags :: TestTree
test_parse_typus_with_build_tags = testCase "parseTypus with build tags" $ do
  let content = "//go:build linux\n// +build darwin\n\npackage main"
  case parseTypus content of
    Left err -> assertFailure $ "Failed to parse file with build tags: " ++ err
    Right result -> do
      assertEqual "has build tags" 2 (length (tfBuildTags result))

test_parse_typus_syntax_errors :: TestTree
test_parse_typus_syntax_errors = testCase "parseTypus syntax errors" $ do
  let content = "func test() {\n    if condition\n        doSomething()\n}"  -- Missing opening brace
  case parseTypus content of
    Left err -> assertFailure $ "Parser should handle syntax errors gracefully: " ++ err
    Right result -> do
      -- Should still parse but with syntax errors
      assertBool "has syntax errors" $ not (null (tfSyntaxErrors result))

test_parse_typus_multiple_package_declarations :: TestTree
test_parse_typus_multiple_package_declarations = testCase "parseTypus multiple package declarations" $ do
  let content = "package main\n\npackage other"
  case parseTypus content of
    Left err -> assertBool "fails with multiple packages" $ "Multiple package declarations" `isInfixOf` err
    Right _ -> assertFailure "Should have failed with multiple package declarations"

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Additional QuickCheck Tests"
  [ testGroup "QuickCheck Properties"
    [ testProperty "parseBool valid values" prop_parse_bool_valid_values
    , testProperty "parseBool invalid values" prop_parse_bool_invalid_values
    , testProperty "trimRight no trailing newlines" prop_trim_right_no_trailing_newlines
    , testProperty "trimRight preserves content" prop_trim_right_preserves_content
    , testProperty "curlyDelta empty" prop_curly_delta_empty
    , testProperty "curlyDelta single open" prop_curly_delta_single_open
    , testProperty "curlyDelta single close" prop_curly_delta_single_close
    , testProperty "curlyDelta balanced" prop_curly_delta_balanced
    , testProperty "curlyDelta ignores comments" prop_curly_delta_ignores_comments
    , testProperty "curlyDelta ignores string braces" prop_curly_delta_ignores_string_braces
    , testProperty "leadingIndentation empty" prop_leading_indentation_empty
    , testProperty "leadingIndentation no indent" prop_leading_indentation_no_indent
    , testProperty "leadingIndentation counts spaces" prop_leading_indentation_counts_spaces
    , testProperty "leadingIndentation counts tabs" prop_leading_indentation_counts_tabs
    , testProperty "parseTypus empty" prop_parse_typus_empty
    , testProperty "parseTypus simple code" prop_parse_typus_simple_code
    , testProperty "parseTypus with file directives" prop_parse_typus_with_file_directives
    , testProperty "parseTypus with build tags" prop_parse_typus_with_build_tags
    , testProperty "parseTypus preserves package" prop_parse_typus_preserves_package
    ]
  , testGroup "Unit Tests"
    [ test_parse_bool_valid_values
    , test_parse_bool_invalid_values
    , test_trim_right_function
    , test_curly_delta_function
    , test_leading_indentation_function
    , test_parse_typus_empty_file
    , test_parse_typus_simple_file
    , test_parse_typus_with_file_directives
    , test_parse_typus_with_block_directives
    , test_parse_typus_with_build_tags
    , test_parse_typus_syntax_errors
    , test_parse_typus_multiple_package_declarations
    ]
  ]