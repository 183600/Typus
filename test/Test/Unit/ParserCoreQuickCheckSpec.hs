{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ParserCoreQuickCheckSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Parser
import SourceLocation
import Utils (trim)
import qualified Data.Text as T
import Data.Char (isSpace)

-- ============================================================================
-- Custom Generators
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

-- Generate build tag lines
genBuildTagLine :: Gen String
genBuildTagLine = oneof
  [ return "//go:build linux"
  , return "// +build linux,amd64"
  , elements ["//go:build windows", "//go:build darwin", "// +build test"]
  ]

-- Generate code lines
genCodeLine :: Gen String
genCodeLine = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t;.,(){}[]"
  return content

-- Generate indented code lines
genIndentedCodeLine :: Gen String
genIndentedCodeLine = do
  indent <- choose (0, 4)
  content <- genCodeLine
  return $ replicate indent ' ' ++ content

-- Generate simple Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  numLines <- choose (1, 10)
  lines <- vectorOf numLines $ oneof
    [ genFileDirectiveLine
    , genBuildTagLine
    , genIndentedCodeLine
    , return ""
    ]
  return $ unlines lines

-- Generate content with block directives
genTypusFileWithBlocks :: Gen String
genTypusFileWithBlocks = do
  beforeLines <- vectorOf (choose (1, 3)) genIndentedCodeLine
  blockDirective <- genBlockDirectiveLine
  blockContent <- vectorOf (choose (1, 3)) genIndentedCodeLine
  afterLines <- vectorOf (choose (1, 3)) genIndentedCodeLine
  return $ unlines (beforeLines ++ [blockDirective] ++ blockContent ++ afterLines)

-- ============================================================================
-- Directive Parsing Properties
-- ============================================================================

prop_parseBoolValidValues :: Property
prop_parseBoolValidValues =
  let validValues = ["on", "off", "true", "false"]
      expectedResults = [Right True, Right False, Right True, Right False]
  in counterexample "parseBool should handle all valid values" $
    conjoin $ zipWith (\val expected -> parseBool val === expected) validValues expectedResults

prop_parseBoolInvalidValues :: String -> Property
prop_parseBoolInvalidValues value =
  let invalid = value `notElem` ["on", "off", "true", "false"] && not (null (trim value))
  in counterexample "parseBool should reject invalid values" $
    invalid ==> case parseBool value of
      Left _ -> property True
      Right _ -> property False

prop_curlyDeltaBasic :: String -> Property
prop_curlyDeltaBasic s =
  let openCount = length (filter (== '{') s)
      closeCount = length (filter (== '}') s)
      expected = openCount - closeCount
  in counterexample "curlyDelta should count braces (ignoring strings and comments for basic case)" $
    not ('"' `elem` s || "//" `isInfixOf` s) ==> curlyDelta s === expected

prop_curlyDeltaEmpty :: Property
prop_curlyDeltaEmpty =
  counterexample "curlyDelta should return 0 for empty string" $
    curlyDelta "" === 0

prop_curlyDeltaWithStrings :: Property
prop_curlyDeltaWithStrings =
  let stringWithBraces = "code \"{not a brace}\" more {code}"
  in counterexample "curlyDelta should ignore braces inside strings" $
    curlyDelta stringWithBraces === 1

prop_curlyDeltaWithComments :: Property
prop_curlyDeltaWithComments =
  let commentWithBraces = "code // {not a brace}\n more {code}"
  in counterexample "curlyDelta should ignore braces in line comments" $
    curlyDelta commentWithBraces === 1

prop_leadingIndentationProperties :: String -> Property
prop_leadingIndentationProperties s =
  let expected = length $ takeWhile isSpace s
  in counterexample "leadingIndentation should count leading spaces/tabs" $
    leadingIndentation s === expected

prop_leadingIndentationEmpty :: Property
prop_leadingIndentationEmpty =
  counterexample "leadingIndentation should return 0 for empty string" $
    leadingIndentation "" === 0

prop_trimRightProperties :: String -> Property
prop_trimRightProperties s =
  let trimmed = trimRight s
      hasTrailingNewline = not (null s) && last s `elem` ['\n', '\r']
  in counterexample "trimRight should remove trailing newlines" $
    hasTrailingNewline ==> not (null trimmed) && last trimmed `notElem` ['\n', '\r']

-- ============================================================================
-- File Directive Properties
-- ============================================================================

prop_defaultFileDirectivesProperties :: Property
prop_defaultFileDirectivesProperties =
  let fd = defaultFileDirectives
  in counterexample "defaultFileDirectives should have all fields as Nothing" $
    fdOwnership fd === Nothing .&.
    fdDependentTypes fd === Nothing .&.
    fdConstraints fd === Nothing

prop_defaultBlockDirectivesProperties :: Property
prop_defaultBlockDirectivesProperties =
  let bd = defaultBlockDirectives
  in counterexample "defaultBlockDirectives should have all fields as Nothing" $
    bdOwnership bd === Nothing .&.
    bdDependentTypes bd === Nothing .&.
    bdConstraints bd === Nothing

prop_updateFileDirectiveOwnership :: Property
prop_updateFileDirectiveOwnership =
  let fd = defaultFileDirectives
      value = locatedWithSpan (emptySpan startPos) True
      result = updateFileDirective fd "ownership" value
  in counterexample "updateFileDirective should update ownership field" $
    result === Right (fd { fdOwnership = Just value })

prop_updateFileDirectiveDependentTypes :: Property
prop_updateFileDirectiveDependentTypes =
  let fd = defaultFileDirectives
      value = locatedWithSpan (emptySpan startPos) True
      result = updateFileDirective fd "dependent_types" value
  in counterexample "updateFileDirective should update dependent_types field" $
    result === Right (fd { fdDependentTypes = Just value })

prop_updateFileDirectiveConstraints :: Property
prop_updateFileDirectiveConstraints =
  let fd = defaultFileDirectives
      value = locatedWithSpan (emptySpan startPos) True
      result = updateFileDirective fd "constraints" value
  in counterexample "updateFileDirective should update constraints and dependent_types" $
    result === Right (fd { fdConstraints = Just value, fdDependentTypes = Just value })

prop_updateFileDirectiveUnknown :: Property
prop_updateFileDirectiveUnknown =
  let fd = defaultFileDirectives
      value = locatedWithSpan (emptySpan startPos) True
      result = updateFileDirective fd "unknown" value
  in counterexample "updateFileDirective should reject unknown directives" $
    case result of
      Left _ -> property True
      Right _ -> property False

-- ============================================================================
-- Parser Properties
-- ============================================================================

prop_parseTypusEmpty :: Property
prop_parseTypusEmpty =
  let result = parseTypus ""
  in counterexample "parseTypus should handle empty input" $
    case result of
      Left _ -> property False
      Right tf -> tfBlocks tf === []

prop_parseTypusSimpleCode :: Property
prop_parseTypusSimpleCode =
  let content = "func main() {\n  println(\"Hello\")\n}"
      result = parseTypus content
  in counterexample "parseTypus should parse simple code" $
    case result of
      Left _ -> property False
      Right tf -> not (null (tfBlocks tf))

prop_parseTypusWithFileDirectives :: Property
prop_parseTypusWithFileDirectives =
  let content = "//! ownership: on\n//! dependent_types: true\n\ncode here"
      result = parseTypus content
  in counterexample "parseTypus should parse file directives" $
    case result of
      Left _ -> property False
      Right tf -> tfDirectives tf /= defaultFileDirectives

prop_parseTypusWithBuildTags :: Property
prop_parseTypusWithBuildTags =
  let content = "//go:build linux\n\ncode here"
      result = parseTypus content
  in counterexample "parseTypus should parse build tags" $
    case result of
      Left _ -> property False
      Right tf -> not (null (tfBuildTags tf))

prop_parseTypusWithBlockDirectives :: Property
prop_parseTypusWithBlockDirectives =
  let content = "some code\n{//! ownership: on}\nblock code\nmore code"
      result = parseTypus content
  in counterexample "parseTypus should parse block directives" $
    case result of
      Left _ -> property False
      Right tf -> any (\b -> cbDirectives b /= defaultBlockDirectives) (tfBlocks tf)

prop_parseTypusRoundtrip :: Property
prop_parseTypusRoundtrip =
  forAll genTypusFileContent $ \content ->
    let result = parseTypus content
    in counterexample "parseTypus should succeed on generated content" $
    case result of
      Left _ -> property False
      Right _ -> property True

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

prop_parseTypusMultiplePackages :: Property
prop_parseTypusMultiplePackages =
  let content = "package main\npackage other"
      result = parseTypus content
  in counterexample "parseTypus should detect multiple package declarations" $
    case result of
      Left err -> "Multiple package" `isInfixOf` err
      Right _ -> property False

prop_parseTypusIfWithoutBrace :: Property
prop_parseTypusIfWithoutBrace =
  let content = "if condition\n  doSomething()"
      result = parseTypus content
  in counterexample "parseTypus should detect if statements without braces" $
    case result of
      Left err -> "missing opening brace" `isInfixOf` err
      Right _ -> property False

-- ============================================================================
-- Block Parsing Properties
-- ============================================================================

prop_codeBlockConstruction :: Property
prop_codeBlockConstruction =
  let directives = defaultBlockDirectives
      content = "test content"
      span = emptySpan startPos
      block = CodeBlock directives content span
  in counterexample "CodeBlock constructor should preserve fields" $
    cbDirectives block === directives .&.
    cbContent block === content .&.
    cbSpan block === span

prop_typusFileConstruction :: Property
prop_typusFileConstruction =
  let directives = defaultFileDirectives
      buildTags = []
      blocks = []
      syntaxErrors = []
      file = TypusFile directives buildTags blocks syntaxErrors
  in counterexample "TypusFile constructor should preserve fields" $
    tfDirectives file === directives .&.
    tfBuildTags file === buildTags .&.
    tfBlocks file === blocks .&.
    tfSyntaxErrors file === syntaxErrors

-- ============================================================================
-- Utility Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Core QuickCheck Tests"
  [ testGroup "Directive Parsing Tests"
      [ testProperty "parseBool handles valid values" prop_parseBoolValidValues
      , testProperty "parseBool rejects invalid values" prop_parseBoolInvalidValues
      ]
  , testGroup "String Processing Tests"
      [ testProperty "curlyDelta counts braces" prop_curlyDeltaBasic
      , testProperty "curlyDelta returns 0 for empty string" prop_curlyDeltaEmpty
      , testProperty "curlyDelta ignores braces in strings" prop_curlyDeltaWithStrings
      , testProperty "curlyDelta ignores braces in comments" prop_curlyDeltaWithComments
      , testProperty "leadingIndentation counts leading spaces" prop_leadingIndentationProperties
      , testProperty "leadingIndentation returns 0 for empty string" prop_leadingIndentationEmpty
      , testProperty "trimRight removes trailing newlines" prop_trimRightProperties
      ]
  , testGroup "File Directive Tests"
      [ testProperty "defaultFileDirectives has all Nothing fields" prop_defaultFileDirectivesProperties
      , testProperty "defaultBlockDirectives has all Nothing fields" prop_defaultBlockDirectivesProperties
      , testProperty "updateFileDirective updates ownership" prop_updateFileDirectiveOwnership
      , testProperty "updateFileDirective updates dependent_types" prop_updateFileDirectiveDependentTypes
      , testProperty "updateFileDirective updates constraints and dependent_types" prop_updateFileDirectiveConstraints
      , testProperty "updateFileDirective rejects unknown directives" prop_updateFileDirectiveUnknown
      ]
  , testGroup "Parser Tests"
      [ testProperty "parseTypus handles empty input" prop_parseTypusEmpty
      , testProperty "parseTypus parses simple code" prop_parseTypusSimpleCode
      , testProperty "parseTypus parses file directives" prop_parseTypusWithFileDirectives
      , testProperty "parseTypus parses build tags" prop_parseTypusWithBuildTags
      , testProperty "parseTypus parses block directives" prop_parseTypusWithBlockDirectives
      , testProperty "parseTypus succeeds on generated content" prop_parseTypusRoundtrip
      ]
  , testGroup "Error Handling Tests"
      [ testProperty "parseTypus detects multiple package declarations" prop_parseTypusMultiplePackages
      , testProperty "parseTypus detects if without braces" prop_parseTypusIfWithoutBrace
      ]
  , testGroup "Data Structure Tests"
      [ testProperty "CodeBlock constructor preserves fields" prop_codeBlockConstruction
      , testProperty "TypusFile constructor preserves fields" prop_typusFileConstruction
      ]
  ]