module Test.Unit.NewCabalParserQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, suchThat)
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.QuickCheck (fastProperty)

-- | QuickCheck tests for Parser module parsing functions
tests :: TestTree
tests =
  testGroup "New Cabal Parser QuickCheck Tests"
    [ testProperty "parseBool handles valid boolean values" prop_parseBoolValid
    , testProperty "parseBool rejects invalid boolean values" prop_parseBoolInvalid
    , testProperty "trimRight removes trailing newlines and carriage returns" prop_trimRightCorrectness
    , testProperty "curlyDelta counts braces correctly ignoring strings" prop_curlyDeltaInStrings
    , testProperty "curlyDelta counts braces correctly ignoring line comments" prop_curlyDeltaInComments
    , testProperty "leadingIndentation counts leading spaces and tabs" prop_leadingIndentationCorrectness
    , testProperty "parseTypus handles empty input" prop_parseTypusEmpty
    , testProperty "parseTypus handles simple content without directives" prop_parseTypusSimple
    , testProperty "parseTypus handles file directives" prop_parseTypusFileDirectives
    , testProperty "parseTypus handles build tags" prop_parseTypusBuildTags
    , testGroup "Edge cases"
        [ testCase "parseBool accepts 'on' and 'off'" $ do
            parseBool "on" @?= Right True
            parseBool "off" @?= Right False
        , testCase "parseBool accepts 'true' and 'false'" $ do
            parseBool "true" @?= Right True
            parseBool "false" @?= Right False
        , testCase "parseBool rejects invalid values" $ do
            parseBool "maybe" @?= Left "Invalid boolean value for directive: maybe"
        , testCase "trimRight removes trailing whitespace" $ do
            trimRight "hello\r\n\r\n" @?= "hello"
            trimRight "world\n\n" @?= "world"
        , testCase "curlyDelta counts braces" $ do
            curlyDelta "{}" @?= 0
            curlyDelta "{{}" @?= 1
            curlyDelta "{}}" @?= -1
        , testCase "curlyDelta ignores braces in strings" $ do
            curlyDelta "\"{\"}" @?= 0
            curlyDelta "\"{\"\"}\"" @?= 0
        , testCase "curlyDelta ignores braces in line comments" $ do
            curlyDelta "// {" @?= 0
            curlyDelta "{ // }" @?= 1
        , testCase "leadingIndentation counts leading spaces and tabs" $ do
            leadingIndentation "    hello" @?= 4
            leadingIndentation "\t\thello" @?= 2
            leadingIndentation " \t \t hello" @?= 4
        , testCase "parseTypus handles empty input" $ do
            let result = parseTypus ""
            case result of
                Left _ -> assertFailure "Should parse empty input successfully"
                Right file -> do
                    tfDirectives file @?= defaultFileDirectives
                    tfBuildTags file @?= []
                    tfBlocks file @?= []
        ]
    ]

-- | Property: parseBool accepts valid boolean values
prop_parseBoolValid :: String -> Property
prop_parseBoolValid input = 
  let normalized = trim input
  in normalized `elem` ["on", "off", "true", "false"] ==>
     case parseBool normalized of
       Left _ -> counterexample ("Should accept: " ++ normalized) False
       Right _ -> True

-- | Property: parseBool rejects invalid boolean values
prop_parseBoolInvalid :: String -> Property
prop_parseBoolInvalid input = 
  let normalized = trim input
  in normalized `notElem` ["on", "off", "true", "false", ""] ==>
     case parseBool normalized of
       Left _ -> True
       Right _ -> counterexample ("Should reject: " ++ normalized) False

-- | Property: trimRight removes trailing newlines and carriage returns
prop_trimRightCorrectness :: String -> Property
prop_trimRightCorrectness input = 
  let withTrailing = input ++ "\r\n\r\n"
      trimmed = trimRight withTrailing
      hasTrailingNewlines = not (null trimmed) && 
                           last trimmed `notElem` ['\r', '\n']
  in hasTrailingNewlines .&&. 
     (null input || take (length input) trimmed == input)

-- | Property: curlyDelta counts braces correctly ignoring those in strings
prop_curlyDeltaInStrings :: String -> String -> Property
prop_curlyDeltaInStrings prefix suffix = 
  let inString = "\"" ++ prefix ++ "{" ++ suffix ++ "\""
      delta = curlyDelta inString
  in delta === 0

-- | Property: curlyDelta counts braces correctly ignoring those in line comments
prop_curlyDeltaInComments :: String -> String -> Property
prop_curlyDeltaInComments before after = 
  let withComment = before ++ "// {" ++ after
      delta = curlyDelta withComment
      expectedDelta = curlyDelta before + curlyDelta after
  in delta === expectedDelta

-- | Property: leadingIndentation counts leading spaces and tabs correctly
prop_leadingIndentationCorrectness :: String -> String -> Property
prop_leadingIndentationCorrectness prefix content = 
  let leading = takeWhile isSpace prefix
      withLeading = leading ++ content
      indentation = leadingIndentation withLeading
  in indentation === length leading

-- | Property: parseTypus handles empty input
prop_parseTypusEmpty :: Property
prop_parseTypusEmpty = 
  case parseTypus "" of
    Left _ -> counterexample "Should parse empty input" False
    Right file -> 
      tfDirectives file === defaultFileDirectives .&&.
      tfBuildTags file === [] .&&.
      tfBlocks file === []

-- | Property: parseTypus handles simple content without directives
prop_parseTypusSimple :: String -> Property
prop_parseTypusSimple content = 
  not (any (`isPrefixOf` trim content) ["//!", "//go:build", "// +build", "{//!"]) ==>
  case parseTypus content of
    Left _ -> counterexample ("Should parse simple content: " ++ content) False
    Right file -> 
      tfDirectives file === defaultFileDirectives .&&.
      length (tfBlocks file) >= 0

-- | Property: parseTypus handles file directives
prop_parseTypusFileDirectives :: Bool -> Bool -> Bool -> Property
prop_parseTypusFileDirectives ownership dependentTypes constraints = 
  let ownershipVal = if ownership then "on" else "off"
      dependentTypesVal = if dependentTypes then "on" else "off"
      constraintsVal = if constraints then "on" else "off"
      directives = "//! ownership: " ++ ownershipVal ++ 
                   ", dependent_types: " ++ dependentTypesVal ++
                   ", constraints: " ++ constraintsVal ++ "\n"
  in case parseTypus directives of
    Left _ -> counterexample ("Should parse file directives: " ++ directives) False
    Right file -> 
      let dirs = tfDirectives file
      in case (fdOwnership dirs, fdDependentTypes dirs, fdConstraints dirs) of
        (Just (Located _ o), Just (Located _ dt), Just (Located _ c)) -> 
          o === ownership .&&. dt === dependentTypes .&&. c === constraints
        _ -> counterexample "Directives not parsed correctly" False

-- | Property: parseTypus handles build tags
prop_parseTypusBuildTags :: String -> Property
prop_parseTypusBuildTags tag = 
  not (null tag) && not (any (`elem` ['\n', '\r']) tag) ==>
  let buildTag = "//go:build " ++ tag ++ "\n"
  in case parseTypus buildTag of
    Left _ -> counterexample ("Should parse build tag: " ++ buildTag) False
    Right file -> 
      length (tfBuildTags file) === 1 .&&.
      tag `isInfixOf` locValue (head (tfBuildTags file))

-- Helper function to trim whitespace
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Helper operator for composing properties
(.&&.) :: Property -> Property -> Property
(.&&.) = (&&)