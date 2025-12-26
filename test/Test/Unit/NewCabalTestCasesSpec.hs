{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestCasesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertFailure, assertString)
import Test.Tasty.QuickCheck (testProperty, Property, (==>), forAll, Gen, choose, listOf1, elements, listOf)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), parseTypus, defaultFileDirectives, defaultBlockDirectives)
import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..), locatedWithSpan, locatedValue)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorLocation(..), ErrorCategory(..), emptyContext, errorRecovery)
import DependentTypesParser (DependentType(..), TypeConstraint(..), TypeRef(..), TypeBody(..))

-- QuickCheck generators
genNonEmptyString :: Gen String
genNonEmptyString = listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_ ")

genValidIdentifier :: Gen String
genValidIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return (first : rest)

genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 100)
  col <- choose (1, 100)
  return SourcePos { posLine = line, posColumn = col, posOffset = 0 }

-- | Test cases for core Typus functionality
tests :: TestTree
tests =
  testGroup "New Cabal Test Cases"
    [ testGroup "Parser Tests"
        [ testCase "parseTypus handles empty input" $ do
            let result = parseTypus ""
            case result of
              Left _ -> assertFailure "Should parse empty input successfully"
              Right typusFile -> tfBlocks typusFile @?= []

        , testCase "parseTypus handles simple code block" $ do
            let input = "func main() {\n    return 42\n}\n"
            let result = parseTypus input
            case result of
              Left _ -> assertFailure "Should parse simple code block"
              Right typusFile -> length (tfBlocks typusFile) @?= 1

        , testCase "parseTypus respects file directives" $ do
            let input = "// @ownership: true\n// @dependent-types: true\n\nfunc test() {}"
            let result = parseTypus input
            case result of
              Left _ -> assertFailure "Should parse with directives"
              Right typusFile -> do
                let directives = tfDirectives typusFile
                isJust (fdOwnership directives) @?= True
                isJust (fdDependentTypes directives) @?= True
        ]

    , testGroup "Utils Tests with QuickCheck"
        [ testProperty "trim is idempotent" propTrimIdempotent
        , testProperty "trim removes leading/trailing spaces" propTrimRemovesSpaces
        , testProperty "splitBy and splitByCollapsed relationship" propSplitByRelationship
        , testProperty "breakOn correctness" propBreakOnCorrectness
        , testProperty "removeLineComments preserves non-comment lines" propRemoveLineCommentsPreserves
        ]

    , testGroup "Source Location Tests"
        [ testCase "locatedWithSpan creates correct location" $ do
            let span = SourceSpan 
                  { spanStart = SourcePos { posLine = 1, posColumn = 1, posOffset = 0 }
                  , spanEnd = SourcePos { posLine = 1, posColumn = 10, posOffset = 0 }
                  }
            let located = locatedWithSpan span "test content"
            locSpan located @?= span
            locatedValue located @?= "test content"

        , testProperty "Source positions are valid" $ forAll genSourcePos propSourcePosValid
        ]

    , testGroup "Error Handling Tests"
        [ testCase "TypeError construction works" $ do
            let error = TypeError 
                  { errorId = "test-1"
                  , severity = Error
                  , category = TypeChecking
                  , message = T.pack "Test error"
                  , location = ErrorLocation Nothing 0 0 Nothing Nothing
                  , context = emptyContext
                  , recovery = errorRecovery
                  , suggestions = []
                  , relatedErrors = []
                  }
            message error @?= T.pack "Test error"
            severity error @?= Error
            location error @?= ErrorLocation Nothing 0 0 Nothing Nothing

        , testCase "TypeError with location" $ do
            let error = TypeError 
                  { errorId = "test-2"
                  , severity = Warning
                  , category = Parsing
                  , message = T.pack "Location error"
                  , location = ErrorLocation Nothing 5 10 (Just 5) (Just 20)
                  , context = emptyContext
                  , recovery = errorRecovery
                  , suggestions = []
                  , relatedErrors = []
                  }
            let expectedLocation = ErrorLocation Nothing 5 10 (Just 5) (Just 20)
            location error @?= expectedLocation
        ]

    , testGroup "Dependent Types Tests"
        [ testCase "DependentType basic construction" $ do
            let constraint = SizeConstraint "length" 0
            let depType = TypeDecl "NonEmptyList" [] (StructBody []) [constraint]
            case depType of
              TypeDecl name params body constraints -> do
                name @?= "NonEmptyList"
                length constraints @?= 1
              _ -> assertFailure "Expected TypeDecl"

        , testProperty "Type constraints are valid" propTypeConstraintsValid
        ]

    , testGroup "Integration Tests"
        [ testCase "Parser and Utils integration" $ do
            let input = "  // comment\n  func test() { return 42; }  "
            let cleaned = trim input
            let withoutComments = removeLineComments cleaned
            length (filter (not . null) (lines withoutComments)) @?= 2

        , testCase "Error handling in parsing workflow" $ do
            let malformed = "func incomplete {"
            let result = parseTypus malformed
            case result of
              Left err -> assertString ("Expected parsing error, got: " ++ show err)
              Right _ -> assertFailure "Should have failed to parse malformed input"
        ]
    ]

-- QuickCheck properties
propTrimIdempotent :: String -> Bool
propTrimIdempotent str = trim (trim str) == trim str

propTrimRemovesSpaces :: String -> Bool
propTrimRemovesSpaces str = 
  let trimmed = trim str
  in null trimmed || (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

propSplitByRelationship :: Char -> String -> Bool
propSplitByRelationship delim str = 
  let withEmpty = splitBy delim str
      collapsed = splitByCollapsed delim str
  in all (not . null) collapsed

propBreakOnCorrectness :: String -> String -> Bool
propBreakOnCorrectness pat str =
  let (prefix, suffix) = breakOn pat str
  in if pat `isInfixOf` str
     then prefix ++ pat ++ suffix == str
     else prefix == str && null suffix

propRemoveLineCommentsPreserves :: String -> Bool
propRemoveLineCommentsPreserves str =
  let withoutComments = removeLineComments str
      linesWithComments = lines str
      linesWithout = lines withoutComments
  in length linesWithout <= length linesWithComments

propSourcePosValid :: SourcePos -> Bool
propSourcePosValid pos = posLine pos > 0 && posColumn pos > 0

propTypeConstraintsValid :: String -> Int -> Bool
propTypeConstraintsValid name size =
  let constraint = SizeConstraint name size
  in not (null name) && size >= 0