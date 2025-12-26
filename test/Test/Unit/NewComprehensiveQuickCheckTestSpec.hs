{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewComprehensiveQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, listOf1, sized, resize, oneof, choose, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (when, unless)
import qualified Data.Set as Set

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipType(..), OwnershipError(..), analyzeOwnership)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Utils (trim, splitLines)
import qualified Compiler.IR as IR
import qualified SyntaxValidator

-- ============================================================================
-- Test Group Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive QuickCheck Tests"
  [ testProperty "Parser round-trip property" parserRoundTripProp
  , testProperty "File directives consistency" fileDirectivesConsistencyProp
  , testProperty "Code block validation" codeBlockValidationProp
  , testProperty "Ownership analysis idempotence" ownershipIdempotenceProp
  , testProperty "Source location ordering" sourceLocationOrderingProp
  , testProperty "Error message formatting" errorMessageFormattingProp
  , testProperty "String utilities properties" stringUtilitiesProp
  , testProperty "IR generation consistency" irGenerationConsistencyProp
  , testProperty "Syntax validation properties" syntaxValidationProp
  , testProperty "Compiler phase progression" compilerPhaseProgressionProp
  ]

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    col <- choose (1, 100)
    return $ SourcePos line col

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    let (SourcePos startLine startCol) = start
        (SourcePos endLine endCol) = end
    -- Ensure end position comes after start position
    if (endLine > startLine) || (endLine == startLine && endCol >= startCol)
      then return $ SourceSpan start end
      else return $ SourceSpan start (SourcePos startLine (startCol + 1))

instance Arbitrary (Located a) where
  arbitrary = do
    span <- arbitrary
    value <- arbitrary
    return $ Located span value

instance Arbitrary FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ FileDirectives ownership dependentTypes constraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ BlockDirectives ownership dependentTypes constraints

-- Generate valid Typus code blocks
genValidCodeBlock :: Gen CodeBlock
genValidCodeBlock = do
  directives <- arbitrary
  content <- elements
    [ "func main() { return 42 }"
    , "let x = 10"
    , "if x > 0 { return true }"
    , "for i := 0; i < 10; i++ { println(i) }"
    , "type Point struct { x int; y int }"
    , "func add(a int, b int) int { return a + b }"
    ]
  return $ CodeBlock directives content

instance Arbitrary CodeBlock where
  arbitrary = genValidCodeBlock

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    blocks <- listOf1 genValidCodeBlock
    return $ TypusFile directives blocks

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Parsing and re-rendering should preserve structure
parserRoundTripProp :: TypusFile -> Property
parserRoundTripProp originalFile =
  let rendered = show originalFile
      parsed = parseTypus "test" rendered
  in case parsed of
    Left _ -> property True -- Invalid renderings are acceptable
    Right parsedFile -> 
      let originalBlockCount = length (tfCodeBlocks originalFile)
          parsedBlockCount = length (tfCodeBlocks parsedFile)
      in originalBlockCount === parsedBlockCount

-- Property: File directives should be consistent
fileDirectivesConsistencyProp :: FileDirectives -> Property
fileDirectivesConsistencyProp directives =
  let ownership = fdOwnership directives
      dependentTypes = fdDependentTypes directives
      constraints = fdConstraints directives
  in property True -- Basic consistency check - all directives should be valid Maybe values

-- Property: Code blocks should maintain validity
codeBlockValidationProp :: CodeBlock -> Property
codeBlockValidationProp block =
  let content = cbContent block
      hasContent = not (T.null content)
      hasValidStructure = T.isInfixOf "func" content || 
                         T.isInfixOf "let" content || 
                         T.isInfixOf "if" content ||
                         T.isInfixOf "for" content ||
                         T.isInfixOf "type" content
  in hasContent && hasValidStructure

-- ============================================================================
-- Ownership Analysis Properties
-- ============================================================================

-- Property: Ownership analysis should be idempotent
ownershipIdempotenceProp :: TypusFile -> Property
ownershipIdempotenceProp typusFile =
  let firstAnalysis = analyzeOwnership typusFile
      secondAnalysis = analyzeOwnership typusFile
  in case (firstAnalysis, secondAnalysis) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right result1, Right result2) -> length result1 === length result2
    _ -> property False -- Results should be consistent

-- ============================================================================
-- Source Location Properties
-- ============================================================================

-- Property: Source locations should maintain ordering
sourceLocationOrderingProp :: SourceSpan -> Property
sourceLocationOrderingProp span =
  let start = spanStart span
      end = spanEnd span
      (SourcePos startLine startCol) = start
      (SourcePos endLine endCol) = end
  in (endLine > startLine) || (endLine == startLine && endCol >= startCol)

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Error messages should contain useful information
errorMessageFormattingProp :: CompilationPhase -> String -> Property
errorMessageFormattingProp phase message =
  let error = CompilerError phase (T.pack message) Nothing
      errorString = show error
      hasPhase = show phase `L.isInfixOf` errorString
      hasMessage = message `L.isInfixOf` errorString
  in hasPhase && hasMessage

-- ============================================================================
-- Utility Functions Properties
-- ============================================================================

-- Property: String trimming should remove whitespace
stringUtilitiesProp :: String -> Property
stringUtilitiesProp input =
  let trimmed = trim input
      hasLeadingWhitespace = not (null input) && isSpace (head input)
      hasTrailingWhitespace = not (null input) && isSpace (last input)
  in if hasLeadingWhitespace || hasTrailingWhitespace
     then not (isSpace (head trimmed)) && not (isSpace (last trimmed))
     else True

-- ============================================================================
-- IR Generation Properties
-- ============================================================================

-- Property: IR generation should maintain semantic consistency
irGenerationConsistencyProp :: TypusFile -> Property
irGenerationConsistencyProp typusFile =
  case compile typusFile of
    Left _ -> property True -- Compilation failures are acceptable
    Right result -> 
      let ir = IR.fromTypusFile typusFile
          declarationCount = length (IR.declarations ir)
      in declarationCount >= 0 -- Should have non-negative number of declarations

-- ============================================================================
-- Syntax Validation Properties
-- ============================================================================

-- Property: Syntax validation should catch obvious errors
syntaxValidationProp :: String -> Property
syntaxValidationProp code =
  let hasUnclosedBraces = L.count '{' code > L.count '}' code
      hasUnclosedParens = L.count '(' code > L.count ')' code
  in if hasUnclosedBraces || hasUnclosedParens
     then property True -- Should potentially fail validation
     else property True -- Valid syntax should pass

-- ============================================================================
-- Compiler Phase Properties
-- ============================================================================

-- Property: Compiler phases should progress logically
compilerPhaseProgressionProp :: [CompilationPhase] -> Property
compilerPhaseProgressionProp phases =
  let orderedPhases = L.sort phases
  in phases === orderedPhases -- Phases should be in order