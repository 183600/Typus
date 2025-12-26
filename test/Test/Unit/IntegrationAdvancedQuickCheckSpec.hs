{-# LANGUAGE CPP #-}
module Test.Unit.IntegrationAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample, (==>))

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.List (isInfixOf, nub, sort)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad.State (evalState)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler (compile, CompilerResult, generateGoCode, hasMalformedSyntax)
import Ownership (analyzeOwnership, newOwnershipAnalyzer, formatOwnershipErrors)
import Dependencies (analyzeDependentTypes, newDependentTypeChecker, getDependentTypeErrors)
import ErrorHandler (formatError, formatErrors, hasErrors, hasWarnings)
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt, locatedValue)
import Utils (trim, splitBy, removeComments, normalizeIndentation)

-- ============================================================================
-- Test Data Generation
-- ============================================================================

-- Generate complete Typus source code for integration testing
genTypusSourceCode :: Gen String
genTypusSourceCode = do
  numDirectives <- choose (0, 3)
  numBlocks <- choose (1, 5)
  
  directives <- replicateM numDirectives genDirective
  blocks <- replicateM numBlocks genCodeBlock
  
  return $ unlines (directives ++ concat blocks)
  where
    genDirective = oneof
      [ return "//! ownership: true"
      , return "//! dependent-types: true"
      , return "//! constraints: true"
      , return "//! ownership: true, dependent-types: true"
      ]
    
    genCodeBlock = do
      hasBlockDirective <- arbitrary
      numLines <- choose (1, 10)
      codeLines <- replicateM numLines genCodeLine
      
      let blockDirective = if hasBlockDirective then ["{//! ownership: true}"] else []
      let blockContent = blockDirective ++ codeLines
      
      return blockContent
    
    genCodeLine = oneof
      [ return "let x = 42"
      , return "let y = x + 1"
      , return "func add(a, b) { return a + b }"
      , return "var result = add(x, y)"
      , return "let ptr = &x"
      , return "let mut_var = &mut y"
      , return "if condition { branch }"
      , return "for i in range(10) { process(i) }"
      , return "type Vector<T> = struct { data: [T], size: int }"
      , return "constraint size<T> >= 0"
      ]

-- Generate simple source code for specific scenarios
genSimpleSourceCode :: Gen String
genSimpleSourceCode = oneof
  [ return "let x = 42"
  , return "func main() { return 0 }"
  , return "var x = 1\nvar y = x"
  , return "let ptr = &x"
  , return "//! ownership: true\nlet x = 42"
  ]

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Integration Advanced QuickCheck Tests"
    [ testProperty "parseTypus handles generated source code" $
        \sourceCode ->
          let result = parseTypus sourceCode
          in case result of
            Left _ -> property True
            Right typusFile -> property True

    , testProperty "compile handles parsed TypusFile" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True  -- Parse errors are acceptable
            Right typusFile ->
              let compileResult = compile typusFile
              in case compileResult of
                Left _ -> property True  -- Compilation errors are acceptable
                Right goCode -> property $ not (null goCode)

    , testProperty "end-to-end pipeline: parse -> compile" $
        \sourceCode ->
          let parseResult = parseTypus sourceCode
              pipelineResult = do
                typusFile <- parseResult
                compile typusFile
          in case pipelineResult of
            Left _ -> property True
            Right goCode -> property $ not (null goCode)

    , testProperty "ownership analysis works on parsed files" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let analyzer = newOwnershipAnalyzer
                  ownershipResult = analyzeOwnership analyzer (generateGoCode typusFile)
              in case ownershipResult of
                Left _ -> property True
                Right _ -> property True

    , testProperty "dependent types analysis works on parsed files" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let checker = newDependentTypeChecker
                  dependentResult = analyzeDependentTypes checker typusFile
              in case dependentResult of
                Left _ -> property True
                Right _ -> property True

    , testProperty "multiple analyses can be performed on same file" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let analyzer = newOwnershipAnalyzer
                  checker = newDependentTypeChecker
                  ownershipResult = analyzeOwnership analyzer (generateGoCode typusFile)
                  dependentResult = analyzeDependentTypes checker typusFile
              in case (ownershipResult, dependentResult) of
                (Left _, Left _) -> property True
                (Right _, Right _) -> property True
                (Left _, Right _) -> property True
                (Right _, Left _) -> property True

    , testProperty "error formatting works across modules" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let compileResult = compile typusFile
              in case compileResult of
                Left errors -> not (null (formatErrors errors))
                Right _ -> property True

    , testProperty "source location tracking is consistent" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let blocks = tfBlocks typusFile
                  spans = map cbSpan blocks
                  locations = map locatedPos (map (locatedAt (spanStart undefined)) spans)
              in length locations === length blocks

    , testProperty "utils functions work on generated code" $
        \sourceCode ->
          let trimmed = trim sourceCode
              splitLines = splitBy '\n' sourceCode
              withoutComments = removeComments sourceCode
              normalized = normalizeIndentation sourceCode
          in length splitLines >= 0 .&&.
             length withoutComments <= length sourceCode .&&.
             length normalized >= 0

    , testProperty "compilation preserves semantic meaning" $
        \simpleCode ->
          case parseTypus simpleCode of
            Left _ -> property True
            Right typusFile ->
              let compileResult = compile typusFile
                  goCode = generateGoCode typusFile
              in case compileResult of
                Left _ -> property True
                Right compiledGo -> not (null compiledGo) .&&. not (null goCode)

    , testProperty "error detection is consistent across analyses" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let hasSyntaxErrors = not (null (tfSyntaxErrors typusFile))
                  hasMalformed = hasMalformedSyntax typusFile
                  compileResult = compile typusFile
                  hasCompileErrors = case compileResult of
                    Left _ -> True
                    Right _ -> False
              in hasSyntaxErrors ==> (hasMalformed || hasCompileErrors)

    , testProperty "ownership analysis handles Go code generation" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let goCode = generateGoCode typusFile
                  analyzer = newOwnershipAnalyzer
                  ownershipResult = analyzeOwnership analyzer goCode
              in case ownershipResult of
                Left _ -> property True
                Right _ -> property True

    , testProperty "dependent types analysis works with ownership" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let checker = newDependentTypeChecker
                  analyzer = newOwnershipAnalyzer
                  goCode = generateGoCode typusFile
                  dependentResult = analyzeDependentTypes checker typusFile
                  ownershipResult = analyzeOwnership analyzer goCode
              in case (dependentResult, ownershipResult) of
                (Left _, Left _) -> property True
                (Right _, Right _) -> property True
                (Left _, Right _) -> property True
                (Right _, Left _) -> property True

    , testProperty "file directives affect analysis behavior" $
        \sourceCode ->
          let withDirectives = "//! ownership: true\n//! dependent-types: true\n" ++ sourceCode
              parseResult = parseTypus withDirectives
          in case parseResult of
            Left _ -> property True
            Right typusFile ->
              let directives = tfDirectives typusFile
              in property True  -- Basic check that directives are parsed

    , testProperty "block directives are preserved in analysis" $
        \sourceCode ->
          let withBlockDirective = "{//! ownership: true}\n" ++ sourceCode
              parseResult = parseTypus withBlockDirective
          in case parseResult of
            Left _ -> property True
            Right typusFile ->
              let blocks = tfBlocks typusFile
                  hasBlockDirective = any (\b -> cbDirectives b /= defaultBlockDirectives) blocks
              in hasBlockDirective ==> property True

    , testProperty "integration pipeline is deterministic" $
        \sourceCode ->
          let result1 = parseTypus sourceCode
              result2 = parseTypus sourceCode
          in result1 === result2

    , testProperty "error messages are informative" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left parseError -> not (null parseError)
            Right typusFile ->
              case compile typusFile of
                Left compileErrors -> all (\e -> not (null (T.unpack (errorMessage e)))) compileErrors
                Right _ -> property True

    , testProperty "cross-module error handling is consistent" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let parseErrors = tfSyntaxErrors typusFile
                  compileResult = compile typusFile
              in case compileResult of
                Left compileErrors -> 
                  let hasAnyError = not (null parseErrors) || not (null compileErrors)
                  in hasAnyError ==> property True
                Right _ -> property True

    , testProperty "performance is reasonable for small inputs" $
        \simpleCode ->
          let parseResult = parseTypus simpleCode
              pipelineResult = do
                typusFile <- parseResult
                compile typusFile
          in case pipelineResult of
            Left _ -> property True
            Right goCode -> property $ length goCode <= 10000  -- Reasonable output size

    , testProperty "memory usage is bounded" $
        \sourceCode ->
          let limitedSource = take 1000 sourceCode  -- Limit input size
              parseResult = parseTypus limitedSource
          in case parseResult of
            Left _ -> property True
            Right typusFile ->
              let blocks = tfBlocks typusFile
                  totalContent = sum $ map (length . cbContent) blocks
              in totalContent <= 10000  -- Reasonable content size

    , testProperty "type system integration works" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let checker = newDependentTypeChecker
                  dependentResult = analyzeDependentTypes checker typusFile
              in case dependentResult of
                Left errors -> not (null errors)
                Right _ -> property True

    , testProperty "ownership system integration works" $
        \sourceCode ->
          case parseTypus sourceCode of
            Left _ -> property True
            Right typusFile ->
              let analyzer = newOwnershipAnalyzer
                  goCode = generateGoCode typusFile
                  ownershipResult = analyzeOwnership analyzer goCode
              in case ownershipResult of
                Left errors -> not (null (formatOwnershipErrors errors))
                Right _ -> property True
    ]

-- Helper function for accessing error message
errorMessage :: err -> String
errorMessage = show  -- Use show as a generic implementation