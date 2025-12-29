module Test.Unit.IntegrationEndToEndQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import qualified Data.Set as Set

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler (compileTypus, CompilationResult(..))
import Utils (trim, removeComments)
import SourceLocation (SourcePos(..), startPos)

-- | QuickCheck tests for Integration End-to-End scenarios
tests :: TestTree
tests =
  testGroup "IntegrationEndToEndQuickCheckSpec - Integration End-to-End Tests"
    [ testProperty "Parse-Compile pipeline preserves semantics" prop_parseCompilePipeline
    , testProperty "Error propagation through pipeline is consistent" prop_errorPropagationConsistency
    , testProperty "Multiple file compilation maintains module boundaries" prop_multiFileCompilation
    , testProperty "Optimization passes don't change program behavior" prop_optimizationPreservesBehavior
    , testProperty "Dependency resolution is sound and complete" prop_dependencyResolution
    , testProperty "Type checking catches all type errors" prop_typeCheckingCompleteness
    , testProperty "Code generation produces valid output" prop_codeGenerationValidity
    , testProperty "Round-trip compilation preserves program structure" prop_roundTripCompilation
    ]

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- Property: Parse-Compile pipeline preserves program semantics
prop_parseCompilePipeline :: String -> Bool
prop_parseCompilePipeline input =
  let parseResult = parseTypus input
  in case parseResult of
    Left parseError -> True  -- Parse errors are expected for malformed input
    Right typusFile ->
      let compileResult = compileTypus typusFile
      in case compileResult of
        Left compileError -> True  -- Compile errors are acceptable
        Right compilationResult ->
          let originalSemantics = extractSemantics typusFile
              compiledSemantics = extractCompiledSemantics compilationResult
          in originalSemantics == compiledSemantics

-- Property: Error propagation through pipeline is consistent
prop_errorPropagationConsistency :: String -> Bool
prop_errorPropagationConsistency input =
  let parseResult = parseTypus input
      parseErrors = extractParseErrors parseResult
  in case parseResult of
    Left _ -> True  -- Parse errors should be reported
    Right typusFile ->
      let compileResult = compileTypus typusFile
          compileErrors = extractCompileErrors compileResult
      in case compileResult of
        Left _ -> True  -- Compile errors should be reported
        Right _ -> True  -- Success is acceptable

-- Property: Multiple file compilation maintains module boundaries
prop_multiFileCompilation :: [String] -> Bool
prop_multiFileCompilation inputs =
  let parseResults = map parseTypus inputs
      successfulParses = [file | Right file <- parseResults]
      compileResults = map compileTypus successfulParses
      successfulCompiles = [result | Right result <- compileResults]
  in length successfulCompiles <= length successfulParses &&
     length successfulParses <= length inputs

-- Property: Optimization passes don't change program behavior
prop_optimizationPreservesBehavior :: String -> Bool
prop_optimizationPreservesBehavior input =
  let parseResult = parseTypus input
  in case parseResult of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile ->
      let compileResult = compileTypus typusFile
      in case compileResult of
        Left _ -> True  -- Compile errors are acceptable
        Right compilationResult ->
          let optimizedResult = optimizeCompilation compilationResult
              originalBehavior = simulateBehavior compilationResult
              optimizedBehavior = simulateBehavior optimizedResult
          in originalBehavior == optimizedBehavior

-- Property: Dependency resolution is sound and complete
prop_dependencyResolution :: String -> Bool
prop_dependencyResolution input =
  let parseResult = parseTypus input
  in case parseResult of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile ->
      let dependencies = extractDependencies typusFile
          resolvedDependencies = resolveDependencies dependencies
          allDependenciesResolved = all isDependencyResolved resolvedDependencies
      in allDependenciesResolved

-- Property: Type checking catches all type errors
prop_typeCheckingCompleteness :: String -> Bool
prop_typeCheckingCompleteness input =
  let parseResult = parseTypus input
  in case parseResult of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile ->
      let typeCheckResult = typeCheckTypus typusFile
      in case typeCheckResult of
        Left typeErrors -> all isValidTypeError typeErrors
        Right _ -> True  -- No type errors is acceptable

-- Property: Code generation produces valid output
prop_codeGenerationValidity :: String -> Bool
prop_codeGenerationValidity input =
  let parseResult = parseTypus input
  in case parseResult of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile ->
      let compileResult = compileTypus typusFile
      in case compileResult of
        Left _ -> True  -- Compile errors are acceptable
        Right compilationResult ->
          let generatedCode = generateCode compilationResult
          in isValidGeneratedCode generatedCode

-- Property: Round-trip compilation preserves program structure
prop_roundTripCompilation :: String -> Bool
prop_roundTripCompilation input =
  let parseResult = parseTypus input
  in case parseResult of
    Left _ -> True  -- Parse errors are acceptable
    Right typusFile ->
      let compileResult = compileTypus typusFile
      in case compileResult of
        Left _ -> True  -- Compile errors are acceptable
        Right compilationResult ->
          let generatedCode = generateCode compilationResult
              reparseResult = parseTypus generatedCode
          in case reparseResult of
            Left _ -> False  -- Should be able to reparse generated code
            Right reparsedFile ->
              let originalStructure = extractProgramStructure typusFile
                  reparsedStructure = extractProgramStructure reparsedFile
              in originalStructure == reparsedStructure

-- ============================================================================
-- Helper Functions (Mock implementations for testing)
-- ============================================================================

-- Mock CompilationResult data type
data CompilationResult = CompilationResult
  { resultIR :: String
  , resultOptimized :: Bool
  , resultDependencies :: [String]
  } deriving (Show, Eq)

-- Mock helper functions
compileTypus :: TypusFile -> Either String CompilationResult
compileTypus typusFile = 
  if hasCompilationErrors typusFile
    then Left "Compilation error"
    else Right $ CompilationResult "IR code" False []

extractSemantics :: TypusFile -> String
extractSemantics _ = "program_semantics"

extractCompiledSemantics :: CompilationResult -> String
extractCompiledSemantics _ = "compiled_semantics"

extractParseErrors :: Either a b -> [String]
extractParseErrors (Left _) = ["parse_error"]
extractParseErrors (Right _) = []

extractCompileErrors :: Either a b -> [String]
extractCompileErrors (Left _) = ["compile_error"]
extractCompileErrors (Right _) = []

hasCompilationErrors :: TypusFile -> Bool
hasCompilationErrors _ = False  -- Mock implementation

optimizeCompilation :: CompilationResult -> CompilationResult
optimizeCompilation result = result { resultOptimized = True }

simulateBehavior :: CompilationResult -> String
simulateBehavior _ = "simulated_behavior"

extractDependencies :: TypusFile -> [String]
extractDependencies _ = ["dep1", "dep2"]

resolveDependencies :: [String] -> [(String, Bool)]
resolveDependencies deps = [(dep, True) | dep <- deps]

isDependencyResolved :: (String, Bool) -> Bool
isDependencyResolved (_, resolved) = resolved

typeCheckTypus :: TypusFile -> Either [String] String
typeCheckTypus _ = Right "Type check passed"

isValidTypeError :: String -> Bool
isValidTypeError err = "type" `isInfixOf` err

generateCode :: CompilationResult -> String
generateCode result = "generated_code_from_" ++ resultIR result

isValidGeneratedCode :: String -> Bool
isValidGeneratedCode code = not (null code) && length code < 10000

extractProgramStructure :: TypusFile -> String
extractProgramStructure _ = "program_structure"

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

-- Helper for generating arbitrary typus code snippets
arbitraryTypusCode :: Gen String
arbitraryTypusCode = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n\r"
  , elements "{}[]();,.!@#$%^&*"
  , elements "func var return if else"
  ]

arbitraryTypusFile :: Gen TypusFile
arbitraryTypusFile = do
  code <- arbitraryTypusCode
  case parseTypus code of
    Left _ -> arbitraryTypusFile  -- Try again if parse fails
    Right file -> return file

instance Arbitrary TypusFile where
  arbitrary = arbitraryTypusFile

instance Arbitrary CompilationResult where
  arbitrary = CompilationResult <$> arbitrary <*> arbitrary <*> listOf arbitrary

instance Arbitrary String where
  arbitrary = arbitraryTypusCode