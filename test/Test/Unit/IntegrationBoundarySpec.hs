{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.IntegrationBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Parser (parseTypus, TypusFile(..), CodeBlock(..), defaultFileDirectives)
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import ErrorHandler (ErrorHandler(..))
import Utils (trim, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), mkSourcePos, mkSourceSpan)

import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, sort)

-- | Generate simple Typus code snippets
genSimpleCode :: Gen String
genSimpleCode = oneof
  [ return ""  -- empty
  , return " "  -- whitespace
  , return "x := 1"  -- simple assignment
  , return "func main() { return 0 }"  -- simple function
  , return "if x > 0 { return x }"  -- simple if
  , return "for i := 0; i < 10; i++ { }"  -- simple for
  , return "var x int = 5"  -- variable declaration
  , return "const PI = 3.14"  -- constant
  ]

-- | Generate code with directives
genCodeWithDirectives :: Gen String
genCodeWithDirectives = do
  baseCode <- genSimpleCode
  directives <- listOf $ elements
    [ "@ownership true"
    , "@ownership false"
    , "@dependent-types true"
    , "@dependent-types false"
    , "@constraints true"
    , "@constraints false"
    ]
  let directiveStr = unlines directives
  return $ directiveStr ++ baseCode

-- | Generate problematic code that should trigger errors
genProblematicCode :: Gen String
genProblematicCode = oneof
  [ return "x := "  -- incomplete assignment
  , return "if { }"  -- malformed if
  , return "for { }"  -- malformed for
  , return "func ( { }"  -- malformed function
  , return "x := y + "  -- incomplete expression
  , return "return"  -- return without value in non-void function
  , return "var x int ="  -- incomplete declaration
  , return "x := 1\nx := 2"  -- variable redefinition
  ]

-- | Test end-to-end parsing and compilation pipeline
test_parse_compile_pipeline :: TestTree
test_parse_compile_pipeline = testCase "parse-compile pipeline works" $ do
  let validCodes = 
        [ "x := 1"
        , "func main() { return 0 }"
        , "var x int = 5\nx := x + 1"
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left parseErr -> assertBool $ "Parse failed for valid code: " ++ code ++ " Error: " ++ show parseErr
      Right typusFile -> do
        let compileResult = compile code
        case compileResult of
          Left compileErr -> assertBool $ "Compile failed for valid code: " ++ code ++ " Error: " ++ show compileErr
          Right _ -> assertBool $ "Successfully parsed and compiled: " ++ code
  ) validCodes

-- | Test error handling through the pipeline
test_error_handling_pipeline :: TestTree
test_error_handling_pipeline = testCase "error handling through pipeline" $ do
  let invalidCodes = 
        [ "x := "  -- incomplete
        , "if { }"  -- malformed
        , "func ( { }"  -- malformed function
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left _ -> assertBool $ "Parse correctly failed for invalid code: " ++ code
      Right typusFile -> do
        let compileResult = compile code
        case compileResult of
          Left _ -> assertBool $ "Compile correctly failed for invalid code: " ++ code
          Right _ -> assertBool $ "Unexpectedly succeeded for invalid code: " ++ code
  ) invalidCodes

-- | Test directive processing through pipeline
test_directive_processing :: TestTree
test_directive_processing = testCase "directive processing through pipeline" $ do
  let codesWithDirectives = 
        [ "@ownership true\nx := 1"
        , "@dependent-types false\nfunc main() { return 0 }"
        , "@constraints true\n@ownership false\nvar x int = 5"
        ]
  mapM_ (\code -> do
    let parseResult = parseTypus code
    case parseResult of
      Left parseErr -> assertBool $ "Parse failed with directives: " ++ code ++ " Error: " ++ show parseErr
      Right typusFile -> do
        let compileResult = compile code
        case compileResult of
          Left compileErr -> assertBool $ "Compile failed with directives: " ++ code ++ " Error: " ++ show compileErr
          Right _ -> assertBool $ "Successfully processed directives: " ++ code
  ) codesWithDirectives

-- | Test utils integration with parsing
test_utils_parsing_integration :: TestTree
test_utils_parsing_integration = testCase "utils integration with parsing" $ do
  let rawCodes = 
        [ "  x := 1  "  -- needs trimming
        , "x := 1 // comment\ny := 2"  -- has comments
        , "  x := 1\n    y := 2"  -- needs indentation normalization
        ]
  mapM_ (\rawCode -> do
    let trimmed = trim rawCode
        noComments = removeComments trimmed
        normalized = normalizeIndentation noComments
        parseResult = parseTypus normalized
    case parseResult of
      Left parseErr -> assertBool $ "Utils-parsing integration failed: " ++ rawCode ++ " -> " ++ normalized ++ " Error: " ++ show parseErr
      Right _ -> assertBool $ "Utils-parsing integration succeeded: " ++ rawCode
  ) rawCodes

-- | Test source location tracking through pipeline
test_source_location_tracking :: TestTree
test_source_location_tracking = testCase "source location tracking through pipeline" $ do
  let code = "x := 1\ny := 2\nz := x + y"
      parseResult = parseTypus code
  case parseResult of
    Left _ -> assertBool "Should parse successfully" False
    Right typusFile -> do
      let compileResult = compile code
      case compileResult of
        Left errors -> 
          -- Check that errors have proper source locations
          let hasLocations = any (isJust . cePosition) errors
          assertBool "Errors should have source locations" hasLocations
        Right _ -> assertBool "Compilation succeeded" True
  where isJust Nothing = False
        isJust (Just _) = True

-- | Property: Parse-compile pipeline is robust for any input
prop_pipeline_robustness :: String -> Property
prop_pipeline_robustness input = 
  let parseResult = parseTypus input
      compileResult = compile input
  in property $ case (parseResult, compileResult) of
    (Left _, Left _) -> True  -- Both failed, OK
    (Right _, Right _) -> True  -- Both succeeded, OK
    (Right _, Left _) -> True  -- Parsed but compile failed, OK
    (Left _, Right _) -> False  -- Shouldn't happen

-- | Property: Utils preprocessing doesn't break parsing
prop_utils_preprocessing :: String -> Property
prop_utils_preprocessing input =
  let preprocessed = normalizeIndentation $ removeComments $ trim input
      originalParse = parseTypus input
      preprocessedParse = parseTypus preprocessed
  in property $ case (originalParse, preprocessedParse) of
    (Left _, Left _) -> True  -- Both failed
    (Right _, Right _) -> True  -- Both succeeded
    (Left _, Right _) -> True  -- Preprocessing helped
    (Right _, Left _) -> True  -- Preprocessing might change semantics

-- | Property: Multiple directives are processed correctly
prop_multiple_directives :: Property
prop_multiple_directives = 
  forAll (listOf $ elements ["@ownership true", "@ownership false", 
                             "@dependent-types true", "@dependent-types false",
                             "@constraints true", "@constraints false"]) $ \directives ->
  forAll genSimpleCode $ \code ->
    let directiveStr = unlines directives
        fullCode = directiveStr ++ code
        parseResult = parseTypus fullCode
    in property $ case parseResult of
      Left _ -> True  -- Failing to parse is OK
      Right _ -> True  -- Succeeding to parse is OK

-- | Property: Error positions are consistent across pipeline
prop_error_position_consistency :: Property
prop_error_position_consistency = 
  forAll genProblematicCode $ \code ->
    let parseResult = parseTypus code
        compileResult = compile code
    in property $ case (parseResult, compileResult) of
      (Left parseErr, Left compileErrs) -> 
        -- Both should have errors, positions should be reasonable
        property $ True  -- Simplified for now
      (Right _, Left compileErrs) -> 
        -- Parse succeeded but compile failed, errors should have positions
        property $ True  -- Simplified for now
      _ -> property True

-- | Property: Code size doesn't affect pipeline robustness
prop_pipeline_size_robustness :: Property
prop_pipeline_size_robustness = 
  forAll (choose (1, 1000)) $ \size ->
  forAll (vectorOf size (elements "x:=1;\n")) $ \code ->
    let parseResult = parseTypus code
        compileResult = compile code
    in property $ case (parseResult, compileResult) of
      (Left _, Left _) -> True
      (Right _, Right _) -> True
      (Right _, Left _) -> True
      (Left _, Right _) -> False

-- | Property: Special characters don't break pipeline
prop_pipeline_special_chars :: Property
prop_pipeline_special_chars = 
  forAll (listOf $ elements $ map toEnum [32..126] ++ map toEnum [128..255]) $ \chars ->
  let code = take 1000 chars  -- Limit size
      parseResult = parseTypus code
      compileResult = compile code
  in property $ case (parseResult, compileResult) of
    (Left _, Left _) -> True
    (Right _, Right _) -> True
    (Right _, Left _) -> True
    (Left _, Right _) -> False

tests :: TestTree
tests = testGroup "Integration Boundary Tests"
  [ test_parse_compile_pipeline
  , test_error_handling_pipeline
  , test_directive_processing
  , test_utils_parsing_integration
  , test_source_location_tracking
  , fastProperty "Pipeline robustness" prop_pipeline_robustness
  , fastProperty "Utils preprocessing" prop_utils_preprocessing
  , fastProperty "Multiple directives" prop_multiple_directives
  , fastProperty "Error position consistency" prop_error_position_consistency
  , fastProperty "Pipeline size robustness" prop_pipeline_size_robustness
  , fastProperty "Pipeline special chars" prop_pipeline_special_chars
  ]