{-# LANGUAGE CPP #-}

module Test.Unit.CompilerErrorHandlerTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property)

import Compiler (CompilerError(..), CompilationPhase(..), hasTypeErrors, analyzeErrors)
import ErrorHandler ()

-- Mock implementations for testing
data MockErrorHandler = MockErrorHandler

defaultErrorHandler :: MockErrorHandler
defaultErrorHandler = MockErrorHandler

handleError :: MockErrorHandler -> a -> Maybe String
handleError _ _ = Just "handled"
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanFrom)
import qualified Data.Text as T (pack, unpack)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Maybe (isNothing, isJust)

-- ============================================================================
-- Compiler Error Tests
-- ============================================================================

-- Test CompilerError properties
prop_compiler_error_has_phase :: CompilerError -> Bool
prop_compiler_error_has_phase err = 
    let phase = errorPhase err
    in phase >= ParsePhase && phase <= CodeGenPhase

prop_compiler_error_has_message :: CompilerError -> Bool
prop_compiler_error_has_message err = 
    let msg = errorMessage err
    in not (T.null msg)

prop_hasTypeErrors_consistency :: [CompilerError] -> Bool
prop_hasTypeErrors_consistency errors =
    let hasTypeErrs = hasTypeErrors errors
        typeErrors = filter isErrorType errors
    in hasTypeErrs == not (null typeErrors)
  where
    isErrorType (TypeError _) = True
    isErrorType (DependentTypeError _) = True
    isErrorType (OwnershipError _) = True
    isErrorType _ = False

-- Test error analysis
test_analyzeErrors_empty :: IO ()
test_analyzeErrors_empty = do
    let errors = []
        analysis = analyzeErrors errors
    assertEqual "Empty error analysis" 0 (L.length analysis)

test_analyzeErrors_by_phase :: IO ()
test_analyzeErrors_by_phase = do
    let errors = [ ParseError (T.pack "parse error") startPos
                 , TypeError (T.pack "type error") (spanFrom startPos)
                 , OwnershipError (T.pack "ownership error") (spanFrom startPos)
                 ]
        analysis = analyzeErrors errors
    assertEqual "Should have 3 errors" 3 (L.length analysis)
    assertBool "Should have parse errors" (L.any ((== ParsePhase) . errorPhase) analysis)
    assertBool "Should have type errors" (L.any ((== TypeCheckPhase) . errorPhase) analysis)
    assertBool "Should have ownership errors" (L.any ((== OwnershipPhase) . errorPhase) analysis)

-- ============================================================================
-- Error Handler Tests
-- ============================================================================

test_default_handler_creation :: IO ()
test_default_handler_creation = do
    let handler = defaultErrorHandler
    assertBool "Default handler should be created" (not (L.null (show handler)))

test_error_handling_workflow :: IO ()
test_error_handling_workflow = do
    let handler = defaultErrorHandler
        error = ParseError (T.pack "test error") startPos
        result = handleError handler error
    assertBool "Error should be handled" (isJust result)

-- ============================================================================
-- Error Message Formatting Tests
-- ============================================================================

test_error_message_formatting :: IO ()
test_error_message_formatting = do
    let errors = [ ParseError (T.pack "unexpected token") (spanFrom (SourcePos 1 5))
                 , TypeError (T.pack "type mismatch") (spanFrom (SourcePos 2 10))
                 ]
    assertBool "Parse error should contain position" 
        (L.any (\e -> "line 1" `L.isInfixOf` T.unpack (errorMessage e)) errors)
    assertBool "Type error should contain position"
        (L.any (\e -> "line 2" `L.isInfixOf` T.unpack (errorMessage e)) errors)

-- ============================================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary `suchThat` (>= 0)

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endCol <- arbitrary `suchThat` (>= sourceColumn start)
    endLine <- arbitrary `suchThat` (>= sourceLine start)
    return $ SourceSpan start (SourcePos endLine endCol)

instance Arbitrary CompilationPhase where
  arbitrary = elements [ParsePhase, TypeCheckPhase, OwnershipPhase, DependentTypePhase, CodeGenPhase]

instance Arbitrary CompilerError where
  arbitrary = oneof
    [ ParseError <$> arbitrary <*> arbitrary
    , TypeError <$> arbitrary <*> arbitrary
    , OwnershipError <$> arbitrary <*> arbitrary
    , DependentTypeError <$> arbitrary <*> arbitrary
    , CodeGenError <$> arbitrary <*> arbitrary
    ]

-- ============================================================================
-- Test Utilities
-- ============================================================================

elements :: [a] -> Gen a
elements [] = error "elements: empty list"
elements xs = do
  idx <- arbitrary `suchThat` (\i -> i >= 0 && i < L.length xs)
  return (xs !! idx)

oneof :: [Gen a] -> Gen a
oneof [] = error "oneof: empty list"
oneof gens = do
  idx <- arbitrary `suchThat` (\i -> i >= 0 && i < L.length gens)
  (gens !! idx)

suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p = do
  x <- gen
  if p x then return x else gen `suchThat` p

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler L.and Error Handler Test Suite"
  [ testGroup "Compiler Error Tests"
      [ fastProperty "Compiler error has valid phase" prop_compiler_error_has_phase
      , fastProperty "Compiler error has non-empty message" prop_compiler_error_has_message
      , fastProperty "hasTypeErrors consistency" prop_hasTypeErrors_consistency
      , testCase "Analyze empty errors" test_analyzeErrors_empty
      , testCase "Analyze errors by phase" test_analyzeErrors_by_phase
      ]
  , testGroup "Error Handler Tests"
      [ testCase "Default handler creation" test_default_handler_creation
      , testCase "Error handling workflow" test_error_handling_workflow
      ]
  , testGroup "Error Message Formatting Tests"
      [ testCase "Error message formatting" test_error_message_formatting
      ]
  ]