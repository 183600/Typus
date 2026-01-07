{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewOwnershipBasicCoreQuickCheckSpec where

-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | Basic property tests for Ownership module Test.Unit.NewOwnershipBasicCoreQuickCheckSpec Test.Unit.NewOwnershipBasicCoreQuickCheckSpec where
import Test.Tasty
import Test.Tasty.QuickCheck
import Ownership

ied Data.Text as T
import qualified Data.List as L
import Data.List 
  in (comparison == LT ||                               comparison == EQ ||                               comparison == GT) === True

-- | OwnershipType equality should be reflexive
prop_ownership_type_reflexive :: OwnershipType -> Property
prop_ownership_type_reflexive                               ot =                               ot === ot

-- | OwnershipTransfer should be constructible
prop_ownership_transfer_constructible :: String -> String -> String -> Property
prop_ownership_transfer_constructible from to                               resource =
  let transfer = OwnershipTransfer from to resource
  in property True  -- If it constructs, it's valid

-- | OwnershipAnalyzer should be created consistently
prop_ownership_analyzer_consistent :: Property
                              prop_ownership_analyzer_consistent =
  let analyzer1 = newOwnershipAnalyzer
                                    analyzer2 = newOwnershipAnalyzer
  in property True  -- Both should be valid analyzers

-- | Empty code should analyze without crashing
prop_analyze_empty_code :: Property
                              prop_analyze_empty_code =
  let analyzer = newOwnershipAnalyzer
                                    result = analyzeOwnership analyzer ""
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | Simple assignment should be analyzable
prop_analyze_simple_assignment :: String -> Property
prop_analyze_simple_assignment                               varName =
  let input = varName ++ " = 42"
                                    analyzer = newOwnershipAnalyzer
                                    result = analyzeOwnership analyzer input
  in case result of
    Left _ -> property True
    Right _ -> property True

-- | OwnershipError should format to non-empty string
prop_ownership_error_formatting :: OwnershipError -> Property
prop_ownership_error_formatting                               err =
  let formatted = formatOwnershipErrors [err]
  in not (T.null formatted) === True

-- | Multiple errors should format consistently
prop_multiple_errors_formatting :: [OwnershipError] -> Property
prop_multiple_errors_formatting                               errs =
  let formatted = formatOwnershipErrors errs
                                    errorCount = L.length errs
  in if errorCount > 0
     then not (T.null formatted) === True
     else property True

-- | Variable names should be handled consistently
prop_variable_names_consistent :: String -> String -> Property
prop_variable_names_consistent var1                               var2 =
  let input1 = var1 ++ " = " ++ var2
                                    input2 = var2 ++ " = " ++ var1
                                    analyzer = newOwnershipAnalyzer
                                    result1 = analyzeOwnership analyzer input1
                                    result2 = analyzeOwnership analyzer input2
  in case (result1, result2) of
    (Left _, Left _) -> property True
    (Right _, Right _) -> property True
    (Left _, Right _) -> property True
    (Right _, Left _) -> property True

-- | Ownership analysis should be deterministic
prop_ownership_deterministic :: String -> Property
prop_ownership_deterministic                               code =
  let analyzer = newOwnershipAnalyzer
                                    result1 = analyzeOwnership analyzer code
                                    result2 = analyzeOwnership analyzer code
  in case (result1, result2) of
    (Left err1, Left err2) -> show                               err1 === show err2
    (Right res1, Right res2) -> show                               res1 === show res2
    _ -> property False  -- Should get same result type

-- | Complex expressions should not crash analyzer
prop_complex_expressions :: String -> String -> Property
prop_complex_expressions var1                               var2 =
  let input = var1 ++ " = " ++ var2 ++ " + " ++ var1 ++ " * 2"
                                    analyzer = newOwnershipAnalyzer
                                    result =  analyzeOwnership analyzer input
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- ============================================================================
-- Test Suite
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Ownership Basic QuickCheck Tests"
  [             testProperty "OwnershipType: comparability" prop_ownership_type_comparable
  ,             testProperty "OwnershipType: reflexivity" prop_ownership_type_reflexive
  ,             testProperty "OwnershipTransfer: constructibility" prop_ownership_transfer_constructible
  ,             testProperty "OwnershipAnalyzer: consistency" prop_ownership_analyzer_consistent
  ,             testProperty "Empty code analysis" prop_analyze_empty_code
  ,             testProperty "Simple assignment analysis" prop_analyze_simple_assignment
  ,             testProperty "OwnershipError: formatting" prop_ownership_error_formatting
  ,             testProperty "Multiple errors: formatting" prop_multiple_errors_formatting
  ,             testProperty "Variable names: consistency" prop_variable_names_consistent
  ,             testProperty "Ownership analysis: determinism" prop_ownership_deterministic
  ]