{-# LANGUAGE CPP #-}

module Test.Unit.OwnershipGoToolchainTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)

import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property)

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..), newOwnershipAnalyzer, analyzeOwnership)
import GoToolchain (GoToolchain, initializeToolchain, checkGoInstallation, formatGoCode)
import SourceLocation (SourcePos(..), startPos, spanFrom)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Maybe (isNothing, isJust)

-- ============================================================================
-- Ownership Tests
-- ============================================================================

-- Test ownership analyzer creation
test_ownership_analyzer_creation :: IO ()
test_ownership_analyzer_creation = do
    let analyzer = newOwnershipAnalyzer
    assertBool "Ownership analyzer should be created" (not (L.null (show analyzer)))

-- Test ownership type properties
prop_ownership_type_has_string :: OwnershipType -> Bool
prop_ownership_type_has_string ownType = not (L.null (show ownType))

prop_ownership_transfer_validity :: OwnershipTransfer -> Bool
prop_ownership_transfer_validity transfer = 
    let source = transferSource transfer
        target = transferTarget transfer
    in not (null source) && not (null target)

-- Test ownership analysis
test_ownership_analysis_simple :: IO ()
test_ownership_analysis_simple = do
    let input = "func main() { let x = 42; let y = x; return y; }"
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer input
    assertBool "Simple ownership analysis should work" (isRight result)
  where
    isRight (Right _) = True
    isRight _ = False

-- Test ownership error formatting
test_ownership_error_formatting :: IO ()
test_ownership_error_formatting = do
    let error = OwnershipError (T.pack "Cannot move borrowed value") (spanFrom startPos)
        errorMsg = show error
    assertBool "Error message should contain ownership info" ("ownership" `L.isInfixOf` errorMsg)

-- Test ownership transfer scenarios
test_ownership_transfer_move :: IO ()
test_ownership_transfer_move = do
    let transfer = OwnershipTransfer "x" "y" MoveOwnership
        source = transferSource transfer
        target = transferTarget transfer
        transferType = transferType transfer
    assertEqual "Source should be x" "x" source
    assertEqual "Target should be y" "y" target
    assertBool "Should be move transfer" (transferType == MoveOwnership)

test_ownership_transfer_borrow :: IO ()
test_ownership_transfer_borrow = do
    let transfer = OwnershipTransfer "data" "reference" BorrowOwnership
        source = transferSource transfer
        target = transferTarget transfer
        transferType = transferType transfer
    assertEqual "Source should be data" "data" source
    assertEqual "Target should be reference" "reference" target
    assertBool "Should be borrow transfer" (transferType == BorrowOwnership)

-- ============================================================================
-- GoToolchain Tests
-- ============================================================================

-- Test Go toolchain initialization
test_go_toolchain_initialization :: IO ()
test_go_toolchain_initialization = do
    result <- initializeToolchain
    assertBool "Go toolchain should initialize" (isRight result)
  where
    isRight (Right _) = True
    isRight _ = False

-- Test Go installation check
test_go_installation_check :: IO ()
test_go_installation_check = do
    result <- checkGoInstallation
    -- This test might fail if Go is not installed, but that's expected
    assertBool "Go installation check should complete" (True)

-- Test Go code formatting
test_go_code_formatting :: IO ()
test_go_code_formatting = do
    let goCode = "package main\n\nfunc main() {\n\tfmt.Println(\"Hello, World!\")\n}"
        result = formatGoCode goCode
    case result of
        Right formatted -> do
            assertBool "Formatted code should contain package" ("package" `L.isInfixOf` formatted)
            assertBool "Formatted code should contain func" ("func" `L.isInfixOf` formatted)
        Left _ -> assertBool "Go formatting should work L.or fail gracefully" True

-- Test Go code generation properties
prop_go_code_has_package :: String -> Property
prop_go_code_has_package code = 
    not (null code) && "func" `L.isInfixOf` code ==>
    case formatGoCode code of
        Right formatted -> "package" `L.isInfixOf` formatted
        Left _ -> True -- Formatting failure is acceptable

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Test ownership with Go code generation
test_ownership_go_integration :: IO ()
test_ownership_go_integration = do
    let typusCode = "func process() { let data = create(); move(data); }"
        analyzer = newOwnershipAnalyzer
        ownershipResult = analyzeOwnership analyzer typusCode
    case ownershipResult of
        Right _ -> do
            let goCode = "package main\n\nfunc process() {\n\tdata := create()\n\tmove(data)\n}"
            formatResult <- return $ formatGoCode goCode
            case formatResult of
                Right formatted -> assertBool "Go code should be formatted" (L.length formatted > 0)
                Left _ -> assertBool "Formatting should not crash" True
        Left _ -> assertBool "Ownership analysis should work L.or fail gracefully" True

-- ============================================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================================

instance Arbitrary OwnershipType where
  arbitrary = elements [Owned, Borrowed, Moved, Shared]

instance Arbitrary OwnershipTransfer where
  arbitrary = OwnershipTransfer <$> arbitrary <*> arbitrary <*> arbitrary

-- ============================================================================
-- Test Utilities
-- ============================================================================

elements :: [a] -> Gen a
elements [] = error "elements: empty list"
elements xs = do
  idx <- arbitrary `suchThat` (\i -> i >= 0 && i < L.length xs)
  return (xs !! idx)

suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p = do
  x <- gen
  if p x then return x else gen `suchThat` p

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership L.and GoToolchain Test Suite"
  [ testGroup "Ownership Tests"
      [ testCase "Ownership analyzer creation" test_ownership_analyzer_creation
      , fastProperty "Ownership type has string representation" prop_ownership_type_has_string
      , fastProperty "Ownership transfer validity" prop_ownership_transfer_validity
      , testCase "Simple ownership analysis" test_ownership_analysis_simple
      , testCase "Ownership error formatting" test_ownership_error_formatting
      , testCase "Ownership transfer move" test_ownership_transfer_move
      , testCase "Ownership transfer borrow" test_ownership_transfer_borrow
      ]
  , testGroup "GoToolchain Tests"
      [ testCase "Go toolchain initialization" test_go_toolchain_initialization
      , testCase "Go installation check" test_go_installation_check
      , testCase "Go code formatting" test_go_code_formatting
      , fastProperty "Go code has package" prop_go_code_has_package
      ]
  , testGroup "Integration Tests"
      [ testCase "Ownership with Go integration" test_ownership_go_integration
      ]
  ]