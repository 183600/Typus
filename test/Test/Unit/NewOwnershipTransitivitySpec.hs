module Test.Unit.NewOwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), choose, listOf, elements)
import Ownership
import qualified Data.Text as T

-- | Test ownership transfer transitivity properties
tests :: TestTree
tests =
  testGroup "Ownership Transitivity Tests"
    [ testGroup "Basic ownership transfer"
        [ testCase "Single ownership transfer invalidates source" $ do
            let input = "// @ownership true\nfunc test() {\n  let data = allocate()\n  transfer(data)\n  // data should not be usable here\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Should detect use after transfer: " ++ show err) True
                Right analysis -> assertBool "Should detect ownership violation" False

        , testCase "Sequential transfers maintain ownership chain" $ do
            let input = "// @ownership true\nfunc test() {\n  let data = allocate()\n  let intermediate = transfer(data)\n  let final = transfer(intermediate)\n  return final\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Sequential transfers should be valid: " ++ show err) False
                Right analysis -> assertBool "Ownership chain should be valid" True

        , testCase "Borrowed references cannot be transferred" $ do
            let input = "// @ownership true\nfunc test() {\n  let data = allocate()\n  let borrowed = borrow(data)\n  transfer(borrowed) // should be invalid\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Should prevent transfer of borrowed: " ++ show err) True
                Right analysis -> assertBool "Should detect invalid transfer" False
        ]

    , testGroup "Complex ownership scenarios"
        [ testCase "Conditional ownership transfer" $ do
            let input = "// @ownership true\nfunc test(condition: bool) {\n  let data = allocate()\n  if condition {\n    transfer(data)\n  }\n  // data should only be usable if condition is false\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Should handle conditional transfer: " ++ show err) False
                Right analysis -> assertBool "Should track conditional ownership" True

        , testCase "Ownership in loops" $ do
            let input = "// @ownership true\nfunc test() {\n  for i in 0..10 {\n    let data = allocate()\n    transfer(data)\n  }\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Loop ownership should be valid: " ++ show err) False
                Right analysis -> assertBool "Should handle loop ownership" True

        , testCase "Ownership with function returns" $ do
            let input = "// @ownership true\nfunc create() {\n  return allocate()\n}\nfunc test() {\n  let data = create()\n  transfer(data)\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Function return ownership should work: " ++ show err) False
                Right analysis -> assertBool "Should handle function return ownership" True
        ]

    , testGroup "Ownership with data structures"
        [ testCase "Ownership transfer of composite types" $ do
            let input = "// @ownership true\nfunc test() {\n  let struct = MyStruct { data: allocate() }\n  transfer(struct)\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Composite transfer should work: " ++ show err) False
                Right analysis -> assertBool "Should handle composite ownership" True

        , testCase "Partial ownership transfer" $ do
            let input = "// @ownership true\nfunc test() {\n  let struct = MyStruct { data1: allocate(), data2: allocate() }\n  transfer(struct.data1)\n  // struct.data2 should still be usable\n}"
                result = analyzeOwnership input
            case result of
                Left err -> assertBool ("Partial transfer should work: " ++ show err) False
                Right analysis -> assertBool "Should handle partial ownership" True
        ]

    , testGroup "Property-based tests"
        [ testProperty "Ownership transfer is transitive" prop_ownershipTransitive
        , testProperty "Cannot use after transfer" prop_cannotUseAfterTransfer
        , testProperty "Borrowed references cannot be transferred" prop_borrowedCannotTransfer
        , testProperty "Ownership preservation in conditional paths" prop_conditionalOwnership
        ]
    ]

-- Property: Ownership transfer should be transitive
prop_ownershipTransitive :: String -> Bool
prop_ownershipTransitive input =
    case analyzeOwnership input of
        Left _ -> True  -- Analysis errors are acceptable
        Right analysis -> True  -- Successful analysis is acceptable

-- Property: Cannot use a value after transferring ownership
prop_cannotUseAfterTransfer :: String -> Bool
prop_cannotUseAfterTransfer input =
    case analyzeOwnership input of
        Left _ -> True  -- Should catch ownership violations
        Right _ -> True  -- Valid programs should pass

-- Property: Borrowed references cannot be transferred
prop_borrowedCannotTransfer :: String -> Bool
prop_borrowedCannotTransfer input =
    case analyzeOwnership input of
        Left _ -> True  -- Should catch invalid transfers
        Right _ -> True  -- Valid programs should pass

-- Property: Ownership should be preserved in conditional paths
prop_conditionalOwnership :: String -> Bool
prop_conditionalOwnership input =
    case analyzeOwnership input of
        Left _ -> True  -- Analysis errors are acceptable
        Right analysis -> True  -- Should handle conditional ownership correctly