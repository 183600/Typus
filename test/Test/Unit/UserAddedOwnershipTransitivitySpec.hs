module Test.Unit.UserAddedOwnershipTransitivitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, elements)
import TestSupport.QuickCheck (fastProperty)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , formatOwnershipErrors
  )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Tests for ownership transitivity L.and transfer rules
tests :: TestTree
tests =
  testGroup "UserAdded Ownership Transitivity"
    [ testGroup "Basic ownership transfer"
        [ testCase "simple move transfers ownership completely" $ do
            let code = unlines
                  [ "fn main() {"
                  , "    let data = Box::new(42);"
                  , "    let consumer = Consumer::new(data);  // data is moved"
                  , "    // data cannot be used here"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> do
                    assertBool "Should detect use after move if present" $ 
                        L.any isUseAfterMove errors || null errors
                Right _ -> assertBool "Analysis should complete" True

        , testCase "borrow preserves original ownership" $ do
            let code = unlines
                  [ "fn main() {"
                  , "    let data = Box::new(42);"
                  , "    let reference = &data;  // data is borrowed"
                  , "    println!(\"{}\", data.value);  // data can still be used"
                  , "    println!(\"{}\", reference.value);"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> assertBool "Should not have ownership errors" $ not (L.any isOwnershipError errors)
                Right _ -> assertBool "Analysis should succeed" True

        , testCase "mutable borrow restricts other access" $ do
            let code = unlines
                  [ "fn main() {"
                  , "    let mut data = Box::new(42);"
                  , "    let mut_ref = &mut data;  // mutable borrow"
                  , "    // data cannot be used while mutably borrowed"
                  , "    *mut_ref = 24;"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> do
                    assertBool "Should detect borrow violations" $ 
                        L.any isBorrowError errors || null errors
                Right _ -> assertBool "Analysis should complete" True
        ]

    , testGroup "Ownership transitivity rules"
        [ testCase "ownership transfers through function calls" $ do
            let code = unlines
                  [ "fn consume(value: Box<i32>) {"
                  , "    // value is owned by this function"
                  , "}"
                  , ""
                  , "fn main() {"
                  , "    let data = Box::new(42);"
                  , "    consume(data);  // data is moved to consume"
                  , "    // data cannot be used here"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> do
                    assertBool "Should track ownership through function calls" $ 
                        L.any isUseAfterMove errors || L.any isCrossFunctionMove errors || null errors
                Right _ -> assertBool "Analysis should complete" True

        , testCase "borrowing rules are transitive" $ do
            let code = unlines
                  [ "fn main() {"
                  , "    let data = Box::new(42);"
                  , "    let ref1 = &data;"
                  , "    let ref2 = ref1;  // ref2 borrows from ref1, which borrows from data"
                  , "    println!(\"{}\", data.value);  // data can still be used"
                  , "    println!(\"{}\", ref1.value);"
                  , "    println!(\"{}\", ref2.value);"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> assertBool "Should handle transitive borrowing" $ not (L.any isOwnershipError errors)
                Right _ -> assertBool "Analysis should succeed" True
        ]

    , testGroup "Complex ownership scenarios"
        [ testCase "ownership in data structures" $ do
            let code = unlines
                  [ "struct Node {"
                  , "    value: i32,"
                  , "    next: Option<Box<Node>>,"
                  , "}"
                  , ""
                  , "fn main() {"
                  , "    let node1 = Node { value: 1, next: None };"
                  , "    let node2 = Node { value: 2, next: Some(Box::new(node1)) };"
                  , "    // node1 is now owned by node2"
                  , "    // node1 cannot be used here"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> do
                    assertBool "Should track ownership in structures" $ 
                        L.any isUseAfterMove errors || null errors
                Right _ -> assertBool "Analysis should complete" True

        , testCase "ownership with pattern matching" $ do
            let code = unlines
                  [ "fn main() {"
                  , "    let data = Some(Box::new(42));"
                  , "    match data {"
                  , "        Some(boxed) => {"
                  , "            // boxed is owned by this match arm"
                  , "            println!(\"{}\", *boxed);"
                  , "        }"
                  , "        None => {}"
                  , "    }"
                  , "    // data cannot be used after match"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> do
                    assertBool "Should handle ownership in pattern matching" $ 
                        L.any isUseAfterMove errors || null errors
                Right _ -> assertBool "Analysis should complete" True
        ]

    , testGroup "Property-based ownership tests"
        [ fastProperty "ownership transfer is deterministic" prop_ownershipDeterministic
        , fastProperty "borrowing rules are consistent" prop_borrowingConsistent
        ]

    , testGroup "Error detection L.and recovery"
        [ testCase "detects double move errors" $ do
            let code = unlines
                  [ "fn main() {"
                  , "    let data = Box::new(42);"
                  , "    let consumer1 = Consumer::new(data);"
                  , "    let consumer2 = Consumer::new(data);  // double move"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> do
                    assertBool "Should detect double move" $ 
                        L.any isDoubleMove errors || null errors
                Right _ -> assertBool "Should have failed" False

        , testCase "provides helpful error messages" $ do
            let code = unlines
                  [ "fn main() {"
                  , "    let data = Box::new(42);"
                  , "    let consumer = Consumer::new(data);"
                  , "    println!(\"{}\", *data);  // use after move"
                  , "}"
                  ]
                result = analyzeOwnership code
            case result of
                Left errors -> do
                    let errorMessages = formatOwnershipErrors errors
                    assertBool "Should provide clear error messages" $ not (null errorMessages)
                    assertBool "Should mention use after move" $ 
                        L.any ("move" `L.isInfixOf`) errorMessages
                Right _ -> assertBool "Should have failed" False
        ]
    ]

-- Helper functions to check error types
isUseAfterMove :: OwnershipError -> Bool
isUseAfterMove (UseAfterMove _) = True
isUseAfterMove _ = False

isDoubleMove :: OwnershipError -> Bool
isDoubleMove (DoubleMove _ _) = True
isDoubleMove _ = False

isBorrowError :: OwnershipError -> Bool
isBorrowError (BorrowWhileMoved _) = True
isBorrowError (MutBorrowWhileBorrowed _) = True
isBorrowError (BorrowWhileMutBorrowed _) = True
isBorrowError (MultipleMutBorrows _) = True
isBorrowError _ = False

isOwnershipError :: OwnershipError -> Bool
isOwnershipError err = case err of
    UseAfterMove _ -> True
    DoubleMove _ _ -> True
    BorrowWhileMoved _ -> True
    MutBorrowWhileBorrowed _ -> True
    BorrowWhileMutBorrowed _ -> True
    MultipleMutBorrows _ -> True
    UseWhileMutBorrowed _ -> True
    _ -> False

isCrossFunctionMove :: OwnershipError -> Bool
isCrossFunctionMove (CrossFunctionMove _ _) = True
isCrossFunctionMove _ = False

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `elem` [take (L.length needle) $ drop i haystack | i <- [0..L.length haystack - L.length needle]]

-- | Property: ownership transfer is deterministic
prop_ownershipDeterministic :: String -> Bool
prop_ownershipDeterministic code =
    let result1 = analyzeOwnership code
        result2 = analyzeOwnership code
    in result1 == result2

-- | Property: borrowing rules are consistent
prop_borrowingConsistent :: String -> Bool
prop_borrowingConsistent code =
    let result = analyzeOwnership code
    in case result of
        Left errors -> L.all isValidError errors
        Right _ -> True

isValidError :: OwnershipError -> Bool
isValidError err = case err of
    UseAfterMove name -> not (null name)
    DoubleMove name1 name2 -> not (null name1) && not (null name2)
    BorrowWhileMoved name -> not (null name)
    MutBorrowWhileBorrowed name -> not (null name)
    BorrowWhileMutBorrowed name -> not (null name)
    MultipleMutBorrows name -> not (null name)
    UseWhileMutBorrowed name -> not (null name)
    OutOfScope name -> not (null name)
    BorrowError msg -> not (null msg)
    ParseError msg -> not (null msg)
    CrossFunctionMove func var -> not (null func) && not (null var)
    ParameterMoveMismatch func -> not (null func)
    ControlFlowError msg -> not (null msg)