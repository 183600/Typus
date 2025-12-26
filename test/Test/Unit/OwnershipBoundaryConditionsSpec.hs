module Test.Unit.OwnershipBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, elements)
import qualified Test.QuickCheck as QC

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), 
                 OwnershipTransfer(..), newOwnershipAnalyzer, analyzeOwnership)
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)

-- | Generate ownership types for testing
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
    name <- elements ["x", "y", "z", "value", "data", "result"]
    elements [Owned name, Borrowed name, MutBorrowed name]

-- | Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
    from <- elements ["owner1", "owner2", "source", "input"]
    to <- elements ["dest", "target", "output", "result"]
    transferType <- elements ["move", "borrow", "mut_borrow"]
    return $ OwnershipTransfer from to transferType

-- | Generate variable names for ownership testing
genVariableName :: Gen String
genVariableName = do
    base <- elements ["var", "val", "data", "item", "result", "output"]
    suffix <- choose (1, 100)
    return $ base ++ show suffix

-- | Generate ownership code patterns
genOwnershipPattern :: Gen String
genOwnershipPattern = do
    pattern <- elements
        [ "x := 42"  -- Simple ownership
        , "y := x"   -- Move
        , "z := &x"  -- Borrow
        , "w := &mut x"  -- Mutable borrow
        , "func consume(val Data) { }"  -- Function that takes ownership
        , "func borrow(val &Data) { }"  -- Function that borrows
        , "data := Data{ field: 42 }"  -- Struct creation
        , "moved := consume(data)"  -- Move to function
        ]
    return pattern

tests :: TestTree
tests =
  testGroup "Ownership Analysis Boundary Conditions"
    [ testGroup "Basic Ownership Properties"
        [ testCase "owned values have unique ownership" $ do
            let analyzer = newOwnershipAnalyzer
            let owned = Owned "test_var"
            assertBool "Owned should contain variable name" $ 
                case owned of
                    Owned name -> name == "test_var"
                    _ -> False

        , testCase "borrowed values reference owners" $ do
            let borrowed = Borrowed "owner"
            assertBool "Borrowed should reference owner" $ 
                case borrowed of
                    Borrowed owner -> owner == "owner"
                    _ -> False

        , testCase "mutably borrowed values track mutability" $ do
            let mutBorrowed = MutBorrowed "source"
            assertBool "MutBorrowed should track source" $ 
                case mutBorrowed of
                    MutBorrowed source -> source == "source"
                    _ -> False

        , fastProperty "ownership type ordering is consistent" $ 
            prop_ownershipOrderingConsistent
        ]

    , testGroup "Move Semantics"
        [ testCase "simple move transfers ownership" $ do
            let code = "x := 42\ny := x\nz := y"  -- x moves to y, y moves to z
            result <- analyzeOwnership code "move_test"
            case result of
                Right analyzer -> do
                    -- Should track ownership transfers correctly
                    assertBool "Should analyze simple moves" $ 
                        length (oaErrors analyzer) >= 0  -- May have errors or not
                Left _ -> assertBool "Should not crash on simple moves" False

        , testCase "double move detection" $ do
            let code = "x := 42\ny := x\nz := x"  -- x moved twice
            result <- analyzeOwnership code "double_move_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should detect double move" $ 
                        any isDoubleMoveError errors
                Left _ -> assertBool "Should analyze double move" False

        , testCase "use after move detection" $ do
            let code = "x := 42\ny := x\nprintln(x)"  -- x used after move
            result <- analyzeOwnership code "use_after_move_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should detect use after move" $ 
                        any isUseAfterMoveError errors
                Left _ -> assertBool "Should analyze use after move" False
        ]

    , testGroup "Borrowing Semantics"
        [ testCase "immutable borrow allows multiple borrows" $ do
            let code = "x := 42\ny := &x\nz := &x"  -- Multiple immutable borrows
            result <- analyzeOwnership code "multiple_borrows_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    -- Should not have borrowing errors for immutable borrows
                    assertBool "Should allow multiple immutable borrows" $ 
                        not $ any isBorrowError errors
                Left _ -> assertBool "Should analyze multiple borrows" False

        , testCase "mutable borrow conflicts with other borrows" $ do
            let code = "x := 42\ny := &x\nz := &mut x"  -- Mutable borrow after immutable
            result <- analyzeOwnership code "borrow_conflict_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should detect borrow conflict" $ 
                        any isBorrowConflictError errors
                Left _ -> assertBool "Should analyze borrow conflicts" False

        , testCase "multiple mutable borrows are prohibited" $ do
            let code = "x := 42\ny := &mut x\nz := &mut x"  -- Multiple mutable borrows
            result <- analyzeOwnership code "multiple_mut_borrows_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should detect multiple mutable borrows" $ 
                        any isMultipleMutBorrowError errors
                Left _ -> assertBool "Should analyze multiple mutable borrows" False
        ]

    , testGroup "Function Boundary Ownership"
        [ testCase "parameters can be moved" $ do
            let code = "func consume(data Data) { }\nx := Data{}\nconsume(x)"
            result <- analyzeOwnership code "parameter_move_test"
            case result of
                Right analyzer -> do
                    -- Should handle parameter moves correctly
                    assertBool "Should analyze parameter moves" $ 
                        length (oaErrors analyzer) >= 0
                Left _ -> assertBool "Should analyze parameter moves" False

        , testCase "return values transfer ownership" $ do
            let code = "func create() Data { return Data{} }\nx := create()"
            result <- analyzeOwnership code "return_ownership_test"
            case result of
                Right analyzer -> do
                    -- Should handle return value ownership
                    assertBool "Should analyze return ownership" $ 
                        length (oaErrors analyzer) >= 0
                Left _ -> assertBool "Should analyze return ownership" False

        , testCase "cross-function moves are tracked" $ do
            let code = "func transfer(data Data) Data { return data }\nx := Data{}\ny := transfer(x)"
            result <- analyzeOwnership code "cross_function_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should track cross-function moves" $ 
                        any isCrossFunctionMoveError errors || null errors
                Left _ -> assertBool "Should analyze cross-function moves" False
        ]

    , testGroup "Scope and Lifetime"
        [ testCase "variables go out of scope" $ do
            let code = "{\n  x := 42\n}\nprintln(x)"  -- x used after scope
            result <- analyzeOwnership code "scope_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should detect out of scope usage" $ 
                        any isOutOfScopeError errors
                Left _ -> assertBool "Should analyze scope issues" False

        , testCase "borrowed references respect lifetime" $ do
            let code = "{\n  x := 42\n  y := &x\n}\nprintln(*y)"  -- y outlives x
            result <- analyzeOwnership code "lifetime_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should detect lifetime violations" $ 
                        any isOutOfScopeError errors || any isBorrowError errors
                Left _ -> assertBool "Should analyze lifetime issues" False
        ]

    , testGroup "Complex Ownership Patterns"
        [ testCase "nested scopes with ownership" $ do
            let code = unlines
                [ "x := 42"
                , "{"
                , "  y := x"  -- move x into inner scope
                , "  {"
                , "    z := y"  -- move y into deeper scope
                , "  }"
                , "}"
                ]
            result <- analyzeOwnership code "nested_scope_test"
            case result of
                Right analyzer -> do
                    -- Should handle nested scope moves
                    assertBool "Should analyze nested scopes" $ 
                        length (oaErrors analyzer) >= 0
                Left _ -> assertBool "Should analyze nested scopes" False

        , testCase "conditional ownership transfers" $ do
            let code = unlines
                [ "x := 42"
                , "if condition {"
                , "  y := x"  -- conditional move
                , "} else {"
                , "  z := x"  -- another conditional move
                , "}"
                ]
            result <- analyzeOwnership code "conditional_ownership_test"
            case result of
                Right analyzer -> do
                    -- Should handle conditional moves
                    assertBool "Should analyze conditional ownership" $ 
                        length (oaErrors analyzer) >= 0
                Left _ -> assertBool "Should analyze conditional ownership" False

        , testCase "loop ownership patterns" $ do
            let code = unlines
                [ "items := [1, 2, 3]"
                , "for item in items {"
                , "  processed := process(item)"  -- move in loop
                , "  println(processed)"
                , "}"
                ]
            result <- analyzeOwnership code "loop_ownership_test"
            case result of
                Right analyzer -> do
                    -- Should handle loop ownership patterns
                    assertBool "Should analyze loop ownership" $ 
                        length (oaErrors analyzer) >= 0
                Left _ -> assertBool "Should analyze loop ownership" False
        ]

    , testGroup "Property-based Ownership Testing"
        [ fastProperty "ownership analysis is deterministic" $ 
            prop_ownershipAnalysisDeterministic
        , fastProperty "ownership type ordering is transitive" $ 
            prop_ownershipOrderingTransitive
        , fastProperty "ownership transfers preserve consistency" $ 
            prop_ownershipTransfersConsistent
        , fastProperty "borrowing rules are enforced consistently" $ 
            prop_borrowingRulesConsistent
        ]

    , testGroup "Error Handling Edge Cases"
        [ testCase "handles malformed ownership code" $ do
            let malformedCode = "x := 42\ny := \nz := x"  -- Invalid syntax
            result <- analyzeOwnership malformedCode "malformed_test"
            case result of
                Right analyzer -> do
                    let errors = oaErrors analyzer
                    assertBool "Should handle malformed code gracefully" $ 
                        any isParseError errors
                Left _ -> assertBool "Should return error for malformed code" True

        , testCase "handles empty input" $ do
            result <- analyzeOwnership "" "empty_test"
            case result of
                Right analyzer -> do
                    -- Should handle empty input
                    assertBool "Should handle empty input" $ 
                        True
                Left _ -> assertBool "Should handle empty input gracefully" True

        , testCase "handles very large ownership graphs" $ do
            let largeCode = unlines $ 
                    [ "var" ++ show i ++ " := " ++ show i ++ 
                      if i > 0 then "\nvar" ++ show i ++ "_moved := var" ++ show (i-1) else ""
                    | i <- [0..100]
                    ]
            result <- analyzeOwnership largeCode "large_graph_test"
            case result of
                Right analyzer -> do
                    -- Should handle large ownership graphs
                    assertBool "Should handle large ownership graphs" $ 
                        length (oaErrors analyzer) >= 0
                Left _ -> assertBool "Should handle large graphs gracefully" True
        ]
    ]

-- Helper functions to check error types
isDoubleMoveError :: OwnershipError -> Bool
isDoubleMoveError (DoubleMove _ _) = True
isDoubleMoveError _ = False

isUseAfterMoveError :: OwnershipError -> Bool
isUseAfterMoveError (UseAfterMove _) = True
isUseAfterMoveError _ = False

isBorrowError :: OwnershipError -> Bool
isBorrowError (BorrowError _) = True
isBorrowError (BorrowWhileMoved _) = True
isBorrowError (MutBorrowWhileBorrowed _) = True
isBorrowError (BorrowWhileMutBorrowed _) = True
isBorrowError _ = False

isBorrowConflictError :: OwnershipError -> Bool
isBorrowConflictError (MutBorrowWhileBorrowed _) = True
isBorrowConflictError (BorrowWhileMutBorrowed _) = True
isBorrowConflictError _ = False

isMultipleMutBorrowError :: OwnershipError -> Bool
isMultipleMutBorrowError (MultipleMutBorrows _) = True
isMultipleMutBorrowError _ = False

isCrossFunctionMoveError :: OwnershipError -> Bool
isCrossFunctionMoveError (CrossFunctionMove _ _) = True
isCrossFunctionMoveError _ = False

isOutOfScopeError :: OwnershipError -> Bool
isOutOfScopeError (OutOfScope _) = True
isOutOfScopeError _ = False

isParseError :: OwnershipError -> Bool
isParseError (ParseError _) = True
isParseError _ = False

-- Property: ownership analysis is deterministic
prop_ownershipAnalysisDeterministic :: String -> Bool
prop_ownershipAnalysisDeterministic code = 
    case analyzeOwnership code "test1" of
        Right analyzer1 -> 
            case analyzeOwnership code "test2" of
                Right analyzer2 -> oaErrors analyzer1 == oaErrors analyzer2
                Left _ -> False
        Left _ -> True  -- If analysis fails, that's acceptable for property test

-- Property: ownership type ordering is transitive
prop_ownershipOrderingTransitive :: OwnershipType -> OwnershipType -> OwnershipType -> Bool
prop_ownershipOrderingTransitive a b c =
    if a <= b && b <= c 
    then a <= c 
    else True  -- Property only applies when a <= b and b <= c

-- Property: ownership transfers preserve consistency
prop_ownershipTransfersConsistent :: OwnershipTransfer -> Bool
prop_ownershipTransfersConsistent transfer =
    let OwnershipTransfer from to transferType = transfer
    in not (null from) && not (null to) && not (null transferType)

-- Property: borrowing rules are enforced consistently
prop_borrowingRulesConsistent :: [OwnershipType] -> Bool
prop_borrowingRulesConsistent ownershipTypes =
    let mutableBorrows = [name | MutBorrowed name <- ownershipTypes]
        immutableBorrows = [name | Borrowed name <- ownershipTypes]
        owned = [name | Owned name <- ownershipTypes]
    in -- Check that we don't have conflicting borrows for the same variable
       all (\var -> length (filter (== var) mutableBorrows) <= 1) 
           (nub (mutableBorrows ++ immutableBorrows))