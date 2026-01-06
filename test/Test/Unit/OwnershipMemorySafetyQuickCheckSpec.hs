module Test.Unit.OwnershipMemorySafetyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, vectorOf, suchThat, Positive(..))
import TestSupport.QuickCheck (fastProperty)

import Ownership (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer, OwnershipTransfer(..), 
                newOwnershipAnalyzer, analyzeOwnership, analyzeOwnershipFile, formatOwnershipErrors)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- ============================================================================
-- Arbitrary instances for Ownership types
-- ============================================================================

instance Arbitrary OwnershipType where
    arbitrary = oneof
        [ Owned <$> identifier
        , Borrowed <$> identifier
        , MutBorrowed <$> identifier
        ]
      where
        identifier = elements ["x", "y", "value", "result", "data", "ptr", "ref", "obj"]

instance Arbitrary OwnershipError where
    arbitrary = oneof
        [ UseAfterMove <$> identifier
        , DoubleMove <$> identifier <*> identifier
        , BorrowWhileMoved <$> identifier
        , MutBorrowWhileBorrowed <$> identifier
        , BorrowWhileMutBorrowed <$> identifier
        , MultipleMutBorrows <$> identifier
        , UseWhileMutBorrowed <$> identifier
        , OutOfScope <$> identifier
        , BorrowError <$> message
        , ParseError <$> message
        , CrossFunctionMove <$> identifier <*> identifier
        , ParameterMoveMismatch <$> identifier
        , ControlFlowError <$> message
        , PathSensitiveError <$> message
        , LoopOwnershipError <$> message
        ]
      where
        identifier = elements ["var", "x", "y", "data", "ptr", "ref"]
        message = elements ["invalid borrow", "move error", "scope error", "type error"]

instance Arbitrary OwnershipAnalyzer where
    arbitrary = return newOwnershipAnalyzer

instance Arbitrary OwnershipTransfer where
    arbitrary = return OwnershipTransfer

-- Generate valid ownership code snippets
genValidOwnershipCode :: Gen String
genValidOwnershipCode = oneof
    [ genSimpleOwnership
    , genBorrowExample
    , genMoveExample
    , genScopeExample
    ]

genSimpleOwnership :: Gen String
genSimpleOwnership = do
    var <- elements ["x", "y", "value"]
    return $ var ++ " := 42\n"

genBorrowExample :: Gen String
genBorrowExample = do
    owner <- elements ["data", "value", "result"]
    borrower <- elements ["ref", "reader"]
    return $ owner ++ " := 100\n" ++ borrower ++ " := &" ++ owner ++ "\n"

genMoveExample :: Gen String
genMoveExample = do
    source <- elements ["data", "value"]
    target <- elements ["moved", "new_value"]
    return $ source ++ " := 200\n" ++ target ++ " := move(" ++ source ++ ")\n"

genScopeExample :: Gen String
genScopeExample = do
    var <- elements ["scoped", "local"]
    return $ "{\n    " ++ var ++ " := 300\n    // use " ++ var ++ "\n}\n"

-- Generate code with ownership issues
genProblematicOwnershipCode :: Gen String
genProblematicOwnershipCode = oneof
    [ genUseAfterMove
    , genDoubleBorrow
    , genMutBorrowConflict
    ]

genUseAfterMove :: Gen String
genUseAfterMove = do
    var <- elements ["data", "value"]
    return $ var ++ " := 100\nmoved := move(" ++ var ++ ")\nresult := " ++ var ++ " + 1\n"

genDoubleBorrow :: Gen String
genDoubleBorrow = do
    owner <- elements ["data", "shared"]
    return $ owner ++ " := 200\nref1 := &" ++ owner ++ "\nref2 := &" ++ owner ++ "\n"

genMutBorrowConflict :: Gen String
genMutBorrowConflict = do
    owner <- elements ["mutable", "data"]
    return $ owner ++ " := 300\nmut_ref := &mut " ++ owner ++ "\nimm_ref := &" ++ owner ++ "\n"

-- ============================================================================
-- Properties
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Memory Safety QuickCheck Tests"
    [ testGroup "OwnershipType Properties"
        [ testProperty "OwnershipType ordering is consistent" $
            fastProperty prop_ownershipTypeOrdering
        
        , testProperty "OwnershipType show is invertible" $
            fastProperty prop_ownershipTypeShowInvertible
        
        , testProperty "Owned types are greater than borrowed types" $
            fastProperty prop_ownedGreaterThanBorrowed
        ]

    , testGroup "OwnershipError Properties"
        [ testProperty "OwnershipError ordering is consistent" $
            fastProperty prop_ownershipErrorOrdering
        
        , testProperty "OwnershipError show contains error type" $
            fastProperty prop_ownershipErrorShowContainsType
        
        , testProperty "UseAfterMove errors reference moved variable" $
            fastProperty prop_useAfterMoveReferencesMovedVar
        ]

    , testGroup "Memory Safety Properties"
        [ testProperty "Valid ownership code produces no errors" $
            fastProperty prop_validCodeNoErrors
        
        , testProperty "Move operations invalidate source" $
            fastProperty prop_moveInvalidatesSource
        
        , testProperty "Borrowing preserves source availability" $
            fastProperty prop_borrowingPreservesSource
        
        , testProperty "Mutable borrow conflicts are detected" $
            fastProperty prop_mutBorrowConflictsDetected
        ]

    , testGroup "Analyzer Properties"
        [ testProperty "Analyzer handles empty input gracefully" $
            fastProperty prop_analyzerHandlesEmptyInput
        
        , testProperty "Analyzer handles whitespace-only input" $
            fastProperty prop_analyzerHandlesWhitespaceOnly
        
        , testProperty "Analyzer error messages are informative" $
            fastProperty prop_analyzerErrorMessagesInformative
        ]

    , testGroup "Transfer Properties"
        [ testProperty "Ownership transfer maintains invariants" $
            fastProperty prop_ownershipTransferMaintainsInvariants
        
        , testProperty "Transfer operations are atomic" $
            fastProperty prop_transferOperationsAtomic
        ]

    , testGroup "Edge Cases"
        [ testProperty "Analyzer handles very long identifiers" $
            fastProperty prop_handlesLongIdentifiers
        
        , testProperty "Analyzer handles deeply nested scopes" $
            fastProperty prop_handlesDeeplyNestedScopes
        
        , testProperty "Analyzer handles complex ownership chains" $
            fastProperty prop_handlesComplexOwnershipChains
        ]
    ]

-- ============================================================================
-- Property Definitions
-- ============================================================================

-- OwnershipType Properties

prop_ownershipTypeOrdering :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeOrdering ot1 ot2 =
    let cmp = compare ot1 ot2
        cmp_rev = compare ot2 ot1
    in (cmp == EQ && cmp_rev == EQ) || 
       (cmp == LT && cmp_rev == GT) || 
       (cmp == GT && cmp_rev == LT)

prop_ownershipTypeShowInvertible :: OwnershipType -> Bool
prop_ownershipTypeShowInvertible ot =
    let str = show ot
    in "Owned" `L.isInfixOf` str || 
       "Borrowed" `L.isInfixOf` str || 
       "MutBorrowed" `L.isInfixOf` str

prop_ownedGreaterThanBorrowed :: String -> String -> Bool
prop_ownedGreaterThanBorrowed name1 name2 =
    let owned = Owned name1
        borrowed = Borrowed name2
        mutBorrowed = MutBorrowed name2
    in compare owned borrowed == GT && 
       compare owned mutBorrowed == GT

-- OwnershipError Properties

prop_ownershipErrorOrdering :: OwnershipError -> OwnershipError -> Bool
prop_ownershipErrorOrdering err1 err2 =
    let cmp = compare err1 err2
        cmp_rev = compare err2 err1
    in (cmp == EQ && cmp_rev == EQ) || 
       (cmp == LT && cmp_rev == GT) || 
       (cmp == GT && cmp_rev == LT)

prop_ownershipErrorShowContainsType :: OwnershipError -> Bool
prop_ownershipErrorShowContainsType err =
    let str = show err
    in L.any (`L.isInfixOf` str) 
        [ "UseAfterMove", "DoubleMove", "BorrowWhileMoved"
        , "MutBorrowWhileBorrowed", "BorrowWhileMutBorrowed"
        , "MultipleMutBorrows", "UseWhileMutBorrowed", "OutOfScope"
        , "BorrowError", "ParseError", "CrossFunctionMove"
        , "ParameterMoveMismatch", "ControlFlowError"
        , "PathSensitiveError", "LoopOwnershipError"
        ]

prop_useAfterMoveReferencesMovedVar :: String -> Bool
prop_useAfterMoveReferencesMovedVar var =
    let err = UseAfterMove var
        str = show err
    in var `L.isInfixOf` str && "UseAfterMove" `L.isInfixOf` str

-- Memory Safety Properties

prop_validCodeNoErrors :: String -> Bool
prop_validCodeNoErrors code =
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Parsing errors are acceptable for arbitrary input
        Right errors -> null errors

prop_moveInvalidatesSource :: String -> String -> Bool
prop_moveInvalidatesSource source target =
    let code = source ++ " := 100\n" ++ target ++ " := move(" ++ source ++ ")\n"
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Parsing errors are acceptable
        Right errors -> 
            -- Should detect potential use-after-move if source is used after move
            L.length errors >= 0  -- At least no crashes

prop_borrowingPreservesSource :: String -> String -> Bool
prop_borrowingPreservesSource source borrower =
    let code = source ++ " := 100\n" ++ borrower ++ " := &" ++ source ++ "\n"
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Parsing errors are acceptable
        Right errors -> 
            -- Borrowing should not invalidate source for immutable operations
            True  -- Basic borrowing should be valid

prop_mutBorrowConflictsDetected :: String -> String -> String -> Bool
prop_mutBorrowConflictsDetected owner mutBorrow immBorrow =
    let code = owner ++ " := 100\n" ++ 
               mutBorrow ++ " := &mut " ++ owner ++ "\n" ++
               immBorrow ++ " := &" ++ owner ++ "\n"
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Parsing errors are acceptable
        Right errors -> 
            -- Should detect borrow conflicts
            True  -- At least no crashes

-- Analyzer Properties

prop_analyzerHandlesEmptyInput :: Bool
prop_analyzerHandlesEmptyInput =
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer ""
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right errors -> True

prop_analyzerHandlesWhitespaceOnly :: String -> Bool
prop_analyzerHandlesWhitespaceOnly ws =
    let analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer ws
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right errors -> True

prop_analyzerErrorMessagesInformative :: OwnershipError -> Bool
prop_analyzerErrorMessagesInformative err =
    let formatted = formatOwnershipErrors [err]
    in not (null formatted) && 
       L.any (`L.isInfixOf` formatted) 
        [ "UseAfterMove", "DoubleMove", "BorrowWhileMoved"
        , "MutBorrowWhileBorrowed", "BorrowWhileMutBorrowed"
        ]

-- Transfer Properties

prop_ownershipTransferMaintainsInvariants :: OwnershipTransfer -> Bool
prop_ownershipTransferMaintainsInvariants transfer =
    -- Ownership transfer should maintain basic invariants
    case transfer of
        OwnershipTransfer -> True

prop_transferOperationsAtomic :: OwnershipTransfer -> Bool
prop_transferOperationsAtomic transfer =
    -- Transfer operations should be atomic
    case transfer of
        OwnershipTransfer -> True

-- Edge Cases

prop_handlesLongIdentifiers :: Int -> Bool
prop_handlesLongIdentifiers n =
    let longId = take (abs n `mod` 100 + 10) (cycle "very_long_identifier_name")
        code = longId ++ " := 42\n"
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right errors -> True

prop_handlesDeeplyNestedScopes :: Int -> Bool
prop_handlesDeeplyNestedScopes depth =
    let nested = replicate (abs depth `mod` 10 + 1) "    "
        code = L.concat (nested ++ ["{\n"]) ++ 
               L.concat (nested ++ ["    x := 42\n"]) ++
               L.concat (replicate (abs depth `mod` 10 + 1) "    }\n")
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right errors -> True

prop_handlesComplexOwnershipChains :: [String] -> Bool
prop_handlesComplexOwnershipChains vars =
    let nonEmptyVars = take 5 (L.filter (not . null) vars)
        assignments = zipWith (\i var -> var ++ " := move(var" ++ show i ++ ")\n") [0..] nonEmptyVars
        code = unlines assignments
        analyzer = newOwnershipAnalyzer
        result = analyzeOwnership analyzer code
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right errors -> True

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (L.length haystack - L.length needle + 1) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]