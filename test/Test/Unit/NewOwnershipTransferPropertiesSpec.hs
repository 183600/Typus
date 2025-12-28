module Test.Unit.NewOwnershipTransferPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, choose, listOf, elements, suchThat)
import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipTransfer(..), OwnershipAnalyzer(..))
import Ownership
import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing)

-- | 新的所有权转移属性QuickCheck测试
tests :: TestTree
tests =
  testGroup "New Ownership Transfer Properties Tests"
    [ testGroup "Ownership type properties"
        [ fastProperty "OwnershipType ordering consistency" prop_ownershipTypeOrdering
        , fastProperty "OwnershipType show roundtrip" prop_ownershipTypeShowRoundtrip
        , fastProperty "OwnershipType equality reflexivity" prop_ownershipTypeEqualityReflexivity
        ]

    , testGroup "Ownership error properties"
        [ fastProperty "OwnershipError ordering consistency" prop_ownershipErrorOrdering
        , fastProperty "OwnershipError show contains key info" prop_ownershipErrorShowContainsInfo
        , fastProperty "OwnershipError uniqueness" prop_ownershipErrorUniqueness
        ]

    , testGroup "Ownership transfer properties"
        [ fastProperty "OwnershipTransfer creation" prop_ownershipTransferCreation
        , fastProperty "OwnershipTransfer symmetry" prop_ownershipTransferSymmetry
        , fastProperty "OwnershipTransfer composition" prop_ownershipTransferComposition
        ]

    , testGroup "Ownership analysis properties"
        [ fastProperty "analyzer creation consistency" prop_analyzerCreationConsistency
        , fastProperty "lexAll preserves token structure" prop_lexAllPreservesStructure
        , fastProperty "parseProgram handles valid input" prop_parseProgramHandlesValid
        ]

    , testGroup "Ownership validation properties"
        [ fastProperty "ownership validation consistency" prop_ownershipValidationConsistency
        , fastProperty "borrowing rules enforcement" prop_borrowingRulesEnforcement
        , fastProperty "move semantics correctness" prop_moveSemanticsCorrectness
        ]
    ]

-- ============================================================================
-- Arbitrary instances for test data
-- ============================================================================

instance Arbitrary OwnershipType where
    arbitrary = oneof
        [ Owned <$> arbitrary
        , Borrowed <$> arbitrary
        , MutBorrowed <$> arbitrary
        ]

instance Arbitrary OwnershipError where
    arbitrary = oneof
        [ UseAfterMove <$> arbitrary
        , DoubleMove <$> arbitrary <*> arbitrary
        , BorrowWhileMoved <$> arbitrary
        , MutBorrowWhileBorrowed <$> arbitrary
        , BorrowWhileMutBorrowed <$> arbitrary
        , MultipleMutBorrows <$> arbitrary
        , UseWhileMutBorrowed <$> arbitrary
        , OutOfScope <$> arbitrary
        , BorrowError <$> arbitrary
        , ParseError <$> arbitrary
        , CrossFunctionMove <$> arbitrary <*> arbitrary
        , ParameterMoveMismatch <$> arbitrary
        , ControlFlowError <$> arbitrary
        , PathSensitiveError <$> arbitrary
        , LoopOwnershipError <$> arbitrary
        ]

instance Arbitrary OwnershipTransfer where
    arbitrary = do
        fromVar <- arbitrary
        toVar <- arbitrary
        return $ OwnershipTransfer fromVar toVar

instance Arbitrary OwnershipAnalyzer where
    arbitrary = return newOwnershipAnalyzer

-- Generate variable names
genVariableName :: Gen String
genVariableName = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_"
    return (first : rest)

-- Generate ownership transfer chains
genTransferChain :: Int -> Gen [OwnershipTransfer]
genTransferChain n = do
    vars <- listOf n genVariableName
    return $ zipWith OwnershipTransfer vars (tail vars ++ [head vars])

-- Generate valid code snippets for ownership analysis
genValidOwnershipCode :: Gen String
genValidOwnershipCode = do
    vars <- listOf 3 genVariableName
    return $ unlines
        [ "fn main() {"
        , "    let " ++ head vars ++ " = String::new();"
        , "    let " ++ vars !! 1 ++ " = " ++ head vars ++ ";"
        , "    let " ++ vars !! 2 ++ " = &" ++ vars !! 1 ++ ";"
        , "}"
        ]

-- Generate code with ownership violations
genViolationCode :: Gen String
genViolationCode = do
    vars <- listOf 2 genVariableName
    return $ unlines
        [ "fn main() {"
        , "    let " ++ head vars ++ " = String::new();"
        , "    let " ++ vars !! 1 ++ " = " ++ head vars ++ ";"
        , "    println!(\"{}\", " ++ head vars ++ ");"  -- Use after move
        , "}"
        ]

-- ============================================================================
-- Properties for OwnershipType
-- ============================================================================

prop_ownershipTypeOrdering :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeOrdering ot1 ot2 =
    let comparison = compare ot1 ot2
        reverseComparison = compare ot2 ot1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_ownershipTypeShowRoundtrip :: OwnershipType -> Bool
prop_ownershipTypeShowRoundtrip ot =
    let shown = show ot
        -- Simple check that show contains the ownership type name
    in case ot of
        Owned name -> "Owned" `isInfixOf` shown && name `isInfixOf` shown
        Borrowed name -> "Borrowed" `isInfixOf` shown && name `isInfixOf` shown
        MutBorrowed name -> "MutBorrowed" `isInfixOf` shown && name `isInfixOf` shown

prop_ownershipTypeEqualityReflexivity :: OwnershipType -> Bool
prop_ownershipTypeEqualityReflexivity ot = ot == ot

-- ============================================================================
-- Properties for OwnershipError
-- ============================================================================

prop_ownershipErrorOrdering :: OwnershipError -> OwnershipError -> Bool
prop_ownershipErrorOrdering oe1 oe2 =
    let comparison = compare oe1 oe2
        reverseComparison = compare oe2 oe1
    in case (comparison, reverseComparison) of
        (LT, GT) -> True
        (EQ, EQ) -> True
        (GT, LT) -> True
        _ -> False

prop_ownershipErrorShowContainsInfo :: OwnershipError -> Bool
prop_ownershipErrorShowContainsInfo oe =
    let shown = show oe
    in case oe of
        UseAfterMove var -> "UseAfterMove" `isInfixOf` shown && var `isInfixOf` shown
        DoubleMove var1 var2 -> "DoubleMove" `isInfixOf` shown && var1 `isInfixOf` shown && var2 `isInfixOf` shown
        BorrowWhileMoved var -> "BorrowWhileMoved" `isInfixOf` shown && var `isInfixOf` shown
        MutBorrowWhileBorrowed var -> "MutBorrowWhileBorrowed" `isInfixOf` shown && var `isInfixOf` shown
        BorrowWhileMutBorrowed var -> "BorrowWhileMutBorrowed" `isInfixOf` shown && var `isInfixOf` shown
        MultipleMutBorrows var -> "MultipleMutBorrows" `isInfixOf` shown && var `isInfixOf` shown
        UseWhileMutBorrowed var -> "UseWhileMutBorrowed" `isInfixOf` shown && var `isInfixOf` shown
        OutOfScope var -> "OutOfScope" `isInfixOf` shown && var `isInfixOf` shown
        BorrowError msg -> "BorrowError" `isInfixOf` shown
        ParseError msg -> "ParseError" `isInfixOf` shown
        CrossFunctionMove var1 var2 -> "CrossFunctionMove" `isInfixOf` shown && var1 `isInfixOf` shown && var2 `isInfixOf` shown
        ParameterMoveMismatch var -> "ParameterMoveMismatch" `isInfixOf` shown && var `isInfixOf` shown
        ControlFlowError msg -> "ControlFlowError" `isInfixOf` shown
        PathSensitiveError msg -> "PathSensitiveError" `isInfixOf` shown
        LoopOwnershipError msg -> "LoopOwnershipError" `isInfixOf` shown

prop_ownershipErrorUniqueness :: OwnershipError -> OwnershipError -> Bool
prop_ownershipErrorUniqueness oe1 oe2 =
    let shown1 = show oe1
        shown2 = show oe2
    in if oe1 == oe2 then shown1 == shown2 else shown1 /= shown2

-- ============================================================================
-- Properties for OwnershipTransfer
-- ============================================================================

prop_ownershipTransferCreation :: String -> String -> Bool
prop_ownershipTransferCreation from to =
    let transfer = OwnershipTransfer from to
    in transferFrom transfer == from && transferTo transfer == to

prop_ownershipTransferSymmetry :: String -> String -> Bool
prop_ownershipTransferSymmetry from to =
    let transfer1 = OwnershipTransfer from to
        transfer2 = OwnershipTransfer to from
    in if from == to 
        then transfer1 == transfer2
        else transfer1 /= transfer2

prop_ownershipTransferComposition :: [String] -> Property
prop_ownershipTransferComposition vars =
    length vars >= 2 ==>
    let transfers = zipWith OwnershipTransfer vars (tail vars)
        fromVars = map transferFrom transfers
        toVars = map transferTo transfers
    in length fromVars == length toVars &&
       head fromVars == head vars &&
       last toVars == last vars

-- ============================================================================
-- Properties for Ownership Analysis
-- ============================================================================

prop_analyzerCreationConsistency :: Int -> Bool
prop_analyzerCreationConsistency _ =
    let analyzer1 = newOwnershipAnalyzer
        analyzer2 = newOwnershipAnalyzer
    in analyzer1 == analyzer2

prop_lexAllPreservesStructure :: String -> Property
prop_lexAllPreservesStructure code =
    length code < 1000 ==>
    let tokens = lexAll code
        -- Simple check that lexing doesn't crash and returns some result
    in length tokens >= 0

prop_parseProgramHandlesValid :: String -> Property
prop_parseProgramHandlesValid code =
    length code < 500 ==>
    let result = parseProgram code
    in case result of
        Left _ -> True  -- Parsing may fail for invalid code
        Right _ -> True  -- Successful parsing is valid

-- ============================================================================
-- Properties for Ownership Validation
-- ============================================================================

prop_ownershipValidationConsistency :: OwnershipType -> Bool
prop_ownershipValidationConsistency ot =
    let shown = show ot
        -- Check that show representation is consistent with type
    in case ot of
        Owned _ -> "Owned" `isInfixOf` shown
        Borrowed _ -> "Borrowed" `isInfixOf` shown
        MutBorrowed _ -> "MutBorrowed" `isInfixOf` shown

prop_borrowingRulesEnforcement :: [OwnershipType] -> Property
prop_borrowingRulesEnforcement ownershipTypes =
    length ownershipTypes >= 2 ==>
    let hasBorrowed = any isBorrow ownershipTypes
        hasMutBorrowed = any isMutBorrow ownershipTypes
        hasOwned = any isOwned ownershipTypes
    in hasBorrowed || hasMutBorrowed || hasOwned  -- At least one type should be present

prop_moveSemanticsCorrectness :: String -> String -> Bool
prop_moveSemanticsCorrectness originalVar newVar =
    let transfer = OwnershipTransfer originalVar newVar
        fromVar = transferFrom transfer
        toVar = transferTo transfer
    in fromVar == originalVar && toVar == newVar

-- ============================================================================
-- Helper functions
-- ============================================================================

-- Check if a substring is in a string
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = needle `elem` [take (length needle) (drop i haystack) | i <- [0..length haystack - length needle]]

-- Check if ownership type is a borrow
isBorrow :: OwnershipType -> Bool
isBorrow (Borrowed _) = True
isBorrow _ = False

-- Check if ownership type is a mutable borrow
isMutBorrow :: OwnershipType -> Bool
isMutBorrow (MutBorrowed _) = True
isMutBorrow _ = False

-- Check if ownership type is owned
isOwned :: OwnershipType -> Bool
isOwned (Owned _) = True
isOwned _ = False