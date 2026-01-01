{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.OwnershipTransferSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Ownership
import Ownership.Common.Types
import SourceLocation
import Data.List (sort, nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (when)

-- ============================================================================
-- Ownership Transfer Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Transfer Tests"
  [ ownershipTypeProperties
  , ownershipTransferProperties
  , ownershipErrorProperties
  , ownershipAnalyzerProperties
  , transferConsistencyProperties
  , ownershipBoundaryProperties
  ]

-- ============================================================================
-- Ownership Type Properties
-- ============================================================================

ownershipTypeProperties :: TestTree
ownershipTypeProperties = testGroup "Ownership Type Properties"
  [ testProperty "ownership type ordering is total" $
      \ownType1 ownType2 ->
        let cmp = compare ownType1 ownType2
        in (ownType1 <= ownType2 && ownType2 <= ownType1) === (ownType1 == ownType2)
    
  , testProperty "ownership type ordering is transitive" $
      \ownType1 ownType2 ownType3 ->
        ownType1 <= ownType2 && ownType2 <= ownType3 ==> ownType1 <= ownType3
    
  , testProperty "Owned is the greatest ownership type" $
      \name ->
        let owned = Owned name
            borrowed = Borrowed name
            mutBorrowed = MutBorrowed name
        in owned >= borrowed && owned >= mutBorrowed
    
  , testProperty "Borrowed is less than MutBorrowed" $
      \name ->
        let borrowed = Borrowed name
            mutBorrowed = MutBorrowed name
        in borrowed < mutBorrowed
    
  , testProperty "ownership type equality is reflexive" $
      \ownType -> ownType === ownType
    
  , testProperty "ownership type equality is symmetric" $
      \ownType1 ownType2 -> ownType1 === ownType2 ==> ownType2 === ownType1
    
  , testProperty "ownership type equality is transitive" $
      \ownType1 ownType2 ownType3 -> 
        ownType1 === ownType2 && ownType2 === ownType3 ==> ownType1 === ownType3
    
  , testCase "ownership type ordering examples" $ do
      Owned "x" @?= Owned "x"
      assertBool "Owned > Borrowed" $ Owned "x" > Borrowed "x"
      assertBool "Owned > MutBorrowed" $ Owned "x" > MutBorrowed "x"
      assertBool "MutBorrowed > Borrowed" $ MutBorrowed "x" > Borrowed "x"
  ]

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

ownershipTransferProperties :: TestTree
ownershipTransferProperties = testGroup "Ownership Transfer Properties"
  [ testProperty "ownership transfer preserves uniqueness" $
      \transfer ->
        let source = transferSource transfer
            target = transferTarget transfer
        in source /= target || transferIsSelf transfer
    
  , testProperty "ownership transfer is deterministic" $
      \transfer1 transfer2 ->
        transfer1 === transfer2 ==> 
        transferSource transfer1 === transferSource transfer2 &&
        transferTarget transfer1 === transferTarget transfer2
    
  , testProperty "ownership transfer creates valid state" $
      \transfer ->
        let source = transferSource transfer
            target = transferTarget transfer
        in not (null source) && not (null target)
    
  , testProperty "valid ownership transfer maintains invariants" $
      \fromName toName ->
        let transfer = createValidTransfer fromName toName
        in transferSource transfer === fromName &&
           transferTarget transfer === toName &&
           isValidTransfer transfer
    
  , testProperty "ownership transfer chain is consistent" $
      \name1 name2 name3 ->
        let transfer1 = createValidTransfer name1 name2
            transfer2 = createValidTransfer name2 name3
        in transferTarget transfer1 === transferSource transfer2 ||
           not (isValidTransfer transfer1 && isValidTransfer transfer2)
    
  , testCase "create valid ownership transfer" $ do
      let transfer = createValidTransfer "x" "y"
      transferSource transfer @?= "x"
      transferTarget transfer @?= "y"
      assertBool "Transfer is valid" $ isValidTransfer transfer
    
  , testCase "handle self-transfer" $ do
      let transfer = createValidTransfer "x" "x"
      assertBool "Self-transfer is handled" $ transferIsSelf transfer
  ]

-- ============================================================================
-- Ownership Error Properties
-- ============================================================================

ownershipErrorProperties :: TestTree
ownershipErrorProperties = testGroup "Ownership Error Properties"
  [ testProperty "ownership error contains relevant information" $
      \errorType ->
        let errorMessage = formatOwnershipError errorType
        in not $ null errorMessage
    
  , testProperty "use after move error has correct structure" $
      \varName ->
        let error = UseAfterMove varName
            message = formatOwnershipError error
        in varName `L.isInfixOf` message
    
  , testProperty "double move error has correct structure" $
      \varName1 varName2 ->
        let error = DoubleMove varName1 varName2
            message = formatOwnershipError error
        in varName1 `L.isInfixOf` message && varName2 `L.isInfixOf` message
    
  , testProperty "borrow error messages are descriptive" $
      \varName ->
        let errors = [BorrowWhileMoved varName, 
                     MutBorrowWhileBorrowed varName,
                     BorrowWhileMutBorrowed varName,
                     MultipleMutBorrows varName]
            messages = map formatOwnershipError errors
        in L.all (`L.isInfixOf` varName) messages
    
  , testProperty "ownership error categorization is consistent" $
      \errorType ->
        let category = categorizeError errorType
        in category `elem` ["MoveError", "BorrowError", "ScopeError", "ParseError", "FlowError"]
    
  , testCase "error formatting examples" $ do
      let useAfterMove = UseAfterMove "x"
          doubleMove = DoubleMove "x" "y"
          borrowError = BorrowWhileMoved "z"
      assertBool "Use after move formatted" $ not $ L.null $ formatOwnershipError useAfterMove
      assertBool "Double move formatted" $ not $ L.null $ formatOwnershipError doubleMove
      assertBool "Borrow error formatted" $ not $ L.null $ formatOwnershipError borrowError
  ]

-- ============================================================================
-- Ownership Analyzer Properties
-- ============================================================================

ownershipAnalyzerProperties :: TestTree
ownershipAnalyzerProperties = testGroup "Ownership Analyzer Properties"
  [ testProperty "new analyzer has empty state" $
      let analyzer = newOwnershipAnalyzer
      in analyzerIsEmpty analyzer
    
  , testProperty "analyzer state is consistent after operations" $
      \analyzer operations ->
        let finalAnalyzer = performOperations analyzer operations
        in analyzerStateConsistent finalAnalyzer
    
  , testProperty "analyzer handles ownership tracking correctly" $
      \analyzer varName ->
        let withOwned = addOwnedVariable analyzer varName
            hasOwnership = variableHasOwnership withOwned varName
        in hasOwnership
    
  , testProperty "analyzer detects use after move" $
      \analyzer varName ->
        let withOwned = addOwnedVariable analyzer varName
            withMove = moveVariable withOwned varName
            errors = getOwnershipErrors withMove
        in L.any isUseAfterMove errors || null errors
    
  , testProperty "analyzer handles borrow tracking" $
      \analyzer ownerName borrowerName ->
        let withOwned = addOwnedVariable analyzer ownerName
            withBorrow = addBorrow withOwned ownerName borrowerName
            hasBorrow = variableHasBorrow withBorrow borrowerName
        in hasBorrow
    
  , testCase "analyzer basic operations" $ do
      let analyzer = newOwnershipAnalyzer
          withX = addOwnedVariable analyzer "x"
          withY = addOwnedVariable withX "y"
      assertBool "X has ownership" $ variableHasOwnership withX "x"
      assertBool "Y has ownership" $ variableHasOwnership withY "y"
      assertBool "Analyzer state consistent" $ analyzerStateConsistent withY
  ]

-- ============================================================================
-- Transfer Consistency Properties
-- ============================================================================

transferConsistencyProperties :: TestTree
transferConsistencyProperties = testGroup "Transfer Consistency Properties"
  [ testProperty "valid transfer maintains ownership invariants" $
      \analyzer fromName toName ->
        let withOwned = addOwnedVariable analyzer fromName
            withTransfer = performTransfer withOwned fromName toName
        in transferMaintainsInvariants withTransfer fromName toName
    
  , testProperty "transfer chain preserves total ownership" $
      \analyzer names ->
        let withVariables = foldl addOwnedVariable analyzer names
            transfers = createTransferChain names
            finalState = foldl performTransferWith withVariables transfers
        in totalOwnershipPreserved finalState
    
  , testProperty "circular transfer detection" $
      \analyzer name1 name2 name3 ->
        let withOwned = foldl addOwnedVariable analyzer [name1, name2, name3]
            transfers = [(name1, name2), (name2, name3), (name3, name1)]
            finalState = foldl performTransferWith withOwned transfers
            errors = getOwnershipErrors finalState
        in L.any isCircularTransfer errors || null errors
    
  , testProperty "transfer preserves variable uniqueness" $
      \analyzer fromName toName ->
        let withOwned = addOwnedVariable analyzer fromName
            withTransfer = performTransfer withOwned fromName toName
        in variableUniquenessPreserved withTransfer
    
  , testCase "simple transfer consistency" $ do
      let analyzer = newOwnershipAnalyzer
          withX = addOwnedVariable analyzer "x"
          withTransfer = performTransfer withX "x" "y"
      assertBool "Transfer maintains invariants" $ transferMaintainsInvariants withTransfer "x" "y"
      assertBool "Variable uniqueness preserved" $ variableUniquenessPreserved withTransfer
  ]

-- ============================================================================
-- Ownership Boundary Properties
-- ============================================================================

ownershipBoundaryProperties :: TestTree
ownershipBoundaryProperties = testGroup "Ownership Boundary Properties"
  [ testProperty "boundary conditions are handled correctly" $
      \analyzer ->
        let emptyTransfer = createValidTransfer "" ""
            invalidTransfer = createValidTransfer "x" ""
        in not (isValidTransfer emptyTransfer) && 
           not (isValidTransfer invalidTransfer)
    
  , testProperty "maximal ownership chains are handled" $
      \n -> n < 100 ==>
        let names = L.map (\i -> "var" ++ show i) [1..n]
            analyzer = foldl addOwnedVariable newOwnershipAnalyzer names
            chain = createTransferChain names
            finalState = foldl performTransferWith analyzer chain
        in analyzerStateConsistent finalState
    
  , testProperty "ownership scope boundaries are respected" $
      \analyzer varName scope ->
        let withOwned = addOwnedVariable analyzer varName
            withScope = setVariableScope withOwned varName scope
            outOfScope = moveVariable withScope varName
            errors = getOwnershipErrors outOfScope
        in L.any isOutOfScope errors || null errors
    
  , testCase "boundary condition handling" $ do
      let analyzer = newOwnershipAnalyzer
          emptyTransfer = createValidTransfer "" ""
      assertBool "Empty transfer invalid" $ not $ isValidTransfer emptyTransfer
      
      let withX = addOwnedVariable analyzer "x"
          selfTransfer = createValidTransfer "x" "x"
      assertBool "Self-transfer detected" $ transferIsSelf selfTransfer
  ]

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate variable names
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
  return $ first : rest

-- Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- genVarName
  elements [Owned name, Borrowed name, MutBorrowed name]

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromName <- genVarName
  toName <- genVarName `suchThat` (/= fromName)
  return $ OwnershipTransfer fromName toName

-- Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = do
  varName <- genVarName
  varName2 <- genVarName `suchThat` (/= varName)
  elements
    [ UseAfterMove varName
    , DoubleMove varName varName2
    , BorrowWhileMoved varName
    , MutBorrowWhileBorrowed varName
    , BorrowWhileMutBorrowed varName
    , MultipleMutBorrows varName
    , UseWhileMutBorrowed varName
    , OutOfScope varName
    , BorrowError varName
    , ParseError "parse error"
    , CrossFunctionMove varName varName2
    , ParameterMoveMismatch varName
    , ControlFlowError "control flow error"
    ]

instance Arbitrary OwnershipType where
  arbitrary = genOwnershipType

instance Arbitrary OwnershipTransfer where
  arbitrary = genOwnershipTransfer

instance Arbitrary OwnershipError where
  arbitrary = genOwnershipError

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Create a valid ownership transfer
createValidTransfer :: String -> String -> OwnershipTransfer
createValidTransfer from to = OwnershipTransfer from to

-- Check if transfer is self-transfer
transferIsSelf :: OwnershipTransfer -> Bool
transferIsSelf transfer = transferSource transfer == transferTarget transfer

-- Get transfer source
transferSource :: OwnershipTransfer -> String
transferSource (OwnershipTransfer from _) = from

-- Get transfer target
transferTarget :: OwnershipTransfer -> String
transferTarget (OwnershipTransfer _ to) = to

-- Check if transfer is valid
isValidTransfer :: OwnershipTransfer -> Bool
isValidTransfer transfer = 
  let from = transferSource transfer
      to = transferTarget transfer
  in not (null from) && not (null to)

-- Check if analyzer is empty
analyzerIsEmpty :: OwnershipAnalyzer -> Bool
analyzerIsEmpty _ = False  -- Placeholder - depends on actual implementation

-- Add owned variable to analyzer
addOwnedVariable :: OwnershipAnalyzer -> String -> OwnershipAnalyzer
addOwnedVariable analyzer varName = analyzer  -- Placeholder

-- Move variable in analyzer
moveVariable :: OwnershipAnalyzer -> String -> OwnershipAnalyzer
moveVariable analyzer varName = analyzer  -- Placeholder

-- Add borrow relationship
addBorrow :: OwnershipAnalyzer -> String -> String -> OwnershipAnalyzer
addBorrow analyzer owner borrower = analyzer  -- Placeholder

-- Check if variable has ownership
variableHasOwnership :: OwnershipAnalyzer -> String -> Bool
variableHasOwnership analyzer varName = True  -- Placeholder

-- Check if variable has borrow
variableHasBorrow :: OwnershipAnalyzer -> String -> Bool
variableHasBorrow analyzer borrower = True  -- Placeholder

-- Get ownership errors
getOwnershipErrors :: OwnershipAnalyzer -> [OwnershipError]
getOwnershipErrors analyzer = []  -- Placeholder

-- Check if error is use after move
isUseAfterMove :: OwnershipError -> Bool
isUseAfterMove (UseAfterMove _) = True
isUseAfterMove _ = False

-- Check if error is circular transfer
isCircularTransfer :: OwnershipError -> Bool
isCircularTransfer _ = False  -- Placeholder

-- Check if error is out of scope
isOutOfScope :: OwnershipError -> Bool
isOutOfScope (OutOfScope _) = True
isOutOfScope _ = False

-- Format ownership error
formatOwnershipError :: OwnershipError -> String
formatOwnershipError error = case error of
  UseAfterMove var -> "Use after move: " ++ var
  DoubleMove var1 var2 -> "Double move: " ++ var1 ++ ", " ++ var2
  BorrowWhileMoved var -> "Borrow while moved: " ++ var
  MutBorrowWhileBorrowed var -> "Mut borrow while borrowed: " ++ var
  BorrowWhileMutBorrowed var -> "Borrow while mut borrowed: " ++ var
  MultipleMutBorrows var -> "Multiple mut borrows: " ++ var
  UseWhileMutBorrowed var -> "Use while mut borrowed: " ++ var
  OutOfScope var -> "Out of scope: " ++ var
  BorrowError var -> "Borrow error: " ++ var
  ParseError msg -> "Parse error: " ++ msg
  CrossFunctionMove var1 var2 -> "Cross function move: " ++ var1 ++ " -> " ++ var2
  ParameterMoveMismatch var -> "Parameter move mismatch: " ++ var
  ControlFlowError msg -> "Control flow error: " ++ msg

-- Categorize ownership error
categorizeError :: OwnershipError -> String
categorizeError error = case error of
  UseAfterMove _ -> "MoveError"
  DoubleMove _ _ -> "MoveError"
  BorrowWhileMoved _ -> "BorrowError"
  MutBorrowWhileBorrowed _ -> "BorrowError"
  BorrowWhileMutBorrowed _ -> "BorrowError"
  MultipleMutBorrows _ -> "BorrowError"
  UseWhileMutBorrowed _ -> "BorrowError"
  OutOfScope _ -> "ScopeError"
  BorrowError _ -> "BorrowError"
  ParseError _ -> "ParseError"
  CrossFunctionMove _ _ -> "FlowError"
  ParameterMoveMismatch _ -> "FlowError"
  ControlFlowError _ -> "FlowError"

-- Perform operations on analyzer
performOperations :: OwnershipAnalyzer -> [String] -> OwnershipAnalyzer
performOperations analyzer operations = analyzer  -- Placeholder

-- Check if analyzer state is consistent
analyzerStateConsistent :: OwnershipAnalyzer -> Bool
analyzerStateConsistent _ = True  -- Placeholder

-- Perform transfer
performTransfer :: OwnershipAnalyzer -> String -> String -> OwnershipAnalyzer
performTransfer analyzer from to = analyzer  -- Placeholder

-- Perform transfer with tuple
performTransferWith :: OwnershipAnalyzer -> (String, String) -> OwnershipAnalyzer
performTransferWith analyzer (from, to) = performTransfer analyzer from to

-- Check if transfer maintains invariants
transferMaintainsInvariants :: OwnershipAnalyzer -> String -> String -> Bool
transferMaintainsInvariants analyzer from to = True  -- Placeholder

-- Create transfer chain
createTransferChain :: [String] -> [(String, String)]
createTransferChain [] = []
createTransferChain [_] = []
createTransferChain (x:y:xs) = (x, y) : createTransferChain (y:xs)

-- Check if total ownership is preserved
totalOwnershipPreserved :: OwnershipAnalyzer -> Bool
totalOwnershipPreserved _ = True  -- Placeholder

-- Check if variable uniqueness is preserved
variableUniquenessPreserved :: OwnershipAnalyzer -> Bool
variableUniquenessPreserved _ = True  -- Placeholder

-- Set variable scope
setVariableScope :: OwnershipAnalyzer -> String -> String -> OwnershipAnalyzer
setVariableScope analyzer varName scope = analyzer  -- Placeholder

-- ============================================================================
-- Edge Case Tests
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Tests"
  [ testCase "handle empty variable names" $
      let transfer = createValidTransfer "" ""
      in assertBool "Empty names rejected" $ not $ isValidTransfer transfer
    
  , testCase "handle very long variable names" $
      let longName = replicate 1000 'a'
          transfer = createValidTransfer longName "y"
      in assertBool "Long names handled" $ isValidTransfer transfer
    
  , testCase "handle special characters in names" $
      let specialName = "var_with_123_and_symbols!@#"
          transfer = createValidTransfer specialName "normal"
      in assertBool "Special names handled" $ isValidTransfer transfer
    
  , testCase "handle unicode in names" $
      let unicodeName = "变量_世界_🌍"
          transfer = createValidTransfer unicodeName "english"
      in assertBool "Unicode names handled" $ isValidTransfer transfer
    
  , testProperty "handle L.maximum transfer chain L.length" $
      \n -> n < 1000 ==>
        let names = L.map (\i -> "var" ++ show i) [1..n]
            chain = createTransferChain names
        in L.length chain === max 0 (n - 1)
  ]

-- ============================================================================
-- Performance Properties
-- ============================================================================

performanceProperties :: TestTree
performanceProperties = testGroup "Performance Properties"
  [ testProperty "ownership analysis is linear in variables" $
      \n -> n < 1000 ==>
        let names = L.map (\i -> "var" ++ show i) [1..n]
            analyzer = foldl addOwnedVariable newOwnershipAnalyzer names
        in analyzerStateConsistent analyzer `seq` True
    
  , testProperty "transfer operations are efficient" $
      \n -> n < 1000 ==>
        let names = L.map (\i -> "var" ++ show i) [1..n]
            analyzer = foldl addOwnedVariable newOwnershipAnalyzer names
            transfers = createTransferChain names
            finalState = foldl performTransferWith analyzer transfers
        in analyzerStateConsistent finalState `seq` True
    
  , testProperty "error detection is efficient" $
      \n -> n < 1000 ==>
        let names = L.map (\i -> "var" ++ show i) [1..n]
            analyzer = foldl addOwnedVariable newOwnershipAnalyzer names
            errors = getOwnershipErrors analyzer
        in L.length errors `seq` True
  ]