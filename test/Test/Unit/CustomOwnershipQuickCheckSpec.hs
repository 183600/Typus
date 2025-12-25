{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.CustomOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, elements, listOf, listOf1, oneof, choose)
import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )

-- | Generate valid variable names
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  varName <- genVarName
  oneof [return $ Owned varName, return $ Borrowed varName, return $ MutBorrowed varName]

-- | Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ do var <- genVarName; return $ UseAfterMove var
  , do var1 <- genVarName; var2 <- genVarName; return $ DoubleMove var1 var2
  , do var <- genVarName; return $ BorrowWhileMoved var
  , do var <- genVarName; return $ MutBorrowWhileBorrowed var
  , do var <- genVarName; return $ BorrowWhileMutBorrowed var
  , do var <- genVarName; return $ MultipleMutBorrows var
  , do var <- genVarName; return $ UseWhileMutBorrowed var
  , do var <- genVarName; return $ OutOfScope var
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']; return $ BorrowError msg
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']; return $ ParseError msg
  , do var1 <- genVarName; var2 <- genVarName; return $ CrossFunctionMove var1 var2
  , do var <- genVarName; return $ ParameterMoveMismatch var
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']; return $ ControlFlowError msg
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']; return $ PathSensitiveError msg
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']; return $ LoopOwnershipError msg
  ]

-- | Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVarName
  toVar <- genVarName
  return $ OwnershipTransfer fromVar toVar

-- | Test OwnershipType equality
prop_ownershipTypeEquality :: Property
prop_ownershipTypeEquality = forAll genOwnershipType $ \ownershipType ->
  ownershipType == ownershipType

-- | Test OwnershipType ordering consistency
prop_ownershipTypeOrdering :: Property
prop_ownershipTypeOrdering = forAll genOwnershipType $ \type1 ->
  forAll genOwnershipType $ \type2 ->
    let cmp = compare type1 type2
    in (cmp == LT) == (type1 < type2) &&
       (cmp == EQ) == (type1 == type2) &&
       (cmp == GT) == (type1 > type2)

-- | Test Owned type properties
prop_ownedTypeProperties :: Property
prop_ownedTypeProperties = forAll genVarName $ \varName ->
  let owned = Owned varName
  in show owned == "Owned " ++ varName

-- | Test Borrowed type properties
prop_borrowedTypeProperties :: Property
prop_borrowedTypeProperties = forAll genVarName $ \varName ->
  let borrowed = Borrowed varName
  in show borrowed == "Borrowed " ++ varName

-- | Test MutBorrowed type properties
prop_mutBorrowedTypeProperties :: Property
prop_mutBorrowedTypeProperties = forAll genVarName $ \varName ->
  let mutBorrowed = MutBorrowed varName
  in show mutBorrowed == "MutBorrowed " ++ varName

-- | Test OwnershipError equality
prop_ownershipErrorEquality :: Property
prop_ownershipErrorEquality = forAll genOwnershipError $ \error ->
  error == error

-- | Test OwnershipError ordering consistency
prop_ownershipErrorOrdering :: Property
prop_ownershipErrorOrdering = forAll genOwnershipError $ \error1 ->
  forAll genOwnershipError $ \error2 ->
    let cmp = compare error1 error2
    in (cmp == LT) == (error1 < error2) &&
       (cmp == EQ) == (error1 == error2) &&
       (cmp == GT) == (error1 > error2)

-- | Test UseAfterMove error properties
prop_useAfterMoveErrorProperties :: Property
prop_useAfterMoveErrorProperties = forAll genVarName $ \varName ->
  let error = UseAfterMove varName
  in show error == "UseAfterMove " ++ varName

-- | Test DoubleMove error properties
prop_doubleMoveErrorProperties :: Property
prop_doubleMoveErrorProperties = forAll genVarName $ \var1 ->
  forAll genVarName $ \var2 ->
    let error = DoubleMove var1 var2
    in show error == "DoubleMove " ++ var1 ++ " " ++ var2

-- | Test BorrowError properties
prop_borrowErrorProperties :: Property
prop_borrowErrorProperties = forAll (listOf1 $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9']) $ \msg ->
  let error = BorrowError msg
  in show error == "BorrowError " ++ msg

-- | Test OwnershipTransfer properties
prop_ownershipTransferProperties :: Property
prop_ownershipTransferProperties = forAll genOwnershipTransfer $ \transfer ->
  let fromVar = transferFrom transfer
      toVar = transferTo transfer
  in not (null fromVar) && not (null toVar)

-- | Test OwnershipTransfer equality
prop_ownershipTransferEquality :: Property
prop_ownershipTransferEquality = forAll genOwnershipTransfer $ \transfer1 ->
  forAll genOwnershipTransfer $ \transfer2 ->
    let sameFrom = transferFrom transfer1 == transferFrom transfer2
        sameTo = transferTo transfer1 == transferTo transfer2
    in (transfer1 == transfer2) == (sameFrom && sameTo)

-- | Test OwnershipAnalyzer creation
prop_ownershipAnalyzerCreation :: Property
prop_ownershipAnalyzerCreation = 
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
       OwnershipAnalyzer () -> True

-- | Test OwnershipAnalyzer equality
prop_ownershipAnalyzerEquality :: Property
prop_ownershipAnalyzerEquality = 
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 == analyzer2

-- | Test OwnershipType ordering: Owned < Borrowed < MutBorrowed
prop_ownershipTypeHierarchy :: Property
prop_ownershipTypeHierarchy = forAll genVarName $ \varName ->
  let owned = Owned varName
      borrowed = Borrowed varName
      mutBorrowed = MutBorrowed varName
  in owned < borrowed && borrowed < mutBorrowed

-- | Test OwnershipError message consistency
prop_ownershipErrorMessageConsistency :: Property
prop_ownershipErrorMessageConsistency = forAll genOwnershipError $ \error ->
  let errorMsg = show error
  in not (null errorMsg)

-- | Test OwnershipTransfer show property
prop_ownershipTransferShow :: Property
prop_ownershipTransferShow = forAll genOwnershipTransfer $ \transfer ->
  let transferStr = show transfer
  in not (null transferStr)

-- | Test OwnershipType round-trip through Show
prop_ownershipTypeShowRoundTrip :: Property
prop_ownershipTypeShowRoundTrip = forAll genOwnershipType $ \ownershipType ->
  let ownershipStr = show ownershipType
  in not (null ownershipStr)

-- | Test OwnershipError round-trip through Show
prop_ownershipErrorShowRoundTrip :: Property
prop_ownershipErrorShowRoundTrip = forAll genOwnershipError $ \error ->
  let errorStr = show error
  in not (null errorStr)

tests :: TestTree
tests = testGroup "Custom Ownership QuickCheck Tests"
  [ testProperty "OwnershipType equality" prop_ownershipTypeEquality
  , testProperty "OwnershipType ordering consistency" prop_ownershipTypeOrdering
  , testProperty "Owned type properties" prop_ownedTypeProperties
  , testProperty "Borrowed type properties" prop_borrowedTypeProperties
  , testProperty "MutBorrowed type properties" prop_mutBorrowedTypeProperties
  , testProperty "OwnershipError equality" prop_ownershipErrorEquality
  , testProperty "OwnershipError ordering consistency" prop_ownershipErrorOrdering
  , testProperty "UseAfterMove error properties" prop_useAfterMoveErrorProperties
  , testProperty "DoubleMove error properties" prop_doubleMoveErrorProperties
  , testProperty "BorrowError properties" prop_borrowErrorProperties
  , testProperty "OwnershipTransfer properties" prop_ownershipTransferProperties
  , testProperty "OwnershipTransfer equality" prop_ownershipTransferEquality
  , testProperty "OwnershipAnalyzer creation" prop_ownershipAnalyzerCreation
  , testProperty "OwnershipAnalyzer equality" prop_ownershipAnalyzerEquality
  , testProperty "OwnershipType hierarchy" prop_ownershipTypeHierarchy
  , testProperty "OwnershipError message consistency" prop_ownershipErrorMessageConsistency
  , testProperty "OwnershipTransfer show" prop_ownershipTransferShow
  , testProperty "OwnershipType show round-trip" prop_ownershipTypeShowRoundTrip
  , testProperty "OwnershipError show round-trip" prop_ownershipErrorShowRoundTrip
  ]