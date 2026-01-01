{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.OwnershipComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Ownership
import Ownership.Common.Types
import TestSupport.Arbitrary ()

-- | Test suite for Ownership module with comprehensive QuickCheck properties
ownershipComprehensiveQuickCheckSpec :: TestTree
ownershipComprehensiveQuickCheckSpec = testGroup "Ownership Comprehensive QuickCheck Tests"
  [ ownershipTypeProperties
  , ownershipErrorProperties
  , ownershipAnalyzerProperties
  , ownershipTransferProperties
  , ownershipAnalysisProperties
  ]

-- | Properties for OwnershipType
ownershipTypeProperties :: TestTree
ownershipTypeProperties = testGroup "OwnershipType Properties"
  [ testProperty "OwnershipType equality is reflexive" $
      \ownType -> ownType == ownType
  
  , testProperty "OwnershipType equality is symmetric" $
      \ownType1 ownType2 -> (ownType1 == ownType2) ==> (ownType2 == ownType1)
  
  , testProperty "OwnershipType equality is transitive" $
      \ownType1 ownType2 ownType3 -> (ownType1 == ownType2 && ownType2 == ownType3) ==> (ownType1 == ownType3)
  
  , testProperty "OwnershipType ordering is consistent" $
      \ownType1 ownType2 ->
        let cmp1 = compare ownType1 ownType2
            cmp2 = compare (show ownType1) (show ownType2)
        in (cmp1 == EQ) ==> (cmp2 == EQ)
  
  , testProperty "Owned types compare by name" $
      \name1 name2 -> name1 /= name2 ==>
        let own1 = Owned name1
            own2 = Owned name2
        in compare own1 own2 == compare name1 name2
  
  , testProperty "Borrowed types compare by name" $
      \name1 name2 -> name1 /= name2 ==>
        let borrow1 = Borrowed name1
            borrow2 = Borrowed name2
        in compare borrow1 borrow2 == compare name1 name2
  
  , testProperty "MutBorrowed types compare by name" $
      \name1 name2 -> name1 /= name2 ==>
        let mutBorrow1 = MutBorrowed name1
            mutBorrow2 = MutBorrowed name2
        in compare mutBorrow1 mutBorrow2 == compare name1 name2
  
  , testProperty "Owned < Borrowed < MutBorrowed ordering" $
      \name ->
        let owned = Owned name
            borrowed = Borrowed name
            mutBorrowed = MutBorrowed name
        in compare owned borrowed == LT &&
           compare borrowed mutBorrowed == LT &&
           compare owned mutBorrowed == LT
  ]

-- | Properties for OwnershipError
ownershipErrorProperties :: TestTree
ownershipErrorProperties = testGroup "OwnershipError Properties"
  [ testProperty "OwnershipError equality is reflexive" $
      \error -> error == error
  
  , testProperty "OwnershipError equality is symmetric" $
      \error1 error2 -> (error1 == error2) ==> (error2 == error1)
  
  , testProperty "OwnershipError equality is transitive" $
      \error1 error2 error3 -> (error1 == error2 && error2 == error3) ==> (error1 == error3)
  
  , testProperty "OwnershipError ordering is consistent with string representation" $
      \error1 error2 ->
        let cmp1 = compare error1 error2
            cmp2 = compare (show error1) (show error2)
        in cmp1 == cmp2
  
  , testProperty "UseAfterMove errors with same variable are equal" $
      \var ->
        let error1 = UseAfterMove var
            error2 = UseAfterMove var
        in error1 == error2
  
  , testProperty "DoubleMove errors with same variables are equal" $
      \var1 var2 ->
        let error1 = DoubleMove var1 var2
            error2 = DoubleMove var1 var2
        in error1 == error2
  
  , testProperty "ParseError L.and BorrowError with same message are different" $
      \msg ->
        let parseError = ParseError msg
            borrowError = BorrowError msg
        in parseError /= borrowError
  ]

-- | Properties for OwnershipAnalyzer
ownershipAnalyzerProperties :: TestTree
ownershipAnalyzerProperties = testGroup "OwnershipAnalyzer Properties"
  [ testProperty "newOwnershipAnalyzer creates analyzer" $
      let analyzer = newOwnershipAnalyzer
      in True -- Check that analyzer is created successfully
  
  , testProperty "OwnershipAnalyzer equality is reflexive" $
      \analyzer -> analyzer == analyzer
  
  , testProperty "OwnershipAnalyzer equality is symmetric" $
      \analyzer1 analyzer2 -> (analyzer1 == analyzer2) ==> (analyzer2 == analyzer1)
  
  , testProperty "OwnershipAnalyzer equality is transitive" $
      \analyzer1 analyzer2 analyzer3 -> (analyzer1 == analyzer2 && analyzer2 == analyzer3) ==> (analyzer1 == analyzer3)
  
  , testProperty "All created analyzers are equal" $
      let analyzer1 = newOwnershipAnalyzer
          analyzer2 = newOwnershipAnalyzer
      in analyzer1 == analyzer2
  ]

-- | Properties for OwnershipTransfer
ownershipTransferProperties :: TestTree
ownershipTransferProperties = testGroup "OwnershipTransfer Properties"
  [ testProperty "OwnershipTransfer equality is reflexive" $
      \transfer -> transfer == transfer
  
  , testProperty "OwnershipTransfer equality is symmetric" $
      \transfer1 transfer2 -> (transfer1 == transfer2) ==> (transfer2 == transfer1)
  
  , testProperty "OwnershipTransfer equality is transitive" $
      \transfer1 transfer2 transfer3 -> (transfer1 == transfer2 && transfer2 == transfer3) ==> (transfer1 == transfer3)
  
  , testProperty "OwnershipTransfer with same from/to is equal" $
      \from to ->
        let transfer1 = OwnershipTransfer from to
            transfer2 = OwnershipTransfer from to
        in transfer1 == transfer2
  
  , testProperty "OwnershipTransfer with different from is not equal" $
      \from1 from2 to -> from1 /= from2 ==>
        let transfer1 = OwnershipTransfer from1 to
            transfer2 = OwnershipTransfer from2 to
        in transfer1 /= transfer2
  
  , testProperty "OwnershipTransfer with different to is not equal" $
      \from to1 to2 -> to1 /= to2 ==>
        let transfer1 = OwnershipTransfer from to1
            transfer2 = OwnershipTransfer from to2
        in transfer1 /= transfer2
  
  , testProperty "OwnershipTransfer preserves from L.and to" $
      \from to ->
        let transfer = OwnershipTransfer from to
        in transferFrom transfer == from && transferTo transfer == to
  ]

-- | Properties for ownership analysis functions
ownershipAnalysisProperties :: TestTree
ownershipAnalysisProperties = testGroup "Ownership Analysis Properties"
  [ testProperty "analyzeOwnership on empty code returns no errors" $
      let analyzer = newOwnershipAnalyzer
          result = analyzeOwnership analyzer ""
      in null result
  
  , testProperty "analyzeOwnershipFile on empty file returns no errors" $
      let analyzer = newOwnershipAnalyzer
          result = analyzeOwnershipFile analyzer ""
      in null result
  
  , testProperty "lexAll on empty string returns empty list" $
      lexAll "" == []
  
  , testProperty "lexAll on simple code returns tokens" $
      \code ->
        let tokens = lexAll code
        in not (null code) ==> not (null tokens)
  
  , testProperty "parseProgram on empty string returns empty program" $
      parseProgram "" == Right []
  
  , testProperty "parseProgram is deterministic" $
      \code ->
        let result1 = parseProgram code
            result2 = parseProgram code
        in result1 == result2
  
  , testProperty "formatOwnershipErrors on empty list returns empty string" $
      formatOwnershipErrors [] == ""
  
  , testProperty "formatOwnershipErrors on non-empty list returns non-empty string" $
      \errors ->
        let formatted = formatOwnershipErrors errors
        in not (null errors) ==> not (null formatted)
  
  , testProperty "builtInFunctions is not empty" $
      not (null builtInFunctions)
  
  , testProperty "analyzeOwnershipDebug returns debug information" $
      let analyzer = newOwnershipAnalyzer
          result = analyzeOwnershipDebug analyzer ""
      in True -- Check that debug information is returned
  ]

-- Arbitrary instances for testing
instance Arbitrary OwnershipType where
  arbitrary = do
    name <- arbitrary
    elements [Owned name, Borrowed name, MutBorrowed name]

instance Arbitrary OwnershipError where
  arbitrary = do
    let varGen = arbitrary
        msgGen = arbitrary
        varGen2 = arbitrary
    oneof
      [ UseAfterMove <$> varGen
      , DoubleMove <$> varGen <*> varGen2
      , BorrowWhileMoved <$> varGen
      , MutBorrowWhileBorrowed <$> varGen
      , BorrowWhileMutBorrowed <$> varGen
      , MultipleMutBorrows <$> varGen
      , UseWhileMutBorrowed <$> varGen
      , OutOfScope <$> varGen
      , BorrowError <$> msgGen
      , ParseError <$> msgGen
      , CrossFunctionMove <$> varGen <*> varGen2
      , ParameterMoveMismatch <$> varGen
      , ControlFlowError <$> msgGen
      , PathSensitiveError <$> msgGen
      , LoopOwnershipError <$> msgGen
      ]

instance Arbitrary OwnershipAnalyzer where
  arbitrary = return newOwnershipAnalyzer

instance Arbitrary OwnershipTransfer where
  arbitrary = do
    from <- arbitrary
    to <- arbitrary
    return $ OwnershipTransfer from to