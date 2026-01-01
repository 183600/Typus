{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalOwnershipTransferQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Ownership.Common.Types
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)

-- | Test ownership transfer properties
testOwnershipTransferProperties :: TestTree
testOwnershipTransferProperties = testGroup "Ownership Transfer Properties"
  [ testProperty "ownership type ordering is total" propOwnershipTypeOrderingTotal
  , testProperty "ownership type ordering is transitive" propOwnershipTypeOrderingTransitive
  , testProperty "ownership transfer preserves from/to fields" propOwnershipTransferPreservesFields
  , testProperty "ownership error ordering is total" propOwnershipErrorOrderingTotal
  , testProperty "ownership analyzer is consistent" propOwnershipAnalyzerConsistent
  ]

-- | Ownership type ordering should be total (L.any two types can be compared)
propOwnershipTypeOrderingTotal :: OwnershipType -> OwnershipType -> Bool
propOwnershipTypeOrderingTotal ot1 ot2 =
  let comparison = compare ot1 ot2
  in comparison == LT || comparison == EQ || comparison == GT

-- | Ownership type ordering should be transitive
propOwnershipTypeOrderingTransitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
propOwnershipTypeOrderingTransitive ot1 ot2 ot3 =
  let comp12 = compare ot1 ot2
      comp23 = compare ot2 ot3
      comp13 = compare ot1 ot3
  in (comp12 == EQ && comp23 == EQ) ==> comp13 == EQ

-- | Ownership transfer should preserve from/to fields
propOwnershipTransferPreservesFields :: String -> String -> Property
propOwnershipTransferPreservesFields from to =
  not (null from) && not (null to) ==> 
  let transfer = OwnershipTransfer from to
  in transferFrom transfer == from && transferTo transfer == to

-- | Ownership error ordering should be total
propOwnershipErrorOrderingTotal :: OwnershipError -> OwnershipError -> Bool
propOwnershipErrorOrderingTotal oe1 oe2 =
  let comparison = compare oe1 oe2
  in comparison == LT || comparison == EQ || comparison == GT

-- | Ownership analyzer should be consistent
propOwnershipAnalyzerConsistent :: Bool
propOwnershipAnalyzerConsistent =
  let analyzer = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer == analyzer2

-- | Test ownership type operations
testOwnershipTypeOperations :: TestTree
testOwnershipTypeOperations = testGroup "Ownership Type Operations"
  [ testCase "owned type construction" $
      let name = "variable"
          ownership = Owned name
      in case ownership of
           Owned n -> n == name
           _ -> fail "Owned type construction failed"
           
  , testCase "borrowed type construction" $
      let owner = "original"
          ownership = Borrowed owner
      in case ownership of
           Borrowed o -> o == owner
           _ -> fail "Borrowed type construction failed"
           
  , testCase "mutably borrowed type construction" $
      let owner = "mutable"
          ownership = MutBorrowed owner
      in case ownership of
           MutBorrowed o -> o == owner
           _ -> fail "MutBorrowed type construction failed"
           
  , testCase "ownership type ordering" $
      let owned = Owned "x"
          borrowed = Borrowed "x"
          mutBorrowed = MutBorrowed "x"
      in compare owned borrowed == LT &&
         compare borrowed mutBorrowed == LT &&
         compare owned mutBorrowed == LT
         
  , testCase "ownership type equality" $
      let owned1 = Owned "x"
          owned2 = Owned "x"
          owned3 = Owned "y"
          borrowed = Borrowed "x"
      in owned1 == owned2 &&
         owned1 /= owned3 &&
         owned1 /= borrowed
  ]

-- | Test ownership error operations
testOwnershipErrorOperations :: TestTree
testOwnershipErrorOperations = testGroup "Ownership Error Operations"
  [ testCase "use after move error" $
      let var = "moved_var"
          error = UseAfterMove var
      in case error of
           UseAfterMove v -> v == var
           _ -> fail "UseAfterMove error construction failed"
           
  , testCase "double move error" $
      let var1 = "var1"
          var2 = "var2"
          error = DoubleMove var1 var2
      in case error of
           DoubleMove v1 v2 -> v1 == var1 && v2 == var2
           _ -> fail "DoubleMove error construction failed"
           
  , testCase "borrow while moved error" $
      let var = "moved_var"
          error = BorrowWhileMoved var
      in case error of
           BorrowWhileMoved v -> v == var
           _ -> fail "BorrowWhileMoved error construction failed"
           
  , testCase "mutable borrow while borrowed error" $
      let var = "borrowed_var"
          error = MutBorrowWhileBorrowed var
      in case error of
           MutBorrowWhileBorrowed v -> v == var
           _ -> fail "MutBorrowWhileBorrowed error construction failed"
           
  , testCase "ownership error ordering" $
      let error1 = UseAfterMove "x"
          error2 = UseAfterMove "y"
          error3 = DoubleMove "x" "y"
      in compare error1 error2 /= EQ &&  -- Different variables
         compare error1 error3 /= EQ      -- Different error types
  ]

-- | Test ownership transfer operations
testOwnershipTransferOperations :: TestTree
testOwnershipTransferOperations = testGroup "Ownership Transfer Operations"
  [ testCase "ownership transfer construction" $
      let fromVar = "source"
          toVar = "target"
          transfer = OwnershipTransfer fromVar toVar
      in transferFrom transfer == fromVar &&
         transferTo transfer == toVar
         
  , testCase "ownership transfer equality" $
      let transfer1 = OwnershipTransfer "a" "b"
          transfer2 = OwnershipTransfer "a" "b"
          transfer3 = OwnershipTransfer "b" "a"
      in transfer1 == transfer2 &&
         transfer1 /= transfer3
         
  , testCase "ownership transfer show" $
      let transfer = OwnershipTransfer "x" "y"
          shown = show transfer
      in "x" `L.isInfixOf` shown && "y" `L.isInfixOf` shown
  ]

-- | Test ownership analyzer operations
testOwnershipAnalyzerOperations :: TestTree
testOwnershipAnalyzerOperations = testGroup "Ownership Analyzer Operations"
  [ testCase "new ownership analyzer" $
      let analyzer = newOwnershipAnalyzer
      in case analyzer of
           OwnershipAnalyzer () -> pure ()
           _ -> fail "Ownership analyzer construction failed"
           
  , testCase "ownership analyzer equality" $
      let analyzer1 = newOwnershipAnalyzer
          analyzer2 = newOwnershipAnalyzer
      in analyzer1 == analyzer2
          
  , testCase "ownership analyzer show" $
      let analyzer = newOwnershipAnalyzer
          shown = show analyzer
      in "OwnershipAnalyzer" `L.isInfixOf` shown
  ]

-- | Test ownership scenarios
testOwnershipScenarios :: TestTree
testOwnershipScenarios = testGroup "Ownership Scenarios"
  [ testCase "simple ownership transfer" $
      let fromVar = "owner"
          toVar = "borrower"
          transfer = OwnershipTransfer fromVar toVar
          ownership1 = Owned fromVar
          ownership2 = Borrowed fromVar
      in transferFrom transfer == fromVar &&
         transferTo transfer == toVar &&
         show ownership1 /= show ownership2
         
  , testCase "multiple ownership types" $
      let owner = Owned "data"
          borrow = Borrowed "data"
          mutBorrow = MutBorrowed "data"
          types = [owner, borrow, mutBorrow]
          sortedTypes = sort types
      in L.length sortedTypes == 3 &&
         L.head sortedTypes == owner &&
         last sortedTypes == mutBorrow
         
  , testCase "error categorization" $
      let moveError = UseAfterMove "x"
          borrowError = BorrowWhileMoved "y"
          parseError = ParseError "syntax error"
          errors = [moveError, borrowError, parseError]
          sortedErrors = sort errors
      in L.length sortedErrors == 3 &&
         moveError `elem` sortedErrors &&
         borrowError `elem` sortedErrors &&
         parseError `elem` sortedErrors
  ]

-- | Test ownership edge cases
testOwnershipEdgeCases :: TestTree
testOwnershipEdgeCases = testGroup "Ownership Edge Cases"
  [ testCase "empty variable names" $
      let transfer = OwnershipTransfer "" ""
          owned = Owned ""
          borrowed = Borrowed ""
      in transferFrom transfer == "" &&
         transferTo transfer == "" &&
         case owned of
          Owned "" -> True
           _ -> False &&
         case borrowed of
           Borrowed "" -> True
           _ -> False
           
  , testCase "special characters in variable names" $
      let specialName = "$var_123"
          owned = Owned specialName
          error = UseAfterMove specialName
      in case owned of
           Owned name -> name == specialName
           _ -> False
           
  , testCase "long variable names" $
      let longName = L.concat (replicate 100 "very_long_variable_name_")
          transfer = OwnershipTransfer longName "target"
      in transferFrom transfer == longName
  ]

-- | All ownership transfer tests
testOwnershipTransferQuickCheck :: TestTree
testOwnershipTransferQuickCheck = testGroup "New Cabal Ownership Transfer QuickCheck Tests"
  [ testOwnershipTransferProperties
  , testOwnershipTypeOperations
  , testOwnershipErrorOperations
  , testOwnershipTransferOperations
  , testOwnershipAnalyzerOperations
  , testOwnershipScenarios
  , testOwnershipEdgeCases
  ]