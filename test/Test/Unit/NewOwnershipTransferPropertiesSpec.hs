module Test.Unit.NewOwnershipTransferPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, choose, listOf, elements, forAll, oneof, suchThat)

import Ownership.Common.Types
  ( OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Data.List (sort, nub)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- Generate variable names
genVariableName :: Gen String
genVariableName = do
  prefix <- elements ["x", "y", "z", "var", "value", "data", "item", "obj"]
  suffix <- choose (1, 100)
  pure $ prefix ++ show suffix

-- Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = oneof
  [ Owned <$> genVariableName
  , Borrowed <$> genVariableName
  , MutBorrowed <$> genVariableName
  ]

-- Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVariableName
  , DoubleMove <$> genVariableName <*> genVariableName
  , BorrowWhileMoved <$> genVariableName
  , MutBorrowWhileBorrowed <$> genVariableName
  , BorrowWhileMutBorrowed <$> genVariableName
  , MultipleMutBorrows <$> genVariableName
  , UseWhileMutBorrowed <$> genVariableName
  , OutOfScope <$> genVariableName
  , BorrowError <$> genVariableName
  , ParseError <$> genVariableName
  , CrossFunctionMove <$> genVariableName <*> genVariableName
  , ParameterMoveMismatch <$> genVariableName
  , ControlFlowError <$> genVariableName
  , PathSensitiveError <$> genVariableName
  , LoopOwnershipError <$> genVariableName
  ]

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = OwnershipTransfer <$> genVariableName <*> genVariableName

-- ============================================================================
-- Property Tests for OwnershipType
-- ============================================================================

-- Property: OwnershipType ordering should be consistent (Owned < Borrowed < MutBorrowed)
prop_ownership_type_ordering_consistency :: Property
prop_ownership_type_ordering_consistency = 
  forAll genVariableName $ \name ->
    let owned = Owned name
        borrowed = Borrowed name
        mutBorrowed = MutBorrowed name
    in owned < borrowed .&&. borrowed < mutBorrowed .&&. owned < mutBorrowed

-- Property: OwnershipType sorting should maintain the same order
prop_ownership_type_sorting :: Property
prop_ownership_type_sorting = 
  forAll (listOf genOwnershipType `suchThat` (not . null)) $ \types ->
    let sortedTypes = sort types
        expectedOrder = sort $ map show types
        actualOrder = map show sortedTypes
    in actualOrder === expectedOrder

-- Property: OwnershipType equality should be based on both constructor and name
prop_ownership_type_equality :: Property
prop_ownership_type_equality = 
  forAll genVariableName $ \name1 ->
    forAll genVariableName $ \name2 ->
      let owned1 = Owned name1
          owned2 = Owned name2
          borrowed1 = Borrowed name1
          borrowed2 = Borrowed name2
      in (owned1 == owned2) === (name1 == name2) .&&.
         (borrowed1 == borrowed2) === (name1 == name2) .&&.
         (owned1 == borrowed1) === False

-- Property: OwnershipType with same constructor but different names should be ordered by name
prop_ownership_type_name_ordering :: Property
prop_ownership_type_name_ordering = 
  forAll genVariableName $ \name1 ->
    forAll genVariableName $ \name2 ->
      let owned1 = Owned name1
          owned2 = Owned name2
      in if name1 < name2 then owned1 < owned2 else owned1 >= owned2

-- ============================================================================
-- Property Tests for OwnershipError
-- ============================================================================

-- Property: OwnershipError sorting should be based on string representation
prop_ownership_error_sorting :: Property
prop_ownership_error_sorting = 
  forAll (listOf genOwnershipError `suchThat` (not . null)) $ \errors ->
    let sortedErrors = sort errors
        expectedOrder = sort $ map show errors
        actualOrder = map show sortedErrors
    in actualOrder === expectedOrder

-- Property: OwnershipError equality should be reflexive
prop_ownership_error_equality_reflexive :: Property
prop_ownership_error_equality_reflexive = 
  forAll genOwnershipError $ \error ->
    error === error

-- Property: OwnershipError equality should be symmetric
prop_ownership_error_equality_symmetric :: Property
prop_ownership_error_equality_symmetric = 
  forAll genOwnershipError $ \error1 ->
    forAll genOwnershipError $ \error2 ->
      (error1 == error2) === (error2 == error1)

-- Property: UseAfterMove errors should be equal only for same variable
prop_use_after_move_equality :: Property
prop_use_after_move_equality = 
  forAll genVariableName $ \var1 ->
    forAll genVariableName $ \var2 ->
      let error1 = UseAfterMove var1
          error2 = UseAfterMove var2
      in (error1 == error2) === (var1 == var2)

-- Property: DoubleMove errors should be equal only for same variable pair
prop_double_move_equality :: Property
prop_double_move_equality = 
  forAll genVariableName $ \var1a ->
    forAll genVariableName $ \var1b ->
      forAll genVariableName $ \var2a ->
        forAll genVariableName $ \var2b ->
          let error1 = DoubleMove var1a var1b
              error2 = DoubleMove var2a var2b
          in (error1 == error2) === (var1a == var2a && var1b == var2b)

-- ============================================================================
-- Property Tests for OwnershipTransfer
-- ============================================================================

-- Property: OwnershipTransfer should preserve from and to variables
prop_ownership_transfer_preservation :: Property
prop_ownership_transfer_preservation = 
  forAll genVariableName $ \from ->
    forAll genVariableName $ \to ->
      let transfer = OwnershipTransfer from to
      in transferFrom transfer === from .&&. transferTo transfer === to

-- Property: OwnershipTransfer equality should be based on both from and to
prop_ownership_transfer_equality :: Property
prop_ownership_transfer_equality = 
  forAll genVariableName $ \from1 ->
    forAll genVariableName $ \to1 ->
      forAll genVariableName $ \from2 ->
        forAll genVariableName $ \to2 ->
          let transfer1 = OwnershipTransfer from1 to1
              transfer2 = OwnershipTransfer from2 to2
          in (transfer1 == transfer2) === (from1 == from2 && to1 == to2)

-- Property: OwnershipTransfer should handle self-transfer correctly
prop_ownership_transfer_self_transfer :: Property
prop_ownership_transfer_self_transfer = 
  forAll genVariableName $ \var ->
    let transfer = OwnershipTransfer var var
        selfTransfer = transferFrom transfer == transferTo transfer
    in selfTransfer === True

-- ============================================================================
-- Property Tests for OwnershipAnalyzer
-- ============================================================================

-- Property: newOwnershipAnalyzer should always return the same analyzer
prop_new_ownership_analyzer_consistency :: Property
prop_new_ownership_analyzer_consistency = 
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 === analyzer2

-- Property: OwnershipAnalyzer should be showable
prop_ownership_analyzer_showable :: Property
prop_ownership_analyzer_showable = 
  let analyzer = newOwnershipAnalyzer
      shown = show analyzer
  in not (null shown)

-- ============================================================================
-- Property Tests for Ownership Type Relationships
-- ============================================================================

-- Property: Owned should be the minimum in any ownership type comparison
prop_owned_is_minimum :: Property
prop_owned_is_minimum = 
  forAll genVariableName $ \name ->
    forAll genOwnershipType $ \otherType ->
      let owned = Owned name
      in case otherType of
        Owned otherName -> if name == otherName then owned === otherType else owned <= otherType
        _ -> owned < otherType

-- Property: MutBorrowed should be the maximum in any ownership type comparison
prop_mut_borrowed_is_maximum :: Property
prop_mut_borrowed_is_maximum = 
  forAll genVariableName $ \name ->
    forAll genOwnershipType $ \otherType ->
      let mutBorrowed = MutBorrowed name
      in case otherType of
        MutBorrowed otherName -> if name == otherName then mutBorrowed === otherType else mutBorrowed >= otherType
        _ -> mutBorrowed > otherType

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_ownership_type_show :: IO ()
test_ownership_type_show = do
  let owned = Owned "x"
      borrowed = Borrowed "y"
      mutBorrowed = MutBorrowed "z"
  show owned @?= "Owned x"
  show borrowed @?= "Borrowed y"
  show mutBorrowed @?= "MutBorrowed z"

test_ownership_error_show :: IO ()
test_ownership_error_show = do
  let useAfterMove = UseAfterMove "x"
      doubleMove = DoubleMove "x" "y"
      borrowWhileMoved = BorrowWhileMoved "z"
  show useAfterMove @?= "UseAfterMove x"
  show doubleMove @?= "DoubleMove x y"
  show borrowWhileMoved @?= "BorrowWhileMoved z"

test_ownership_transfer_show :: IO ()
test_ownership_transfer_show = do
  let transfer = OwnershipTransfer "source" "target"
  show transfer @?= "OwnershipTransfer {transferFrom = \"source\", transferTo = \"target\"}"

test_ownership_analyzer_creation :: IO ()
test_ownership_analyzer_creation = do
  let analyzer = newOwnershipAnalyzer
  show analyzer @?= "OwnershipAnalyzer ()"

test_ownership_type_ordering :: IO ()
test_ownership_type_ordering = do
  let owned = Owned "x"
      borrowed = Borrowed "x"
      mutBorrowed = MutBorrowed "x"
      types = [mutBorrowed, owned, borrowed]
      sortedTypes = sort types
  sortedTypes @?= [owned, borrowed, mutBorrowed]

test_ownership_error_ordering :: IO ()
test_ownership_error_ordering = do
  let errors = 
        [ UseAfterMove "z"
        , DoubleMove "a" "b"
        , BorrowWhileMoved "y"
        , UseAfterMove "x"
        ]
      sortedErrors = sort errors
  map show sortedErrors @?= 
    [ "DoubleMove a b"
    , "BorrowWhileMoved y"
    , "UseAfterMove x"
    , "UseAfterMove z"
    ]

test_complex_ownership_scenarios :: IO ()
test_complex_ownership_scenarios = do
  -- Test multiple ownership types with same name
  let owned1 = Owned "data"
      borrowed1 = Borrowed "data"
      mutBorrowed1 = MutBorrowed "data"
      
      -- Test ownership transfers
      transfer1 = OwnershipTransfer "data" "new_data"
      transfer2 = OwnershipTransfer "data" "backup"
      
      -- Test complex errors
      crossFunctionMove = CrossFunctionMove "param" "global"
      controlFlowError = ControlFlowError "unreachable_code"
  
  -- Verify ordering
  sort [mutBorrowed1, owned1, borrowed1] @?= [owned1, borrowed1, mutBorrowed1]
  
  -- Verify transfer properties
  transferFrom transfer1 @?= "data"
  transferTo transfer1 @?= "new_data"
  
  -- Verify error properties
  show crossFunctionMove @?= "CrossFunctionMove param global"
  show controlFlowError @?= "ControlFlowError unreachable_code"

-- Helper operator for property composition
(.&&.) :: Property -> Property -> Property
p1 .&&. p2 = p1 ==> p2

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Ownership Transfer Properties Tests"
  [ -- OwnershipType properties
    testProperty "Ownership type ordering consistency" prop_ownership_type_ordering_consistency
  , testProperty "Ownership type sorting" prop_ownership_type_sorting
  , testProperty "Ownership type equality" prop_ownership_type_equality
  , testProperty "Ownership type name ordering" prop_ownership_type_name_ordering
  
  -- OwnershipError properties
  , testProperty "Ownership error sorting" prop_ownership_error_sorting
  , testProperty "Ownership error equality reflexive" prop_ownership_error_equality_reflexive
  , testProperty "Ownership error equality symmetric" prop_ownership_error_equality_symmetric
  , testProperty "UseAfterMove equality" prop_use_after_move_equality
  , testProperty "DoubleMove equality" prop_double_move_equality
  
  -- OwnershipTransfer properties
  , testProperty "Ownership transfer preservation" prop_ownership_transfer_preservation
  , testProperty "Ownership transfer equality" prop_ownership_transfer_equality
  , testProperty "Ownership transfer self transfer" prop_ownership_transfer_self_transfer
  
  -- OwnershipAnalyzer properties
  , testProperty "New ownership analyzer consistency" prop_new_ownership_analyzer_consistency
  , testProperty "Ownership analyzer showable" prop_ownership_analyzer_showable
  
  -- Ownership type relationships
  , testProperty "Owned is minimum" prop_owned_is_minimum
  , testProperty "MutBorrowed is maximum" prop_mut_borrowed_is_maximum
  
  -- Unit tests
  , testCase "Ownership type show" test_ownership_type_show
  , testCase "Ownership error show" test_ownership_error_show
  , testCase "Ownership transfer show" test_ownership_transfer_show
  , testCase "Ownership analyzer creation" test_ownership_analyzer_creation
  , testCase "Ownership type ordering" test_ownership_type_ordering
  , testCase "Ownership error ordering" test_ownership_error_ordering
  , testCase "Complex ownership scenarios" test_complex_ownership_scenarios
  ]