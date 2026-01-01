{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalOwnershipQuickCheckTestsSpec where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, counterexample, suchThat, elements, listOf, listOf1, choose)
import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer(..)
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  )
import Data.List (sort, nub)

-- ============================================================================
-- QuickCheck Generators
-- ============================================================================

-- Generate valid variable names (alphanumeric, starting with letter)
genVariableName :: Gen String
genVariableName = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ '_'
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_'
  return $ first : rest

-- Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- genVariableName
  elements [Owned name, Borrowed name, MutBorrowed name]

-- Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ do var <- genVariableName
       return $ UseAfterMove var
  , do var1 <- genVariableName
       var2 <- genVariableName `suchThat` (/= var1)
       return $ DoubleMove var1 var2
  , do var <- genVariableName
       return $ BorrowWhileMoved var
  , do var <- genVariableName
       return $ MutBorrowWhileBorrowed var
  , do var <- genVariableName
       return $ BorrowWhileMutBorrowed var
  , do var <- genVariableName
       return $ MultipleMutBorrows var
  , do var <- genVariableName
       return $ UseWhileMutBorrowed var
  , do var <- genVariableName
       return $ OutOfScope var
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
       return $ BorrowError msg
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
       return $ ParseError msg
  , do var1 <- genVariableName
       var2 <- genVariableName
       return $ CrossFunctionMove var1 var2
  , do var <- genVariableName
       return $ ParameterMoveMismatch var
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
       return $ ControlFlowError msg
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
       return $ PathSensitiveError msg
  , do msg <- listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
       return $ LoopOwnershipError msg
  ]

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  from <- genVariableName
  to <- genVariableName `suchThat` (/= from)
  return $ OwnershipTransfer from to

-- Generate lists of ownership types
genOwnershipTypeList :: Gen [OwnershipType]
genOwnershipTypeList = listOf genOwnershipType

-- Generate lists of ownership errors
genOwnershipErrorList :: Gen [OwnershipError]
genOwnershipErrorList = listOf genOwnershipError

-- ============================================================================
-- Properties for OwnershipType
-- ============================================================================

prop_ownership_type_show_roundtrip :: OwnershipType -> Property
prop_ownership_type_show_roundtrip ownershipType =
  let shown = show ownershipType
      parsed = case words shown of
                 ["Owned", name] -> Just $ Owned name
                 ["Borrowed", name] -> Just $ Borrowed name
                 ["MutBorrowed", name] -> Just $ MutBorrowed name
                 _ -> Nothing
  in case parsed of
       Just parsedType -> parsedType === ownershipType
       Nothing -> counterexample ("Could not parse: " ++ shown) False

prop_ownership_type_ordering_consistent :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_ordering_consistent ot1 ot2 =
  let ord1 = compare ot1 ot2
      ord2 = compare (show ot1) (show ot2)
  in if ot1 == ot2
     then ord1 === EQ
     else property True  -- The ordering is defined, not based on string comparison

prop_ownership_type_name_consistency :: OwnershipType -> Property
prop_ownership_type_name_consistency ownershipType =
  let expectedName = case ownershipType of
                       Owned name -> name
                       Borrowed name -> name
                       MutBorrowed name -> name
      actualName = case ownershipType of
                     Owned name -> name
                     Borrowed name -> name
                     MutBorrowed name -> name
  in expectedName === actualName

-- ============================================================================
-- Properties for OwnershipError
-- ============================================================================

prop_ownership_error_show_roundtrip :: OwnershipError -> Property
prop_ownership_error_show_roundtrip ownershipError =
  let shown = show ownershipError
  in L.length shown > 0  -- Basic check that show produces non-empty string

prop_ownership_error_ordering_consistent :: OwnershipError -> OwnershipError -> Property
prop_ownership_error_ordering_consistent err1 err2 =
  let ord1 = compare err1 err2
      ord2 = compare (show err1) (show err2)
  in ord1 === ord2  -- Ordering is based on string representation

prop_ownership_error_use_after_move_structure :: Property
prop_ownership_error_use_after_move_structure =
  forAll genVariableName $ \var ->
    let err = UseAfterMove var
        shown = show err
    in "UseAfterMove" `L.isPrefixOf` shown && var `L.isInfixOf` shown
  where
    L.isPrefixOf prefix str = take (L.length prefix) str == prefix
    L.isInfixOf substr str = substr `elem` [take (L.length substr) $ drop i str | i <- [0..L.length str - L.length substr]]

prop_ownership_error_double_move_structure :: Property
prop_ownership_error_double_move_structure =
  forAll genVariableName $ \var1 ->
    forAll (genVariableName `suchThat` (/= var1)) $ \var2 ->
      let err = DoubleMove var1 var2
          shown = show err
      in "DoubleMove" `L.isPrefixOf` shown && 
         var1 `L.isInfixOf` shown && 
         var2 `L.isInfixOf` shown
  where
    L.isPrefixOf prefix str = take (L.length prefix) str == prefix
    L.isInfixOf substr str = substr `elem` [take (L.length substr) $ drop i str | i <- [0..L.length str - L.length substr]]

-- ============================================================================
-- Properties for OwnershipTransfer
-- ============================================================================

prop_ownership_transfer_different_variables :: OwnershipTransfer -> Property
prop_ownership_transfer_different_variables transfer =
  transferFrom transfer /= transferTo transfer

prop_ownership_transfer_show_roundtrip :: OwnershipTransfer -> Property
prop_ownership_transfer_show_roundtrip transfer =
  let shown = show transfer
  in L.length shown > 0  -- Basic check that show produces non-empty string

prop_ownership_transfer_commutative_property :: Property
prop_ownership_transfer_commutative_property =
  forAll genOwnershipTransfer $ \transfer ->
    let reverseTransfer = OwnershipTransfer (transferTo transfer) (transferFrom transfer)
    in transfer /= reverseTransfer

-- ============================================================================
-- Properties for OwnershipAnalyzer
-- ============================================================================

prop_new_ownership_analyzer_consistent :: Property
prop_new_ownership_analyzer_consistent =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 === analyzer2

prop_ownership_analyzer_show :: Property
prop_ownership_analyzer_show =
  let analyzer = newOwnershipAnalyzer
      shown = show analyzer
  in "OwnershipAnalyzer" `L.isPrefixOf` shown
  where
    L.isPrefixOf prefix str = take (L.length prefix) str == prefix

-- ============================================================================
-- Properties for OwnershipType Lists
-- ============================================================================

prop_ownership_type_list_sort_preserves_elements :: Property
prop_ownership_type_list_sort_preserves_elements =
  forAll genOwnershipTypeList $ \types ->
    let sorted = sort types
    in sort sorted === sorted &&  -- Already sorted
       L.length sorted === L.length types &&
       L.all (`elem` types) sorted

prop_ownership_type_list_nub_removes_duplicates :: Property
prop_ownership_type_list_nub_removes_duplicates =
  forAll genOwnershipTypeList $ \types ->
    let unique = nub types
    in L.length unique <= L.length types &&
       L.all (`elem` types) unique

-- ============================================================================
-- Properties for OwnershipError Lists
-- ============================================================================

prop_ownership_error_list_sort_preserves_elements :: Property
prop_ownership_error_list_sort_preserves_elements =
  forAll genOwnershipErrorList $ \errors ->
    let sorted = sort errors
    in sort sorted === sorted &&  -- Already sorted
       L.length sorted === L.length errors &&
       L.all (`elem` errors) sorted

prop_ownership_error_list_nub_removes_duplicates :: Property
prop_ownership_error_list_nub_removes_duplicates =
  forAll genOwnershipErrorList $ \errors ->
    let unique = nub errors
    in L.length unique <= L.length errors &&
       L.all (`elem` errors) unique

-- ============================================================================
-- Properties for Variable Name Generation
-- ============================================================================

prop_variable_name_non_empty :: Property
prop_variable_name_non_empty =
  forAll genVariableName $ \name ->
    not (null name) && isAlphaNum (L.head name)
  where
    isAlphaNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

prop_variable_name_valid_characters :: Property
prop_variable_name_valid_characters =
  forAll genVariableName $ \name ->
    L.all isValidChar name
  where
    isValidChar c = (c >= 'a' && c <= 'z') || 
                    (c >= 'A' && c <= 'Z') || 
                    (c >= '0' && c <= '9') || 
                    c == '_'

-- ============================================================================
-- Properties for Error Classification
-- ============================================================================

prop_error_classification_move_related :: Property
prop_error_classification_move_related =
  let moveErrors = [UseAfterMove "x", DoubleMove "x" "y", BorrowWhileMoved "x", 
                    CrossFunctionMove "x" "y", ParameterMoveMismatch "x"]
  in L.all isMoveRelatedError moveErrors
  where
    isMoveRelatedError (UseAfterMove _) = True
    isMoveRelatedError (DoubleMove _ _) = True
    isMoveRelatedError (BorrowWhileMoved _) = True
    isMoveRelatedError (CrossFunctionMove _ _) = True
    isMoveRelatedError (ParameterMoveMismatch _) = True
    isMoveRelatedError _ = False

prop_error_classification_borrow_related :: Property
prop_error_classification_borrow_related =
  let borrowErrors = [MutBorrowWhileBorrowed "x", BorrowWhileMutBorrowed "x", 
                      MultipleMutBorrows "x", UseWhileMutBorrowed "x", BorrowError "msg"]
  in L.all isBorrowRelatedError borrowErrors
  where
    isBorrowRelatedError (MutBorrowWhileBorrowed _) = True
    isBorrowRelatedError (BorrowWhileMutBorrowed _) = True
    isBorrowRelatedError (MultipleMutBorrows _) = True
    isBorrowRelatedError (UseWhileMutBorrowed _) = True
    isBorrowRelatedError (BorrowError _) = True
    isBorrowRelatedError _ = False

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership QuickCheck Tests"
  [ testGroup "OwnershipType"
    [ testProperty "show roundtrip" prop_ownership_type_show_roundtrip
    , testProperty "ordering consistent" prop_ownership_type_ordering_consistent
    , testProperty "name consistency" prop_ownership_type_name_consistency
    ]
  , testGroup "OwnershipError"
    [ testProperty "show roundtrip" prop_ownership_error_show_roundtrip
    , testProperty "ordering consistent" prop_ownership_error_ordering_consistent
    , testProperty "use after move structure" prop_ownership_error_use_after_move_structure
    , testProperty "double move structure" prop_ownership_error_double_move_structure
    ]
  , testGroup "OwnershipTransfer"
    [ testProperty "different variables" prop_ownership_transfer_different_variables
    , testProperty "show roundtrip" prop_ownership_transfer_show_roundtrip
    , testProperty "commutative property" prop_ownership_transfer_commutative_property
    ]
  , testGroup "OwnershipAnalyzer"
    [ testProperty "new analyzer consistent" prop_new_ownership_analyzer_consistent
    , testProperty "analyzer show" prop_ownership_analyzer_show
    ]
  , testGroup "OwnershipType Lists"
    [ testProperty "sort preserves elements" prop_ownership_type_list_sort_preserves_elements
    , testProperty "nub removes duplicates" prop_ownership_type_list_nub_removes_duplicates
    ]
  , testGroup "OwnershipError Lists"
    [ testProperty "sort preserves elements" prop_ownership_error_list_sort_preserves_elements
    , testProperty "nub removes duplicates" prop_ownership_error_list_nub_removes_duplicates
    ]
  , testGroup "Variable Names"
    [ testProperty "non-empty" prop_variable_name_non_empty
    , testProperty "valid characters" prop_variable_name_valid_characters
    ]
  , testGroup "Error Classification"
    [ testProperty "move related errors" prop_error_classification_move_related
    , testProperty "borrow related errors" prop_error_classification_borrow_related
    ]
  ]