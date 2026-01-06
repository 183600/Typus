module Test.Unit.OwnershipAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck ((===), property, testProperty, Property, forAll, Gen, choose, arbitrary, listOf, elements, oneof, suchThat)
import TestSupport.QuickCheck (fastProperty)

import Ownership.Common.Types (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), 
                              OwnershipTransfer(..), newOwnershipAnalyzer)
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing)
import Data.Ord (comparing)

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate variable names
genVarName :: Gen String
genVarName = do
  first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

-- Generate ownership types
genOwnershipType :: Gen OwnershipType
genOwnershipType = oneof
  [ Owned <$> genVarName
  , Borrowed <$> genVarName
  , MutBorrowed <$> genVarName
  ]

-- Generate ownership errors
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVarName
  , DoubleMove <$> genVarName <*> genVarName
  , BorrowWhileMoved <$> genVarName
  , MutBorrowWhileBorrowed <$> genVarName
  , BorrowWhileMutBorrowed <$> genVarName
  , MultipleMutBorrows <$> genVarName
  , UseWhileMutBorrowed <$> genVarName
  , OutOfScope <$> genVarName
  , BorrowError <$> arbitrary
  , ParseError <$> arbitrary
  , CrossFunctionMove <$> genVarName <*> genVarName
  , ParameterMoveMismatch <$> genVarName
  , ControlFlowError <$> arbitrary
  , PathSensitiveError <$> arbitrary
  , LoopOwnershipError <$> arbitrary
  ]

-- Generate ownership transfers
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = OwnershipTransfer <$> genVarName <*> genVarName

-- Generate lists of ownership types
genOwnershipTypeList :: Gen [OwnershipType]
genOwnershipTypeList = listOf genOwnershipType

-- Generate lists of ownership errors
genOwnershipErrorList :: Gen [OwnershipError]
genOwnershipErrorList = listOf genOwnershipError

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: OwnershipType equality is reflexive
prop_ownershipTypeReflexive :: OwnershipType -> Bool
prop_ownershipTypeReflexive ot = ot == ot

-- Property: OwnershipType equality is symmetric
prop_ownershipTypeSymmetric :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeSymmetric ot1 ot2 = (ot1 == ot2) == (ot2 == ot1)

-- Property: OwnershipType equality is transitive
prop_ownershipTypeTransitive :: OwnershipType -> OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeTransitive ot1 ot2 ot3 =
  (ot1 == ot2 && ot2 == ot3) ==> (ot1 == ot3)

-- Property: OwnershipType ordering is consistent
prop_ownershipTypeOrderingConsistent :: OwnershipType -> OwnershipType -> Bool
prop_ownershipTypeOrderingConsistent ot1 ot2 =
  let ordering = compare ot1 ot2
  in case (ot1, ot2) of
       (Owned _, Owned _) -> ordering == comparing (show) ot1 ot2
       (Owned _, _) -> ordering == LT
       (Borrowed _, Borrowed _) -> ordering == comparing (show) ot1 ot2
       (Borrowed _, MutBorrowed _) -> ordering == LT
       (Borrowed _, Owned _) -> ordering == GT
       (MutBorrowed _, MutBorrowed _) -> ordering == comparing (show) ot1 ot2
       (MutBorrowed _, _) -> ordering == GT

-- Property: OwnershipError equality is reflexive
prop_ownershipErrorReflexive :: OwnershipError -> Bool
prop_ownershipErrorReflexive oe = oe == oe

-- Property: OwnershipError equality is symmetric
prop_ownershipErrorSymmetric :: OwnershipError -> OwnershipError -> Bool
prop_ownershipErrorSymmetric oe1 oe2 = (oe1 == oe2) == (oe2 == oe1)

-- Property: OwnershipError equality is transitive
prop_ownershipErrorTransitive :: OwnershipError -> OwnershipError -> OwnershipError -> Bool
prop_ownershipErrorTransitive oe1 oe2 oe3 =
  (oe1 == oe2 && oe2 == oe3) ==> (oe1 == oe3)

-- Property: OwnershipTransfer preserves from L.and to fields
prop_ownershipTransferPreservesFields :: String -> String -> Bool
prop_ownershipTransferPreservesFields from to =
  let transfer = OwnershipTransfer from to
  in transferFrom transfer == from && transferTo transfer == to

-- Property: OwnershipTransfer equality is based on fields
prop_ownershipTransferEquality :: String -> String -> String -> String -> Bool
prop_ownershipTransferEquality from1 to1 from2 to2 =
  let transfer1 = OwnershipTransfer from1 to1
      transfer2 = OwnershipTransfer from2 to2
  in (transfer1 == transfer2) == (from1 == from2 && to1 == to2)

-- Property: Show instances produce non-empty strings
prop_ownershipTypeShowNonEmpty :: OwnershipType -> Bool
prop_ownershipTypeShowNonEmpty ot = not (L.null (show ot))

-- Property: Show instances produce non-empty strings for errors
prop_ownershipErrorShowNonEmpty :: OwnershipError -> Bool
prop_ownershipErrorShowNonEmpty oe = not (L.null (show oe))

-- Property: Show instances produce non-empty strings for transfers
prop_ownershipTransferShowNonEmpty :: OwnershipTransfer -> Bool
prop_ownershipTransferShowNonEmpty ot = not (L.null (show ot))

-- Property: OwnershipAnalyzer constructor produces consistent result
prop_ownershipAnalyzerConsistent :: Bool
prop_ownershipAnalyzerConsistent =
  let analyzer1 = newOwnershipAnalyzer
      analyzer2 = newOwnershipAnalyzer
  in analyzer1 == analyzer2

-- Property: Sorting ownership types preserves order properties
prop_sortOwnershipTypesPreservesOrder :: [OwnershipType] -> Bool
prop_sortOwnershipTypesPreservesTypes ots =
  let sorted = sort ots
  in L.length sorted == L.length ots && L.all (`elem` ots) sorted

-- Property: Sorting ownership errors preserves order properties
prop_sortOwnershipErrorsPreservesOrder :: [OwnershipError] -> Bool
prop_sortOwnershipErrorsPreservesOrder oes =
  let sorted = sort oes
  in L.length sorted == L.length oes && L.all (`elem` oes) sorted

-- Property: Unique ownership types can be deduplicated
prop_uniqueOwnershipTypes :: [OwnershipType] -> Bool
prop_uniqueOwnershipTypes ots =
  let unique = nub ots
  in L.length unique <= L.length ots && L.all (`elem` ots) unique

-- Property: Unique ownership errors can be deduplicated
prop_uniqueOwnershipErrors :: [OwnershipError] -> Bool
prop_uniqueOwnershipErrors oes =
  let unique = nub oes
  in L.length unique <= L.length oes && L.all (`elem` oes) unique

-- Property: OwnershipType constructors produce valid types
prop_ownedConstructorValid :: String -> Bool
prop_ownedConstructorValid name = not (null name) ==> 
  let owned = Owned name
  in case owned of
       Owned n -> n == name
       _ -> False

prop_borrowedConstructorValid :: String -> Bool
prop_borrowedConstructorValid name = not (null name) ==>
  let borrowed = Borrowed name
  in case borrowed of
       Borrowed n -> n == name
       _ -> False

prop_mutBorrowedConstructorValid :: String -> Bool
prop_mutBorrowedConstructorValid name = not (null name) ==>
  let mutBorrowed = MutBorrowed name
  in case mutBorrowed of
       MutBorrowed n -> n == name
       _ -> False

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Ownership Advanced QuickCheck Tests"
  [ testGroup "OwnershipType Properties"
    [ testProperty "OwnershipType equality is reflexive" prop_ownershipTypeReflexive
    , testProperty "OwnershipType equality is symmetric" prop_ownershipTypeSymmetric
    , testProperty "OwnershipType equality is transitive" prop_ownershipTypeTransitive
    , testProperty "OwnershipType ordering is consistent" prop_ownershipTypeOrderingConsistent
    , testProperty "Show instances produce non-empty strings" prop_ownershipTypeShowNonEmpty
    , testProperty "Owned constructor produces valid types" prop_ownedConstructorValid
    , testProperty "Borrowed constructor produces valid types" prop_borrowedConstructorValid
    , testProperty "MutBorrowed constructor produces valid types" prop_mutBorrowedConstructorValid
    ]

  , testGroup "OwnershipError Properties"
    [ testProperty "OwnershipError equality is reflexive" prop_ownershipErrorReflexive
    , testProperty "OwnershipError equality is symmetric" prop_ownershipErrorSymmetric
    , testProperty "OwnershipError equality is transitive" prop_ownershipErrorTransitive
    , testProperty "Show instances produce non-empty strings for errors" prop_ownershipErrorShowNonEmpty
    ]

  , testGroup "OwnershipTransfer Properties"
    [ testProperty "OwnershipTransfer preserves from L.and to fields" prop_ownershipTransferPreservesFields
    , testProperty "OwnershipTransfer equality is based on fields" prop_ownershipTransferEquality
    , testProperty "Show instances produce non-empty strings for transfers" prop_ownershipTransferShowNonEmpty
    ]

  , testGroup "OwnershipAnalyzer Properties"
    [ testProperty "OwnershipAnalyzer constructor produces consistent result" prop_ownershipAnalyzerConsistent
    ]

  , testGroup "Collection Properties"
    [ testProperty "Sorting ownership types preserves order properties" prop_sortOwnershipTypesPreservesOrder
    , testProperty "Sorting ownership errors preserves order properties" prop_sortOwnershipErrorsPreservesOrder
    , testProperty "Unique ownership types can be deduplicated" prop_uniqueOwnershipTypes
    , testProperty "Unique ownership errors can be deduplicated" prop_uniqueOwnershipErrors
    ]

  , testGroup "Unit Tests"
    [ testCase "Create Owned type" $ do
        let owned = Owned "x"
        case owned of
          Owned name -> name @?= "x"
          _ -> assertBool "Should be Owned" False

    , testCase "Create Borrowed type" $ do
        let borrowed = Borrowed "x"
        case borrowed of
          Borrowed name -> name @?= "x"
          _ -> assertBool "Should be Borrowed" False

    , testCase "Create MutBorrowed type" $ do
        let mutBorrowed = MutBorrowed "x"
        case mutBorrowed of
          MutBorrowed name -> name @?= "x"
          _ -> assertBool "Should be MutBorrowed" False

    , testCase "OwnershipType ordering" $ do
        let owned = Owned "x"
            borrowed = Borrowed "x"
            mutBorrowed = MutBorrowed "x"
        compare owned borrowed @?= LT
        compare borrowed mutBorrowed @?= LT
        compare mutBorrowed owned @?= GT

    , testCase "Create UseAfterMove error" $ do
        let error = UseAfterMove "x"
        case error of
          UseAfterMove var -> var @?= "x"
          _ -> assertBool "Should be UseAfterMove" False

    , testCase "Create DoubleMove error" $ do
        let error = DoubleMove "x" "y"
        case error of
          DoubleMove var1 var2 -> do
            var1 @?= "x"
            var2 @?= "y"
          _ -> assertBool "Should be DoubleMove" False

    , testCase "Create BorrowError" $ do
        let error = BorrowError "message"
        case error of
          BorrowError msg -> msg @?= "message"
          _ -> assertBool "Should be BorrowError" False

    , testCase "Create OwnershipTransfer" $ do
        let transfer = OwnershipTransfer "from" "to"
        transferFrom transfer @?= "from"
        transferTo transfer @?= "to"

    , testCase "OwnershipTransfer equality" $ do
        let transfer1 = OwnershipTransfer "a" "b"
            transfer2 = OwnershipTransfer "a" "b"
            transfer3 = OwnershipTransfer "a" "c"
        transfer1 @?= transfer2
        assertBool "Different transfers should not be equal" $ transfer1 /= transfer3

    , testCase "Create OwnershipAnalyzer" $ do
        let analyzer = newOwnershipAnalyzer
        case analyzer of
          OwnershipAnalyzer () -> return ()
          _ -> assertBool "Should be OwnershipAnalyzer" False

    , testCase "Show instances" $ do
        let owned = Owned "x"
            borrowed = Borrowed "y"
            mutBorrowed = MutBorrowed "z"
            error = UseAfterMove "x"
            transfer = OwnershipTransfer "a" "b"
        show owned @?= "Owned x"
        show borrowed @?= "Borrowed y"
        show mutBorrowed @?= "MutBorrowed z"
        show error @?= "UseAfterMove x"
        show transfer @?= "OwnershipTransfer {transferFrom = \"a\", transferTo = \"b\"}"

    , testCase "Sorting ownership types" $ do
        let types = [MutBorrowed "z", Owned "x", Borrowed "y"]
            sorted = sort types
        sorted @?= [Owned "x", Borrowed "y", MutBorrowed "z"]

    , testCase "Sorting ownership errors" $ do
        let errors = [UseAfterMove "z", DoubleMove "x" "y", BorrowError "msg"]
            sorted = sort errors
        sorted @?= [BorrowError "msg", DoubleMove "x" "y", UseAfterMove "z"]

    , testCase "Deduplicating ownership types" $ do
        let types = [Owned "x", Borrowed "y", Owned "x", MutBorrowed "z"]
            unique = nub types
        unique @?= [Owned "x", Borrowed "y", MutBorrowed "z"]

    , testCase "Deduplicating ownership errors" $ do
        let errors = [UseAfterMove "x", BorrowError "msg", UseAfterMove "x"]
            unique = nub errors
        unique @?= [UseAfterMove "x", BorrowError "msg"]
    ]
  ]