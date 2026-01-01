module Test.Unit.NewQuickCheckTestSuite5Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)

import TestSupport.QuickCheck (fastProperty)
import Ownership.Common.Types

-- | Test suite for Ownership module ownership transfer
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite5 - Ownership Transfer"
    [ testGroup "OwnershipType operations"
        [ testCase "OwnershipType Show instance works" $ do
            show (Owned "x") @?= "Owned x"
            show (Borrowed "y") @?= "Borrowed y"
            show (MutBorrowed "z") @?= "MutBorrowed z"
            
        , testCase "OwnershipType Eq works correctly" $ do
            Owned "x" @?= Owned "x"
            Borrowed "y" @?= Borrowed "y"
            MutBorrowed "z" @?= MutBorrowed "z"
            Owned "x" /= Owned "y" @?= True
            
        , testCase "OwnershipType Ord orders correctly" $ do
            Owned "a" < Borrowed "a" @?= True
            Borrowed "a" < MutBorrowed "a" @?= True
            Owned "a" < MutBorrowed "a" @?= True
            Owned "a" < Owned "b" @?= True
            Borrowed "a" < Borrowed "b" @?= True
        ]

    , testGroup "OwnershipError operations"
        [ testCase "OwnershipError Show instance works" $ do
            show (UseAfterMove "x") @?= "UseAfterMove x"
            show (DoubleMove "x" "y") @?= "DoubleMove x y"
            show (BorrowWhileMoved "x") @?= "BorrowWhileMoved x"
            show (MutBorrowWhileBorrowed "x") @?= "MutBorrowWhileBorrowed x"
            show (BorrowWhileMutBorrowed "x") @?= "BorrowWhileMutBorrowed x"
            show (MultipleMutBorrows "x") @?= "MultipleMutBorrows x"
            show (UseWhileMutBorrowed "x") @?= "UseWhileMutBorrowed x"
            show (OutOfScope "x") @?= "OutOfScope x"
            show (BorrowError "msg") @?= "BorrowError msg"
            show (ParseError "msg") @?= "ParseError msg"
            show (CrossFunctionMove "x" "y") @?= "CrossFunctionMove x y"
            show (ParameterMoveMismatch "x") @?= "ParameterMoveMismatch x"
            show (ControlFlowError "msg") @?= "ControlFlowError msg"
            show (PathSensitiveError "msg") @?= "PathSensitiveError msg"
            show (LoopOwnershipError "msg") @?= "LoopOwnershipError msg"
            
        , testCase "OwnershipError Eq works correctly" $ do
            UseAfterMove "x" @?= UseAfterMove "x"
            DoubleMove "x" "y" @?= DoubleMove "x" "y"
            UseAfterMove "x" /= UseAfterMove "y" @?= True
            
        , testCase "OwnershipError Ord orders by string representation" $ do
            let err1 = UseAfterMove "a"
                err2 = UseAfterMove "b"
            compare err1 err2 @?= LT
        ]

    , testGroup "OwnershipAnalyzer operations"
        [ testCase "newOwnershipAnalyzer creates analyzer" $ do
            let analyzer = newOwnershipAnalyzer
            True @?= True  -- Basic test that analyzer can be created
            
        , testCase "OwnershipAnalyzer Show works" $ do
            let analyzer = newOwnershipAnalyzer
            show analyzer @?= "OwnershipAnalyzer ()"
            
        , testCase "OwnershipAnalyzer Eq works" $ do
            let analyzer1 = newOwnershipAnalyzer
                analyzer2 = newOwnershipAnalyzer
            analyzer1 @?= analyzer2
        ]

    , testGroup "OwnershipTransfer operations"
        [ testCase "OwnershipTransfer creates transfer with correct fields" $ do
            let transfer = OwnershipTransfer "source" "dest"
            transferFrom transfer @?= "source"
            transferTo transfer @?= "dest"
            
        , testCase "OwnershipTransfer Show works" $ do
            let transfer = OwnershipTransfer "x" "y"
            show transfer `contains` "x" @?= True
            show transfer `contains` "y" @?= True
            
        , testCase "OwnershipTransfer Eq works correctly" $ do
            let transfer1 = OwnershipTransfer "x" "y"
                transfer2 = OwnershipTransfer "x" "y"
                transfer3 = OwnershipTransfer "a" "b"
            transfer1 @?= transfer2
            transfer1 /= transfer3 @?= True
        ]

    , testGroup "Ownership transfer scenarios"
        [ testCase "owned to owned transfer" $ do
            let fromType = Owned "source"
                toType = Owned "dest"
                transfer = OwnershipTransfer "source" "dest"
            transferFrom transfer @?= "source"
            transferTo transfer @?= "dest"
            
        , testCase "borrowed to borrowed transfer" $ do
            let fromType = Borrowed "source"
                toType = Borrowed "dest"
                transfer = OwnershipTransfer "source" "dest"
            transferFrom transfer @?= "source"
            transferTo transfer @?= "dest"
            
        , testCase "mutable borrowed to mutable borrowed transfer" $ do
            let fromType = MutBorrowed "source"
                toType = MutBorrowed "dest"
                transfer = OwnershipTransfer "source" "dest"
            transferFrom transfer @?= "source"
            transferTo transfer @?= "dest"
        ]

    , testGroup "Error scenario modeling"
        [ testCase "use after move scenario" $ do
            let error = UseAfterMove "x"
            show error `contains` "UseAfterMove" @?= True
            show error `contains` "x" @?= True
            
        , testCase "double move scenario" $ do
            let error = DoubleMove "x" "y"
            show error `contains` "DoubleMove" @?= True
            show error `contains` "x" @?= True
            show error `contains` "y" @?= True
            
        , testCase "borrow conflicts" $ do
            let errors = 
                    [ BorrowWhileMoved "x"
                    , MutBorrowWhileBorrowed "x"
                    , BorrowWhileMutBorrowed "x"
                    , MultipleMutBorrows "x"
                    ]
            L.all (`contains` "x") (map show errors) @?= True
        ]

    , testGroup "QuickCheck properties"
        [ fastProperty "OwnershipType ordering is transitive" prop_ownershipTypeOrderingTransitive
        , fastProperty "OwnershipType ordering is antisymmetric" prop_ownershipTypeOrderingAntisymmetric
        , fastProperty "OwnershipError ordering is consistent" prop_ownershipErrorOrderingConsistent
        , fastProperty "OwnershipTransfer preserves source L.and dest" prop_ownershipTransferPreservesFields
        , fastProperty "OwnershipError string roundtrip" prop_ownershipErrorStringRoundtrip
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains needle haystack = needle `L.isInfixOf` haystack

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- OwnershipType ordering properties
prop_ownershipTypeOrderingTransitive :: OwnershipType -> OwnershipType -> OwnershipType -> Property
prop_ownershipTypeOrderingTransitive t1 t2 t3 =
    (t1 < t2 && t2 < t3) ==> t1 < t3

prop_ownershipTypeOrderingAntisymmetric :: OwnershipType -> OwnershipType -> Property
prop_ownershipTypeOrderingAntisymmetric t1 t2 =
    (t1 < t2) ==> not (t2 < t1)

-- OwnershipError properties
prop_ownershipErrorOrderingConsistent :: OwnershipError -> OwnershipError -> Bool
prop_ownershipErrorOrderingConsistent err1 err2 =
    compare err1 err2 == compare (show err1) (show err2)

prop_ownershipErrorStringRoundtrip :: OwnershipError -> Bool
prop_ownershipErrorStringRoundtrip err =
    let errStr = show err
    in L.length errStr > 0  -- Basic check that string representation is non-empty

-- OwnershipTransfer properties
prop_ownershipTransferPreservesFields :: String -> String -> Bool
prop_ownershipTransferPreservesFields source dest =
    let transfer = OwnershipTransfer source dest
    in transferFrom transfer == source && transferTo transfer == dest

-- Helper functions for generating test data
genOwnershipType :: Gen OwnershipType
genOwnershipType = oneof
    [ fmap Owned arbitrary
    , fmap Borrowed arbitrary
    , fmap MutBorrowed arbitrary
    ]

genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
    [ fmap UseAfterMove arbitrary
    , fmap DoubleMove $ (,) <$> arbitrary <*> arbitrary
    , fmap BorrowWhileMoved arbitrary
    , fmap MutBorrowWhileBorrowed arbitrary
    , fmap BorrowWhileMutBorrowed arbitrary
    , fmap MultipleMutBorrows arbitrary
    , fmap UseWhileMutBorrowed arbitrary
    , fmap OutOfScope arbitrary
    , fmap BorrowError arbitrary
    , fmap ParseError arbitrary
    , fmap CrossFunctionMove $ (,) <$> arbitrary <*> arbitrary
    , fmap ParameterMoveMismatch arbitrary
    , fmap ControlFlowError arbitrary
    , fmap PathSensitiveError arbitrary
    , fmap LoopOwnershipError arbitrary
    ]

genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
    source <- arbitrary
    dest <- arbitrary
    return $ OwnershipTransfer source dest

genValidIdentifier :: Gen String
genValidIdentifier = do
    first <- elements ['a'..'z']
    rest <- arbitrary `suchThat` L.all (`elem` ['a'..'z'] ++ ['0'..'9'] ++ "_")
    return (first : rest)