module Test.Unit.OwnershipTransferQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import Data.List (sort, nub)
import Ownership.Common.Types

-- | Generate arbitrary ownership types
instance Arbitrary OwnershipType where
  arbitrary = do
    name <- listOf1 (elements ['a'..'z', 'A'..'Z', '0'..'9', '_'])
    frequency
      [ (3, return $ Owned name)
      , (3, return $ Borrowed name)
      , (2, return $ MutBorrowed name)
      ]

-- | Generate arbitrary ownership errors
instance Arbitrary OwnershipError where
  arbitrary = frequency
    [ (2, do var <- listOf1 (elements ['a'..'z'])
            return $ UseAfterMove var)
    , (2, do var1 <- listOf1 (elements ['a'..'z'])
            var2 <- listOf1 (elements ['a'..'z'])
            return $ DoubleMove var1 var2)
    , (1, do var <- listOf1 (elements ['a'..'z'])
            return $ BorrowWhileMoved var)
    , (1, do var <- listOf1 (elements ['a'..'z'])
            return $ MutBorrowWhileBorrowed var)
    , (1, do var <- listOf1 (elements ['a'..'z'])
            return $ BorrowWhileMutBorrowed var)
    , (1, do var <- listOf1 (elements ['a'..'z'])
            return $ MultipleMutBorrows var)
    , (1, do var <- listOf1 (elements ['a'..'z'])
            return $ UseWhileMutBorrowed var)
    , (1, do var <- listOf1 (elements ['a'..'z'])
            return $ OutOfScope var)
    , (1, do msg <- listOf1 (elements ['a'..'z', ' '])
            return $ BorrowError msg)
    , (1, do msg <- listOf1 (elements ['a'..'z', ' '])
            return $ ParseError msg)
    , (1, do var1 <- listOf1 (elements ['a'..'z'])
            var2 <- listOf1 (elements ['a'..'z'])
            return $ CrossFunctionMove var1 var2)
    , (1, do var <- listOf1 (elements ['a'..'z'])
            return $ ParameterMoveMismatch var)
    , (1, do msg <- listOf1 (elements ['a'..'z', ' '])
            return $ ControlFlowError msg)
    , (1, do msg <- listOf1 (elements ['a'..'z', ' '])
            return $ PathSensitiveError msg)
    , (1, do msg <- listOf1 (elements ['a'..'z', ' '])
            return $ LoopOwnershipError msg)
    ]

-- | Generate arbitrary ownership transfers
instance Arbitrary OwnershipTransfer where
  arbitrary = do
    from <- listOf1 (elements ['a'..'z', 'A'..'Z', '0'..'9', '_'])
    to <- listOf1 (elements ['a'..'z', 'A'..'Z', '0'..'9', '_'])
    return $ OwnershipTransfer from to

-- | Generate variable names
genVariableName :: Gen String
genVariableName = listOf1 $ elements ['a'..'z', 'A'..'Z', '_']

-- | Generate ownership types for specific variables
genOwnershipTypeFor :: String -> Gen OwnershipType
genOwnershipTypeFor var = do
  frequency
    [ (3, return $ Owned var)
    , (3, return $ Borrowed var)
    , (2, return $ MutBorrowed var)
    ]

tests :: TestTree
tests =
  testGroup "Ownership transfer QuickCheck tests"
    [ testGroup "OwnershipType properties"
        [ testCase "ownership type ordering is consistent" $ do
            let owned = Owned "x"
                borrowed = Borrowed "x"
                mutBorrowed = MutBorrowed "x"
            owned < borrowed @?= True
            borrowed < mutBorrowed @?= True
            owned < mutBorrowed @?= True

        , fastProperty "ownership type ordering is transitive" $
            \type1 type2 type3 ->
              type1 <= type2 && type2 <= type3 ==> type1 <= type3

        , fastProperty "ownership type ordering is total" $
            \type1 type2 ->
              type1 <= type2 || type2 <= type1

        , fastProperty "same ownership types are equal" $
            \name ->
              let owned1 = Owned name
                  owned2 = Owned name
              in owned1 == owned2

        , fastProperty "different names create different ownership types" $
            \name1 name2 ->
              name1 /= name2 ==>
                Owned name1 /= Owned name2

        , testCase "ownership type show representation" $ do
            show (Owned "x") @?= "Owned x"
            show (Borrowed "y") @?= "Borrowed y"
            show (MutBorrowed "z") @?= "MutBorrowed z"

        , fastProperty "ownership type show is invertible for simple cases" $
            \name ->
              let owned = Owned name
                  shown = show owned
              in "Owned " ++ name `isPrefixOf` shown

        , fastProperty "ownership type preserves variable name" $
            \name ->
              let owned = Owned name
                  borrowed = Borrowed name
                  mutBorrowed = MutBorrowed name
              in case owned of
                   Owned n -> n == name
                   _ -> False &&
               case borrowed of
                 Borrowed n -> n == name
                 _ -> False &&
               case mutBorrowed of
                 MutBorrowed n -> n == name
                 _ -> False
        ]

    , testGroup "OwnershipTransfer properties"
        [ testCase "ownership transfer preserves fields" $ do
            let transfer = OwnershipTransfer "from" "to"
            transferFrom transfer @?= "from"
            transferTo transfer @?= "to"

        , fastProperty "ownership transfer equality" $
            \from1 to1 from2 to2 ->
              let transfer1 = OwnershipTransfer from1 to1
                  transfer2 = OwnershipTransfer from2 to2
              in transfer1 == transfer2 == (from1 == from2 && to1 == to2)

        , fastProperty "ownership transfer show representation" $
            \from to ->
              let transfer = OwnershipTransfer from to
                  shown = show transfer
              in from `isInfixOf` shown && to `isInfixOf` shown

        , fastProperty "self-transfer is valid" $
            \var ->
              let transfer = OwnershipTransfer var var
              in transferFrom transfer == transferTo transfer

        , fastProperty "transfer direction matters" $
            \from to ->
              from /= to ==>
                let transfer1 = OwnershipTransfer from to
                    transfer2 = OwnershipTransfer to from
                in transfer1 /= transfer2
        ]

    , testGroup "OwnershipError properties"
        [ testCase "error types are distinguishable" $ do
            let useAfterMove = UseAfterMove "x"
                doubleMove = DoubleMove "x" "y"
                borrowWhileMoved = BorrowWhileMoved "x"
            useAfterMove /= doubleMove @?= True
            doubleMove /= borrowWhileMoved @?= True
            useAfterMove /= borrowWhileMoved @?= True

        , fastProperty "error ordering is consistent" $
            \error1 error2 ->
              let comp1 = compare error1 error2
                  comp2 = compare (show error1) (show error2)
              in comp1 == comp2

        , fastProperty "error ordering is total" $
            \error1 error2 ->
              let comp = compare error1 error2
              in comp == LT || comp == EQ || comp == GT

        , fastProperty "error show representation contains relevant information" $
            \error ->
              let shown = show error
              in case error of
                   UseAfterMove var -> var `isInfixOf` shown
                   DoubleMove var1 var2 -> var1 `isInfixOf` shown && var2 `isInfixOf` shown
                   BorrowWhileMoved var -> var `isInfixOf` shown
                   MutBorrowWhileBorrowed var -> var `isInfixOf` shown
                   BorrowWhileMutBorrowed var -> var `isInfixOf` shown
                   MultipleMutBorrows var -> var `isInfixOf` shown
                   UseWhileMutBorrowed var -> var `isInfixOf` shown
                   OutOfScope var -> var `isInfixOf` shown
                   BorrowError msg -> msg `isInfixOf` shown
                   ParseError msg -> msg `isInfixOf` shown
                   CrossFunctionMove var1 var2 -> var1 `isInfixOf` shown && var2 `isInfixOf` shown
                   ParameterMoveMismatch var -> var `isInfixOf` shown
                   ControlFlowError msg -> msg `isInfixOf` shown
                   PathSensitiveError msg -> msg `isInfixOf` shown
                   LoopOwnershipError msg -> msg `isInfixOf` shown

        , fastProperty "same errors are equal" $
            \error ->
              error == error

        , fastProperty "error equality is symmetric" $
            \error1 error2 ->
              (error1 == error2) == (error2 == error1)

        , fastProperty "error equality is transitive" $
            \error1 error2 error3 ->
              error1 == error2 && error2 == error3 ==> error1 == error3
        ]

    , testGroup "OwnershipAnalyzer properties"
        [ testCase "analyzer creation" $ do
            let analyzer = newOwnershipAnalyzer
            analyzer @?= OwnershipAnalyzer ()

        , testCase "analyzer show representation" $ do
            let analyzer = newOwnershipAnalyzer
            show analyzer @?= "OwnershipAnalyzer ()"

        , fastProperty "analyzer equality" $
            \_ ->
              let analyzer1 = newOwnershipAnalyzer
                  analyzer2 = newOwnershipAnalyzer
              in analyzer1 == analyzer2
        ]

    , testGroup "Complex ownership scenarios"
        [ fastProperty "ownership transfer chain" $
            \vars ->
              let transfers = zipWith OwnershipTransfer vars (tail vars ++ [head vars])
              in length transfers == length vars &&
                 all (\t -> transferFrom t `elem` vars && transferTo t `elem` vars) transfers

        , fastProperty "multiple ownership types for same variable" $
            \var ->
              let owned = Owned var
                  borrowed = Borrowed var
                  mutBorrowed = MutBorrowed var
              in owned /= borrowed && borrowed /= mutBorrowed && owned /= mutBorrowed

        , fastProperty "error types cover all ownership violations" $
            \var ->
              let errors = 
                    [ UseAfterMove var
                    , DoubleMove var var
                    , BorrowWhileMoved var
                    , MutBorrowWhileBorrowed var
                    , BorrowWhileMutBorrowed var
                    , MultipleMutBorrows var
                    , UseWhileMutBorrowed var
                    , OutOfScope var
                    ]
              in all (\e -> var `isInfixOf` show e) errors

        , fastProperty "ownership transfer preserves variable identity" $
            \from to ->
              let transfer = OwnershipTransfer from to
              in transferFrom transfer == from && transferTo transfer == to

        , fastProperty "ownership type hierarchy" $
            \var ->
              let owned = Owned var
                  borrowed = Borrowed var
                  mutBorrowed = MutBorrowed var
              in owned < borrowed && borrowed < mutBorrowed &&
                 owned < mutBorrowed
        ]

    , testGroup "Boundary conditions"
        [ testCase "empty variable names" $ do
            let owned = Owned ""
                borrowed = Borrowed ""
                mutBorrowed = MutBorrowed ""
            show owned @?= "Owned "
            show borrowed @?= "Borrowed "
            show mutBorrowed @?= "MutBorrowed "

        , testCase "very long variable names" $ do
            let longName = replicate 1000 'x'
                owned = Owned longName
            length (show owned) @?= length ("Owned " ++ longName)

        , fastProperty "special characters in variable names" $
            \name ->
              let owned = Owned name
                  shown = show owned
              in "Owned " `isPrefixOf` shown

        , testCase "ownership transfer with same variables" $ do
            let transfer = OwnershipTransfer "same" "same"
            transferFrom transfer @?= "same"
            transferTo transfer @?= "same"

        , fastProperty "error messages with empty strings" $ do
            let borrowError = BorrowError ""
                parseError = ParseError ""
                controlFlowError = ControlFlowError ""
                pathSensitiveError = PathSensitiveError ""
                loopOwnershipError = LoopOwnershipError ""
            show borrowError @?= "BorrowError "
            show parseError @?= "ParseError "
            show controlFlowError @?= "ControlFlowError "
            show pathSensitiveError @?= "PathSensitiveError "
            show loopOwnershipError @?= "LoopOwnershipError "
        ]
    ]

-- Helper function for prefix check
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Helper function for infix check
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys