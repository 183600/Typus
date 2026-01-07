module Test.Unit.OwnershipReporterSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


 TestTree
testMultipleErrorsFormatting =             testCase "Multiple errors formatting" $ do
              let errors = 
        [ UseAfterMove "x"
        , DoubleMove "a" "b"
        , BorrowWhileMoved "y"
        , MutBorrowWhileBorrowed "z"
        ]
                                    formatted = formatOwnershipErrors errors
                                    expectedParts = 
        [ "Use after move: x"
        , "Double move: a to b"
        , "Borrow while moved: y"
        , "Mutable borrow while borrowed: z"
        ]
  
  mapM_ (\part -> 
assertBool ("Should contain: " ++ part) (part `L.isInfixOf` formatted)
  ) expectedParts

testErrorTypeCoverage :: TestTree
testErrorTypeCoverage =             testCase "All error types should be formatable" $ do
              let allErrorTypes = 
        [ UseAfterMove "test"
        , DoubleMove "src" "dst"
        , BorrowWhileMoved "test"
        , MutBorrowWhileBorrowed "test"
        , BorrowWhileMutBorrowed "test"
        , MultipleMutBorrows "test"
        , UseWhileMutBorrowed "test"
        , OutOfScope "test"
        , BorrowError "test"
        , ParseError "test message"
        , CrossFunctionMove "src" "dst"
        , ParameterMoveMismatch "param"
        , ControlFlowError "control flow"
        , PathSensitiveError "path sensitive"
        , LoopOwnershipError "loop ownership"
        ]
  
  mapM_ (\error -> do
                let formatted = formatOwnershipErrors [error]
assertBool ("Error should be formatable: " ++ show error) 
               (not $ null formatted)
  ) allErrorTypes

testFormattingProperties :: TestTree
testFormattingProperties =             testProperty "Formatting preserves error information" $
  forAll arbitraryErrorList $ \errors -> do
                let formatted = formatOwnershipErrors errors
return $ not (null formatted) ==> L.length (words formatted) > 0

-- Helper generator for arbitrary errors
arbitraryError :: Gen OwnershipError
                              arbitraryError = elements
  [ UseAfterMove "var"
  , DoubleMove "source" "dest"
  , BorrowWhileMoved "movedVar"
  , MutBorrowWhileBorrowed "borrowedVar"
  , BorrowWhileMutBorrowed "mutBorrowedVar"
  , MultipleMutBorrows "mutVar"
  , UseWhileMutBorrowed "usedVar"
  , OutOfScope "scopeVar"
  , BorrowError "errorVar"
  , ParseError "parse error message"
  , CrossFunctionMove "funcSource" "funcDest"
  , ParameterMoveMismatch "param"
  , ControlFlowError "control flow issue"
  , PathSensitiveError "path sensitive issue"
  , LoopOwnershipError "loop ownership issue"
  ]

arbitraryErrorList :: Gen [OwnershipError]
                              arbitraryErrorList = listOf1 arbitraryError

-- Helper function to check if a string is contained in another
isInfixOf :: String -> String -> Bool
isInfixOf needle                               haystack = needle `elem` words haystack