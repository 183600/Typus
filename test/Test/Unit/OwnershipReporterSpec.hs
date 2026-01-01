module Test.Unit.OwnershipReporterSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Property, forAll, Gen, arbitrary, elements, listOf1)

import Ownership.Common.Types 
  ( OwnershipError(..)
  )
import Ownership.Reporter (formatOwnershipErrors)

tests :: TestTree
tests = testGroup "Ownership.Reporter Tests"
  [ testFormatOwnershipErrors
  , testErrorFormattingConsistency
  , testMultipleErrorsFormatting
  , testErrorTypeCoverage
  , testFormattingProperties
  ]

testFormatOwnershipErrors :: TestTree
testFormatOwnershipErrors = testCase "Format individual ownership errors" $ do
  -- Test UseAfterMove
  let useAfterMove = UseAfterMove "variable"
      formatted1 = formatOwnershipErrors [useAfterMove]
  assertEqual "UseAfterMove formatting" 
    "Use after move: variable" formatted1
  
  -- Test DoubleMove
  let doubleMove = DoubleMove "source" "destination"
      formatted2 = formatOwnershipErrors [doubleMove]
  assertEqual "DoubleMove formatting"
    "Double move: source to destination" formatted2
  
  -- Test BorrowWhileMoved
  let borrowWhileMoved = BorrowWhileMoved "movedVar"
      formatted3 = formatOwnershipErrors [borrowWhileMoved]
  assertEqual "BorrowWhileMoved formatting"
    "Borrow while moved: movedVar" formatted3
  
  -- Test MutBorrowWhileBorrowed
  let mutBorrowWhileBorrowed = MutBorrowWhileBorrowed "borrowedVar"
      formatted4 = formatOwnershipErrors [mutBorrowWhileBorrowed]
  assertEqual "MutBorrowWhileBorrowed formatting"
    "Mutable borrow while borrowed: borrowedVar" formatted4
  
  -- Test BorrowWhileMutBorrowed
  let borrowWhileMutBorrowed = BorrowWhileMutBorrowed "mutBorrowedVar"
      formatted5 = formatOwnershipErrors [borrowWhileMutBorrowed]
  assertEqual "BorrowWhileMutBorrowed formatting"
    "Borrow while mut borrowed: mutBorrowedVar" formatted5
  
  -- Test MultipleMutBorrows
  let multipleMutBorrows = MultipleMutBorrows "mutVar"
      formatted6 = formatOwnershipErrors [multipleMutBorrows]
  assertEqual "MultipleMutBorrows formatting"
    "Multiple mutable borrows: mutVar" formatted6
  
  -- Test UseWhileMutBorrowed
  let useWhileMutBorrowed = UseWhileMutBorrowed "usedVar"
      formatted7 = formatOwnershipErrors [useWhileMutBorrowed]
  assertEqual "UseWhileMutBorrowed formatting"
    "Use while mut borrowed: usedVar" formatted7
  
  -- Test OutOfScope
  let outOfScope = OutOfScope "scopeVar"
      formatted8 = formatOwnershipErrors [outOfScope]
  assertEqual "OutOfScope formatting"
    "Out of scope: scopeVar" formatted8
  
  -- Test BorrowError
  let borrowError = BorrowError "errorVar"
      formatted9 = formatOwnershipErrors [borrowError]
  assertEqual "BorrowError formatting"
    "Borrow error: errorVar" formatted9
  
  -- Test ParseError
  let parseError = ParseError "syntax error"
      formatted10 = formatOwnershipErrors [parseError]
  assertEqual "ParseError formatting"
    "Parse error: syntax error" formatted10
  
  -- Test CrossFunctionMove
  let crossFunctionMove = CrossFunctionMove "funcSource" "funcDest"
      formatted11 = formatOwnershipErrors [crossFunctionMove]
  assertEqual "CrossFunctionMove formatting"
    "Cross-function move: funcSource to funcDest" formatted11
  
  -- Test ParameterMoveMismatch
  let parameterMoveMismatch = ParameterMoveMismatch "param"
      formatted12 = formatOwnershipErrors [parameterMoveMismatch]
  assertEqual "ParameterMoveMismatch formatting"
    "Parameter move mismatch: param" formatted12
  
  -- Test ControlFlowError
  let controlFlowError = ControlFlowError "control flow issue"
      formatted13 = formatOwnershipErrors [controlFlowError]
  assertEqual "ControlFlowError formatting"
    "Control flow error: control flow issue" formatted13
  
  -- Test PathSensitiveError
  let pathSensitiveError = PathSensitiveError "path sensitive issue"
      formatted14 = formatOwnershipErrors [pathSensitiveError]
  assertEqual "PathSensitiveError formatting"
    "Path sensitive error: path sensitive issue" formatted14
  
  -- Test LoopOwnershipError
  let loopOwnershipError = LoopOwnershipError "loop ownership issue"
      formatted15 = formatOwnershipErrors [loopOwnershipError]
  assertEqual "LoopOwnershipError formatting"
    "Loop ownership error: loop ownership issue" formatted15

testErrorFormattingConsistency :: TestTree
testErrorFormattingConsistency = testCase "Error formatting consistency" $ do
  let errors = [UseAfterMove "var1", DoubleMove "src" "dst"]
      formatted = formatOwnershipErrors errors
  
  assertBool "Should contain both errors" 
    ("Use after move: var1" `L.isInfixOf` formatted && 
     "Double move: src to dst" `L.isInfixOf` formatted)
  assertBool "Should separate errors with semicolon" (';' `elem` formatted)

testMultipleErrorsFormatting :: TestTree
testMultipleErrorsFormatting = testCase "Multiple errors formatting" $ do
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
testErrorTypeCoverage = testCase "All error types should be formatable" $ do
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
testFormattingProperties = testProperty "Formatting preserves error information" $
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
L.isInfixOf needle haystack = needle `elem` words haystack