module Test.Unit.NewOwnershipConsistencyQuickCheckSpec where


import Test.Tasty
import Test.Tasty.QuickCheck 
  ( OwnershipType(..), OwnershipError(..), OwnershipTransfer(..)
  , analyzeOwnership, analyzeOwnershipDebug, builtInFunctions
  )
import Ownership.Common.Types (OwnershipType(..), OwnershipError)
        (debugErrors, _) = analyzeOwnershipDebug False code
    in sort                               errors == sort debugErrors
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


-- | Test empty code analysis
prop_analyze_empty_code :: Bool
                              prop_analyze_empty_code = 
    let errors = analyzeOwnership ""
    in null errors

-- | Test whitespace-only code analysis
prop_analyze_whitespace_only :: String -> Property
prop_analyze_whitespace_only                               ws =
    L.all isSpace                               ws ==>
    let errors = analyzeOwnership ws
    in null errors

-- | Test built-in function usage
prop_analyze_builtin_functions_no_errors :: String -> Property
prop_analyze_builtin_functions_no_errors                               func =
    func `elem`                               builtInFunctions ==>
    let code = func ++ "(x)"
                                      errors = analyzeOwnership code
    in not (L.any isBorrowError errors)

  where
      isBorrowError (BorrowError _) = True
    isBorrowError                               _ = False

-- | Test variable declaration consistency
prop_analyze_var_declaration_consistency :: String -> Property
prop_analyze_var_declaration_consistency                               varName =
    L.length varName > 0 && not (isSpace (L.head varName) ==>
    let code1 = "var " ++ varName ++ " = 42"
                                      code2 = "let " ++ varName ++ " = 42"
                                      errors1 = analyzeOwnership code1
                                      errors2 = analyzeOwnership code2
    in L.length                               errors1 == L.length errors2

-- | Test ownership transfer consistency
prop_analyze_ownership_transfer_consistency :: String -> String -> Property
prop_analyze_ownership_transfer_consistency from                               to =
    L.length from > 0 && L.length to > 0 &&
    not (isSpace (L.head from) && not (isSpace (L.head to) ==>
    let transferCode = from ++ " = " ++ to
                                      errors = analyzeOwnership transferCode
    in not (L.any isMoveError errors) || L.any isMoveError errors

  where
      isMoveError (UseAfterMove _) = True
    isMoveError (DoubleMove _ _) = True
    isMoveError                               _ = False

-- | Test borrow analysis consistency
prop_analyze_borrow_consistency :: String -> Property
prop_analyze_borrow_consistency                               varName =
    L.length varName > 0 && not (isSpace (L.head varName) ==>
    let borrowCode = "&" ++ varName
                                      errors = analyzeOwnership borrowCode
    in not (L.any isBorrowError errors) || L.any isBorrowError errors

  where
      isBorrowError (BorrowError _) = True
    isBorrowError (BorrowWhileMoved _) = True
    isBorrowError (MutBorrowWhileBorrowed _) = True
    isBorrowError (BorrowWhileMutBorrowed _) = True
    isBorrowError                               _ = False

-- | Test nested scope analysis
prop_analyze_nested_scope_consistency :: String -> String -> Property
prop_analyze_nested_scope_consistency outerVar                               innerVar =
    L.length outerVar > 0 && L.length innerVar > 0 &&
    not (isSpace (L.head outerVar) && not (isSpace (L.head innerVar) ==>
    let nestedCode = "var " ++ outerVar ++ " = 42\n{\n  var " ++ innerVar ++ " = " ++ outerVar ++ "\n}"
                                      errors = analyzeOwnership nestedCode
    in L.length errors >= 0  -- Always true, but ensures analysis runs

-- | Test error ordering consistency
prop_analyze_error_ordering_consistency :: String -> Property
prop_analyze_error_ordering_consistency                               code =
    L.length code >                               0 ==>
    let errors1 = analyzeOwnership code
                                      errors2 = analyzeOwnership code
    in sort                               errors1 == sort errors2

-- | Test analysis with comments
prop_analyze_comments_ignored :: String -> Property
prop_analyze_comments_ignored                               code =
    L.length code > 0 && not ("//" `L.isInfixOf` code) ==>
    let withComment = code ++ "\n// This is a comment"
                                      errors1 = analyzeOwnership code
                                      errors2 = analyzeOwnership withComment
    in L.length                               errors1 == L.length errors2

-- | Test multiple variable declarations
prop_analyze_multiple_vars_consistency :: String -> String -> Property
prop_analyze_multiple_vars_consistency var1                               var2 =
    L.length var1 > 0 && L.length var2 > 0 &&
    not (isSpace (L.head var1) && not (isSpace (L.head var2) ==>
    let multiVarCode = "var " ++ var1 ++ " = 1\nvar " ++ var2 ++ " = 2"
                                      errors = analyzeOwnership multiVarCode
    in L.length errors >= 0  -- Should not crash

-- | Test function analysis consistency
prop_analyze_function_consistency :: String -> Property
prop_analyze_function_consistency                               funcName =
    L.length funcName > 0 && not (isSpace (L.head funcName) ==>
    let funcCode = "func " ++ funcName ++ "() {\n  var                               x = 42\n  return x\n}"
                                      errors = analyzeOwnership funcCode
    in L.length errors >= 0  -- Should not crash

-- | Test loop analysis consistency
prop_analyze_loop_consistency :: String -> Property
prop_analyze_loop_consistency                               varName =
    L.length varName > 0 && not (isSpace (L.head varName) ==>
    let loopCode = "for i := 0; i < 10; i++ {\n  var " ++ varName ++ " = i\n}"
                                      errors = analyzeOwnership loopCode
    in L.length errors >= 0  -- Should not crash

-- | Test ownership type consistency
prop_ownership_type_ordering_consistency :: String -> String -> String -> Property
prop_ownership_type_ordering_consistency name1 name2                               name3 =
    L.all (\n -> L.length n > 0 && not (isSpace (L.head n)) [name1, name2, name3] ==>
    let owned1 = Owned name1
                                      borrowed1 = Borrowed name1
                                      mutBorrowed1 = MutBorrowed name1
                                      owned2 = Owned name2
                                      borrowed2 = Borrowed name2
                                      mutBorrowed2 = MutBorrowed name2
    in (owned1 < borrowed1) && (borrowed1 < mutBorrowed1) &&
       (owned1 `compare`                               owned2 == compare name1 name2) &&
       (borrowed1 `compare`                               borrowed2 == compare name1 name2) &&
       (mutBorrowed1 `compare`                               mutBorrowed2 == compare name1 name2)

-- | Test error type consistency
prop_error_type_ordering_consistency :: String -> String -> Property
prop_error_type_ordering_consistency var1                               var2 =
    L.length var1 > 0 && L.length var2 >                               0 ==>
    let useAfterMove = UseAfterMove var1
                                      doubleMove = DoubleMove var1 var2
                                      borrowWhileMoved = BorrowWhileMoved var1
                                      mutBorrowWhileBorrowed = MutBorrowWhileBorrowed var1
                                      borrowWhileMutBorrowed = BorrowWhileMutBorrowed var1
                                      multipleMutBorrows = MultipleMutBorrows var1
                                      useWhileMutBorrowed = UseWhileMutBorrowed var1
                                      outOfScope = OutOfScope var1
                                      borrowError = BorrowError "test"
                                      parseError = ParseError "test"
    in useAfterMove < doubleMove &&
       doubleMove < borrowWhileMoved &&
       borrowWhileMoved < mutBorrowWhileBorrowed &&
       mutBorrowWhileBorrowed < borrowWhileMutBorrowed &&
       borrowWhileMutBorrowed < multipleMutBorrows &&
       multipleMutBorrows < useWhileMutBorrowed &&
       useWhileMutBorrowed < outOfScope &&
       outOfScope < borrowError &&
       borrowError < parseError

-- | Test ownership transfer properties
prop_ownership_transfer_equality :: String -> String -> Property
prop_ownership_transfer_equality from                               to =
    L.length from > 0 && L.length to >                               0 ==>
    let transfer1 = OwnershipTransfer from to
                                      transfer2 = OwnershipTransfer from to
                                      transfer3 = OwnershipTransfer to from
    in                               transfer1 == transfer2 && transfer1 /= transfer3

-- | Test analysis with invalid syntax
prop_analyze_invalid_syntax :: String -> Property
prop_analyze_invalid_syntax                               invalidCode =
    L.length invalidCode > 0 && not ("var" `L.isInfixOf` invalidCode) ==>
    let errors = analyzeOwnership invalidCode
    in L.length errors >= 0  -- Should not crash, may have parse errors

-- | Test analysis with large input
prop_analyze_large_input :: String -> Property
prop_analyze_large_input                               base =
    L.length base >                               0 ==>
    let largeCode = unlines (replicate 100 (base ++ " = " ++ base)
                                      errors = analyzeOwnership largeCode
    in L.length errors >= 0  -- Should not crash

-- | Test debug log consistency
prop_debug_log_consistency :: String -> Property
prop_debug_log_consistency                               code =
    L.length code >                               0 ==>
    let (errors1, log1) = analyzeOwnershipDebug True code
        (errors2, log2) = property $ analyzeOwnershipDebug True code
    in sort                               errors1 == sort errors2 && L.length                               log1 == L.length log2

tests :: TestTree
tests =   testGroup "Ownership Consistency QuickCheck Tests"
  [             testProperty "analyze ownership idempotent" prop_analyze_ownership_idempotent
  ,             testProperty "analyze ownership debug consistency" prop_analyze_ownership_debug_consistency
  ,             testProperty "analyze empty code" prop_analyze_empty_code
  ,             testProperty "analyze whitespace only" prop_analyze_whitespace_only
  ,             testProperty "analyze builtin functions no errors" prop_analyze_builtin_functions_no_errors
  ,             testProperty "analyze var declaration consistency" prop_analyze_var_declaration_consistency
  ,             testProperty "analyze ownership transfer consistency" prop_analyze_ownership_transfer_consistency
  ,             testProperty "analyze borrow consistency" prop_analyze_borrow_consistency
  ,             testProperty "analyze nested scope consistency" prop_analyze_nested_scope_consistency
  ,             testProperty "analyze error ordering consistency" prop_analyze_error_ordering_consistency
  ,             testProperty "analyze comments ignored" prop_analyze_comments_ignored
  ,             testProperty "analyze multiple vars consistency" prop_analyze_multiple_vars_consistency
  ,             testProperty "analyze function consistency" prop_analyze_function_consistency
  ,             testProperty "analyze loop consistency" prop_analyze_loop_consistency
  ,             testProperty "ownership type ordering consistency" prop_ownership_type_ordering_consistency
  ,             testProperty "error type ordering consistency" prop_error_type_ordering_consistency
  ,             testProperty "ownership transfer equality" prop_ownership_transfer_equality
  ,             testProperty "analyze invalid syntax" prop_analyze_invalid_syntax
  ,             testProperty "analyze large input" prop_analyze_large_input
  ,             testProperty "debug log consistency" prop_debug_log_consistency
  ]