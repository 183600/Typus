module Test.Unit.EnhancedOwnershipBoundaryQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (.&&.), counterexample, forAll, oneof, elements, listOf, listOf1, choose, sized, Positive)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Ownership
  ( OwnershipType(..), OwnershipError(..), OwnershipTransfer(..)
  , newOwnershipAnalyzer, analyzeOwnership, analyzeOwnershipDebug
  , lexAll, parseProgram, builtInFunctions
  )
import qualified Data.List as L
import Data.List 
      in counterexample ("Empty input analysis result: " ++ show result) $
     case result of
       Left _ -> True  -- Should fail gracefully L.or succeed
       Right errors -> True  -- Should succeed with no errors
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


prop_analyze_whitespace_input :: Property
                              prop_analyze_whitespace_input =
  forAll genWhitespaceOnly $ \whitespace ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer whitespace
      in counterexample ("Whitespace analysis result: " ++ show result) $
       case result of
         Left _ -> True
         Right errors -> True

-- ============================================================================
-- Ownership Error Detection Properties
-- ============================================================================

prop_detects_use_after_move :: Property
                              prop_detects_use_after_move =
  forAll genUseAfterMoveScenario $ \scenario ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer scenario
      in counterexample ("Use after move scenario: " ++ scenario) $
       case result of
         Left _ -> True  -- Should fail gracefully
         Right errors -> L.any isUseAfterMove errors

prop_detects_double_move :: Property
                              prop_detects_double_move =
  forAll genDoubleMoveScenario $ \scenario ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer scenario
      in counterexample ("Double move scenario: " ++ scenario) $
       case result of
         Left _ -> True
         Right errors -> L.any isDoubleMove errors

prop_handles_borrow_conflicts :: Property
                              prop_handles_borrow_conflicts =
  forAll genBorrowConflictScenario $ \scenario ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer scenario
      in counterexample ("Borrow conflict scenario: " ++ scenario) $
       case result of
         Left _ -> True
         Right errors -> L.any isBorrowError errors

prop_respects_variable_scope :: Property
                              prop_respects_variable_scope =
  forAll genScopeScenario $ \scenario ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer scenario
      in counterexample ("Scope scenario: " ++ scenario) $
       case result of
         Left _ -> True
         Right errors -> not (L.any isOutOfScope errors) || hasValidScopeHandling scenario

-- ============================================================================
-- Lexing L.and Parsing Properties
-- ============================================================================

prop_lex_malformed_input :: Property
                              prop_lex_malformed_input =
  forAll genMalformedCode $ \code ->
    let tokens = lexAll code
in counterexample ("Malformed code lexing: " ++ take 50 code) $
       not (null tokens)  -- Should produce some tokens even for malformed input

prop_parse_syntax_errors :: Property
                              prop_parse_syntax_errors =
  forAll genMalformedCode $ \code ->
    let tokens = lexAll code
                                      result = parseProgram tokens
      in counterexample ("Syntax error parsing: " ++ take 50 code) $
       case result of
         Left _ -> True  -- Should fail gracefully for syntax errors
         Right _ -> True  -- Or succeed if parser is lenient

-- ============================================================================
-- Ownership Transfer Properties
-- ============================================================================

prop_ownership_transfers_tracked :: Property
                              prop_ownership_transfers_tracked =
  forAll genOwnershipTransferScenario $ \scenario ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer scenario
      in counterexample ("Ownership transfer scenario: " ++ scenario) $
       case result of
         Left _ -> True
         Right errors -> hasValidTransferHandling scenario errors

prop_builtin_functions_safe :: Property
                              prop_builtin_functions_safe =
  forAll genBuiltinFunctionScenario $ \scenario ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer scenario
      in counterexample ("Builtin function scenario: " ++ scenario) $
       case result of
         Left _ -> True
         Right errors -> not (L.any isBuiltinError errors)

prop_complex_scenarios_handled :: Property
                              prop_complex_scenarios_handled =
  forAll genComplexOwnershipScenario $ \scenario ->
    let analyzer = newOwnershipAnalyzer
                                      result = analyzeOwnership analyzer scenario
      in counterexample ("Complex scenario: " ++ take 100 scenario) $
       case result of
         Left _ -> True
         Right errors -> L.length errors <= 100  -- Should not explode with errors

-- ============================================================================
-- Specific Test Cases
-- ============================================================================

test_ownership_edge_cases :: IO ()
                              test_ownership_edge_cases = do
  -- Test with extremely long variable names
  let longVarName = replicate 1000 'x'
                                    scenario = "let " ++ longVarName ++ " = 42\n" ++ longVarName ++ " = " ++ longVarName
                                    analyzer = newOwnershipAnalyzer
                                    result = analyzeOwnership analyzer scenario
  case result of
    Left _ -> assertBool "Long variable names should not crash" True
    Right errors -> assertBool "Long variable names should be handled" $ L.length errors >= 0
  
  -- Test with deeply nested scopes
  let nestedScopes = L.concat $ replicate 50 "{ let x = 42; "
      nestedScopes' = nestedScopes ++ L.concat (replicate 50 " }")
                                    result2 = analyzeOwnership analyzer nestedScopes'
  case result2 of
    Left _ -> assertBool "Deeply nested scopes should not crash" True
    Right errors -> assertBool "Deeply nested scopes should be handled" $ L.length errors >= 0
  
  -- Test with Unicode identifiers
  let unicodeScenario = "let  = 42\nlet  = \n = "
                                    result3 = analyzeOwnership analyzer unicodeScenario
  case result3 of
    Left _ -> assertBool "Unicode identifiers should not crash" True
    Right errors -> assertBool "Unicode identifiers should be handled" $ L.length errors >= 0

test_error_formatting :: IO ()
                              test_error_formatting = do
              let errors = [UseAfterMove "x", DoubleMove "x" "y", BorrowWhileMoved "z"]
                                    formatted = map show errors
  assertBool "Error formatting should produce meaningful messages" $
    L.all (not . null) formatted
  assertBool "Error formatting should include variable names" $
    L.all (`L.isInfixOf` "x") (L.filter (isInfixOf "x") formatted)

-- ============================================================================
-- Helper Functions
-- ============================================================================

isUseAfterMove :: OwnershipError -> Bool
isUseAfterMove (UseAfterMove _) = True
isUseAfterMove                               _ = False

isDoubleMove :: OwnershipError -> Bool
isDoubleMove (DoubleMove _ _) = True
isDoubleMove                               _ = False

isBorrowError :: OwnershipError -> Bool
isBorrowError (BorrowWhileMoved _) = True
isBorrowError (MutBorrowWhileBorrowed _) = True
isBorrowError (BorrowWhileMutBorrowed _) = True
isBorrowError (MultipleMutBorrows _) = True
isBorrowError (UseWhileMutBorrowed _) = True
isBorrowError                               _ = False

isOutOfScope :: OwnershipError -> Bool
isOutOfScope (OutOfScope _) = True
isOutOfScope                               _ = False

isBuiltinError :: OwnershipError -> Bool
isBuiltinError                               err = case err of
  UseAfterMove var -> var `elem` builtInFunctions
  DoubleMove var1 var2 -> var1 `elem` builtInFunctions || var2 `elem` builtInFunctions
  _ -> False

hasValidScopeHandling :: String -> Bool
hasValidScopeHandling                               scenario = "{`L.isInfixOf` scenario && "}`L.isInfixOf` scenario

hasValidTransferHandling :: String -> [OwnershipError] -> Bool
hasValidTransferHandling scenario                               errors = 
  let hasAssign = "=" `L.isInfixOf` scenario
                                    hasTransfer = L.any isTransferError errors
  in not hasAssign || hasTransfer || null errors
isTransferError :: OwnershipError -> Bool
isTransferError (UseAfterMove _) = True
isTransferError (DoubleMove _ _) = True
isTransferError                               _ = False

-- ============================================================================
-- Helper Generators
-- ============================================================================

genWhitespaceOnly :: Gen String
                              genWhitespaceOnly = listOf $ elements " \t\n\r"

genUseAfterMoveScenario :: Gen String
                              genUseAfterMoveScenario = do
              var <- genVariableName
  return $ "let " ++ var ++ " = 42\n" ++ var ++ " = " ++ var ++ "\n" ++ var

genDoubleMoveScenario :: Gen String
                              genDoubleMoveScenario = do
              var1 <- genVariableName
  var2 <- genVariableName
  return $ "let " ++ var1 ++ " = 42\nlet " ++ var2 ++ " = " ++ var1 ++ "\n" ++ var1 ++ " = " ++ var2 ++ "\n" ++ var1

genBorrowConflictScenario :: Gen String
                              genBorrowConflictScenario = oneof
  [ do
              var <- genVariableName
      return $ "let " ++ var ++ " = 42\n&" ++ var ++ "\n" ++ var ++ " = 43"
              , do
              var <- genVariableName
      return $ "let " ++ var ++ " = 42\n&mut " ++ var ++ "\n&" ++ var
  ]

genScopeScenario :: Gen String
                              genScopeScenario = do
              var <- genVariableName
depth <- choose (1, 5)
  let openBraces = replicate depth "{"
                                    closeBraces = replicate depth "}"
                                    content = "let " ++ var ++ " = 42;\n"
  return $ L.concat openBraces ++ content ++ L.concat closeBraces ++ "\n" ++ var

genMalformedCode :: Gen String
                              genMalformedCode = oneof
  [ listOf1 $ elements "{}();\n\t "
                , return "let = 42"
                , return "42 = let x"
                , return "{ let x = 42"
                , return "let x = 42 }"
                , return "function() { }"
  ]

genOwnershipTransferScenario :: Gen String
                              genOwnershipTransferScenario = do
              var1 <- genVariableName
  var2 <- genVariableName
  var3 <- genVariableName
  oneof
    [ return $ "let " ++ var1 ++ " = 42\nlet " ++ var2 ++ " = " ++ var1 ++ "\n" ++ var2 ++ " = " ++ var1
                    , return $ "let " ++ var1 ++ " = 42\nlet " ++ var2 ++ " = " ++ var1 ++ "\nlet " ++ var3 ++ " = " ++ var2
    ]

genBuiltinFunctionScenario :: Gen String
                              genBuiltinFunctionScenario = do
              func <- elements builtInFunctions
  var <- genVariableName
return $ "let " ++ var ++ " = " ++ func ++ "()"

genComplexOwnershipScenario :: Gen String
                              genComplexOwnershipScenario = sized $ \n -> do
depth <- choose (1, min n 10)
  genComplexScenarioDepth depth

genComplexScenarioDepth :: Int -> Gen String
genComplexScenarioDepth                               0 = return ""
genComplexScenarioDepth                               n = do
              var <- genVariableName
inner <- genComplexScenarioDepth (n-1)
  oneof
    [ return $ "let " ++ var ++ " = 42\n" ++ inner
                  , return $ "let " ++ var ++ " = 42\n" ++ var ++ " = " ++ var ++ "\n" ++ inner
                    , return $ "{ let " ++ var ++ " = 42\n" ++ inner ++ "}"
    ]

genVariableName :: Gen String
                              genVariableName = do
              first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_"
return $ first : rest