module Test.Unit.NewSyntaxValidatorBoundarySpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck ((===), property,             testProperty, Property, Arbitrary(..), Gen, choose, listOf, elements, forAll, oneof, suchThat)
import SyntaxValidator ()
import qualified Data.Set as Set
import Data.List ()
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


-- Generate column numbers
genColumnNumber :: Gen Int
                              genColumnNumber = choose (1, 200)

-- Generate error messages
genErrorMessage :: Gen String
                              genErrorMessage = do
              base <- elements ["syntax error", "invalid token", "missing", "unexpected", "undeclared"]
  detail <- choose (1, 10)
  pure $ base ++ " " ++ show detail

-- Generate line content
genLineContent :: Gen String
                              genLineContent = do
              L.length' <- choose (0, 100)
  listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t{}();,."

-- Generate syntax errors
genSyntaxError :: Gen SyntaxError
                              genSyntaxError = SyntaxError
  <$> genErrorType
  <*> genErrorMessage
  <*> genLineNumber
  <*> genColumnNumber
  <*> genLineContent

-- Generate token content
genTokenContent :: Gen String
                              genTokenContent = do
              L.length' <- choose (1, 20)
  listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

-- Generate tokens
genToken :: Gen Token
                              genToken = oneof
  [ TString <$> genTokenContent <*> genLineNumber <*> genColumnNumber
  , TComment <$> genTokenContent <*> genLineNumber <*> genColumnNumber
  , TIdentifier <$> genTokenContent <*> genLineNumber <*> genColumnNumber
  , TKeyword <$> genTokenContent <*> genLineNumber <*> genColumnNumber
  , TOperator <$> genTokenContent <*> genLineNumber <*> genColumnNumber
  , TDelimiter <$> elements "{}()[];,." <*> genLineNumber <*> genColumnNumber
  , TNumber <$> genTokenContent <*> genLineNumber <*> genColumnNumber
  , TWhitespace <$> genLineNumber <*> genColumnNumber
  , TNewline <$> genLineNumber
  , TUnknown <$> genTokenContent <*> genLineNumber <*> genColumnNumber
  ]

-- Generate scope names
genScopeName :: Gen String
                              genScopeName = elements ["global", "function", "block", "loop", "conditional"]

-- Generate variable names
genVariableName :: Gen String
                              genVariableName = do
              prefix <- elements ["var", "x", "y", "z", "temp", "result"]
  suffix <- choose (1, 100)
  pure $ prefix ++ show suffix

-- Generate function names
genFunctionName :: Gen String
                              genFunctionName = do
              prefix <- elements ["func", "method", "compute", "process"]
  suffix <- choose (1, 100)
  pure $ prefix ++ show suffix

-- Generate scopes
genScope :: Gen Scope
                              genScope = do
              name <- genScopeName
  vars <- Set.fromList <$> listOf genVariableName
  funcs <- Set.fromList <$> listOf genFunctionName
  parent <- oneof [pure Nothing, fmap Just genScope]
  pure $ Scope name vars funcs parent

-- Generate languages
genLanguage :: Gen Language
                              genLanguage = elements [Go, Typus, GoAndTypus, Unknown]

-- ============================================================================
-- Property Tests for ErrorType
-- ============================================================================

-- Property: ErrorType equality should be reflexive
prop_error_type_equality_reflexive :: Property
                              prop_error_type_equality_reflexive = 
  forAll genErrorType $ \errorType ->
                                  errorType === errorType

-- Property: ErrorType equality should be symmetric
prop_error_type_equality_symmetric :: Property
                              prop_error_type_equality_symmetric = 
  forAll genErrorType $ \errorType1 ->
    forAll genErrorType $ \errorType2 ->
      (errorType1 == errorType2) === (errorType2 == errorType1)

-- ============================================================================
-- Property Tests for SyntaxError
-- ============================================================================

-- Property: SyntaxError equality should be reflexive
prop_syntax_error_equality_reflexive :: Property
                              prop_syntax_error_equality_reflexive = 
  forAll genSyntaxError $ \error ->
                                  error === error

-- Property: SyntaxError equality should be symmetric
prop_syntax_error_equality_symmetric :: Property
                              prop_syntax_error_equality_symmetric = 
  forAll genSyntaxError $ \error1 ->
    forAll genSyntaxError $ \error2 ->
      (error1 == error2) === (error2 == error1)

-- Property: SyntaxError with same components should be equal
prop_syntax_error_structural_equality :: Property
                              prop_syntax_error_structural_equality = 
  forAll genErrorType $ \errorType ->
    forAll genErrorMessage $ \message ->
      forAll genLineNumber $ \line ->
        forAll genColumnNumber $ \column ->
          forAll genLineContent $ \lineContent ->
            let error1 = SyntaxError errorType message line column lineContent
                                              error2 = SyntaxError errorType message line column lineContent
            in                               error1 === error2

-- Property: SyntaxError ordering should be consistent
prop_syntax_error_ordering_consistency :: Property
                              prop_syntax_error_ordering_consistency = 
  forAll genSyntaxError $ \error1 ->
    forAll genSyntaxError $ \error2 ->
      let comparison = compare error1 error2
                                        reverseComparison = compare error2 error1
      in if                               error1 == error2 then                               comparison === EQ else                               comparison === negate reverseComparison

-- Property: SyntaxError sorting should maintain order by message, then line, then column
prop_syntax_error_sorting :: Property
                              prop_syntax_error_sorting = 
  forAll (listOf genSyntaxError `suchThat` (not . null) $ \errors ->
    let sortedErrors = sort errors
        checkOrder [] = True
        checkOrder [_] = True
        checkOrder (e1:e2:rest) = 
          let msgOrder = compare (errorMessage e1) (errorMessage e2)
                                            lineOrder = compare (lineNumber e1) (lineNumber e2)
                                            colOrder = compare (columnNumber e1) (columnNumber e2)
          in case msgOrder of
            LT -> True
            EQ -> case lineOrder of
              LT -> True
              EQ -> colOrder <= EQ
              GT -> False
            GT -> False
    in L.all (\(e1, e2) -> compare e1 e2 <= EQ) (zip sortedErrors (L.tail sortedErrors)

-- ============================================================================
-- Property Tests for Token
-- ============================================================================

-- Property: Token equality should be reflexive
prop_token_equality_reflexive :: Property
                              prop_token_equality_reflexive = 
  forAll genToken $ \token ->
                                  token === token

-- Property: Token equality should be symmetric
prop_token_equality_symmetric :: Property
                              prop_token_equality_symmetric = 
  forAll genToken $ \token1 ->
    forAll genToken $ \token2 ->
      (token1 == token2) === (token2 == token1)

-- Property: Tokens with same type L.and content should be equal
prop_token_structural_equality :: Property
                              prop_token_structural_equality = 
  forAll genTokenContent $ \content ->
    forAll genLineNumber $ \line ->
      forAll genColumnNumber $ \column ->
        let token1 = TString content line column
                                          token2 = TString content line column
        in                               token1 === token2

-- ============================================================================
-- Property Tests for Scope
-- ============================================================================

-- Property: Scope equality should be reflexive
prop_scope_equality_reflexive :: Property
                              prop_scope_equality_reflexive = 
  forAll genScope $ \scope ->
                                  scope === scope

-- Property: Scope equality should be symmetric
prop_scope_equality_symmetric :: Property
                              prop_scope_equality_symmetric = 
  forAll genScope $ \scope1 ->
    forAll genScope $ \scope2 ->
      (scope1 == scope2) === (scope2 == scope1)

-- Property: Scope with same name should be equal regardless of set order
prop_scope_set_order_independence :: Property
                              prop_scope_set_order_independence = 
  forAll genScopeName $ \name ->
    forAll (listOf genVariableName) $ \vars1 ->
      forAll (listOf genFunctionName) $ \funcs1 ->
        let varsSet1 = Set.fromList vars1
                                          funcsSet1 = Set.fromList funcs1
                                          varsSet2 = Set.fromList (L.reverse vars1)
                                          funcsSet2 = Set.fromList (L.reverse funcs1)
                                          scope1 = Scope name varsSet1 funcsSet1 Nothing
                                          scope2 = Scope name varsSet2 funcsSet2 Nothing
        in                               scope1 === scope2

-- ============================================================================
-- Property Tests for SyntaxValidator
-- ============================================================================

-- Property: New syntax validator should have no errors
prop_new_validator_no_errors :: Property
                              prop_new_validator_no_errors = 
  let validator = newSyntaxValidator
                                    errors = validatorErrors validator
  in null errors

-- Property: New syntax validator should have global scope
prop_new_validator_global_scope :: Property
                              prop_new_validator_global_scope = 
  let validator = newSyntaxValidator
                                    scope = currentScope validator
  in scopeName                               scope === "global"

-- Property: New syntax validator should have empty brace stack
prop_new_validator_empty_brace_stack :: Property
                              prop_new_validator_empty_brace_stack = 
  let validator = newSyntaxValidator
                                    braceStack = braceStack validator
  in null braceStack

-- Property: New syntax validator should have unknown language initially
prop_new_validator_unknown_language :: Property
                              prop_new_validator_unknown_language = 
  let validator = newSyntaxValidator
                                    lang = language validator
  in                               lang === Unknown

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_syntax_error_creation :: IO ()
                              test_syntax_error_creation = do
              let error = SyntaxError
        {                               errorType = MissingBrace
        ,                               errorMessage = "Missing closing brace"
        ,                               lineNumber = 10
        ,                               columnNumber = 5
        ,                               lineContent = "func main() {"
        }
  
  errorType error @?= MissingBrace
  errorMessage error @?= "Missing closing brace"
  lineNumber error @?= 10
  columnNumber error @?= 5
  lineContent error @?= "func main() {"

test_token_creation :: IO ()
                              test_token_creation = do
              let stringToken = TString "hello" 1 1
                                    commentToken = TComment "comment" 2 3
                                    identifierToken = TIdentifier "variable" 3 5
                                    keywordToken = TKeyword "func" 4 1
                                    operatorToken = TOperator "+" 5 10
                                    delimiterToken = TDelimiter '{' 6 1
                                    numberToken = TNumber "42" 7 2
                                    whitespaceToken = TWhitespace 8 1
                                    newlineToken = TNewline 9
                                    unknownToken = TUnknown "???" 10 5
  
  show stringToken @?= "TString \"hello\" 1 1"
  show commentToken @?= "TComment \"comment\" 2 3"
  show identifierToken @?= "TIdentifier \"variable\" 3 5"
  show keywordToken @?= "TKeyword \"func\" 4 1"
  show operatorToken @?= "TOperator \"+\" 5 10"
  show delimiterToken @?= "TDelimiter '{' 6 1"
  show numberToken @?= "TNumber \"42\" 7 2"
  show whitespaceToken @?= "TWhitespace 8 1"
  show newlineToken @?= "TNewline 9"
  show unknownToken @?= "TUnknown \"???\" 10 5"

test_scope_creation :: IO ()
                              test_scope_creation = do
              let globalScope = Scope "global" Set.empty Set.empty Nothing
                                    funcScope = Scope "function" (Set.fromList ["x", "y"]) (Set.fromList ["helper"]) (Just globalScope)
  
  scopeName globalScope @?= "global"
  scopeVariables globalScope @?= Set.empty
  scopeFunctions globalScope @?= Set.empty
  parentScope globalScope @?= Nothing
  
  scopeName funcScope @?= "function"
  scopeVariables funcScope @?= Set.fromList ["x", "y"]
  scopeFunctions funcScope @?= Set.fromList ["helper"]
  parentScope funcScope @?= Just globalScope

test_syntax_validator_creation :: IO ()
                              test_syntax_validator_creation = do
              let validator = newSyntaxValidator
  validatorErrors validator @?= []
  scopeName (currentScope validator) @?= "global"
  braceStack validator @?= []
  language validator @?= Unknown
  tokens validator @?= []
  hasPackageDecl validator @?= False
  hasMainFunc validator @?= False

test_syntax_error_ordering :: IO ()
                              test_syntax_error_ordering = do
              let error1 = SyntaxError MissingBrace "error1" 1 1 "line1"
                                    error2 = SyntaxError MissingParenthesis "error1" 1 1 "line1"
                                    error3 = SyntaxError MissingBrace "error2" 1 1 "line1"
                                    error4 = SyntaxError MissingBrace "error1" 2 1 "line2"
                                    error5 = SyntaxError MissingBrace "error1" 1 2 "line1"
      
                                    errors = [error5, error3, error1, error4, error2]
                                    sortedErrors = sort errors
  
  sortedErrors @?= [error1, error2, error3, error4, error5]

test_complex_validation_scenarios :: IO ()
                              test_complex_validation_scenarios = do
  -- Test validation of simple valid code
  let validCode = "package main\n\nfunc main() {\n    return 42\n}\n"
                                    validErrors = validateFile validCode
  
  -- Test validation of code with syntax errors
  let invalidCode = "package main\n\nfunc main() {\n    return 42\n  // Missing closing brace\n"
                                    invalidErrors = validateFile invalidCode
  
  -- Valid code should have no errors (L.or only warnings)
  L.length validErrors @?= 0
  
  -- Invalid code should have errors
  L.length invalidErrors @?= 1
  errorType (L.head invalidErrors) @?= MissingBrace

test_error_formatting :: IO ()
                              test_error_formatting = do
              let error = SyntaxError
        {                               errorType = MissingBrace
        ,                               errorMessage = "Missing closing brace"
        ,                               lineNumber = 10
        ,                               columnNumber = 5
        ,                               lineContent = "func main() {"
        }
                                    formatted = formatSyntaxError error
  
  -- The formatted error should contain key information
  formatted `contains` "Missing closing brace"
  formatted `contains` "line 10"
  formatted `contains` "column 5"
  where
      contains x                               y = y `L.isInfixOf` x

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests =   testGroup "New Syntax Validator Boundary Tests"
  [ -- ErrorType properties
            testProperty "ErrorType equality reflexive" prop_error_type_equality_reflexive
  ,             testProperty "ErrorType equality symmetric" prop_error_type_equality_symmetric
  
  -- SyntaxError properties
  ,             testProperty "SyntaxError equality reflexive" prop_syntax_error_equality_reflexive
  ,             testProperty "SyntaxError equality symmetric" prop_syntax_error_equality_symmetric
  ,             testProperty "SyntaxError structural equality" prop_syntax_error_structural_equality
  ,             testProperty "SyntaxError ordering consistency" prop_syntax_error_ordering_consistency
  ,             testProperty "SyntaxError sorting" prop_syntax_error_sorting
  
  -- Token properties
  ,             testProperty "Token equality reflexive" prop_token_equality_reflexive
  ,             testProperty "Token equality symmetric" prop_token_equality_symmetric
  ,             testProperty "Token structural equality" prop_token_structural_equality
  
  -- Scope properties
  ,             testProperty "Scope equality reflexive" prop_scope_equality_reflexive
  ,             testProperty "Scope equality symmetric" prop_scope_equality_symmetric
  ,             testProperty "Scope set order independence" prop_scope_set_order_independence
  
  -- SyntaxValidator properties
  ,             testProperty "New validator no errors" prop_new_validator_no_errors
  ,             testProperty "New validator global scope" prop_new_validator_global_scope
  ,             testProperty "New validator empty brace stack" prop_new_validator_empty_brace_stack
  ,             testProperty "New validator unknown language" prop_new_validator_unknown_language
  
  -- Unit tests
    ,             testCase "SyntaxError creation" test_syntax_error_creation
    ,             testCase "Token creation" test_token_creation
    ,             testCase "Scope creation" test_scope_creation
    ,             testCase "SyntaxValidator creation" test_syntax_validator_creation
    ,             testCase "SyntaxError ordering" test_syntax_error_ordering
    ,             testCase "Complex validation scenarios" test_complex_validation_scenarios
    ,             testCase "Error formatting" test_error_formatting
  ]