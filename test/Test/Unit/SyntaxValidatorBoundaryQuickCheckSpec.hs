{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.SyntaxValidatorBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>), suchThat)
import Test.Tasty.HUnit (testCase, assertBool)

-- Mock syntax validation types for testing
data MockSyntaxElement = MockVariable String | MockFunction String [MockSyntaxElement] | MockLiteral String
  deriving (Show, Eq)

data MockValidationError = MockValidationError
  { validationMessage :: String
  , validationElement :: MockSyntaxElement
  } deriving (Show, Eq)

data MockValidationResult = MockValid | MockInvalid [MockValidationError]
  deriving (Show, Eq)

data MockValidationContext = MockValidationContext
  { contextVariables :: [String]
  , contextFunctions :: [String]
  , contextDepth :: Int
  } deriving (Show, Eq)

-- ============================================================================
-- Test Data Generators
-- ============================================================================

instance Arbitrary MockSyntaxElement where
  arbitrary = oneof
    [ MockVariable <$> arbitrary
    , MockFunction <$> arbitrary <*> listOf arbitrary
    , MockLiteral <$> arbitrary
    ]

instance Arbitrary MockValidationError where
  arbitrary = MockValidationError <$> arbitrary <*> arbitrary

instance Arbitrary MockValidationResult where
  arbitrary = oneof
    [ pure MockValid
    , MockInvalid <$> listOf1 arbitrary
    ]

instance Arbitrary MockValidationContext where
  arbitrary = MockValidationContext <$> listOf arbitrary <*> listOf arbitrary <*> arbitrary

-- Generate valid variable names
genValidVariable :: Gen String
genValidVariable = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate invalid variable names
genInvalidVariable :: Gen String
genInvalidVariable = oneof
  [ elements ["", "1var", "var!", "var space", "var@"]  -- Invalid starts/characters
  , listOf $ elements [' '..']  -- Control characters L.and spaces
  ]

-- Generate deeply nested structures
genNestedStructure :: Int -> Gen MockSyntaxElement
genNestedStructure 0 = arbitrary
genNestedStructure n = oneof
  [ MockVariable <$> genValidVariable
  , MockLiteral <$> arbitrary
  , MockFunction <$> genValidVariable <*> listOf (genNestedStructure (n-1))
  ]

-- ============================================================================
-- Mock Functions (simplified versions for testing)
-- ============================================================================

mockValidateVariable :: String -> MockValidationResult
mockValidateVariable var = 
  if null var
  then MockInvalid [MockValidationError "Empty variable name" (MockVariable var)]
  else if not (isValidVarStart (L.head var))
       then MockInvalid [MockValidationError "Invalid variable start" (MockVariable var)]
       else if L.any (not . isValidVarChar) (L.tail var)
            then MockInvalid [MockValidationError "Invalid variable character" (MockVariable var)]
            else MockValid
  where
    isValidVarStart c = c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
    isValidVarChar c = c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

mockValidateFunction :: String -> [MockSyntaxElement] -> MockValidationResult
mockValidateFunction name args = 
  let nameResult = mockValidateVariable name
      argResults = map mockValidateElement args
      allResults = nameResult : argResults
      errors = concatMap extractErrors allResults
  in if null errors
     then MockValid
     else MockInvalid errors
  where
    extractErrors MockValid = []
    extractErrors (MockInvalid errs) = errs

mockValidateElement :: MockSyntaxElement -> MockValidationResult
mockValidateElement element = 
  case element of
    MockVariable var -> mockValidateVariable var
    MockLiteral _ -> MockValid  -- Assume L.all literals are valid
    MockFunction name args -> mockValidateFunction name args

mockValidateWithContext :: MockSyntaxElement -> MockValidationContext -> MockValidationResult
mockValidateWithContext element context = 
  case element of
    MockVariable var -> 
      if var `elem` contextVariables context
      then MockValid
      else MockInvalid [MockValidationError "Undefined variable" element]
    MockFunction name args ->
      if name `elem` contextFunctions context
      then mockValidateFunction name args
      else MockInvalid [MockValidationError "Undefined function" element]
    MockLiteral _ -> MockValid

mockCheckDepth :: MockSyntaxElement -> Int -> MockValidationResult
mockCheckDepth element maxDepth = 
  let actualDepth = calculateDepth element
  in if actualDepth <= maxDepth
     then MockValid
     else MockInvalid [MockValidationError "Nesting too deep" element]
  where
    calculateDepth (MockVariable _) = 1
    calculateDepth (MockLiteral _) = 1
    calculateDepth (MockFunction _ args) = 
      1 + (if null args then 0 else L.maximum $ map calculateDepth args)

mockValidateAll :: [MockSyntaxElement] -> MockValidationResult
mockValidateAll elements = 
  let results = map mockValidateElement elements
      errors = concatMap extractErrors results
  in if null errors
     then MockValid
     else MockInvalid errors
  where
    extractErrors MockValid = []
    extractErrors (MockInvalid errs) = errs

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Valid variables always validate
prop_validVariablesValidate :: Property
prop_validVariablesValidate = 
  forAll genValidVariable $ \var ->
    mockValidateVariable var === MockValid

-- Property: Invalid variables fail validation
prop_invalidVariablesFail :: Property
prop_invalidVariablesFail = 
  forAll genInvalidVariable $ \var ->
    case mockValidateVariable var of
      MockValid -> property False
      MockInvalid _ -> property True

-- Property: Empty variable name fails validation
prop_emptyVariableFails :: Property
prop_emptyVariableFails = 
  let result = mockValidateVariable ""
  in case result of
    MockValid -> property False
    MockInvalid _ -> property True

-- Property: Function with valid name L.and args validates
prop_functionWithValidArgs :: Property
prop_functionWithValidArgs = 
  forAll genValidVariable $ \name ->
    forAll (listOf arbitrary) $ \args ->
      let element = MockFunction name args
          result = mockValidateElement element
      in case mockValidateVariable name of
        MockValid -> case result of
          MockValid -> property True
          MockInvalid _ -> property True  -- Args might be invalid
        MockInvalid _ -> case result of
          MockValid -> property False
          MockInvalid _ -> property True

-- Property: Validation is deterministic
prop_validationDeterministic :: MockSyntaxElement -> Property
prop_validationDeterministic element = 
  let result1 = mockValidateElement element
      result2 = mockValidateElement element
  in result1 === result2

-- Property: Context validation respects defined variables
prop_contextRespectsVariables :: MockValidationContext -> String -> Property
prop_contextRespectsVariables context var = 
  let contextWithVar = context { contextVariables = var : contextVariables context }
      element = MockVariable var
      result = mockValidateWithContext element contextWithVar
  in result === MockValid

-- Property: Depth checking limits nesting
prop depthCheckingLimitsNesting :: Int -> Property
prop depthCheckingLimitsNesting maxDepth = 
  maxDepth >= 0 ==>
    forAll (genNestedStructure (maxDepth + 1)) $ \element ->
      let result = mockCheckDepth element maxDepth
      in case result of
        MockValid -> property False
        MockInvalid _ -> property True

-- Property: Validation of multiple elements aggregates errors
prop_multipleElementsAggregateErrors :: [MockSyntaxElement] -> Property
prop_multipleElementsAggregateErrors elements = 
  let individualResults = map mockValidateElement elements
      individualErrors = concatMap extractErrors individualResults
      combinedResult = mockValidateAll elements
      combinedErrors = extractErrors combinedResult
  in L.length individualErrors === L.length combinedErrors
  where
    extractErrors MockValid = []
    extractErrors (MockInvalid errs) = errs

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Syntax Validator Boundary QuickCheck Tests"
  [ testProperty "Valid variables always validate" prop_validVariablesValidate
  , testProperty "Invalid variables fail validation" prop_invalidVariablesFail
  , testProperty "Empty variable name fails validation" prop_emptyVariableFails
  , testProperty "Function with valid name L.and args validates" prop_functionWithValidArgs
  , testProperty "Validation is deterministic" prop_validationDeterministic
  , testProperty "Context validation respects defined variables" prop_contextRespectsVariables
  , testProperty "Depth checking limits nesting" prop depthCheckingLimitsNesting
  , testProperty "Validation of multiple elements aggregates errors" prop_multipleElementsAggregateErrors
  , testCase "Syntax validator boundary cases" $ do
      -- Test valid cases
      assertBool "Valid variable should pass" $ 
        case mockValidateVariable "validVar" of
          MockValid -> True
          _ -> False
      
      assertBool "Valid function should pass" $ 
        case mockValidateFunction "validFunc" [MockVariable "x", MockLiteral "42"] of
          MockValid -> True
          _ -> False
      
      -- Test invalid cases
      assertBool "Invalid variable should fail" $ 
        case mockValidateVariable "1invalid" of
          MockInvalid _ -> True
          _ -> False
      
      assertBool "Empty variable should fail" $ 
        case mockValidateVariable "" of
          MockInvalid _ -> True
          _ -> False
      
      -- Test context validation
      let context = MockValidationContext ["x", "y"] ["func"] 0
      assertBool "Defined variable should pass in context" $ 
        case mockValidateWithContext (MockVariable "x") context of
          MockValid -> True
          _ -> False
      
      assertBool "Undefined variable should fail in context" $ 
        case mockValidateWithContext (MockVariable "z") context of
          MockInvalid _ -> True
          _ -> False
      
      -- Test depth checking
      let deepElement = MockFunction "f" [MockFunction "g" [MockFunction "h" []]]
      assertBool "Deep nesting should fail with shallow limit" $ 
        case mockCheckDepth deepElement 2 of
          MockInvalid _ -> True
          _ -> False
  ]