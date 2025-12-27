{-# LANGUAGE CPP #-}
module Test.Unit.SyntaxValidatorValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, elements, listOf, choose, suchThat)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub)
import qualified Data.Text as T

import SyntaxValidator (SyntaxValidator, ValidationResult(..), ErrorType(..))
import qualified SyntaxValidator as SV
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import TestSupport.Arbitrary ()

-- | Test syntax validator functionality
testSyntaxValidatorValidation :: TestTree
testSyntaxValidatorValidation = testGroup "Syntax Validator Validation"
  [ testBasicValidation
  , testDirectiveValidation
  , testBlockValidation
  , testExpressionValidation
  , testValidationRules
  ]

-- | Test basic syntax validation
testBasicValidation :: TestTree
testBasicValidation = testGroup "Basic Syntax Validation"
  [ fastProperty "valid syntax passes validation" prop_validSyntaxPasses
  , fastProperty "invalid syntax fails validation" prop_invalidSyntaxFails
  , fastProperty "validation preserves error information" prop_validationPreservesErrors
  , testCase "empty file validation" testEmptyFileValidation
  , testCase "simple file validation" testSimpleFileValidation
  , testCase "complex file validation" testComplexFileValidation
  ]

-- | Test directive validation
testDirectiveValidation :: TestTree
testDirectiveValidation = testGroup "Directive Validation"
  [ fastProperty "valid directives pass validation" prop_validDirectivesPass
  , fastProperty "invalid directives fail validation" prop_invalidDirectivesFail
  , fastProperty "directive consistency is checked" prop_directiveConsistencyChecked
  , testCase "ownership directive validation" testOwnershipDirectiveValidation
  , testCase "dependent types directive validation" testDependentTypesDirectiveValidation
  , testCase "combined directive validation" testCombinedDirectiveValidation
  ]

-- | Test block validation
testBlockValidation :: TestTree
testBlockValidation = testGroup "Block Validation"
  [ fastProperty "valid blocks pass validation" prop_validBlocksPass
  , fastProperty "invalid blocks fail validation" prop_invalidBlocksFail
  , fastProperty "block structure is validated" prop_blockStructureValidated
  , testCase "empty block validation" testEmptyBlockValidation
  , testCase "code block validation" testCodeBlockValidation
  , testCase "nested block validation" testNestedBlockValidation
  ]

-- | Test expression validation
testExpressionValidation :: TestTree
testExpressionValidation = testGroup "Expression Validation"
  [ fastProperty "valid expressions pass validation" prop_validExpressionsPass
  , fastProperty "invalid expressions fail validation" prop_invalidExpressionsFail
  , fastProperty "expression types are validated" prop_expressionTypesValidated
  , testCase "literal expression validation" testLiteralExpressionValidation
  , testCase "binary expression validation" testBinaryExpressionValidation
  , testCase "function call validation" testFunctionCallValidation
  ]

-- | Test validation rules
testValidationRules :: TestTree
testValidationRules = testGroup "Validation Rules"
  [ fastProperty "identifier rules are enforced" prop_identifierRulesEnforced
  , fastProperty "type rules are enforced" prop_typeRulesEnforced
  , fastProperty "semantic rules are enforced" prop_semanticRulesEnforced
  , testCase "identifier rules" testIdentifierRules
  , testCase "type rules" testTypeRules
  , testCase "semantic rules" testSemanticRules
  ]

-- | Property tests
prop_validSyntaxPasses :: TypusFile -> Property
prop_validSyntaxPasses file =
  let validator = SV.newSyntaxValidator
      result = SV.validateFile validator file
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> length errors === 0  -- Valid files should have no errors

prop_invalidSyntaxFails :: TypusFile -> Property
prop_invalidSyntaxFails file =
  let validator = SV.newSyntaxValidator
      invalidFile = file { typusBlocks = [] }  -- Make it invalid
      result = SV.validateFile validator invalidFile
  in case result of
    ValidationResult False errors -> length errors > 0 === True
    ValidationResult True _ -> property False  -- Invalid files should not pass

prop_validationPreservesErrors :: TypusFile -> Property
prop_validationPreservesErrors file =
  let validator = SV.newSyntaxValidator
      result = SV.validateFile validator file
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> all isValidError errors === True

prop_validDirectivesPass :: FileDirectives -> Property
prop_validDirectivesPass directives =
  let validator = SV.newSyntaxValidator
      result = SV.validateDirectives validator directives
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> length errors === 0

prop_invalidDirectivesFail :: FileDirectives -> Property
prop_invalidDirectivesFail directives =
  let validator = SV.newSyntaxValidator
      result = SV.validateDirectives directives
  in case result of
    ValidationResult False errors -> length errors > 0 === True
    ValidationResult True _ -> property False

prop_directiveConsistencyChecked :: FileDirectives -> [BlockDirectives] -> Property
prop_directiveConsistencyChecked fileDirectives blockDirectives =
  let validator = SV.newSyntaxValidator
      result = SV.validateDirectiveConsistency validator fileDirectives blockDirectives
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> all isValidError errors === True

prop_validBlocksPass :: [CodeBlock] -> Property
prop_validBlocksPass blocks =
  let validator = SV.newSyntaxValidator
      result = SV.validateBlocks validator blocks
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> length errors === 0

prop_invalidBlocksFail :: [CodeBlock] -> Property
prop_invalidBlocksFail blocks =
  let validator = SV.newSyntaxValidator
      invalidBlocks = map (\b -> b { codeBlockContent = "" }) blocks
      result = SV.validateBlocks validator invalidBlocks
  in case result of
    ValidationResult False errors -> length errors > 0 === True
    ValidationResult True _ -> property False

prop_blockStructureValidated :: [CodeBlock] -> Property
prop_blockStructureValidated blocks =
  let validator = SV.newSyntaxValidator
      result = SV.validateBlockStructure validator blocks
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> all isValidError errors === True

prop_validExpressionsPass :: String -> Property
prop_validExpressionsPass expression =
  let validator = SV.newSyntaxValidator
      result = SV.validateExpression validator expression
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> length errors === 0

prop_invalidExpressionsFail :: String -> Property
prop_invalidExpressionsFail expression =
  let validator = SV.newSyntaxValidator
      invalidExpression = "invalid expression with syntax error"
      result = SV.validateExpression validator invalidExpression
  in case result of
    ValidationResult False errors -> length errors > 0 === True
    ValidationResult True _ -> property False

prop_expressionTypesValidated :: String -> Property
prop_expressionTypesValidated expression =
  let validator = SV.newSyntaxValidator
      result = SV.validateExpressionTypes validator expression
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> all isValidError errors === True

prop_identifierRulesEnforced :: String -> Property
prop_identifierRulesEnforced identifier =
  let validator = SV.newSyntaxValidator
      result = SV.validateIdentifier validator identifier
  in case result of
    ValidationResult True _ -> isValidIdentifier identifier === True
    ValidationResult False _ -> isValidIdentifier identifier === False

prop_typeRulesEnforced :: String -> Property
prop_typeRulesEnforced typeStr =
  let validator = SV.newSyntaxValidator
      result = SV.validateType validator typeStr
  in case result of
    ValidationResult True _ -> isValidType typeStr === True
    ValidationResult False _ -> isValidType typeStr === False

prop_semanticRulesEnforced :: String -> Property
prop_semanticRulesEnforced code =
  let validator = SV.newSyntaxValidator
      result = SV.validateSemantics validator code
  in case result of
    ValidationResult True _ -> property True
    ValidationResult False errors -> all isValidError errors === True

-- | Unit tests
testEmptyFileValidation :: IO ()
testEmptyFileValidation = do
  let emptyFile = TypusFile (FileDirectives Nothing Nothing Nothing) [] [] []
      validator = SV.newSyntaxValidator
      result = SV.validateFile validator emptyFile
  case result of
    ValidationResult True _ -> assertBool "empty file should be valid" $ True
    ValidationResult False errors -> assertBool "empty file should not have errors" $ null errors

testSimpleFileValidation :: IO ()
testSimpleFileValidation = do
  let simpleFile = TypusFile
        (FileDirectives Nothing Nothing Nothing)
        []
        [CodeBlock (BlockDirectives Nothing Nothing Nothing) "func main() {}" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 16 16))]
        []
      validator = SV.newSyntaxValidator
      result = SV.validateFile validator simpleFile
  case result of
    ValidationResult True _ -> assertBool "simple file should be valid" $ True
    ValidationResult False errors -> assertBool "simple file should not have errors" $ null errors

testComplexFileValidation :: IO ()
testComplexFileValidation = do
  let complexFile = TypusFile
        (FileDirectives (Just (Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20)))) Nothing Nothing)
        [Located "linux" (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 6 6))]
        [ CodeBlock (BlockDirectives (Just (Located True (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 20 20)))) Nothing Nothing)
                  "func add(x int, y int) int { return x + y }" 
                  (SourceSpan (SourcePos 2 1 0) (SourcePos 2 40 40))
        , CodeBlock (BlockDirectives Nothing Nothing Nothing)
                  "func main() { result := add(5, 3) }"
                  (SourceSpan (SourcePos 3 1 0) (SourcePos 3 32 32))
        ]
        []
      validator = SV.newSyntaxValidator
      result = SV.validateFile validator complexFile
  case result of
    ValidationResult True _ -> assertBool "complex file should be valid" $ True
    ValidationResult False errors -> do
      assertBool "complex file should have minimal errors" $ length errors <= 2
      mapM_ (assertBool "errors should be valid" . isValidError) errors

testOwnershipDirectiveValidation :: IO ()
testOwnershipDirectiveValidation = do
  let validDirective = FileDirectives (Just (Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20)))) Nothing Nothing
      invalidDirective = FileDirectives (Just (Located (Located True undefined undefined) undefined undefined)) Nothing Nothing  -- Invalid structure
      validator = SV.newSyntaxValidator
      validResult = SV.validateDirectives validator validDirective
      invalidResult = SV.validateDirectives validator invalidDirective
  case validResult of
    ValidationResult True _ -> assertBool "valid ownership directive should pass" $ True
    ValidationResult False errors -> assertBool "valid directive should not have errors" $ null errors
  case invalidResult of
    ValidationResult False errors -> assertBool "invalid directive should fail" $ not (null errors)
    ValidationResult True _ -> assertBool "invalid directive should not pass" $ False

testDependentTypesDirectiveValidation :: IO ()
testDependentTypesDirectiveValidation = do
  let validDirective = FileDirectives Nothing (Just (Located True (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 25 25)))) Nothing
      invalidDirective = FileDirectives Nothing (Just (Located False undefined undefined)) Nothing
      validator = SV.newSyntaxValidator
      validResult = SV.validateDirectives validator validDirective
      invalidResult = SV.validateDirectives validator invalidDirective
  case validResult of
    ValidationResult True _ -> assertBool "valid dependent types directive should pass" $ True
    ValidationResult False errors -> assertBool "valid directive should not have errors" $ null errors
  case invalidResult of
    ValidationResult False errors -> assertBool "invalid directive should fail" $ not (null errors)
    ValidationResult True _ -> assertBool "invalid directive should not pass" $ False

testCombinedDirectiveValidation :: IO ()
testCombinedDirectiveValidation = do
  let fileDirectives = FileDirectives 
        (Just (Located True (SourcePos 1 1 0) (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20))))
        (Just (Located True (SourcePos 2 1 0) (SourceSpan (SourcePos 2 1 0) (SourcePos 2 25 25))))
        (Just (Located True (SourcePos 3 1 0) (SourceSpan (SourcePos 3 1 0) (SourcePos 3 20 20))))
      blockDirectives = [BlockDirectives (Just (Located True (SourcePos 4 1 0) (SourceSpan (SourcePos 4 1 0) (SourcePos 4 20 20)))) Nothing Nothing]
      validator = SV.newSyntaxValidator
      result = SV.validateDirectiveConsistency validator fileDirectives blockDirectives
  case result of
    ValidationResult True _ -> assertBool "consistent directives should pass" $ True
    ValidationResult False errors -> do
      assertBool "should have specific error count" $ length errors <= 3
      mapM_ (assertBool "errors should be valid" . isValidError) errors

testEmptyBlockValidation :: IO ()
testEmptyBlockValidation = do
  let emptyBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0))
      validator = SV.newSyntaxValidator
      result = SV.validateBlocks validator [emptyBlock]
  case result of
    ValidationResult True _ -> assertBool "empty block should be valid" $ True
    ValidationResult False errors -> assertBool "empty block should have minimal errors" $ length errors <= 1

testCodeBlockValidation :: IO ()
testCodeBlockValidation = do
  let codeBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "func test() { return 42 }" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 25 25))
      invalidBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "func invalid { return 42" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 20 20))
      validator = SV.newSyntaxValidator
      validResult = SV.validateBlocks validator [codeBlock]
      invalidResult = SV.validateBlocks validator [invalidBlock]
  case validResult of
    ValidationResult True _ -> assertBool "valid code block should pass" $ True
    ValidationResult False errors -> assertBool "valid block should not have errors" $ null errors
  case invalidResult of
    ValidationResult False errors -> assertBool "invalid block should fail" $ not (null errors)
    ValidationResult True _ -> assertBool "invalid block should not pass" $ False

testNestedBlockValidation :: IO ()
testNestedBlockValidation = do
  let outerBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "func outer() {" (SourceSpan (SourcePos 1 1 0) (SourcePos 1 13 13))
      innerBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "func inner() { return 42 }" (SourceSpan (SourcePos 2 1 0) (SourcePos 2 25 25))
      endBlock = CodeBlock (BlockDirectives Nothing Nothing Nothing) "}" (SourceSpan (SourcePos 3 1 0) (SourcePos 3 1 1))
      validator = SV.newSyntaxValidator
      result = SV.validateBlocks validator [outerBlock, innerBlock, endBlock]
  case result of
    ValidationResult True _ -> assertBool "nested blocks should be valid" $ True
    ValidationResult False errors -> do
      assertBool "nested blocks should have minimal errors" $ length errors <= 2
      mapM_ (assertBool "errors should be valid" . isValidError) errors

testLiteralExpressionValidation :: IO ()
testLiteralExpressionValidation = do
  let validLiterals = ["42", "\"hello\"", "true", "3.14"]
      invalidLiterals = ["", "42a", "\"unclosed", "trueish"]
      validator = SV.newSyntaxValidator
      validResults = map (SV.validateExpression validator) validLiterals
      invalidResults = map (SV.validateExpression validator) invalidLiterals
  mapM_ (\result -> case result of
    ValidationResult True _ -> assertBool "valid literal should pass" $ True
    ValidationResult False errors -> assertBool "valid literal should not have errors" $ null errors) validResults
  mapM_ (\result -> case result of
    ValidationResult False errors -> assertBool "invalid literal should fail" $ not (null errors)
    ValidationResult True _ -> assertBool "invalid literal should not pass" $ False) invalidResults

testBinaryExpressionValidation :: IO ()
testBinaryExpressionValidation = do
  let validExpressions = ["1 + 2", "x * y", "a && b", "c || d"]
      invalidExpressions = ["1 +", "* y", "&& b", "c ||"]
      validator = SV.newSyntaxValidator
      validResults = map (SV.validateExpression validator) validExpressions
      invalidResults = map (SV.validateExpression validator) invalidExpressions
  mapM_ (\result -> case result of
    ValidationResult True _ -> assertBool "valid binary expression should pass" $ True
    ValidationResult False errors -> assertBool "valid expression should not have errors" $ null errors) validResults
  mapM_ (\result -> case result of
    ValidationResult False errors -> assertBool "invalid binary expression should fail" $ not (null errors)
    ValidationResult True _ -> assertBool "invalid expression should not pass" $ False) invalidResults

testFunctionCallValidation :: IO ()
testFunctionCallValidation = do
  let validCalls = ["func()", "func(1, 2)", "obj.method()", "obj.method(arg)"]
      invalidCalls = ["func", "func(", "func(1,)", "obj..method()"]
      validator = SV.newSyntaxValidator
      validResults = map (SV.validateExpression validator) validCalls
      invalidResults = map (SV.validateExpression validator) invalidCalls
  mapM_ (\result -> case result of
    ValidationResult True _ -> assertBool "valid function call should pass" $ True
    ValidationResult False errors -> assertBool "valid call should not have errors" $ null errors) validResults
  mapM_ (\result -> case result of
    ValidationResult False errors -> assertBool "invalid function call should fail" $ not (null errors)
    ValidationResult True _ -> assertBool "invalid call should not pass" $ False) invalidResults

testIdentifierRules :: IO ()
testIdentifierRules = do
  let validIdentifiers = ["x", "myVar", "foo_bar", "test123", "MyClass"]
      invalidIdentifiers = ["123abc", "_private", "with space", "with-dash", ""]
      validator = SV.newSyntaxValidator
      validResults = map (SV.validateIdentifier validator) validIdentifiers
      invalidResults = map (SV.validateIdentifier validator) invalidIdentifiers
  mapM_ (\result -> case result of
    ValidationResult True _ -> assertBool "valid identifier should pass" $ True
    ValidationResult False _ -> assertBool "valid identifier should not fail" $ False) validResults
  mapM_ (\result -> case result of
    ValidationResult False _ -> assertBool "invalid identifier should fail" $ True
    ValidationResult True _ -> assertBool "invalid identifier should not pass" $ False) invalidResults

testTypeRules :: IO ()
testTypeRules = do
  let validTypes = ["int", "string", "bool", "[]int", "map[string]int"]
      invalidTypes = ["", "123type", "type with space", "type-with-dash"]
      validator = SV.newSyntaxValidator
      validResults = map (SV.validateType validator) validTypes
      invalidResults = map (SV.validateType validator) invalidTypes
  mapM_ (\result -> case result of
    ValidationResult True _ -> assertBool "valid type should pass" $ True
    ValidationResult False _ -> assertBool "valid type should not fail" $ False) validResults
  mapM_ (\result -> case result of
    ValidationResult False _ -> assertBool "invalid type should fail" $ True
    ValidationResult True _ -> assertBool "invalid type should not pass" $ False) invalidResults

testSemanticRules :: IO ()
testSemanticRules = do
  let validCode = "func add(x int, y int) int { return x + y }"
      invalidCode = "func add(x int, y string) int { return x + y }"  -- Type mismatch
      validator = SV.newSyntaxValidator
      validResult = SV.validateSemantics validator validCode
      invalidResult = SV.validateSemantics validator invalidCode
  case validResult of
    ValidationResult True _ -> assertBool "valid code should pass semantic validation" $ True
    ValidationResult False errors -> assertBool "valid code should not have semantic errors" $ null errors
  case invalidResult of
    ValidationResult False errors -> assertBool "invalid code should fail semantic validation" $ not (null errors)
    ValidationResult True _ -> assertBool "invalid code should not pass semantic validation" $ False

-- | Helper functions
isValidError :: SyntaxValidator.SyntaxError -> Bool
isValidError error = 
  let errorType = SyntaxValidator.errorType error
      location = SyntaxValidator.errorLocation error
      message = SyntaxValidator.errorMessage error
  in not (T.null message) && isValidLocation location

isValidLocation :: SourceSpan -> Bool
isValidLocation (SourceSpan start end) =
  sourcePosLine start <= sourcePosLine end &&
  (if sourcePosLine start == sourcePosLine end
   then sourcePosColumn start <= sourcePosColumn end
   else True)

sourcePosLine :: SourcePos -> Int
sourcePosLine (SourcePos line _ _) = line

sourcePosColumn :: SourcePos -> Int
sourcePosColumn (SourcePos _ col _) = col

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (c:cs) = isValidStartChar c && all isValidChar cs
  where
    isValidStartChar c = c `elem` ['a'..'z'] ++ ['A'..'Z']
    isValidChar c = c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

isValidType :: String -> Bool
isValidType [] = False
isValidType typeStr = 
  let baseTypes = ["int", "string", "bool", "float", "double"]
      isBaseType = typeStr `elem` baseTypes
      isArrayType = "[]" `isPrefixOf` typeStr
      isMapType = "map[" `isPrefixOf` typeStr && "]" `isInfixOf` typeStr
  in isBaseType || isArrayType || isMapType

-- | Test collection
tests :: TestTree
tests = testGroup "Syntax Validator Validation Tests"
  [ testSyntaxValidatorValidation
  ]
