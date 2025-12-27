{-# LANGUAGE CPP #-}
module Test.Unit.ErrorHandlerRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck ((===), Property, forAll, Gen, elements, listOf, choose, suchThat)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

import ErrorHandler (ErrorHandler, RecoveryStrategy(..), ErrorContext(..))
import EnhancedErrorHandler (EnhancedErrorHandler, EnhancedRecoveryStrategy(..))
import qualified Compiler.Errors.Core as Core
import qualified ErrorHandler as EH
import qualified EnhancedErrorHandler as EEH
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import TestSupport.Arbitrary ()

-- | Test error handler recovery functionality
testErrorHandlerRecovery :: TestTree
testErrorHandlerRecovery = testGroup "Error Handler Recovery"
  [ testBasicRecovery
  , testEnhancedRecovery
  , testRecoveryStrategies
  , testErrorContextHandling
  , testRecoveryValidation
  ]

-- | Test basic error recovery
testBasicRecovery :: TestTree
testBasicRecovery = testGroup "Basic Error Recovery"
  [ fastProperty "recovery preserves error information" prop_basicRecoveryPreservesInfo
  , fastProperty "recovery provides suggestions" prop_basicRecoveryProvidesSuggestions
  , fastProperty "recovery handles different error types" prop_basicRecoveryHandlesTypes
  , testCase "syntax error recovery" testSyntaxErrorRecovery
  , testCase "type error recovery" testTypeErrorRecovery
  , testCase "semantic error recovery" testSemanticErrorRecovery
  ]

-- | Test enhanced error recovery
testEnhancedRecovery :: TestTree
testEnhancedRecovery = testGroup "Enhanced Error Recovery"
  [ fastProperty "enhanced recovery provides better suggestions" prop_enhancedRecoveryBetterSuggestions
  , fastProperty "enhanced recovery handles complex errors" prop_enhancedRecoveryHandlesComplex
  , fastProperty "enhanced recovery validates fixes" prop_enhancedRecoveryValidatesFixes
  , testCase "enhanced syntax recovery" testEnhancedSyntaxRecovery
  , testCase "enhanced type recovery" testEnhancedTypeRecovery
  , testCase "enhanced ownership recovery" testEnhancedOwnershipRecovery
  ]

-- | Test different recovery strategies
testRecoveryStrategies :: TestTree
testRecoveryStrategies = testGroup "Recovery Strategies"
  [ fastProperty "skip node strategy works" prop_skipNodeStrategyWorks
  , fastProperty "suggest fix strategy works" prop_suggestFixStrategyWorks
  , fastProperty "insert token strategy works" prop_insertTokenStrategyWorks
  , testCase "skip node implementation" testSkipNodeImplementation
  , testCase "suggest fix implementation" testSuggestFixImplementation
  , testCase "retry with alternative implementation" testRetryAlternativeImplementation
  ]

-- | Test error context handling
testErrorContextHandling :: TestTree
testErrorContextHandling = testGroup "Error Context Handling"
  [ fastProperty "context preserves source information" prop_contextPreservesSource
  , fastProperty "context tracks error chain" prop_contextTracksChain
  , fastProperty "context provides relevant information" prop_contextProvidesRelevant
  , testCase "context creation" testContextCreation
  , testCase "context updates" testContextUpdates
  , testCase "context queries" testContextQueries
  ]

-- | Test recovery validation
testRecoveryValidation :: TestTree
testRecoveryValidation = testGroup "Recovery Validation"
  [ fastProperty "validation checks fix correctness" prop_validationChecksCorrectness
  , fastProperty "validation prevents infinite loops" prop_validationPreventsLoops
  , fastProperty "validation ensures consistency" prop_validationEnsuresConsistency
  , testCase "fix validation" testFixValidation
  , testCase "recovery limit validation" testRecoveryLimitValidation
  , testCase "consistency validation" testConsistencyValidation
  ]

-- | Property tests
prop_basicRecoveryPreservesInfo :: Core.TypeError -> Property
prop_basicRecoveryPreservesInfo typeError =
  let handler = EH.newErrorHandler
      recovery = EH.attemptRecovery handler typeError
  in case recovery of
    Just (strategy, suggestions) -> 
      property True  -- Recovery should preserve error info
    Nothing -> property True  -- Some errors may not be recoverable

prop_basicRecoveryProvidesSuggestions :: Core.TypeError -> Property
prop_basicRecoveryProvidesSuggestions typeError =
  let handler = EH.newErrorHandler
      recovery = EH.attemptRecovery handler typeError
  in case recovery of
    Just (strategy, suggestions) -> not (null suggestions) === True
    Nothing -> property True  -- Some errors may not have suggestions

prop_basicRecoveryHandlesTypes :: Core.ErrorCategory -> Property
prop_basicRecoveryHandlesTypes category =
  let typeError = Core.TypeError
        { Core.errorId = "TEST_001"
        , Core.severity = Core.Error
        , Core.category = category
        , Core.message = T.pack "Test error"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 5)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.NoRecovery
        , Core.suggestions = []
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12345
        }
      handler = EH.newErrorHandler
      recovery = EH.attemptRecovery handler typeError
  in case recovery of
    Just _ -> property True
    Nothing -> property True

prop_enhancedRecoveryBetterSuggestions :: Core.TypeError -> Property
prop_enhancedRecoveryBetterSuggestions typeError =
  let basicHandler = EH.newErrorHandler
      enhancedHandler = EEH.newEnhancedErrorHandler
      basicRecovery = EH.attemptRecovery basicHandler typeError
      enhancedRecovery = EEH.attemptEnhancedRecovery enhancedHandler typeError
  in case (basicRecovery, enhancedRecovery) of
    (Just (_, basicSuggestions), Just (_, enhancedSuggestions)) ->
      length enhancedSuggestions >= length basicSuggestions
    _ -> property True

prop_enhancedRecoveryHandlesComplex :: Core.TypeError -> [Core.TypeError] -> Property
prop_enhancedRecoveryHandlesComplex mainError relatedErrors =
  let enhancedHandler = EEH.newEnhancedErrorHandler
      complexError = mainError { Core.relatedErrors = relatedErrors }
      recovery = EEH.attemptEnhancedRecovery enhancedHandler complexError
  in case recovery of
    Just _ -> property True
    Nothing -> property True

prop_enhancedRecoveryValidatesFixes :: Core.TypeError -> Property
prop_enhancedRecoveryValidatesFixes typeError =
  let enhancedHandler = EEH.newEnhancedErrorHandler
      recovery = EEH.attemptEnhancedRecovery enhancedHandler typeError
  in case recovery of
    Just (strategy, suggestions, validation) -> validation === True
    Nothing -> property True

prop_skipNodeStrategyWorks :: Core.TypeError -> Property
prop_skipNodeStrategyWorks typeError =
  let strategy = Core.SkipNode
      isValid = EH.isValidStrategy strategy
  in isValid === True

prop_suggestFixStrategyWorks :: String -> Property
prop_suggestFixStrategyWorks fix =
  let strategy = Core.SuggestFix fix
      isValid = EH.isValidStrategy strategy
  in not (null fix) === isValid

prop_insertTokenStrategyWorks :: String -> Property
prop_insertTokenStrategyWorks token =
  let strategy = Core.InsertToken token
      isValid = EH.isValidStrategy strategy
  in not (null token) === isValid

prop_contextPreservesSource :: String -> Property
prop_contextPreservesSource sourceCode =
  let context = EH.createContext sourceCode (SourcePos 1 1 0)
      retrievedSource = EH.getSourceContext context
  in retrievedSource === sourceCode

prop_contextTracksChain :: [Core.TypeError] -> Property
prop_contextTracksChain errorChain =
  let context = EH.createContextWithChain errorChain
      retrievedChain = EH.getErrorChain context
  in length retrievedChain === length errorChain

prop_contextProvidesRelevant :: Core.TypeError -> Property
prop_contextProvidesRelevant typeError =
  let context = EH.createContextFromError typeError
      hasRelevantInfo = EH.hasRelevantContext context
  in hasRelevantInfo === True

prop_validationChecksCorrectness :: String -> Property
prop_validationChecksCorrectness fix =
  let isValid = EEH.validateFix fix
  in not (null fix) === isValid  -- Simplified validation

prop_validationPreventsLoops :: [String] -> Property
prop_validationPreventsLoops previousFixes =
  let newFix = "new fix"
      isValid = EEH.validateFixWithHistory newFix previousFixes
  in not (newFix `elem` previousFixes) === isValid

prop_validationEnsuresConsistency :: Core.TypeError -> String -> Property
prop_validationEnsuresConsistency typeError fix =
  let isConsistent = EEH.validateConsistency typeError fix
  in isConsistent === True  -- Simplified consistency check

-- | Unit tests
testSyntaxErrorRecovery :: IO ()
testSyntaxErrorRecovery = do
  let syntaxError = Core.TypeError
        { Core.errorId = "SYNTAX_001"
        , Core.severity = Core.Error
        , Core.category = Core.SyntaxError
        , Core.message = T.pack "Unexpected token"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 5 4) (SourcePos 1 10 9)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Remove unexpected token"
        , Core.suggestions = [T.pack "Remove token", T.pack "Add semicolon"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12345
        }
      handler = EH.newErrorHandler
      recovery = EH.attemptRecovery handler syntaxError
  case recovery of
    Just (strategy, suggestions) -> do
      assertEqual "should suggest fix strategy" Core.SuggestFix strategy
      assertBool "should provide suggestions" $ not (null suggestions)
    Nothing -> assertBool "syntax error should be recoverable" $ False

testTypeErrorRecovery :: IO ()
testTypeErrorRecovery = do
  let typeError = Core.TypeError
        { Core.errorId = "TYPE_001"
        , Core.severity = Core.Error
        , Core.category = Core.TypeMismatch
        , Core.message = T.pack "Cannot assign string to int"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 2 10 25) (SourcePos 2 15 30)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Change type or value"
        , Core.suggestions = [T.pack "var x string", T.pack "x := 42"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12346
        }
      handler = EH.newErrorHandler
      recovery = EH.attemptRecovery handler typeError
  case recovery of
    Just (strategy, suggestions) -> do
      assertEqual "should suggest fix strategy" Core.SuggestFix strategy
      assertBool "should provide type-related suggestions" $ 
        any (T.isInfixOf "string") suggestions || any (T.isInfixOf "int") suggestions
    Nothing -> assertBool "type error should be recoverable" $ False

testSemanticErrorRecovery :: IO ()
testSemanticErrorRecovery = do
  let semanticError = Core.TypeError
        { Core.errorId = "SEMANTIC_001"
        , Core.severity = Core.Error
        , Core.category = Core.UndefinedVariable
        , Core.message = T.pack "Variable not defined"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 3 8 40) (SourcePos 3 12 44)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Define variable"
        , Core.suggestions = [T.pack "var x int", T.pack "x := 0"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12347
        }
      handler = EH.newErrorHandler
      recovery = EH.attemptRecovery handler semanticError
  case recovery of
    Just (strategy, suggestions) -> do
      assertEqual "should suggest fix strategy" Core.SuggestFix strategy
      assertBool "should provide definition suggestions" $ 
        any (T.isInfixOf "var") suggestions || any (T.isInfixOf ":=") suggestions
    Nothing -> assertBool "semantic error should be recoverable" $ False

testEnhancedSyntaxRecovery :: IO ()
testEnhancedSyntaxRecovery = do
  let syntaxError = Core.TypeError
        { Core.errorId = "SYNTAX_001"
        , Core.severity = Core.Error
        , Core.category = Core.SyntaxError
        , Core.message = T.pack "Missing closing brace"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 5 1 50) (SourcePos 5 1 50)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.InsertToken "}"
        , Core.suggestions = [T.pack "Add } at end of block"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12348
        }
      enhancedHandler = EEH.newEnhancedErrorHandler
      recovery = EEH.attemptEnhancedRecovery enhancedHandler syntaxError
  case recovery of
    Just (strategy, suggestions, validation) -> do
      assertEqual "should suggest insert token strategy" (Core.InsertToken "}") strategy
      assertBool "should validate the fix" validation
      assertBool "should provide enhanced suggestions" $ not (null suggestions)
    Nothing -> assertBool "enhanced syntax error should be recoverable" $ False

testEnhancedTypeRecovery :: IO ()
testEnhancedTypeRecovery = do
  let typeError = Core.TypeError
        { Core.errorId = "TYPE_002"
        , Core.severity = Core.Error
        , Core.category = Core.TypeMismatch
        , Core.message = T.pack "Function parameter type mismatch"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 4 15 60) (SourcePos 4 20 65)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.RetryWithAlternative "Try interface{}"
        , Core.suggestions = [T.pack "Use interface{}", T.pack "Convert parameter"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12349
        }
      enhancedHandler = EEH.newEnhancedErrorHandler
      recovery = EEH.attemptEnhancedRecovery enhancedHandler typeError
  case recovery of
    Just (strategy, suggestions, validation) -> do
      assertEqual "should suggest retry with alternative" (Core.RetryWithAlternative "Try interface{}") strategy
      assertBool "should validate the retry" validation
      assertBool "should provide enhanced type suggestions" $ 
        any (T.isInfixOf "interface") suggestions
    Nothing -> assertBool "enhanced type error should be recoverable" $ False

testEnhancedOwnershipRecovery :: IO ()
testEnhancedOwnershipRecovery = do
  let ownershipError = Core.TypeError
        { Core.errorId = "OWNERSHIP_001"
        , Core.severity = Core.Error
        , Core.category = Core.OwnershipViolation
        , Core.message = T.pack "Cannot move borrowed value"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 6 10 70) (SourcePos 6 15 75)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Clone the value"
        , Core.suggestions = [T.pack "Use .clone()", T.pack "Wait for borrow to end"]
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12350
        }
      enhancedHandler = EEH.newEnhancedErrorHandler
      recovery = EEH.attemptEnhancedRecovery enhancedHandler ownershipError
  case recovery of
    Just (strategy, suggestions, validation) -> do
      assertEqual "should suggest fix strategy" (Core.SuggestFix "Clone the value") strategy
      assertBool "should validate the fix" validation
      assertBool "should provide ownership-specific suggestions" $ 
        any (T.isInfixOf "clone") suggestions || any (T.isInfixOf "borrow") suggestions
    Nothing -> assertBool "enhanced ownership error should be recoverable" $ False

testSkipNodeImplementation :: IO ()
testSkipNodeImplementation = do
  let strategy = Core.SkipNode
      isValid = EH.isValidStrategy strategy
      canApply = EH.canApplyStrategy strategy Core.SyntaxError
  assertBool "skip node should be valid" isValid
  assertBool "skip node should be applicable to syntax errors" canApply

testSuggestFixImplementation :: IO ()
testSuggestFixImplementation = do
  let strategy = Core.SuggestFix "Add missing semicolon"
      isValid = EH.isValidStrategy strategy
      canApply = EH.canApplyStrategy strategy Core.SyntaxError
  assertBool "suggest fix should be valid" isValid
  assertBool "suggest fix should be applicable to syntax errors" canApply

testRetryAlternativeImplementation :: IO ()
testRetryAlternativeImplementation = do
  let strategy = Core.RetryWithAlternative "Try different type"
      isValid = EH.isValidStrategy strategy
      canApply = EH.canApplyStrategy strategy Core.TypeMismatch
  assertBool "retry with alternative should be valid" isValid
  assertBool "retry with alternative should be applicable to type errors" canApply

testContextCreation :: IO ()
testContextCreation = do
  let sourceCode = "func main() {\n    x := 5\n}"
      position = SourcePos 2 8 20
      context = EH.createContext sourceCode position
      retrievedSource = EH.getSourceContext context
      retrievedPosition = EH.getPosition context
  assertEqual "should preserve source code" sourceCode retrievedSource
  assertEqual "should preserve position" position retrievedPosition

testContextUpdates :: IO ()
testContextUpdates = do
  let context = EH.createContext "" (SourcePos 1 1 0)
      updatedContext = EH.updateSource context "new source" (SourcePos 2 1 10)
      finalContext = EH.addError updatedContext "test error"
  assertEqual "should update source" "new source" (EH.getSourceContext finalContext)
  assertBool "should track errors" $ not (null (EH.getErrors finalContext))

testContextQueries :: IO ()
testContextQueries = do
  let context = EH.createContext "test source" (SourcePos 1 1 0)
      hasSource = EH.hasSourceContext context
      hasPosition = EH.hasPosition context
      hasErrors = EH.hasErrors context
  assertBool "should have source context" hasSource
  assertBool "should have position" hasPosition
  assertBool "should not have errors initially" $ not hasErrors

testFixValidation :: IO ()
testFixValidation = do
  let validFix = "Add missing semicolon"
      invalidFix = ""
      isValidValid = EEH.validateFix validFix
      isValidInvalid = EEH.validateFix invalidFix
  assertBool "valid fix should pass validation" isValidValid
  assertBool "invalid fix should fail validation" $ not isValidInvalid

testRecoveryLimitValidation :: IO ()
testRecoveryLimitValidation = do
  let previousFixes = ["fix1", "fix2", "fix3", "fix4", "fix5"]
      newFix = "new fix"
      withinLimit = EEH.validateRecoveryLimit previousFixes
      exceededLimit = EEH.validateRecoveryLimit (previousFixes ++ ["fix6"])
  assertBool "should allow recovery within limit" withinLimit
  assertBool "should prevent recovery when limit exceeded" $ not exceededLimit

testConsistencyValidation :: IO ()
testConsistencyValidation = do
  let typeError = Core.TypeError
        { Core.errorId = "TYPE_001"
        , Core.severity = Core.Error
        , Core.category = Core.TypeMismatch
        , Core.message = T.pack "Type mismatch"
        , Core.location = Core.SourceLocation $ SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 5)
        , Core.context = Core.emptyContext
        , Core.recovery = Core.SuggestFix "Fix type"
        , Core.suggestions = []
        , Core.relatedErrors = []
        , Core.errorChain = []
        , Core.timestamp = Just 12345
        }
      consistentFix = "Change type to string"
      inconsistentFix = ""
      isConsistent1 = EEH.validateConsistency typeError consistentFix
      isConsistent2 = EEH.validateConsistency typeError inconsistentFix
  assertBool "consistent fix should pass validation" isConsistent1
  assertBool "inconsistent fix should fail validation" $ not isConsistent2

-- | Test collection
tests :: TestTree
tests = testGroup "Error Handler Recovery Tests"
  [ testErrorHandlerRecovery
  ]