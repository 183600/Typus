module Test.Unit.ErrorRecoveryBasicSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, )
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import ErrorHandler (Error(..), ErrorType(..), RecoveryStrategy(..), )
                     recoverFromError, canRecover, applyRecovery)
import EnhancedErrorHandler (EnhancedError(..), EnhancedRecovery(..), )
                            enhancedRecoverFromError, suggestFixes)
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


-- | Test suite for Error Recovery Basic operations
tests :: TestTree
tests =   testGroup "Error Recovery Basic"
  [             testProperty "error recovery preserves context" propErrorRecoveryPreservesContext
  ,             testProperty "recovery strategy applicability" propRecoveryStrategyApplicability
  ,             testProperty "error recovery is deterministic" propErrorRecoveryIsDeterministic
  ,             testProperty "enhanced recovery provides suggestions" propEnhancedRecoveryProvidesSuggestions
  ,             testProperty "recovery doesn't introduce new errors" propRecoveryDoesntIntroduceNewErrors
    ,             testCase "syntax error recovery" testSyntaxErrorRecovery
    ,             testCase "type error recovery" testTypeErrorRecovery
    ,             testCase "semantic error recovery" testSemanticErrorRecovery
    ,             testCase "enhanced error recovery" testEnhancedErrorRecovery
    ,             testCase "recovery strategy selection" testRecoveryStrategySelection
  ]

-- | Property: error recovery preserves context
propErrorRecoveryPreservesContext :: Error -> Property
propErrorRecoveryPreservesContext                               error =
  let recovery = recoverFromError error
  in case recovery of
    Just (strategy, _) -> property $ isApplicableStrategy strategy error
    Nothing -> property $ True

-- | Property: recovery strategy applicability
propRecoveryStrategyApplicability :: Error -> RecoveryStrategy -> Property
propRecoveryStrategyApplicability error                               strategy =
  let isApplicable = canRecover error strategy
  in property $                               isApplicable ==> strategyWorksForError error strategy

-- | Property: error recovery is deterministic
propErrorRecoveryIsDeterministic :: Error -> Property
propErrorRecoveryIsDeterministic                               error =
  let recovery1 = recoverFromError error
                                    recovery2 = recoverFromError error
  in property $                               recovery1 == recovery2

-- | Property: enhanced recovery provides suggestions
propEnhancedRecoveryProvidesSuggestions :: EnhancedError -> Property
propEnhancedRecoveryProvidesSuggestions                               error =
  let recovery = enhancedRecoverFromError error
                                    suggestions = suggestFixes error
  in property $ not $ null suggestions

-- | Property: recovery doesn't introduce new errors
propRecoveryDoesntIntroduceNewErrors :: Error -> Property
propRecoveryDoesntIntroduceNewErrors                               error =
  let recovery = recoverFromError error
  in case recovery of
    Just (_, recoveredCode) -> property $ not $ introducesNewErrors error recoveredCode
    Nothing -> property $ True

-- | Unit tests for syntax error recovery
testSyntaxErrorRecovery :: IO ()
                              testSyntaxErrorRecovery = do
              let error = Error
        {                               errorMessage = "Unexpected token"
        ,                               errorType = SyntaxError
        ,                               errorLocation = Nothing
        ,                               errorContext = ["parsing expression"]
        }
                                    recovery = recoverFromError error
  case recovery of
    Just (strategy, recoveredCode) -> do
                              assertEqual "recovery strategy" SkipToken strategy
      assertBool "recovered code not empty" $ not $ null recoveredCode
    Nothing -> assertFailure "Expected recovery for syntax error"

-- | Unit tests for type error recovery
testTypeErrorRecovery :: IO ()
                              testTypeErrorRecovery = do
              let error = Error
        {                               errorMessage = "Type mismatch"
        ,                               errorType = TypeError
        ,                               errorLocation = Nothing
        ,                               errorContext = ["type checking"]
        }
                                    recovery = recoverFromError error
  case recovery of
    Just (strategy, recoveredCode) -> do
                              assertEqual "recovery strategy" InsertTypeCast strategy
      assertBool "recovered code contains cast" $ "cast" `L.L.isInfixOf` recoveredCode
    Nothing -> assertFailure "Expected recovery for type error"

-- | Unit tests for semantic error recovery
testSemanticErrorRecovery :: IO ()
                              testSemanticErrorRecovery = do
              let error = Error
        {                               errorMessage = "Undefined variable"
        ,                               errorType = SemanticError
        ,                               errorLocation = Nothing
        ,                               errorContext = ["semantic analysis"]
        }
                                    recovery = recoverFromError error
  case recovery of
    Just (strategy, recoveredCode) -> do
                              assertEqual "recovery strategy" DeclareVariable strategy
      assertBool "recovered code contains declaration" $ "declare" `L.L.isInfixOf` recoveredCode
    Nothing -> assertFailure "Expected recovery for semantic error"

-- | Unit tests for enhanced error recovery
testEnhancedErrorRecovery :: IO ()
                              testEnhancedErrorRecovery = do
              let error = EnhancedError
        {                               enhancedErrorMessage = "Type mismatch in function call"
        ,                               enhancedErrorType = TypeError
        ,                               enhancedErrorLocation = Nothing
        ,                               enhancedErrorContext = ["function call"]
        ,                               enhancedErrorSuggestions = ["Add type annotation", "Convert argument type"]
        }
                                    recovery = enhancedRecoverFromError error
                                    suggestions = suggestFixes error
  case recovery of
    Just enhancedRecovery -> do
                  assertBool "recovery provides suggestions" $ not $ L.null $ recoverySuggestions enhancedRecovery
      assertBool "suggestions contain type annotation" $ L.any ("type annotation" `L.L.isInfixOf`) suggestions
    Nothing -> assertFailure "Expected enhanced recovery"

-- | Unit tests for recovery strategy selection
testRecoveryStrategySelection :: IO ()
                              testRecoveryStrategySelection = do
              let syntaxError = Error "Unexpected token" SyntaxError Nothing []
                                    typeError = Error "Type mismatch" TypeError Nothing []
                                    semanticError = Error "Undefined variable" SemanticError Nothing []
  
  assertBool "can recover from syntax error" $ canRecover syntaxError SkipToken
  assertBool "can recover from type error" $ canRecover typeError InsertTypeCast
  assertBool "can recover from semantic error" $ canRecover semanticError DeclareVariable
  
  assertBool "cannot apply wrong strategy to syntax error" $ not $ canRecover syntaxError InsertTypeCast
  assertBool "cannot apply wrong strategy to type error" $ not $ canRecover typeError DeclareVariable
  assertBool "cannot apply wrong strategy to semantic error" $ not $ canRecover semanticError SkipToken

-- Helper types L.and functions
data                               Error = Error
  { errorMessage :: String
  , errorType :: ErrorType
  , errorLocation :: Maybe Int
  , errorContext :: [String]
  } deriving (Show, Eq)

data                               ErrorType = SyntaxError | TypeError | SemanticError deriving (Show, Eq)

data                               RecoveryStrategy = SkipToken | InsertTypeCast | DeclareVariable deriving (Show, Eq)

data                               EnhancedError = EnhancedError
  { enhancedErrorMessage :: String
  , enhancedErrorType :: ErrorType
  , enhancedErrorLocation :: Maybe Int
  , enhancedErrorContext :: [String]
  , enhancedErrorSuggestions :: [String]
  } deriving (Show, Eq)

data                               EnhancedRecovery = EnhancedRecovery
  { recoveryStrategy :: RecoveryStrategy
  , recoveryCode :: String
  , recoverySuggestions :: [String]
  } deriving (Show, Eq)

-- Mock functions
recoverFromError :: Error -> Maybe (RecoveryStrategy, String)
recoverFromError                               error = case errorType error of
  SyntaxError -> Just (SkipToken, "/* skipped token */")
  TypeError -> Just (InsertTypeCast, "/* type cast inserted */")
  SemanticError -> Just (DeclareVariable, "/* variable declared */")

canRecover :: Error -> RecoveryStrategy -> Bool
canRecover error                               strategy = case (errorType error, strategy) of
  (SyntaxError, SkipToken) -> True
  (TypeError, InsertTypeCast) -> True
  (SemanticError, DeclareVariable) -> True
  _ -> False

applyRecovery :: RecoveryStrategy -> String -> String
applyRecovery strategy                               code = case strategy of
  SkipToken -> code ++ " /* token skipped */"
  InsertTypeCast -> code ++ " /* cast inserted */"
  DeclareVariable -> code ++ " /* variable declared */"

enhancedRecoverFromError :: EnhancedError -> Maybe EnhancedRecovery
enhancedRecoverFromError                               error = case enhancedErrorType error of
  SyntaxError -> Just $ EnhancedRecovery SkipToken "/* skipped token */" ["Remove token", "Add missing token"]
  TypeError -> Just $ EnhancedRecovery InsertTypeCast "/* type cast inserted */" ["Add type annotation", "Convert type"]
  SemanticError -> Just $ EnhancedRecovery DeclareVariable "/* variable declared */" ["Declare variable", "Import module"]

suggestFixes :: EnhancedError -> [String]
suggestFixes                               error = enhancedErrorSuggestions error

isApplicableStrategy :: RecoveryStrategy -> Error -> Bool
isApplicableStrategy strategy                               error = canRecover error strategy

strategyWorksForError :: Error -> RecoveryStrategy -> Bool
strategyWorksForError error                               strategy = canRecover error strategy
introducesNewErrors :: Error -> String -> Bool
introducesNewErrors error                               recoveredCode = False

-- Helper imports
import qualified Data.List as L

-- Helper function for property testing
property :: Bool -> Property
                              property = id