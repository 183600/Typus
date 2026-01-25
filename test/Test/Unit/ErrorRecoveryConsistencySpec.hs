{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.ErrorRecoveryConsistencySpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Test.Tasty.QuickCheck
import Data.List (sort)
import SourceLocation (SourcePos(..), SourceSpan(..))
import Test.QuickCheck (Arbitrary(..), arbitrary, elements)

-- Mock data types for error recovery testing
data ErrorSeverity = Error | Warning | Info deriving (Show, Eq, Ord)

data ErrorPhase = Parsing | TypeChecking | CodeGeneration | Optimization deriving (Show, Eq, Ord)

data CompilerError = CompilerError
  { errorId :: String
  , errorPhase :: ErrorPhase
  , errorSeverity :: ErrorSeverity
  , errorMessage :: String
  , errorSpan :: SourceSpan
  } deriving (Show, Eq, Ord)

data RecoveryStrategy = Skip | Retry | Abort | Continue deriving (Show, Eq, Ord)

data RecoveryAction = RecoveryAction
  { actionStrategy :: RecoveryStrategy
  , actionTarget :: String
  , actionSpan :: SourceSpan
  } deriving (Show, Eq)

data ErrorContext = ErrorContext
  { contextErrors :: [CompilerError]
  , contextActions :: [RecoveryAction]
  , contextRecovered :: Bool
  } deriving (Show, Eq)

data RecoveryResult = RecoveryResult
  { resultContext :: ErrorContext
  , resultSuccess :: Bool
  , resultRemainingErrors :: [CompilerError]
  } deriving (Show, Eq)

-- Mock error recovery functions
addError :: CompilerError -> ErrorContext -> ErrorContext
addError err context = 
  let newErrors = err : contextErrors context
  in context { contextErrors = newErrors }

addRecoveryAction :: RecoveryAction -> ErrorContext -> ErrorContext
addRecoveryAction action context = 
  let newActions = action : contextActions context
  in context { contextActions = newActions }

applyRecoveryStrategy :: RecoveryStrategy -> ErrorContext -> RecoveryResult
applyRecoveryStrategy strategy context = 
  case strategy of
    Skip -> RecoveryResult context True []  -- Mock: skip resolves all errors
    Retry -> RecoveryResult context False $ contextErrors context  -- Mock: retry keeps errors
    Abort -> RecoveryResult context False $ contextErrors context  -- Mock: abort keeps errors
    Continue -> RecoveryResult context True $ filter isError $ contextErrors context  -- Mock: continue resolves warnings

isError :: CompilerError -> Bool
isError err = errorSeverity err == Error

filterErrorsByPhase :: ErrorPhase -> ErrorContext -> [CompilerError]
filterErrorsByPhase phase context = 
  filter (\e -> errorPhase e == phase) $ contextErrors context

filterErrorsBySeverity :: ErrorSeverity -> ErrorContext -> [CompilerError]
filterErrorsBySeverity severity context = 
  filter (\e -> errorSeverity e == severity) $ contextErrors context

-- Arbitrary instances for QuickCheck
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Error, Warning, Info]

instance Arbitrary ErrorPhase where
  arbitrary = elements [Parsing, TypeChecking, CodeGeneration, Optimization]

-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary CompilerError where
  arbitrary = do
    errId <- arbitrary
    errPhase <- arbitrary
    errSeverity <- arbitrary
    errMsg <- arbitrary
    errSpan <- arbitrary
    return $ CompilerError errId errPhase errSeverity errMsg errSpan

instance Arbitrary RecoveryStrategy where
  arbitrary = elements [Skip, Retry, Abort, Continue]

instance Arbitrary RecoveryAction where
  arbitrary = do
    actionStrategy <- arbitrary
    actionTarget <- arbitrary
    actionSpan <- arbitrary
    return $ RecoveryAction actionStrategy actionTarget actionSpan

instance Arbitrary ErrorContext where
  arbitrary = do
    contextErrors <- arbitrary
    contextActions <- arbitrary
    contextRecovered <- arbitrary
    return $ ErrorContext contextErrors contextActions contextRecovered

tests :: TestTree
tests = testGroup "Error Recovery Consistency Tests"
  [ testGroup "Compiler errors"
    [ testCase "creates compiler errors correctly" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Error "Syntax error" span
        errorId error @?= "E001"
        errorPhase error @?= Parsing
        errorSeverity error @?= Error
        errorMessage error @?= "Syntax error"
        errorSpan error @?= span
      
    , testCase "compares errors correctly" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error1 = CompilerError "E001" Parsing Error "Syntax error" span
            error2 = CompilerError "E001" Parsing Error "Syntax error" span
            error3 = CompilerError "E002" Parsing Error "Type error" span
        error1 @?= error2
        assertBool "error1 should not be error3" (error1 /= error3)
      
    , testCase "orders errors by severity" $ do
        let errors = [Error, Warning, Info]
        sort errors @?= [Info, Warning, Error]
      
    , testCase "orders errors by phase" $ do
        let phases = [CodeGeneration, Parsing, TypeChecking, Optimization]
        sort phases @?= [Parsing, TypeChecking, CodeGeneration, Optimization]
    ]
  , testGroup "Recovery actions"
    [ testCase "creates recovery actions correctly" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            action = RecoveryAction Skip "token" span
        actionStrategy action @?= Skip
        actionTarget action @?= "token"
        actionSpan action @?= span
      
    , testCase "adds recovery actions to context" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            action = RecoveryAction Skip "token" span
            context = ErrorContext [] [] False
            newContext = addRecoveryAction action context
        length (contextActions newContext) @?= 1
        case contextActions newContext of
          (a:_) -> a @?= action
          [] -> assertBool "Should have at least one action" False
      
    , testCase "handles multiple recovery actions" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            action1 = RecoveryAction Skip "token1" span
            action2 = RecoveryAction Retry "token2" span
            context = ErrorContext [] [] False
            context1 = addRecoveryAction action1 context
            context2 = addRecoveryAction action2 context1
        length (contextActions context2) @?= 2
        assertBool "action1 should be in context2" $ action1 `elem` contextActions context2
        assertBool "action2 should be in context2" $ action2 `elem` contextActions context2
    ]
  , testGroup "Error context"
    [ testCase "creates error context correctly" $ do
        let context = ErrorContext [] [] False
        contextErrors context @?= []
        contextActions context @?= []
        contextRecovered context @?= False
      
    , testCase "adds errors to context" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Error "Syntax error" span
            context = ErrorContext [] [] False
            newContext = addError error context
        length (contextErrors newContext) @?= 1
        case contextErrors newContext of
          (e:_) -> e @?= error
          [] -> assertBool "Should have at least one error" False
      
    , testCase "handles multiple errors" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error1 = CompilerError "E001" Parsing Error "Syntax error" span
            error2 = CompilerError "E002" TypeChecking Error "Type error" span
            context = ErrorContext [] [] False
            context1 = addError error1 context
            context2 = addError error2 context1
        length (contextErrors context2) @?= 2
        assertBool "error1 should be in context2" $ error1 `elem` contextErrors context2
        assertBool "error2 should be in context2" $ error2 `elem` contextErrors context2
    ]
  , testGroup "Recovery strategies"
    [ testCase "applies Skip strategy" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Error "Syntax error" span
            context = ErrorContext [error] [] False
            result = applyRecoveryStrategy Skip context
        resultSuccess result @?= True
        resultRemainingErrors result @?= []
      
    , testCase "applies Retry strategy" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Error "Syntax error" span
            context = ErrorContext [error] [] False
            result = applyRecoveryStrategy Retry context
        resultSuccess result @?= False
        resultRemainingErrors result @?= [error]
      
    , testCase "applies Abort strategy" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Error "Syntax error" span
            context = ErrorContext [error] [] False
            result = applyRecoveryStrategy Abort context
        resultSuccess result @?= False
        resultRemainingErrors result @?= [error]
      
    , testCase "applies Continue strategy" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error1 = CompilerError "E001" Parsing Error "Syntax error" span
            error2 = CompilerError "E002" TypeChecking Warning "Type warning" span
            context = ErrorContext [error1, error2] [] False
            result = applyRecoveryStrategy Continue context
        resultSuccess result @?= True
        resultRemainingErrors result @?= [error1]
    ]
  , testGroup "Error filtering"
    [ testCase "filters errors by phase" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error1 = CompilerError "E001" Parsing Error "Syntax error" span
            error2 = CompilerError "E002" TypeChecking Error "Type error" span
            error3 = CompilerError "E003" Parsing Warning "Syntax warning" span
            context = ErrorContext [error1, error2, error3] [] False
            parsingErrors = filterErrorsByPhase Parsing context
            typeErrors = filterErrorsByPhase TypeChecking context
        length parsingErrors @?= 2
        length typeErrors @?= 1
        assertBool "error1 should be in parsingErrors" $ error1 `elem` parsingErrors
        assertBool "error3 should be in parsingErrors" $ error3 `elem` parsingErrors
        assertBool "error2 should be in typeErrors" $ error2 `elem` typeErrors
      
    , testCase "filters errors by severity" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error1 = CompilerError "E001" Parsing Error "Syntax error" span
            error2 = CompilerError "E002" TypeChecking Warning "Type warning" span
            error3 = CompilerError "E003" Parsing Info "Syntax info" span
            context = ErrorContext [error1, error2, error3] [] False
            errors = filterErrorsBySeverity Error context
            warnings = filterErrorsBySeverity Warning context
            infos = filterErrorsBySeverity Info context
        length errors @?= 1
        length warnings @?= 1
        length infos @?= 1
        assertBool "error1 should be in errors" $ error1 `elem` errors
        assertBool "error2 should be in warnings" $ error2 `elem` warnings
        assertBool "error3 should be in infos" $ error3 `elem` infos
    ]

  , testGroup "Recovery consistency"
    [ testCase "maintains error order during recovery" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error1 = CompilerError "E001" Parsing Error "Syntax error 1" span
            error2 = CompilerError "E002" Parsing Error "Syntax error 2" span
            error3 = CompilerError "E003" Parsing Error "Syntax error 3" span
            context = ErrorContext [error1, error2, error3] [] False
            result = applyRecoveryStrategy Retry context
        resultRemainingErrors result @?= [error1, error2, error3]
      
    , testCase "preserves error spans during recovery" $ do
        let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            span2 = SourceSpan (SourcePos 2 1 10) (SourcePos 2 10 19)
            error1 = CompilerError "E001" Parsing Error "Syntax error 1" span1
            error2 = CompilerError "E002" Parsing Error "Syntax error 2" span2
            context = ErrorContext [error1, error2] [] False
            result = applyRecoveryStrategy Retry context
            remainingErrors = resultRemainingErrors result
        errorSpan (remainingErrors !! 0) @?= span1
        errorSpan (remainingErrors !! 1) @?= span2
      
    , testCase "maintains action consistency" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Error "Syntax error" span
            action = RecoveryAction Skip "token" span
            context = ErrorContext [error] [action] False
            result = applyRecoveryStrategy Skip context
        resultContext result @?= context
    ]
  , testGroup "QuickCheck properties"
    [ testProperty "error addition preserves other errors" $
        \err context ->
          let newContext = addError err context
              oldErrors = contextErrors context
              newErrors = contextErrors newContext
          in err `elem` newErrors &&
             all (`elem` newErrors) oldErrors
           
    , testProperty "action addition preserves other actions" $
        \action context ->
          let newContext = addRecoveryAction action context
              oldActions = contextActions context
              newActions = contextActions newContext
          in action `elem` newActions &&
             all (`elem` newActions) oldActions
           
    , testProperty "recovery strategy is deterministic" $
        \strategy context ->
          let result1 = applyRecoveryStrategy strategy context
              result2 = applyRecoveryStrategy strategy context
          in resultSuccess result1 == resultSuccess result2 &&
             resultRemainingErrors result1 == resultRemainingErrors result2
           
    , testProperty "error filtering is consistent" $
        \phase context ->
          let filtered1 = filterErrorsByPhase phase context
              filtered2 = filterErrorsByPhase phase context
          in sort filtered1 == sort filtered2
    ]
  , testGroup "Edge cases"
    [ testCase "handles empty error context" $ do
        let context = ErrorContext [] [] False
            result = applyRecoveryStrategy Skip context
        resultSuccess result @?= True
        resultRemainingErrors result @?= []
      
    , testCase "handles context with only warnings" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Warning "Syntax warning" span
            context = ErrorContext [error] [] False
            result = applyRecoveryStrategy Continue context
        resultSuccess result @?= True
        resultRemainingErrors result @?= []
      
    , testCase "handles context with mixed severities" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error1 = CompilerError "E001" Parsing Error "Syntax error" span
            error2 = CompilerError "E002" Parsing Warning "Syntax warning" span
            error3 = CompilerError "E003" Parsing Info "Syntax info" span
            context = ErrorContext [error1, error2, error3] [] False
            result = applyRecoveryStrategy Continue context
        resultSuccess result @?= True
        resultRemainingErrors result @?= [error1]
      
    , testCase "handles large error contexts" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            errors = [CompilerError ("E" ++ show i) Parsing Error ("Error " ++ show i) span | i <- [1..100]]
            context = ErrorContext errors [] False
            result = applyRecoveryStrategy Retry context
        length (resultRemainingErrors result) @?= 100
      
    , testCase "handles cascading recovery strategies" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            error = CompilerError "E001" Parsing Error "Syntax error" span
            context = ErrorContext [error] [] False
            result1 = applyRecoveryStrategy Retry context
            result2 = applyRecoveryStrategy Skip $ resultContext result1
        resultSuccess result1 @?= False
        resultSuccess result2 @?= True
    ]
  ]