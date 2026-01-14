{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ErrorRecoveryConsistencySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort, nub, intersect, union, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Set as Set
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Mock data types for error recovery testing
data ErrorSeverity = Error | Warning | Info deriving (Show, Eq, Ord)

data ErrorPhase = Parsing | TypeChecking | CodeGeneration | Optimization deriving (Show, Eq, Ord)

data CompilerError = CompilerError
  { errorId :: String
  , errorPhase :: ErrorPhase
  , errorSeverity :: ErrorSeverity
  , errorMessage :: String
  , errorSpan :: SourceSpan
  } deriving (Show, Eq)

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
addError error context = 
  let newErrors = error : contextErrors context
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
isError error = errorSeverity error == Error

filterErrorsByPhase :: ErrorPhase -> ErrorContext -> [CompilerError]
filterErrorsByPhase phase context = 
  filter (\e -> errorPhase e == phase) $ contextErrors context

filterErrorsBySeverity :: ErrorSeverity -> ErrorContext -> [CompilerError]
filterErrorsBySeverity severity context = 
  filter (\e -> errorSeverity e == severity) $ contextErrors context

spec :: Spec
spec = describe "Error Recovery Consistency Tests" $ do

  describe "Compiler errors" $ do
    it "creates compiler errors correctly" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Error "Syntax error" span
      errorId error `shouldBe` "E001"
      errorPhase error `shouldBe` Parsing
      errorSeverity error `shouldBe` Error
      errorMessage error `shouldBe` "Syntax error"
      errorSpan error `shouldBe` span
      
    it "compares errors correctly" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error1 = CompilerError "E001" Parsing Error "Syntax error" span
          error2 = CompilerError "E001" Parsing Error "Syntax error" span
          error3 = CompilerError "E002" Parsing Error "Type error" span
      error1 `shouldBe` error2
      error1 `shouldNotBe` error3
      
    it "orders errors by severity" $ do
      let errors = [Error, Warning, Info]
      sort errors `shouldBe` [Info, Warning, Error]
      
    it "orders errors by phase" $ do
      let phases = [CodeGeneration, Parsing, TypeChecking, Optimization]
      sort phases `shouldBe` [Parsing, TypeChecking, CodeGeneration, Optimization]

  describe "Recovery actions" $ do
    it "creates recovery actions correctly" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          action = RecoveryAction Skip "token" span
      actionStrategy action `shouldBe` Skip
      actionTarget action `shouldBe` "token"
      actionSpan action `shouldBe` span
      
    it "adds recovery actions to context" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          action = RecoveryAction Skip "token" span
          context = ErrorContext [] [] False
          newContext = addRecoveryAction action context
      length (contextActions newContext) `shouldBe` 1
      head (contextActions newContext) `shouldBe` action
      
    it "handles multiple recovery actions" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          action1 = RecoveryAction Skip "token1" span
          action2 = RecoveryAction Retry "token2" span
          context = ErrorContext [] [] False
          context1 = addRecoveryAction action1 context
          context2 = addRecoveryAction action2 context1
      length (contextActions context2) `shouldBe` 2
      action1 `elem` contextActions context2 `shouldBe` True
      action2 `elem` contextActions context2 `shouldBe` True

  describe "Error context" $ do
    it "creates error context correctly" $ do
      let context = ErrorContext [] [] False
      contextErrors context `shouldBe` []
      contextActions context `shouldBe` []
      contextRecovered context `shouldBe` False
      
    it "adds errors to context" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Error "Syntax error" span
          context = ErrorContext [] [] False
          newContext = addError error context
      length (contextErrors newContext) `shouldBe` 1
      head (contextErrors newContext) `shouldBe` error
      
    it "handles multiple errors" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error1 = CompilerError "E001" Parsing Error "Syntax error" span
          error2 = CompilerError "E002" TypeChecking Error "Type error" span
          context = ErrorContext [] [] False
          context1 = addError error1 context
          context2 = addError error2 context1
      length (contextErrors context2) `shouldBe` 2
      error1 `elem` contextErrors context2 `shouldBe` True
      error2 `elem` contextErrors context2 `shouldBe` True

  describe "Recovery strategies" $ do
    it "applies Skip strategy" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Error "Syntax error" span
          context = ErrorContext [error] [] False
          result = applyRecoveryStrategy Skip context
      resultSuccess result `shouldBe` True
      resultRemainingErrors result `shouldBe` []
      
    it "applies Retry strategy" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Error "Syntax error" span
          context = ErrorContext [error] [] False
          result = applyRecoveryStrategy Retry context
      resultSuccess result `shouldBe` False
      resultRemainingErrors result `shouldBe` [error]
      
    it "applies Abort strategy" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Error "Syntax error" span
          context = ErrorContext [error] [] False
          result = applyRecoveryStrategy Abort context
      resultSuccess result `shouldBe` False
      resultRemainingErrors result `shouldBe` [error]
      
    it "applies Continue strategy" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error1 = CompilerError "E001" Parsing Error "Syntax error" span
          error2 = CompilerError "E002" TypeChecking Warning "Type warning" span
          context = ErrorContext [error1, error2] [] False
          result = applyRecoveryStrategy Continue context
      resultSuccess result `shouldBe` True
      resultRemainingErrors result `shouldBe` [error1]

  describe "Error filtering" $ do
    it "filters errors by phase" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error1 = CompilerError "E001" Parsing Error "Syntax error" span
          error2 = CompilerError "E002" TypeChecking Error "Type error" span
          error3 = CompilerError "E003" Parsing Warning "Syntax warning" span
          context = ErrorContext [error1, error2, error3] [] False
          parsingErrors = filterErrorsByPhase Parsing context
          typeErrors = filterErrorsByPhase TypeChecking context
      length parsingErrors `shouldBe` 2
      length typeErrors `shouldBe` 1
      error1 `elem` parsingErrors `shouldBe` True
      error3 `elem` parsingErrors `shouldBe` True
      error2 `elem` typeErrors `shouldBe` True
      
    it "filters errors by severity" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error1 = CompilerError "E001" Parsing Error "Syntax error" span
          error2 = CompilerError "E002" TypeChecking Warning "Type warning" span
          error3 = CompilerError "E003" Parsing Info "Syntax info" span
          context = ErrorContext [error1, error2, error3] [] False
          errors = filterErrorsBySeverity Error context
          warnings = filterErrorsBySeverity Warning context
          infos = filterErrorsBySeverity Info context
      length errors `shouldBe` 1
      length warnings `shouldBe` 1
      length infos `shouldBe` 1
      error1 `elem` errors `shouldBe` True
      error2 `elem` warnings `shouldBe` True
      error3 `elem` infos `shouldBe` True

  describe "Recovery consistency" $ do
    it "maintains error order during recovery" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error1 = CompilerError "E001" Parsing Error "Syntax error 1" span
          error2 = CompilerError "E002" Parsing Error "Syntax error 2" span
          error3 = CompilerError "E003" Parsing Error "Syntax error 3" span
          context = ErrorContext [error1, error2, error3] [] False
          result = applyRecoveryStrategy Retry context
      resultRemainingErrors result `shouldBe` [error1, error2, error3]
      
    it "preserves error spans during recovery" $ do
      let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          span2 = SourceSpan (SourcePos 2 1 10) (SourcePos 2 10 19)
          error1 = CompilerError "E001" Parsing Error "Syntax error 1" span1
          error2 = CompilerError "E002" Parsing Error "Syntax error 2" span2
          context = ErrorContext [error1, error2] [] False
          result = applyRecoveryStrategy Retry context
          let remainingErrors = resultRemainingErrors result
          errorSpan (remainingErrors !! 0) `shouldBe` span1
          errorSpan (remainingErrors !! 1) `shouldBe` span2
      
    it "maintains action consistency" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Error "Syntax error" span
          action = RecoveryAction Skip "token" span
          context = ErrorContext [error] [action] False
          result = applyRecoveryStrategy Skip context
      resultContext result `shouldBe` context

  describe "QuickCheck properties" $ do
    it "error addition preserves other errors" $ property $
      \error context ->
        let newContext = addError error context
            oldErrors = contextErrors context
            newErrors = contextErrors newContext
        in error `elem` newErrors &&
           all (`elem` newErrors) oldErrors
           
    it "action addition preserves other actions" $ property $
      \action context ->
        let newContext = addRecoveryAction action context
            oldActions = contextActions context
            newActions = contextActions newContext
        in action `elem` newActions &&
           all (`elem` newActions) oldActions
           
    it "recovery strategy is deterministic" $ property $
      \strategy context ->
        let result1 = applyRecoveryStrategy strategy context
            result2 = applyRecoveryStrategy strategy context
        in resultSuccess result1 `shouldBe` resultSuccess result2 &&
           resultRemainingErrors result1 `shouldBe` resultRemainingErrors result2
           
    it "error filtering is consistent" $ property $
      \phase context ->
        let filtered1 = filterErrorsByPhase phase context
            filtered2 = filterErrorsByPhase phase context
        in sort filtered1 `shouldBe` sort filtered2

  describe "Edge cases" $ do
    it "handles empty error context" $ do
      let context = ErrorContext [] [] False
          result = applyRecoveryStrategy Skip context
      resultSuccess result `shouldBe` True
      resultRemainingErrors result `shouldBe` []
      
    it "handles context with only warnings" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Warning "Syntax warning" span
          context = ErrorContext [error] [] False
          result = applyRecoveryStrategy Continue context
      resultSuccess result `shouldBe` True
      resultRemainingErrors result `shouldBe` []
      
    it "handles context with mixed severities" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error1 = CompilerError "E001" Parsing Error "Syntax error" span
          error2 = CompilerError "E002" Parsing Warning "Syntax warning" span
          error3 = CompilerError "E003" Parsing Info "Syntax info" span
          context = ErrorContext [error1, error2, error3] [] False
          result = applyRecoveryStrategy Continue context
      resultSuccess result `shouldBe` True
      resultRemainingErrors result `shouldBe` [error1]
      
    it "handles large error contexts" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          errors = [CompilerError ("E" ++ show i) Parsing Error ("Error " ++ show i) span | i <- [1..100]]
          context = ErrorContext errors [] False
          result = applyRecoveryStrategy Retry context
      length (resultRemainingErrors result) `shouldBe` 100
      
    it "handles cascading recovery strategies" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          error = CompilerError "E001" Parsing Error "Syntax error" span
          context = ErrorContext [error] [] False
          result1 = applyRecoveryStrategy Retry context
          result2 = applyRecoveryStrategy Skip $ resultContext result1
      resultSuccess result1 `shouldBe` False
      resultSuccess result2 `shouldBe` True