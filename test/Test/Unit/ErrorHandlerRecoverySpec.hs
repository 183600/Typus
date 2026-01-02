{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Compiler.Errors.Core
  ( ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , TypeError(..)
  , ErrorCollector
  , newErrorCollector
  , addError
  , addWarning
  , addInfo
  , getErrors
  , getWarnings
  , getInfo
  , getAllMessages
  , hasErrors
  , hasWarnings
  , canRecoverFrom
  , shouldContinueAfter
  , getErrorColumn
  )

-- | Generate a valid error severity
genSeverity :: Gen ErrorSeverity
genSeverity = elements [Fatal, Error, Warning, Info]

-- | Generate a valid error category
genCategory :: Gen ErrorCategory
genCategory = elements 
  [ SyntaxError
  , TypeError
  , NameError
  , ScopeError
  , ImportError
  , ModuleError
  , DependencyError
  , OwnershipError
  , BorrowError
  , LifetimeError
  , ResourceError
  , ConcurrencyError
  , MemoryError
  , IOError
  , ConfigError
  , InternalError
  , UserError
  , Warning
  , Info
  , DependentTypeError
  , ConstraintError
  , InferenceError
  , UnificationError
  , SubstitutionError
  ]

-- | Generate a valid error location
genErrorLocation :: Gen ErrorLocation
genErrorLocation = do
  line <- choose (1, 1000)
  column <- choose (1, 100)
  endLine <- choose (line, line + 50)
  endColumn <- if endLine == line 
                 then choose (column, column + 50)
                 else choose (1, 100)
  filePath <- choose (Nothing, Just "test.typus")
  return $ ErrorLocation filePath line column (Just endLine) (Just endColumn)

-- | Generate error context
genErrorContext :: Gen ErrorContext
genErrorContext = do
  function <- choose (Nothing, Just "testFunction")
  module' <- choose (Nothing, Just "TestModule")
  return $ emptyContext { ecFunction = function, ecModule = module' }

-- | Generate a recovery strategy
genRecoveryStrategy :: Gen ErrorRecovery
genRecoveryStrategy = do
  canRecover <- elements [True, False]
  shouldContinue <- elements [True, False]
  recoveryAction <- choose (Nothing, Just "retry operation")
  recoveryHint <- choose (Nothing, Just "check syntax")
  recoveryCost <- choose (1, 100)
  recoveryConfidence <- choose (0.0, 1.0)
  return $ ErrorRecovery canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

-- | Generate a type error
genTypeError :: Gen TypeError
genTypeError = do
  errorId <- listOf1 (elements ['a'..'z'])
  severity <- genSeverity
  category <- genCategory
  message <- listOf1 (elements $ ['a'..'z'] ++ " ")
  location <- genErrorLocation
  context <- genErrorContext
  return $ TypeError errorId severity category (T.pack message) location context

instance Arbitrary ErrorSeverity where
  arbitrary = genSeverity

instance Arbitrary ErrorCategory where
  arbitrary = genCategory

instance Arbitrary ErrorLocation where
  arbitrary = genErrorLocation

instance Arbitrary TypeError where
  arbitrary = genTypeError

instance Arbitrary ErrorRecovery where
  arbitrary = genRecoveryStrategy

-- Property: severity priority ordering is consistent
prop_severityPriority_ordering :: Property
prop_severityPriority_ordering =
  severityPriority Fatal > severityPriority Error .&&.
  severityPriority Error > severityPriority Warning .&&.
  severityPriority Warning > severityPriority Info

-- Property: fatal recovery cannot recover from fatal errors
prop_fatalRecovery_cannotRecover :: Property
prop_fatalRecovery_cannotRecover =
  not (canRecover fatalRecovery) .&&.
  not (shouldContinueAfter fatalRecovery)

-- Property: info recovery can always continue
prop_infoRecovery_canContinue :: Property
prop_infoRecovery_canContinue =
  canRecover infoRecovery .&&.
  shouldContinueAfter infoRecovery

-- Property: custom recovery strategy preserves provided values
prop_customRecovery_preservesValues :: Property
prop_customRecovery_preservesValues =
  forAll arbitrary $ \canRec ->
    forAll arbitrary $ \shouldCont ->
      let recovery = customRecovery canRec shouldCont
      in canRecover recovery === canRec .&&.
         shouldContinueAfter recovery === shouldCont

-- Property: error recovery should not continue but can recover
prop_errorRecovery_properties :: Property
prop_errorRecovery_properties =
  canRecover errorRecovery .&&.
  not (shouldContinueAfter errorRecovery)

-- Property: warning recovery should continue L.and can recover
prop_warningRecovery_properties :: Property
prop_warningRecovery_properties =
  canRecover warningRecovery .&&.
  shouldContinueAfter warningRecovery

-- Property: error collector starts empty
prop_errorCollector_startsEmpty :: Property
prop_errorCollector_startsEmpty =
  let collector = newErrorCollector
  in not (hasErrors collector) .&&.
     not (hasWarnings collector) .&&.
     L.null (getErrors collector) .&&.
     L.null (getWarnings collector) .&&.
     L.null (getInfo collector)

-- Property: adding error increases error count
prop_addError_increasesErrorCount :: Property
prop_addError_increasesErrorCount =
  forAll genTypeError $ \err ->
    let collector1 = newErrorCollector
        collector2 = addError err collector1
    in L.length (getErrors collector2) === L.length (getErrors collector1) + 1 .&&.
       hasErrors collector2

-- Property: adding warning increases warning count
prop_addWarning_increasesWarningCount :: Property
prop_addWarning_increasesWarningCount =
  forAll genTypeError $ \warn ->
    let collector1 = newErrorCollector
        collector2 = addWarning warn collector1
    in L.length (getWarnings collector2) === L.length (getWarnings collector1) + 1 .&&.
       hasWarnings collector2

-- Property: adding info increases info count
prop_addInfo_increasesInfoCount :: Property
prop_addInfo_increasesInfoCount =
  forAll genTypeError $ \info ->
    let collector1 = newErrorCollector
        collector2 = addInfo info collector1
    in L.length (getInfo collector2) === L.length (getInfo collector1) + 1

-- Property: filtering by severity preserves order L.and content
prop_filterBySeverity_preservesContent :: Property
prop_filterBySeverity_preservesContent =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genSeverity $ \severity ->
      let filtered = filterBySeverity severity errors
          expected = L.filter (\e -> severity e >= severity) errors
      in L.length filtered === L.length expected .&&.
         L.all (\e -> severity e >= severity) filtered

-- Property: filtering by category preserves content
prop_filterByCategory_preservesContent :: Property
prop_filterByCategory_preservesContent =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genCategory $ \cat ->
      let filtered = filterByCategory cat errors
          expected = L.filter (\e -> category e == cat) errors
      in L.length filtered === L.length expected .&&.
         L.all (\e -> category e == cat) filtered

-- Property: hasCategory correctly identifies category presence
prop_hasCategory_identifiesPresence :: Property
prop_hasCategory_identifiesPresence =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genCategory $ \cat ->
      let hasCat = hasCategory cat errors
          hasCatExpected = L.any (\e -> category e == cat) errors
      in hasCat === hasCatExpected

-- Property: combining errors preserves L.all errors
prop_combineErrors_preservesAll :: Property
prop_combineErrors_preservesAll =
  forAll (listOf1 genTypeError) $ \errors1 ->
    forAll (listOf1 genTypeError) $ \errors2 ->
      let combined = combineErrors errors1 errors2
          totalLength = L.length errors1 + L.length errors2
      in L.length combined === totalLength .&&.
         L.all (\e -> e `elem` errors1 || e `elem` errors2) combined

-- Property: combined error severity is L.maximum of severities
prop_combinedErrorSeverity_isMaximum :: Property
prop_combinedErrorSeverity_isMaximum =
  forAll (listOf1 genTypeError) $ \errors ->
    let combined = combineErrors errors
        expectedSeverity = L.maximum $ map severity errors
    in combinedErrorSeverity combined === expectedSeverity

-- Property: errorAt "test-id" location" prop_getErrorColumn_returnsCorrectColumn
    ]