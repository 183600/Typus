{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorHandlerRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
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
  , errorAt
  , warningAt
  , infoAt
  , withLocation
  , withContext
  , combineErrors
  , combinedErrorSeverity
  , filterBySeverity
  , filterByCategory
  , hasCategory
  , severityPriority
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , emptyContext
  , _unknownLocation
  , getErrorLine
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

-- Property: warning recovery should continue and can recover
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
     null (getErrors collector) .&&.
     null (getWarnings collector) .&&.
     null (getInfo collector)

-- Property: adding error increases error count
prop_addError_increasesErrorCount :: Property
prop_addError_increasesErrorCount =
  forAll genTypeError $ \err ->
    let collector1 = newErrorCollector
        collector2 = addError err collector1
    in length (getErrors collector2) === length (getErrors collector1) + 1 .&&.
       hasErrors collector2

-- Property: adding warning increases warning count
prop_addWarning_increasesWarningCount :: Property
prop_addWarning_increasesWarningCount =
  forAll genTypeError $ \warn ->
    let collector1 = newErrorCollector
        collector2 = addWarning warn collector1
    in length (getWarnings collector2) === length (getWarnings collector1) + 1 .&&.
       hasWarnings collector2

-- Property: adding info increases info count
prop_addInfo_increasesInfoCount :: Property
prop_addInfo_increasesInfoCount =
  forAll genTypeError $ \info ->
    let collector1 = newErrorCollector
        collector2 = addInfo info collector1
    in length (getInfo collector2) === length (getInfo collector1) + 1

-- Property: filtering by severity preserves order and content
prop_filterBySeverity_preservesContent :: Property
prop_filterBySeverity_preservesContent =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genSeverity $ \severity ->
      let filtered = filterBySeverity severity errors
          expected = filter (\e -> severity e >= severity) errors
      in length filtered === length expected .&&.
         all (\e -> severity e >= severity) filtered

-- Property: filtering by category preserves content
prop_filterByCategory_preservesContent :: Property
prop_filterByCategory_preservesContent =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genCategory $ \cat ->
      let filtered = filterByCategory cat errors
          expected = filter (\e -> category e == cat) errors
      in length filtered === length expected .&&.
         all (\e -> category e == cat) filtered

-- Property: hasCategory correctly identifies category presence
prop_hasCategory_identifiesPresence :: Property
prop_hasCategory_identifiesPresence =
  forAll (listOf1 genTypeError) $ \errors ->
    forAll genCategory $ \cat ->
      let hasCat = hasCategory cat errors
          hasCatExpected = any (\e -> category e == cat) errors
      in hasCat === hasCatExpected

-- Property: combining errors preserves all errors
prop_combineErrors_preservesAll :: Property
prop_combineErrors_preservesAll =
  forAll (listOf1 genTypeError) $ \errors1 ->
    forAll (listOf1 genTypeError) $ \errors2 ->
      let combined = combineErrors errors1 errors2
          totalLength = length errors1 + length errors2
      in length combined === totalLength .&&.
         all (\e -> e `elem` errors1 || e `elem` errors2) combined

-- Property: combined error severity is maximum of severities
prop_combinedErrorSeverity_isMaximum :: Property
prop_combinedErrorSeverity_isMaximum =
  forAll (listOf1 genTypeError) $ \errors ->
    let combined = combineErrors errors
        expectedSeverity = maximum $ map severity errors
    in combinedErrorSeverity combined === expectedSeverity

-- Property: errorAt creates error with correct location
prop_errorAt_correctLocation :: Property
prop_errorAt_correctLocation =
  forAll genErrorLocation $ \loc ->
    forAll arbitrary $ \severity ->
      forAll arbitrary $ \category ->
        forAll arbitrary $ \message ->
          let err = errorAt loc severity category message
          in location err === loc .&&.
             severity err === severity .&&.
             category err === category .&&.
             message err === message

-- Property: warningAt creates warning with correct severity
prop_warningAt_correctSeverity :: Property
prop_warningAt_correctSeverity =
  forAll genErrorLocation $ \loc ->
    forAll arbitrary $ \category ->
      forAll arbitrary $ \message ->
        let warn = warningAt loc category message
        in location warn === loc .&&.
           severity warn === Warning .&&.
           category warn === category .&&.
           message warn === message

-- Property: infoAt creates info with correct severity
prop_infoAt_correctSeverity :: Property
prop_infoAt_correctSeverity =
  forAll genErrorLocation $ \loc ->
    forAll arbitrary $ \category ->
      forAll arbitrary $ \message ->
        let info = infoAt loc category message
        in location info === loc .&&.
           severity info === Info .&&.
           category info === category .&&.
           message info === message

-- Property: withLocation updates location correctly
prop_withLocation_updatesLocation :: Property
prop_withLocation_updatesLocation =
  forAll genTypeError $ \err ->
    forAll genErrorLocation $ \newLoc ->
      let updatedErr = withLocation newLoc err
      in location updatedErr === newLoc .&&.
         errorId updatedErr === errorId err .&&.
         severity updatedErr === severity err .&&.
         category updatedErr === category err .&&.
         message updatedErr === message err

-- Property: withContext updates context correctly
prop_withContext_updatesContext :: Property
prop_withContext_updatesContext =
  forAll genTypeError $ \err ->
    forAll genErrorContext $ \newContext ->
      let updatedErr = withContext newContext err
      in context updatedErr === newContext .&&.
         errorId updatedErr === errorId err .&&.
         severity updatedErr === severity err .&&.
         category updatedErr === category err .&&.
         message updatedErr === message err .&&.
         location updatedErr === location err

-- Property: getErrorLine returns correct line from location
prop_getErrorLine_returnsCorrectLine :: Property
prop_getErrorLine_returnsCorrectLine =
  forAll genErrorLocation $ \loc ->
    getErrorLine loc === line loc

-- Property: getErrorColumn returns correct column from location
prop_getErrorColumn_returnsCorrectColumn :: Property
prop_getErrorColumn_returnsCorrectColumn =
  forAll genErrorLocation $ \loc ->
    getErrorColumn loc === column loc

tests :: TestTree
tests =
  testGroup "ErrorHandler Recovery Properties"
    [ fastProperty "severity priority ordering is consistent" prop_severityPriority_ordering
    , fastProperty "fatal recovery cannot recover from fatal errors" prop_fatalRecovery_cannotRecover
    , fastProperty "info recovery can always continue" prop_infoRecovery_canContinue
    , fastProperty "custom recovery strategy preserves provided values" prop_customRecovery_preservesValues
    , fastProperty "error recovery should not continue but can recover" prop_errorRecovery_properties
    , fastProperty "warning recovery should continue and can recover" prop_warningRecovery_properties
    , fastProperty "error collector starts empty" prop_errorCollector_startsEmpty
    , fastProperty "adding error increases error count" prop_addError_increasesErrorCount
    , fastProperty "adding warning increases warning count" prop_addWarning_increasesWarningCount
    , fastProperty "adding info increases info count" prop_addInfo_increasesInfoCount
    , fastProperty "filtering by severity preserves content" prop_filterBySeverity_preservesContent
    , fastProperty "filtering by category preserves content" prop_filterByCategory_preservesContent
    , fastProperty "hasCategory correctly identifies category presence" prop_hasCategory_identifiesPresence
    , fastProperty "combining errors preserves all errors" prop_combineErrors_preservesAll
    , fastProperty "combined error severity is maximum of severities" prop_combinedErrorSeverity_isMaximum
    , fastProperty "errorAt creates error with correct location" prop_errorAt_correctLocation
    , fastProperty "warningAt creates warning with correct severity" prop_warningAt_correctSeverity
    , fastProperty "infoAt creates info with correct severity" prop_infoAt_correctSeverity
    , fastProperty "withLocation updates location correctly" prop_withLocation_updatesLocation
    , fastProperty "withContext updates context correctly" prop_withContext_updatesContext
    , fastProperty "getErrorLine returns correct line from location" prop_getErrorLine_returnsCorrectLine
    , fastProperty "getErrorColumn returns correct column from location" prop_getErrorColumn_returnsCorrectColumn
    ]