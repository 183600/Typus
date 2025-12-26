{-# LANGUAGE CPP #-}
module Test.Unit.ErrorHandlerAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, choose, listOf, forAll, Property, (===), counterexample, (==>))

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (sort, sortOn)

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
  , severityPriority
  , isAtLeast
  , canRecoverFrom
  , shouldContinueAfter
  , formatError
  , formatErrors
  , errorAt
  , warningAt
  , infoAt
  , fatalError
  , errorWithCategory
  , filterBySeverity
  , filterByCategory
  , hasErrors
  , hasWarnings
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , getErrorLine
  , getErrorColumn
  , _atLocation
  , _atFileLocation
  , _atRange
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  )

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath <- oneof [return Nothing, fmap Just arbitrary]
    line <- choose (1, 1000)
    column <- choose (1, 200)
    endLine <- oneof [return Nothing, fmap Just (choose (line, line + 100))]
    endColumn <- oneof [return Nothing, fmap Just (choose (column, column + 100))]
    return $ ErrorLocation filePath line column endLine endColumn

instance Arbitrary ErrorContext where
  arbitrary = do
    contextCode <- oneof [return Nothing, fmap Just arbitrary]
    contextFunction <- oneof [return Nothing, fmap Just arbitrary]
    contextVariable <- oneof [return Nothing, fmap Just arbitrary]
    contextType <- oneof [return Nothing, fmap Just arbitrary]
    contextAdditional <- listOf $ do
      key <- arbitrary
      value <- arbitrary
      return (key, value)
    return $ ErrorContext contextCode contextFunction contextVariable contextType contextAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRecover <- arbitrary
    shouldContinue <- arbitrary
    recoveryAction <- oneof [return Nothing, fmap Just arbitrary]
    recoveryHint <- oneof [return Nothing, fmap Just arbitrary]
    recoveryCost <- choose (0, 100)
    recoveryConfidence <- choose (0.0, 1.0)
    return $ RecoveryStrategy canRecover shouldContinue recoveryAction recoveryHint recoveryCost recoveryConfidence

instance Arbitrary TypeError where
  arbitrary = do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> arbitrary)
    relatedErrors <- listOf arbitrary
    errorChain <- listOf arbitrary
    timestamp <- oneof [return Nothing, fmap Just arbitrary]
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
    , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
    , IntegrationError <$> arbitrary <*> arbitrary
    , CrossAnalyzerError <$> arbitrary <*> arbitrary <*> listOf arbitrary
    ]

-- ============================================================================
-- Property Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "ErrorHandler Advanced QuickCheck Tests"
    [ testProperty "severityPriority respects ordering: Fatal > Error > Warning > Info" $
        \sev1 sev2 ->
          let p1 = severityPriority sev1
              p2 = severityPriority sev2
          in (sev1 > sev2) === (p1 > p2)

    , testProperty "isAtLeast is reflexive" $
        \sev -> isAtLeast sev sev

    , testProperty "isAtLeast is transitive" $
        \sev1 sev2 sev3 ->
          isAtLeast sev1 sev2 && isAtLeast sev2 sev3 ==> isAtLeast sev1 sev3

    , testProperty "filterBySeverity preserves ordering" $
        \errors minSev ->
          let filtered = filterBySeverity minSev errors
              sorted = sortOn severityPriority filtered
          in filtered === sorted

    , testProperty "filterByCategory only returns errors of specified category" $
        \errors cat ->
          let filtered = filterByCategory cat errors
          in all (\e -> category e == cat) filtered

    , testProperty "filterBySeverity with Fatal returns only Fatal errors" $
        \errors ->
          let fatalErrors = filterBySeverity Fatal errors
          in all (\e -> severity e == Fatal) fatalErrors

    , testProperty "hasErrors is True iff there are Error or Fatal severity errors" $
        \errors ->
          let hasErrorOrFatal = any (\e -> severity e == Error || severity e == Fatal) errors
          in hasErrors errors === hasErrorOrFatal

    , testProperty "hasWarnings is True iff there are Warning severity errors" $
        \errors ->
          let hasWarningSeverity = any (\e -> severity e == Warning) errors
          in hasWarnings errors === hasWarningSeverity

    , testProperty "errorAt creates error with correct location" $
        \line column msg ->
          let location = _atLocation line column
              err = errorAt line column msg
          in getErrorLine (location err) === line .&&.
             getErrorColumn (location err) === column

    , testProperty "warningAt creates warning with correct severity" $
        \line column msg ->
          let err = warningAt line column msg
          in severity err === Warning

    , testProperty "infoAt creates info with correct severity" $
        \line column msg ->
          let err = infoAt line column msg
          in severity err === Info

    , testProperty "fatalError creates fatal error with correct severity" $
        \msg ->
          let err = fatalError msg
          in severity err === Fatal

    , testProperty "errorWithCategory sets category correctly" $
        \sev cat msg ->
          let err = errorWithCategory sev cat msg
          in category err === cat

    , testProperty "formatError always returns non-empty string" $
        \err ->
          let formatted = formatError err
          in not (null formatted)

    , testProperty "formatError includes severity string" $
        \err ->
          let formatted = formatError err
              severityStr = case severity err of
                Fatal -> "FATAL"
                Error -> "ERROR"
                Warning -> "WARNING"
                Info -> "INFO"
          in severityStr `isInfixOf` formatted

    , testProperty "formatErrors handles empty list" $
        \errors ->
          null errors ==> null (formatErrors errors)

    , testProperty "formatErrors returns one line per error" $
        \errors ->
          let formatted = formatErrors errors
              linesCount = length (lines formatted)
          in not (null errors) ==> linesCount >= length errors

    , testProperty "combinedErrorSeverity extracts severity correctly" $
        \combinedErr ->
          let extractedSev = combinedErrorSeverity combinedErr
          in case combinedErr of
            OwnershipErrorCombined sev _ -> extractedSev === sev
            DependentTypeErrorCombined sev _ -> extractedSev === sev
            IntegrationError _ sev -> extractedSev === sev
            CrossAnalyzerError _ sev _ -> extractedSev === sev

    , testProperty "filterCombinedErrorsBySeverity respects minimum severity" $
        \combinedErrs minSev ->
          let filtered = filterCombinedErrorsBySeverity minSev combinedErrs
          in all (\err -> isAtLeast minSev (combinedErrorSeverity err)) filtered

    , testProperty "ErrorLocation helpers create valid locations" $
        \line column endLine endColumn ->
          let loc1 = _atLocation line column
              loc2 = _atFileLocation "test.txt" line column
              loc3 = _atRange line column endLine endColumn
          in getErrorLine loc1 === line .&&.
             getErrorColumn loc1 === column .&&.
             getErrorLine loc2 === line .&&.
             getErrorColumn loc2 === column .&&.
             filePath loc2 === Just "test.txt" .&&.
             getErrorLine loc3 === line .&&.
             getErrorColumn loc3 === column

    , testProperty "Recovery strategies have consistent properties" $
        \recovery ->
          let canRec = canRecover recovery
              shouldCont = shouldContinueAfter recovery
              cost = recoveryCost recovery
              confidence = recoveryConfidence recovery
          in cost >= 0 && cost <= 100 .&&.
             confidence >= 0.0 && confidence <= 1.0 .&&.
             (not canRec ==> not shouldCont)

    , testProperty "Fatal recovery cannot recover and should not continue" $
        \recovery ->
          recovery === fatalRecovery ==>
          not (canRecover recovery) .&&.
          not (shouldContinueAfter recovery)

    , testProperty "Error recovery can recover and should continue" $
        \recovery ->
          recovery === errorRecovery ==>
          canRecover recovery .&&.
          shouldContinueAfter recovery

    , testProperty "Warning recovery can recover and should continue" $
        \recovery ->
          recovery === warningRecovery ==>
          canRecover recovery .&&.
          shouldContinueAfter recovery

    , testProperty "Info recovery can recover and should continue" $
        \recovery ->
          recovery === infoRecovery ==>
          canRecover recovery .&&.
          shouldContinueAfter recovery

    , testProperty "emptyContext has all fields as Nothing or empty" $
        let ctx = emptyContext
        in contextCode ctx === Nothing .&&.
           contextFunction ctx === Nothing .&&.
           contextVariable ctx === Nothing .&&.
           contextType ctx === Nothing .&&.
           null (contextAdditional ctx)

    , testProperty "TypeError preserves all fields when created" $
        \err ->
          let id = errorId err
              sev = severity err
              cat = category err
              msg = message err
              loc = location err
              ctx = context err
              rec = recovery err
              suggs = suggestions err
              related = relatedErrors err
              chain = errorChain err
              ts = timestamp err
          in errorId err === id .&&.
             severity err === sev .&&.
             category err === cat .&&.
             message err === msg .&&.
             location err === loc .&&.
             context err === ctx .&&.
             recovery err === rec .&&.
             suggestions err === suggs .&&.
             relatedErrors err === related .&&.
             errorChain err === chain .&&.
             timestamp err === ts

    , testProperty "Error ordering by severity priority is consistent" $
        \err1 err2 ->
          let p1 = severityPriority (severity err1)
              p2 = severityPriority (severity err2)
          in (err1 > err2) === (p1 > p2)
    ]

-- Helper function
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    isPrefixOf _ _ = False

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(x:xs') = xs : tails xs'
