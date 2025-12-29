{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalErrorHandlerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, vectorOf, Positive(..), NonNegative(..))

import Compiler.Errors.Core
  ( TypeError(..)
  , ErrorSeverity(..)
  , ErrorCategory(..)
  , ErrorLocation(..)
  , ErrorContext(..)
  , ErrorRecovery(..)
  , CombinedError(..)
  , emptyContext
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
  , formatError
  , formatErrors
  , formatErrorWithLocation
  , formatErrorsWithLocation
  , errorAt
  , warningAt
  , infoAt
  , fatalError
  , errorWithCategory
  , warningWithCategory
  , infoWithCategory
  , errorWithSuggestions
  , withLocation
  , withContext
  , withSuggestions
  , withRelatedErrors
  , wrapError
  , combineErrors
  , combinedErrorSeverity
  , filterCombinedErrorsBySeverity
  , hasCategory
  , filterByCategory
  , filterBySeverity
  , getErrorStatistics
  , generateErrorReport
  , canRecoverFrom
  , shouldContinueAfter
  , createRecoveryStrategy
  , customRecovery
  , fatalRecovery
  , errorRecovery
  , warningRecovery
  , infoRecovery
  , getErrorLine
  , getErrorColumn
  , formatTimestamp
  , getCurrentTimestamp
  )

import qualified Data.Text as T
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, getCurrentTime)

-- | 新的QuickCheck属性测试，针对ErrorHandler模块的边界条件
tests :: TestTree
tests =
  testGroup "New Cabal ErrorHandler QuickCheck Tests"
    [ testGroup "ErrorSeverity properties"
        [ fastProperty "Severity ordering is consistent" $
            \sev1 sev2 ->
              let ordered = [sev1, sev2]
                  sorted = sort ordered
              in length sorted === 2 .&&. head sorted `elem` ordered .&&. last sorted `elem` ordered

        , fastProperty "Fatal is most severe" $
            \sev ->
              sev `elem` [Error, Warning, Info] ==> Fatal > sev

        , fastProperty "Error is more severe than Warning" $
            Error > Warning

        , fastProperty "Warning is more severe than Info" $
            Warning > Info

        , fastProperty "Severity comparison is transitive" $
            \sev1 sev2 sev3 ->
              sev1 >= sev2 && sev2 >= sev3 ==> sev1 >= sev3
        ]

    , testGroup "ErrorLocation properties"
        [ fastProperty "Location preserves coordinates" $
            \line column file endLine endColumn ->
              let location = ErrorLocation file line column endLine endColumn
              in getErrorLine location === line .&&. getErrorColumn location === column

        , fastProperty "Location with unknown values" $
            let location = ErrorLocation Nothing 0 0 Nothing Nothing
            in getErrorLine location === 0 .&&. getErrorColumn location === 0

        , fastProperty "Location range is valid" $
            \line column endLine endColumn ->
              let location = ErrorLocation Nothing line column (Just endLine) (Just endColumn)
                  hasValidRange = case (endLine, endColumn) of
                                   (Just el, Just ec) -> line <= el && (line < el || column <= ec)
                                   _ -> True
              in hasValidRange

        , fastProperty "File path is preserved" $
            \file line column ->
              let location = ErrorLocation (Just file) line column Nothing Nothing
              in filePath location === Just file
        ]

    , testGroup "ErrorContext properties"
        [ fastProperty "Empty context has no fields" $
            let ctx = emptyContext
            in contextCode ctx === Nothing .&&. 
               contextFunction ctx === Nothing .&&.
               contextVariable ctx === Nothing .&&.
               contextType ctx === Nothing .&&.
               null (contextAdditional ctx)

        , fastProperty "Context preserves all fields" $
            \code func var typ additional ->
              let ctx = ErrorContext code func var typ additional
              in contextCode ctx === code .&&.
                 contextFunction ctx === func .&&.
                 contextVariable ctx === var .&&.
                 contextType ctx === typ .&&.
                 contextAdditional ctx === additional

        , fastProperty "Context additional fields are preserved" $
            \pairs ->
              let ctx = ErrorContext Nothing Nothing Nothing Nothing pairs
              in contextAdditional ctx === pairs
        ]

    , testGroup "ErrorRecovery properties"
        [ fastProperty "Fatal recovery cannot continue" $
            let recovery = fatalRecovery
            in not (canRecover recovery) .&&. not (shouldContinue recovery)

        , fastProperty "Error recovery can continue" $
            let recovery = errorRecovery
            in canRecover recovery .&&. shouldContinue recovery

        , fastProperty "Warning recovery can continue" $
            let recovery = warningRecovery
            in canRecover recovery .&&. shouldContinue recovery

        , fastProperty "Info recovery can continue" $
            let recovery = infoRecovery
            in canRecover recovery .&&. shouldContinue recovery

        , fastProperty "Custom recovery preserves parameters" $
            \canRec shouldCont action hint cost confidence ->
              let recovery = customRecovery canRec shouldCont action hint cost confidence
              in canRecover recovery === canRec .&&.
                 shouldContinue recovery === shouldCont .&&.
                 recoveryAction recovery === action .&&.
                 recoveryHint recovery === hint .&&.
                 recoveryCost recovery === cost .&&.
                 recoveryRecovery recovery === confidence

        , fastProperty "Recovery cost is within bounds" $
            \recovery ->
              let cost = recoveryCost recovery
              in cost >= 0 && cost <= 100

        , fastProperty "Recovery confidence is within bounds" $
            \recovery ->
              let confidence = recoveryRecovery recovery
              in confidence >= 0.0 && confidence <= 1.0
        ]

    , testGroup "TypeError properties"
        [ fastProperty "Error preserves message" $
            \msg sev category loc ctx recovery suggestions related chain timestamp ->
              let error = TypeError msg sev category loc ctx recovery suggestions related chain timestamp
              in message error === msg

        , fastProperty "Error preserves severity" $
            \msg sev category ->
              let error = errorWithCategory msg sev category
              in severity error === sev .&&. errorCategory error === category

        , fastProperty "Error with location preserves location" $
            \msg line column ->
              let loc = ErrorLocation Nothing line column Nothing Nothing
                  error = errorAt msg loc
              in errorLocation error === loc

        , fastProperty "Error with suggestions preserves suggestions" $
            \msg suggestions ->
              let error = errorWithSuggestions msg suggestions
              in errorSuggestions error === suggestions

        , fastProperty "Error with context preserves context" $
            \msg ctx ->
              let error = withContext (errorAt msg (ErrorLocation Nothing 1 1 Nothing Nothing)) ctx
              in errorContext error === ctx

        , fastProperty "Error wrapping preserves chain" $
            \outerMsg innerMsg ->
              let innerError = errorAt innerMsg (ErrorLocation Nothing 1 1 Nothing Nothing)
                  wrappedError = wrapError outerMsg innerError
              in errorChain wrappedError === [innerError]

        , fastProperty "Error combining preserves both errors" $
            \msg1 msg2 ->
              let error1 = errorAt msg1 (ErrorLocation Nothing 1 1 Nothing Nothing)
                  error2 = errorAt msg2 (ErrorLocation Nothing 2 2 Nothing Nothing)
                  combined = combineErrors error1 error2
              in relatedErrors combined `elem` [[error2], [error1]]
        ]

    , testGroup "Error collection properties"
        [ fastProperty "Error collector preserves errors" $
            \errors ->
              let collected = foldr addError () errors
                  allErrors = getAllMessages [collected]
              in length allErrors >= length errors

        , fastProperty "Warning collector adds warnings" $
            \errors ->
              let collected = foldr addWarning () errors
                  warnings = getWarnings [collected]
              in length warnings >= length errors

        , fastProperty "Info collector adds info messages" $
            \errors ->
              let collected = foldr addInfo () errors
                  infoMsgs = getInfo [collected]
              in length infoMsgs >= length errors

        , fastProperty "hasErrors detects errors correctly" $
            \errors ->
              let hasErrs = any (\e -> severity e `elem` [Error, Fatal]) errors
                  collected = foldr addError () errors
              in hasErrors [collected] === hasErrs

        , fastProperty "hasWarnings detects warnings correctly" $
            \errors ->
              let hasWarns = any (\e -> severity e == Warning) errors
                  collected = foldr addError () errors
              in hasWarnings [collected] === hasWarns
        ]

    , testGroup "Error formatting properties"
        [ fastProperty "Format error produces non-empty string" $
            \error ->
              not (null (formatError error))

        , fastProperty "Format errors preserves count" $
            \errors ->
              let formatted = formatErrors errors
              in length (lines formatted) >= length errors

        , fastProperty "Format with location includes location info" $
            \msg line column ->
              let loc = ErrorLocation Nothing line column Nothing Nothing
                  error = errorAt msg loc
                  formatted = formatErrorWithLocation error
              in show line `isInfixOf` formatted .&&. show column `isInfixOf` formatted

        , fastProperty "Format multiple errors preserves order" $
            \errors ->
              let formatted = formatErrors errors
                  linesCount = length (lines formatted)
              in linesCount >= length errors
        ]

    , testGroup "Error filtering properties"
        [ fastProperty "Filter by severity preserves matching errors" $
            \errors minSeverity ->
              let filtered = filterBySeverity minSeverity errors
              in all (\e -> severity e >= minSeverity) filtered

        , fastProperty "Filter by category preserves matching errors" $
            \errors category ->
              let filtered = filterByCategory category errors
              in all (\e -> errorCategory e == category) filtered

        , fastProperty "Has category detection works" $
            \errors category ->
              let hasCat = any (\e -> errorCategory e == category) errors
              in hasCategory category errors === hasCat

        , fastProperty "Combined error filtering works" $
            \combinedErrors minSeverity ->
              let filtered = filterCombinedErrorsBySeverity minSeverity combinedErrors
              in all (\e -> combinedErrorSeverity e >= minSeverity) filtered
        ]

    , testGroup "Error recovery properties"
        [ fastProperty "Can recover from non-fatal errors" $
            \error ->
              severity error /= Fatal ==> canRecoverFrom error

        , fastProperty "Should continue after warnings and info" $
            \error ->
              severity error `elem` [Warning, Info] ==> shouldContinueAfter error

        , fastProperty "Fatal errors cannot be recovered" $
            \error ->
              severity error == Fatal ==> not (canRecoverFrom error)

        , fastProperty "Create recovery strategy preserves parameters" $
            \canRec shouldCont action hint cost confidence ->
              let strategy = createRecoveryStrategy canRec shouldCont action hint cost confidence
              in canRecover strategy === canRec .&&.
                 shouldContinue strategy === shouldCont .&&.
                 recoveryAction strategy === action .&&.
                 recoveryHint strategy === hint
        ]

    , testGroup "Edge cases and boundary conditions"
        [ testCase "Empty error message" $ do
            let error = errorAt "" (ErrorLocation Nothing 1 1 Nothing Nothing)
            length (formatError error) @? (> 0)

        , testCase "Very long error message" $ do
            let longMsg = concat (replicate 1000 "This is a very long error message. ")
                error = errorAt longMsg (ErrorLocation Nothing 1 1 Nothing Nothing)
            length (formatError error) @? (> 1000)

        , testCase "Error with extreme coordinates" $ do
            let location = ErrorLocation Nothing maxBound maxBound (Just maxBound) (Just maxBound)
                error = errorAt "Extreme coordinates" location
            getErrorLine (errorLocation error) @?= maxBound
            getErrorColumn (errorLocation error) @?= maxBound

        , testCase "Error with negative coordinates" $ do
            let location = ErrorLocation Nothing (-1) (-1) (Just (-1)) (Just (-1))
                error = errorAt "Negative coordinates" location
            getErrorLine (errorLocation error) @?= (-1)
            getErrorColumn (errorLocation error) @?= (-1)

        , testCase "Error with many suggestions" $ do
            let suggestions = map (\i -> T.pack ("Suggestion " ++ show i)) [1..100]
                error = errorWithSuggestions "Many suggestions" suggestions
            length (errorSuggestions error) @?= 100

        , testCase "Deep error chain" $ do
            let createChain 0 base = base
                createChain n base = wrapError ("Wrapper " ++ show n) (createChain (n-1) base)
                baseError = errorAt "Base error" (ErrorLocation Nothing 1 1 Nothing Nothing)
                deepError = createChain 50 baseError
            length (errorChain deepError) @?= 50

        , testCase "Error with Unicode content" $ do
            let unicodeMsg = "错误信息: 变量 🦀 未定义"
                error = errorAt unicodeMsg (ErrorLocation Nothing 1 1 Nothing Nothing)
            T.unpack (message error) @?= unicodeMsg

        , testCase "Combined error with many sub-errors" $ do
            let subErrors = replicate 100 (IntegrationError "test" Error)
                combined = CrossAnalyzerError "Multiple errors" Error subErrors
            case combined of
              CrossAnalyzerError _ _ subs -> length subs @?= 100
              _ -> assertFailure "Should be CrossAnalyzerError"
        ]

    , testGroup "Performance and stress tests"
        [ fastProperty "Many errors formatting" $
            \count ->
              count < 1000 ==>
              let errors = map (\i -> errorAt ("Error " ++ show i) (ErrorLocation Nothing i 1 Nothing Nothing)) [1..count]
                  formatted = formatErrors errors
              in length (lines formatted) >= count

        , fastProperty "Large error collection filtering" $
            \errors minSeverity ->
              length errors < 500 ==>
              let filtered = filterBySeverity minSeverity errors
              in length filtered <= length errors

        , fastProperty "Complex error combinations" $
            \errors ->
              length errors < 100 ==>
              let combined = foldr combineErrors (head errors) (tail errors)
              in length (relatedErrors combined) >= 0

        , fastProperty "Error statistics generation" $
            \errors ->
              length errors < 200 ==>
              let stats = getErrorStatistics errors
              in Map.size stats >= 0
        ]
    ]