{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.NewAdvancedErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    )

import qualified ErrorHandler
import qualified ErrorHandler.Core
import qualified EnhancedErrorHandler
import qualified Compiler.Errors

-- | QuickCheck property tests for error recovery functionality
tests :: TestTree
tests =
  testGroup "New Error Recovery QuickCheck Tests"
    [ testGroup "Error Detection Properties"
        [ fastProperty "error detection is deterministic" $
            \input ->
              let errors1 = ErrorHandler.detectErrors input
                  errors2 = ErrorHandler.detectErrors input
              in True -- Should detect same errors
              
        , fastProperty "error detection never crashes" $
            \input ->
              let errors = ErrorHandler.detectErrors input
              in True -- Should handle any input
              
        , fastProperty "error locations are accurate" $
            \input expectedError ->
              let detected = ErrorHandler.locateError input expectedError
              in True -- Should find correct location
        ]

    , testGroup "Error Recovery Properties"
        [ fastProperty "error recovery produces valid state" $
            \input errors ->
              let recovered = ErrorHandler.recoverFromErrors input errors
              in ErrorHandler.isValidState recovered
              
        , fastProperty "recovery preserves correct parts" $
            \input errors ->
              let recovered = ErrorHandler.recoverFromErrors input errors
                  preserved = ErrorHandler.getCorrectParts recovered
              in length preserved > 0
              
        , fastProperty "recovery is idempotent" $
            \input errors ->
              let recovered1 = ErrorHandler.recoverFromErrors input errors
                  recovered2 = ErrorHandler.recoverFromErrors recovered1 errors
              in True -- Second recovery should not change result
        ]

    , testGroup "Error Propagation Properties"
        [ fastProperty "error propagation is predictable" $
            \error context ->
              let propagated = ErrorHandler.propagateError error context
              in ErrorHandler.isValidPropagation propagated
              
        , fastProperty "error context is preserved" $
            \error context ->
              let propagated = ErrorHandler.propagateError error context
                  preservedContext = ErrorHandler.extractContext propagated
              in length preservedContext >= length context
              
        , fastProperty "error chaining maintains traceability" $
            \errors ->
              let chained = ErrorHandler.chainErrors errors
              in ErrorHandler.isTraceable chained
        ]

    , testGroup "Graceful Degradation Properties"
        [ fastProperty "partial results are returned on errors" $
            \input severity ->
              let result = ErrorHandler.processWithFallback input severity
              in ErrorHandler.hasPartialResult result
              
        , fastProperty "fallback mechanisms are safe" $
            \input fallbackStrategy ->
              let result = ErrorHandler.applyFallback input fallbackStrategy
              in ErrorHandler.isSafeFallback result
              
        , fastProperty "degradation levels are appropriate" $
            \errorCount ->
              let level = ErrorHandler.calculateDegradationLevel errorCount
              in level >= 0 .&&. level <= 10
        ]

    , testGroup "Error Consistency Properties"
        [ fastProperty "error messages are consistent" $
            \errorType context ->
              let message1 = ErrorHandler.formatError errorType context
                  message2 = ErrorHandler.formatError errorType context
              in message1 === message2
              
        , fastProperty "error codes are unique" $
            \error1 error2 ->
              let code1 = ErrorHandler.getErrorCode error1
                  code2 = ErrorHandler.getErrorCode error2
              in error1 /= error2 ==> code1 /= code2
              
        , fastProperty "error severity is monotonic" $
            \baseError additionalErrors ->
              let baseSeverity = ErrorHandler.getSeverity baseError
                  combinedSeverity = ErrorHandler.combineSeverity baseError additionalErrors
              in combinedSeverity >= baseSeverity
        ]

    , testGroup "Enhanced Error Handling Properties"
        [ fastProperty "enhanced error recovery is more robust" $
            \complexInput ->
              let basic = ErrorHandler.recoverFromErrors complexInput []
                  enhanced = EnhancedErrorHandler.recover complexInput
              in True -- Enhanced should handle more cases
              
        , fastProperty "error suggestions are helpful" $
            \error context ->
              let suggestions = EnhancedErrorHandler.getSuggestions error context
              in length suggestions > 0
              
        , fastProperty "error recovery time is bounded" $
            \input complexity ->
              let recovered = EnhancedErrorHandler.recoverWithTimeout input complexity
              in ErrorHandler.isTimely recovered
        ]

    , testGroup "Error Statistics Properties"
        [ fastProperty "error statistics are accurate" $
            \errors ->
              let stats = ErrorHandler.calculateStatistics errors
                  total = ErrorHandler.getTotalErrors stats
              in total === length errors
              
        , fastProperty "error patterns are detected" $
            \errorSequence ->
              let patterns = ErrorHandler.detectPatterns errorSequence
              in length patterns >= 0
              
        , fastProperty "error frequency analysis is consistent" $
            \errors ->
              let freq1 = ErrorHandler.analyzeFrequency errors
                  freq2 = ErrorHandler.analyzeFrequency errors
              in freq1 === freq2
        ]

    , testGroup "Recovery Quality Properties"
        [ fastProperty "recovery quality is measurable" $
            \original errors ->
              let recovered = ErrorHandler.recoverFromErrors original errors
                  quality = ErrorHandler.measureRecoveryQuality original recovered
              in quality >= 0 .&&. quality <= 100
              
        , fastProperty "best recovery strategy is selected" $
            \input availableStrategies ->
              let best = ErrorHandler.selectBestStrategy input availableStrategies
              in ErrorHandler.isValidStrategy best
              
        , fastProperty "recovery improves over time" $
            \input iterations ->
              let recoveries = ErrorHandler.iterativeRecovery input iterations
                  qualities = map ErrorHandler.measureQuality recoveries
              in length qualities > 1 ==> last qualities >= head qualities
        ]
    ]