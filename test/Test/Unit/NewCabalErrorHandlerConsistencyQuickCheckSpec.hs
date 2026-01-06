{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalErrorHandlerConsistencyQuickCheckSpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck (property)
import Compiler.Errors.Core
import SourceLocation (SourcePos(..), SourceSpan(..), posAt, spanBetween)
import Data.Time (UTCTime)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T (pack, unpack)

-- | Test error handler consistency properties
testErrorHandlerConsistencyProperties :: TestTree
testErrorHandlerConsistencyProperties = testGroup "Error Handler Consistency Properties"
  [ testProperty "error collector preserves order" propErrorCollectorPreservesOrder
  , testProperty "error filtering maintains invariants" propErrorFilteringMaintainsInvariants
  , testProperty "error severity ordering is consistent" propSeverityOrderingConsistent
  , testProperty "error combination preserves L.all information" propErrorCombinationPreservesInfo
  , testProperty "error location utilities are consistent" propErrorLocationConsistent
  , testProperty "error context operations are idempotent" propErrorContextIdempotent
  ]

-- | Error collector should preserve the order of added errors
propErrorCollectorPreservesOrder :: [String] -> [String] -> [String] -> Property
propErrorCollectorPreservesOrder errorMessages warningMessages infoMessages =
  not (null errorMessages) ==> 
  let collector = newErrorCollector
      collector1 = foldl addError collector (L.map (errorAt "test-id" 1 1)) errorMessages)
      collector2 = foldl addWarning collector1 (L.map (warningAt "test-id" 1 1)) warningMessages)
      collector3 = foldl addInfo collector2 (L.map (infoAt "test-id" 1 1)) infoMessages)
      
      errors = getErrors collector3
      warnings = getWarnings collector3
      infos = getInfo collector3
      
      errorMessages' = map getErrorMessage errors
      warningMessages' = map getErrorMessage warnings
      infoMessages' = map getErrorMessage infos
  in errorMessages' == errorMessages &&
     warningMessages' == warningMessages &&
     infoMessages' == infoMessages
  where
    getErrorMessage (TypeError message _ _ _ _) = T.unpack message

-- | Error filtering should maintain basic invariants
propErrorFilteringMaintainsInvariants :: [ErrorSeverity] -> Property
propErrorFilteringMaintainsInvariants severities =
  not (null severities) ==> 
  let errors = zipWith (\i sev -> 
        TypeError (T.pack $ "Error " ++ show i) sev ErrorCategorySyntax 
                  (ErrorLocation Nothing i i Nothing Nothing) emptyContext) 
        [1..] severities
      
      filteredErrors = filterBySeverity ErrorSeverityError errors
      filteredWarnings = filterBySeverity ErrorSeverityWarning errors
      
      hasOnlyErrors = L.all (\e -> teSeverity e == ErrorSeverityError) filteredErrors
      hasOnlyWarnings = L.all (\e -> teSeverity e == ErrorSeverityWarning) filteredWarnings
  in hasOnlyErrors && hasOnlyWarnings

-- | Error severity ordering should be consistent
propSeverityOrderingConsistent :: ErrorSeverity -> ErrorSeverity -> Bool
propSeverityOrderingConsistent sev1 sev2 =
  let combined1 = combineErrors 
        (TypeError "Error1" sev1 ErrorCategorySyntax 
                   (ErrorLocation (startPos) Nothing) emptyContext)
        (TypeError "Error2" sev2 ErrorCategorySyntax 
                   (ErrorLocation Nothing 2 2 Nothing Nothing) emptyContext)
      combined2 = combineErrors 
        (TypeError "Error2" sev2 ErrorCategorySyntax 
                   (ErrorLocation Nothing 2 2 Nothing Nothing) emptyContext)
        (TypeError "Error1" sev1 ErrorCategorySyntax 
                   (ErrorLocation (startPos) Nothing) emptyContext)
  in combinedErrorSeverity combined1 == combinedErrorSeverity combined2

-- | Error combination should preserve L.all information from both errors
propErrorCombinationPreservesInfo :: String -> String -> ErrorSeverity -> ErrorSeverity -> Property
propErrorCombinationPreservesInfo msg1 msg2 sev1 sev2 =
  not (null msg1) && not (null msg2) ==> 
  let error1 = TypeError (T.pack msg1) sev1 ErrorCategorySyntax 
                         (ErrorLocation (startPos) Nothing) emptyContext
      error2 = TypeError (T.pack msg2) sev2 ErrorCategoryType 
                         (ErrorLocation Nothing 2 2 Nothing Nothing) emptyContext
      combined = combineErrors error1 error2
  in case combined of
       CombinedError errors _ _ -> 
         length errors == 2 &&
         any (\e -> T.unpack (teMessage e) == msg1) errors &&
         any (\e -> T.unpack (teMessage e) == msg2) errors

-- | Error location utilities should be consistent
propErrorLocationConsistent :: Int -> Int -> Int -> Int -> Property
propErrorLocationConsistent line1 col1 line2 col2 =
  line1 > 0 && col1 > 0 && line2 > 0 && col2 > 0 ==> 
  let pos1 = posAt line1 col1
      pos2 = posAt line2 col2
      span = spanBetween pos1 pos2
      errorLoc = toErrorLocationWithSpan span
  in line errorLoc == line1 &&
     column errorLoc == col1 &&
     endLine errorLoc == Just line2 &&
     endColumn errorLoc == Just col2

-- | Error context operations should be idempotent
propErrorContextIdempotent :: String -> String -> Property
propErrorContextIdempotent key value =
  not (null key) ==> 
  let context1 = emptyContext
      context2 = withContext key value context1
      context3 = withContext key value context2
  in context2 == context3

-- | Test error handling edge cases
testErrorHandlingEdgeCases :: TestTree
testErrorHandlingEdgeCases = testGroup "Error Handling Edge Cases"
  [ testCase "empty error collector" $
      let collector = newErrorCollector
      in L.null (getErrors collector) && 
         null (getWarnings collector) && 
         null (getInfo collector) &&
         not (hasErrors collector) &&
         not (hasWarnings collector)
         
  , testCase "error with no location" $
      let error = TypeError "Test error" ErrorSeverityError ErrorCategorySyntax 
                           (ErrorLocation Nothing 0 0 Nothing Nothing) emptyContext
          line = getErrorLine error
          column = getErrorColumn error
      in line == 0 && column == 0
      
  , testCase "error with location" $
      let error = TypeError "Test error" ErrorSeverityError ErrorCategorySyntax 
                           (ErrorLocation Nothing 5 10 Nothing Nothing) emptyContext
          line = getErrorLine error
          column = getErrorColumn error
      in line == 5 && column == 10
      
  , testCase "combine errors with different severities" $
      let error1 = TypeError "Error1" ErrorSeverityError ErrorCategorySyntax 
                             (ErrorLocation (startPos) Nothing) emptyContext
          error2 = TypeError "Error2" ErrorSeverityWarning ErrorCategoryType 
                             (ErrorLocation Nothing 2 2 Nothing Nothing) emptyContext
          combined = combineErrors error1 error2
          severity = combinedErrorSeverity combined
      in severity == ErrorSeverityError  -- Error should dominate warning
      
  , testCase "filter errors by category" $
      let errors = 
            [ TypeError "Syntax error" ErrorSeverityError ErrorCategorySyntax 
                        (ErrorLocation (startPos) Nothing) emptyContext
            , TypeError "Type error" ErrorSeverityError ErrorCategoryType 
                        (ErrorLocation Nothing 2 2 Nothing Nothing) emptyContext
            , TypeError "Another syntax error" ErrorSeverityWarning ErrorCategorySyntax 
                        (ErrorLocation Nothing 3 3 Nothing Nothing) emptyContext
            ]
          syntaxErrors = filterByCategory ErrorCategorySyntax errors
          typeErrors = filterByCategory ErrorCategoryType errors
      in L.length syntaxErrors == 2 && L.length typeErrors == 1
  ]

-- | Test error formatting
testErrorFormatting :: TestTree
testErrorFormatting = testGroup "Error Formatting"
  [ testCase "format error without location" $
      let error = TypeError "Test error" ErrorSeverityError ErrorCategorySyntax 
                           (ErrorLocation Nothing 0 0 Nothing Nothing) emptyContext
          formatted = formatError error
      in "Test error" `L.isInfixOf` formatted &&
         "Error" `L.isInfixOf` formatted &&
         "Syntax" `L.isInfixOf` formatted
         
  , testCase "format error with location" $
      let error = TypeError "Test error" ErrorSeverityError ErrorCategorySyntax 
                           (ErrorLocation Nothing 5 10 (Just 5) (Just 15)) emptyContext
          formatted = formatErrorWithLocation error
      in "Test error" `L.isInfixOf` formatted &&
         "5:10" `L.isInfixOf` formatted &&
         "5:15" `L.isInfixOf` formatted
         
  , testCase "format multiple errors" $
      let errors = 
            [ TypeError "First error" ErrorSeverityError ErrorCategorySyntax 
                        (ErrorLocation (startPos) Nothing) emptyContext
            , TypeError "Second error" ErrorSeverityWarning ErrorCategoryType 
                        (ErrorLocation Nothing 2 2 Nothing Nothing) emptyContext
            ]
          formatted = formatErrors errors
      in "First error" `L.isInfixOf` formatted &&
         "Second error" `L.isInfixOf` formatted
  ]

-- | Test error recovery
testErrorRecovery :: TestTree
testErrorRecovery = testGroup "Error Recovery"
  [ testCase "can recover from warning" $
      let error = TypeError "Warning" ErrorSeverityWarning ErrorCategorySyntax 
                           (ErrorLocation (startPos) Nothing) emptyContext
      in canRecoverFrom error
      
  , testCase "cannot recover from error" $
      let error = TypeError "Error" ErrorSeverityError ErrorCategorySyntax 
                           (ErrorLocation (startPos) Nothing) emptyContext
      in not (canRecoverFrom error)
      
  , testCase "should continue after warning" $
      let error = TypeError "Warning" ErrorSeverityWarning ErrorCategorySyntax 
                           (ErrorLocation (startPos) Nothing) emptyContext
      in shouldContinueAfter error
      
  , testCase "should not continue after error" $
      let error = TypeError "Error" ErrorSeverityError ErrorCategorySyntax 
                           (ErrorLocation (startPos) Nothing) emptyContext
      in not (shouldContinueAfter error)
  ]

-- | Test error statistics
testErrorStatistics :: TestTree
testErrorStatistics = testGroup "Error Statistics"
  [ testCase "empty statistics" $
      let collector = newErrorCollector
          stats = getErrorStatistics collector
      in stats == Map.fromList 
            [(ErrorSeverityError, 0), (ErrorSeverityWarning, 0), (ErrorSeverityInfo, 0)]
        
  , testCase "statistics with mixed errors" $
      let collector = newErrorCollector
          collector1 = addError (errorAt "test-id" 1 1) "Error1") collector
          collector2 = addError (errorAt "test-id" 2 2) "Error2") collector1
          collector3 = addWarning (warningAt "test-id" 3 3) "Warning1") collector2
          collector4 = addInfo (infoAt "test-id" 4 4) "Info1") collector3
          stats = getErrorStatistics collector4
      in Map.lookup ErrorSeverityError stats == Just 2 &&
         Map.lookup ErrorSeverityWarning stats == Just 1 &&
         Map.lookup ErrorSeverityInfo stats == Just 1
  ]

-- | All error handler consistency tests
testErrorHandlerConsistencyQuickCheck :: TestTree
testErrorHandlerConsistencyQuickCheck = testGroup "New Cabal Error Handler Consistency QuickCheck Tests"
  [ testErrorHandlerConsistencyProperties
  , testErrorHandlingEdgeCases
  , testErrorFormatting
  , testErrorRecovery
  , testErrorStatistics
  ]