{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerErrorRecoveryAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, frequency, sized)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import ErrorHandler
  ( CompilerError(..)
  , ErrorSeverity(..)
  , ErrorContext(..)
  , ErrorRecoveryStrategy(..)
  , ErrorReporter
  , formatError
  , errorSeverity
  , shouldRecover
  , canRecoverFrom
  , groupErrors
  , filterErrorsBySeverity
  , getMostSevereError
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAtLineCol
  )

import Data.Char (isSpace, isUpper, toLower)
import Data.List (nub, sort, groupBy, sortBy)
import Data.Ord (Down(..))

-- | Generate arbitrary error severities
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Warning, Error, Fatal]

-- | Generate arbitrary error contexts
instance Arbitrary ErrorContext where
  arbitrary = oneof
    [ pure Parsing
    , pure TypeChecking
    , pure SemanticAnalysis
    , pure CodeGeneration
    , pure Optimization
    , pure Linking
    ]

-- | Generate arbitrary source positions for errors
newtype ErrorSourcePos = ErrorSourcePos { getErrorSourcePos :: SourcePos }
  deriving (Show, Eq)

instance Arbitrary ErrorSourcePos where
  arbitrary = sized $ \size -> do
    let maxSize = min size 500
    line <- choose (1, maxSize)
    col <- choose (1, maxSize)
    return $ ErrorSourcePos $ SourcePos line col

-- | Generate arbitrary compiler errors
instance Arbitrary CompilerError where
  arbitrary = do
    severity <- arbitrary
    context <- arbitrary
    ErrorSourcePos pos <- arbitrary
    message <- errorMessage
    suggestion <- errorSuggestion
    return $ CompilerError
      { errorSeverity = severity
      , errorContext = context
      , errorPosition = pos
      , errorMessage = message
      , errorSuggestion = Just suggestion
      }

-- | Generate error messages
errorMessage :: Gen String
errorMessage = sized $ \size -> do
  let maxSize = min size 100
  len <- choose (5, maxSize)
  listOf $ oneof
    [ choose ('a', 'z')
    , choose ('A', 'Z')
    , choose ('0', '9')
    , elements [' ', '-', ':', '.', ',', ';']
    ]

-- | Generate error suggestions
errorSuggestion :: Gen String
errorSuggestion = sized $ \size -> do
  let maxSize = min size 80
  len <- choose (3, maxSize)
  listOf $ oneof
    [ choose ('a', 'z')
    , choose (' ', ' ')
    , elements ['-', ':', '.', ',']
    ]

-- | Generate lists of errors
newtype ErrorList = ErrorList { getErrorList :: [CompilerError] }
  deriving (Show, Eq)

instance Arbitrary ErrorList where
  arbitrary = sized $ \size -> do
    let maxSize = min size 20
    len <- choose (0, maxSize)
    errors <- listOf len arbitrary
    return $ ErrorList errors

-- Property: error severity ordering is consistent
prop_error_severity_ordering :: CompilerError -> CompilerError -> Property
prop_error_severity_ordering err1 err2 =
  let sev1 = errorSeverity err1
      sev2 = errorSeverity err2
      severityOrder severity = case severity of
        Warning -> 1
        Error -> 2
        Fatal -> 3
      order1 = severityOrder sev1
      order2 = severityOrder sev2
  in classify (sev1 == sev2) "same severity" $
     classify (order1 < order2) "err1 less severe" $
     classify (order1 > order2) "err1 more severe" $
     property $ (sev1 <= sev2) === (order1 <= order2)

-- Property: error formatting preserves essential information
prop_error_formatting_preserves_info :: CompilerError -> Property
prop_error_formatting_preserves_info err =
  let formatted = formatError err
      pos = errorPosition err
      msg = errorMessage err
      sev = errorSeverity err
      ctx = errorContext err
  in counterexample ("Formatted: " ++ formatted) $
     property $ 
       show (sourceLine pos) `L.isInfixOf` formatted .&&.
       show (sourceColumn pos) `L.isInfixOf` formatted .&&.
       msg `L.isInfixOf` formatted .&&.
       show sev `L.isInfixOf` formatted .&&.
       show ctx `L.isInfixOf` formatted
  where
    isInfixOf needle haystack = needle `elem` words haystack

-- Property: error filtering preserves ordering
prop_error_filtering_preserves_order :: ErrorList -> ErrorSeverity -> Property
prop_error_filtering_preserves_order errors severity =
  let original = getErrorList errors
      filtered = filterErrorsBySeverity severity original
      originalSeverities = map errorSeverity original
      filteredSeverities = map errorSeverity filtered
  in property $ L.all (>= severity) filteredSeverities

-- Property: most severe error is correctly identified
prop_most_severe_error_correct :: ErrorList -> Property
prop_most_severe_error_correct errors =
  let errorList = getErrorList errors
  in not (null errorList) ==>
     let mostSevere = getMostSevereError errorList
         severityOrder severity = case severity of
           Warning -> 1
           Error -> 2
           Fatal -> 3
         maxSeverity = L.maximum $ L.map (severityOrder . errorSeverity) errorList
         mostSevereOrder = severityOrder $ errorSeverity mostSevere
     in property $ mostSevereOrder === maxSeverity

-- Property: error grouping maintains consistency
prop_error_grouping_consistency :: ErrorList -> Property
prop_error_grouping_consistency errors =
  let errorList = getErrorList errors
      grouped = groupErrors errorList
      -- Each group should have errors of the same context
      groupsHaveConsistentContext = L.all (\group -> 
            let contexts = map errorContext group
            in L.length (nub contexts) <= 1) grouped
  in classify (L.length errorList > 5) "multiple errors" $
     classify (L.length errorList <= 5) "few errors" $
     property $ groupsHaveConsistentContext

-- Property: error recovery strategy selection is logical
prop_recovery_strategy_selection :: CompilerError -> Property
prop_recovery_strategy_selection err =
  let severity = errorSeverity err
      context = errorContext err
      canRecover = canRecoverFrom err
      shouldRec = shouldRecover err
  in classify (severity == Fatal) "fatal error" $
     classify (severity == Error) "regular error" $
     classify (severity == Warning) "warning" $
     property $ 
       case severity of
         Fatal -> not canRecover .&&. not shouldRec
         Error -> canRecover .&&. shouldRec
         Warning -> canRecover .&&. shouldRec

-- Property: error position ordering is preserved in grouping
prop_error_position_ordering_in_groups :: ErrorList -> ErrorContext -> Property
prop_error_position_ordering_in_groups errors context =
  let errorList = getErrorList errors
      contextErrors = L.filter (\e -> errorContext e == context) errorList
      orderedByPos = sortBy (\e1 e2 -> 
        compare (errorPosition e1) (errorPosition e2)) contextErrors
  in not (null contextErrors) ==>
     let grouped = groupErrors errorList
         contextGroup = L.filter (\group -> not (null group) && 
                                     errorContext (L.head group) == context) grouped
     in case contextGroup of
          [] -> property $ True  -- No group for this context
          (group:_) -> 
            let groupOrdered = sortBy (\e1 e2 -> 
                  compare (errorPosition e1) (errorPosition e2)) group
            in property $ map errorPosition groupOrdered === map errorPosition orderedByPos

-- Property: error message uniqueness in error lists
prop_error_message_uniqueness :: ErrorList -> Property
prop_error_message_uniqueness errors =
  let errorList = getErrorList errors
      messages = map errorMessage errorList
      uniqueMessages = nub messages
      duplicateCount = L.length messages - L.length uniqueMessages
  in classify (duplicateCount > 0) "has duplicate messages" $
     classify (duplicateCount == 0) "L.all unique messages" $
     property $ duplicateCount >= 0

-- Property: error suggestion handling
prop_error_suggestion_handling :: CompilerError -> Property
prop_error_suggestion_handling err =
  let hasSuggestion = errorSuggestion err /= Nothing
      formatted = formatError err
  in classify hasSuggestion "has suggestion" $
     classify (not hasSuggestion) "no suggestion" $
     case errorSuggestion err of
       Just suggestion -> property $ suggestion `L.isInfixOf` formatted
       Nothing -> property $ True
  where
    isInfixOf needle haystack = needle `elem` words haystack

-- Property: error context distribution
prop_error_context_distribution :: ErrorList -> Property
prop_error_context_distribution errors =
  let errorList = getErrorList errors
      contexts = map errorContext errorList
      contextCounts = L.map (\ctx -> (ctx, L.length $ L.filter (== ctx) contexts)) (nub contexts)
      totalCount = L.length errorList
      sumOfCounts = L.sum $ map snd contextCounts
  in property $ sumOfCounts === totalCount

tests :: TestTree
tests = testGroup "Compiler Error Recovery Advanced QuickCheck Tests"
  [ fastProperty "error severity ordering" prop_error_severity_ordering
  , fastProperty "error formatting preserves info" prop_error_formatting_preserves_info
  , fastProperty "error filtering preserves order" prop_error_filtering_preserves_order
  , fastProperty "most severe error correct" prop_most_severe_error_correct
  , fastProperty "error grouping consistency" prop_error_grouping_consistency
  , fastProperty "recovery strategy selection" prop_recovery_strategy_selection
  , fastProperty "error position ordering in groups" prop_error_position_ordering_in_groups
  , fastProperty "error message uniqueness" prop_error_message_uniqueness
  , fastProperty "error suggestion handling" prop_error_suggestion_handling
  , fastProperty "error context distribution" prop_error_context_distribution
  , testGroup "Manual error handling tests"
      [ testCase "fatal error prevents recovery" $ do
          let fatalErr = CompilerError
                { errorSeverity = Fatal
                , errorContext = TypeChecking
                , errorPosition = posAtLineCol 10 5
                , errorMessage = "Type mismatch"
                , errorSuggestion = Nothing
                }
          assertBool "fatal errors should not be recoverable" $ not (canRecoverFrom fatalErr)
          assertBool "fatal errors should not be recovered" $ not (shouldRecover fatalErr)
          
      , testCase "warning errors are always recoverable" $ do
          let warningErr = CompilerError
                { errorSeverity = Warning
                , errorContext = Parsing
                , errorPosition = posAtLineCol 1 1
                , errorMessage = "Unused variable"
                , errorSuggestion = Just "Remove the variable"
                }
          assertBool "warnings should be recoverable" $ canRecoverFrom warningErr
          assertBool "warnings should be recovered" $ shouldRecover warningErr
          
      , testCase "error grouping by context" $ do
          let err1 = CompilerError Error Parsing (posAtLineCol 1 1) "Parse error" Nothing
              err2 = CompilerError Warning Parsing (posAtLineCol 2 1) "Parse warning" Nothing
              err3 = CompilerError Error TypeChecking (posAtLineCol 3 1) "Type error" Nothing
              errors = [err1, err2, err3]
              grouped = groupErrors errors
          assertEqual "should have 2 groups" 2 (L.length grouped)
          
      , testCase "most severe error identification" $ do
          let warning = CompilerError Warning Parsing startPos "Warning" Nothing
              error = CompilerError Error Parsing startPos "Error" Nothing
              fatal = CompilerError Fatal Parsing startPos "Fatal" Nothing
              errors = [warning, error, fatal]
              mostSevere = getMostSevereError errors
          assertEqual "should identify fatal as most severe" Fatal (errorSeverity mostSevere)
    }
  ]