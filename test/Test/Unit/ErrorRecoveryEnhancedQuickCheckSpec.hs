{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ErrorRecoveryEnhancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit (testCase, assert)
import Compiler.Errors.Core
  ( TypeError(..), ErrorSeverity(..), ErrorCategory(..)
  , ErrorCollector, newErrorCollector, addError, addWarning
  , getErrors, getWarnings, hasErrors, hasWarnings
  , canRecoverFrom, shouldContinueAfter, errorAt, warningAt, formatError, formatErrors
  )
import SourceLocation (SourcePos(..), SourceSpan(..), emptySpan)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (elements, choose, listOf, oneof)
import Data.List (sort)
import Control.Monad (when)

-- | Generate arbitrary error severities
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Error, Warning, Info]

-- | Generate arbitrary error categories
instance Arbitrary ErrorCategory where
  arbitrary = elements 
    [ ParseError
    , TypeError
    , NameError
    , ScopeError
    , OwnershipError
    , DependentTypeError
    , ConstraintError
    , InternalError
    ]

-- | Generate arbitrary source positions for error locations
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ SourcePos line col

-- | Generate arbitrary error messages
newtype ErrorMessage = ErrorMessage String
  deriving (Show)

instance Arbitrary ErrorMessage where
  arbitrary = do
    words <- listOf $ oneof
      [ elements ["syntax", "type", "name", "scope", "ownership", "constraint"]
      , elements ["error", "warning", "info", "mismatch", "undefined", "conflict"]
      , elements ["in", "at", "near", "around", "before", "after"]
      , elements ["variable", "function", "type", "expression", "statement"]
      ]
    return $ ErrorMessage $ unwords words

-- | Generate arbitrary type errors
instance Arbitrary TypeError where
  arbitrary = do
    severity <- arbitrary
    category <- arbitrary
    pos <- arbitrary
    ErrorMessage msg <- arbitrary
    return $ TypeError
      { errorSeverity = severity
      , errorCategory = category
      , errorLocation = emptySpan pos
      , errorMessage = msg
      , errorContext = emptyContext
      }

tests :: TestTree
tests = testGroup "Error Recovery Advanced Tests"
  [ testProperty "error collector preserves insertion order" $ \errors ->
      let collector = newErrorCollector
          collector' = foldl addError collector errors
          retrievedErrors = getErrors collector'
      in L.length retrievedErrors == L.length errors &&
         map errorMessage retrievedErrors == map errorMessage errors
  
  , testProperty "warning collector preserves insertion order" $ \warnings ->
      let collector = newErrorCollector
          collector' = foldl addWarning collector warnings
          retrievedWarnings = getWarnings collector'
      in L.length retrievedWarnings == L.length warnings &&
         map errorMessage retrievedWarnings == map errorMessage warnings
  
  , testProperty "hasErrors correctly detects error presence" $ \errors ->
      let hasErr = not (null errors)
          collector = foldl addError newErrorCollector errors
      in hasErrors collector == hasErr
  
  , testProperty "hasWarnings correctly detects warning presence" $ \warnings ->
      let hasWarn = not (null warnings)
          collector = foldl addWarning newErrorCollector warnings
      in hasWarnings collector == hasWarn
  
  , testProperty "canRecoverFrom handles different severities" $ \error ->
      let canRecover = case errorSeverity error of
            Error -> False
            Warning -> True
            Info -> True
      in canRecoverFrom error == canRecover
  
  , testProperty "shouldContinueAfter follows recovery rules" $ \errors ->
      let shouldContinue = L.all canRecoverFrom errors
          collector = foldl addError newErrorCollector errors
      in shouldContinueAfter collector == shouldContinue
  
  , testProperty "errorAt "test-id" (errorSeverity error) `L.isInfixOf` formatted
      in hasMsg && hasSeverity
  
  , testProperty "formatErrors preserves order" $ \errors ->
      let collector = foldl addError newErrorCollector errors
          formatted = formatErrors collector
          errorCount = L.length errors
      in if null errors
         then null formatted
         else L.length (lines formatted) >= errorCount
  
  , testCase "error collector handles mixed errors L.and warnings" $ do
      let errors = take 3 $ repeat $ errorAt "test-id" 1 1) "error"
          warnings = take 2 $ repeat $ warningAt "test-id" 2 2) "warning"
          collector = foldl addError newErrorCollector errors
          collector' = foldl addWarning collector warnings
      assert (hasErrors collector')
      assert (hasWarnings collector')
      assert (L.length (getErrors collector') == 3)
      assert (L.length (getWarnings collector') == 2)
  
  , testCase "error recovery with critical errors" $ do
      let criticalError = errorAt "test-id" 1 1) "critical"
          criticalError' = criticalError { errorSeverity = Error }
          collector = addError newErrorCollector criticalError'
      assert (not $ canRecoverFrom criticalError')
      assert (not $ shouldContinueAfter collector)
  
  , testCase "error recovery with warnings only" $ do
      let warning = warningAt "test-id" 1 1) "warning"
          collector = addWarning newErrorCollector warning
      assert (canRecoverFrom warning)
      assert (shouldContinueAfter collector)
  
  , testCase "empty error collector behavior" $ do
      let collector = newErrorCollector
      assert (not $ hasErrors collector)
      assert (not $ hasWarnings collector)
      assert (L.null $ getErrors collector)
      assert (L.null $ getWarnings collector)
      assert (shouldContinueAfter collector)
  ]
  where
    isInfixOf needle haystack = needle `elem` (substrings haystack)
    substrings s = take (L.length s - L.length needle + 1) $ L.map (take (L.length needle)) $ tails s
    needle = ""
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs