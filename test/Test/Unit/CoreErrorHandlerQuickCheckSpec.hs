{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreErrorHandlerQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
-- | Core ErrorHandler module QuickCheck tests

import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.Arbitrary
import TestSupport.QuickCheck
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)
import Control.Monad.State (execState)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import Data.Time (UTCTime)

import Compiler.Errors.Core

-- ============================================================================
-- ErrorHandler QuickCheck Tests
-- ============================================================================

-- | Test that newErrorCollector creates an empty collector
prop_newErrorCollectorEmpty :: Property
prop_newErrorCollectorEmpty =
  let errors = []
  in property $ not (hasErrors errors) && not (hasWarnings errors) && null (getAllMessages errors)

-- | All tests
tests :: TestTree
tests = testGroup "CoreErrorHandler QuickCheck Tests"
  [ testProperty "newErrorCollectorEmpty" prop_newErrorCollectorEmpty
  ]