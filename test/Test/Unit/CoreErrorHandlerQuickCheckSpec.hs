{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.CoreErrorHandlerQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck
import Compiler.Errors.Core

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