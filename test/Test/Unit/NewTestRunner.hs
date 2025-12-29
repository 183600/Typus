{-# LANGUAGE CPP #-}
module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Unit.SourceLocationMathQuickCheckSpec
import qualified Test.Unit.ParserConsistencyQuickCheckSpec
import qualified Test.Unit.ErrorHandlingRecoveryQuickCheckSpec
import qualified Test.Unit.DependencyCycleQuickCheckSpec
import qualified Test.Unit.CompilerIROptimizationQuickCheckSpec
import qualified Test.Unit.StringProcessingBoundaryQuickCheckSpec
import qualified Test.Unit.TypeSystemInferenceQuickCheckSpec

main :: IO ()
main = defaultMain $ testGroup "New QuickCheck Test Modules"
    [ Test.Unit.SourceLocationMathQuickCheckSpec.tests
    , Test.Unit.ParserConsistencyQuickCheckSpec.tests
    , Test.Unit.ErrorHandlingRecoveryQuickCheckSpec.tests
    , Test.Unit.DependencyCycleQuickCheckSpec.tests
    , Test.Unit.CompilerIROptimizationQuickCheckSpec.tests
    , Test.Unit.StringProcessingBoundaryQuickCheckSpec.tests
    , Test.Unit.TypeSystemInferenceQuickCheckSpec.tests
    ]