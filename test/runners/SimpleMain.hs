module Main where

import Test.Tasty
import Test.Unit.ConciseUtilsQuickCheckSpec (tests)
import Test.Unit.ConciseSourceLocationQuickCheckSpec (tests)
import Test.Unit.ConciseParserQuickCheckSpec (tests)
import Test.Unit.ConciseCompilerQuickCheckSpec (tests)
import Test.Unit.ConciseErrorHandlerQuickCheckSpec (tests)
import Test.Unit.ConciseOwnershipQuickCheckSpec (tests)
import Test.Unit.ConciseDependenciesQuickCheckSpec (tests)
import Test.Unit.ConciseIntegrationQuickCheckSpec (tests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Typus Test Suite"
  [ testGroup "Concise Utils QuickCheck Tests" [Test.Unit.ConciseUtilsQuickCheckSpec.tests]
  , testGroup "Concise SourceLocation QuickCheck Tests" [Test.Unit.ConciseSourceLocationQuickCheckSpec.tests]
  , testGroup "Concise Parser QuickCheck Tests" [Test.Unit.ConciseParserQuickCheckSpec.tests]
  , testGroup "Concise Compiler QuickCheck Tests" [Test.Unit.ConciseCompilerQuickCheckSpec.tests]
  , testGroup "Concise ErrorHandler QuickCheck Tests" [Test.Unit.ConciseErrorHandlerQuickCheckSpec.tests]
  , testGroup "Concise Ownership QuickCheck Tests" [Test.Unit.ConciseOwnershipQuickCheckSpec.tests]
  , testGroup "Concise Dependencies QuickCheck Tests" [Test.Unit.ConciseDependenciesQuickCheckSpec.tests]
  , testGroup "Concise Integration QuickCheck Tests" [Test.Unit.ConciseIntegrationQuickCheckSpec.tests]
  ]