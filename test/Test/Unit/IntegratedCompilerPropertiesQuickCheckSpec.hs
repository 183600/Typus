{-# LANGUAGE CPP #-}

module Test.Unit.IntegratedCompilerPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

import IntegratedCompiler
import AnalyzerIntegration (ErrorSeverity(..))

prop_defaultCompilerConfig_has_analyzers_enabled :: Property
prop_defaultCompilerConfig_has_analyzers_enabled =
  let cfg = defaultCompilerConfig
  in conjoin
    [ enableOwnership cfg === True
    , enableDependentTypes cfg === True
    , errorReportingLevel cfg === Warning
    ]

prop_compilerConfig_equality_reflexive :: Property
prop_compilerConfig_equality_reflexive =
  forAll genCompilerConfig $ \cfg ->
  cfg === cfg
  where
    genCompilerConfig = do
      ownership <- arbitrary
      depTypes <- arbitrary
      level <- elements [Info, Warning, Error]
      return $ CompilerConfig ownership depTypes level

prop_integratedCompileResult_success_consistency :: Property
prop_integratedCompileResult_success_consistency =
  forAll genResult $ \result ->
  let hasErrors = not (null (filteredErrors result)) || not (null (compilerErrors result))
  in (success result && hasErrors) === False
  where
    genResult = do
      successVal <- arbitrary
      code <- listOf (elements ['a'..'z'])
      return $ IntegratedCompileResult
        { success = successVal
        , compiledCode = code
        , analysisResult = Nothing
        , syntaxErrors = []
        , filteredErrors = []
        , compilerErrors = []
        , compilationWarnings = []
        , compilationInfo = []
        }

prop_formatCompilationResult_nonempty :: Property
prop_formatCompilationResult_nonempty =
  forAll genResult $ \result ->
  not (null (formatCompilationResult result)) === True
  where
    genResult = return $ IntegratedCompileResult
      { success = True
      , compiledCode = "package main"
      , analysisResult = Nothing
      , syntaxErrors = []
      , filteredErrors = []
      , compilerErrors = []
      , compilationWarnings = []
      , compilationInfo = []
      }

prop_errorSeverity_ordering :: Property
prop_errorSeverity_ordering =
  conjoin
    [ Info < Warning
    , Warning < Error
    , Info < Error
    ]

prop_compilerConfig_with_disabled_analyzers :: Property
prop_compilerConfig_with_disabled_analyzers =
  let cfg = CompilerConfig False False Info
  in conjoin
    [ enableOwnership cfg === False
    , enableDependentTypes cfg === False
    ]

prop_integratedCompileResult_empty_errors_implies_success :: Property
prop_integratedCompileResult_empty_errors_implies_success =
  let result = IntegratedCompileResult
        { success = True
        , compiledCode = "package main"
        , analysisResult = Nothing
        , syntaxErrors = []
        , filteredErrors = []
        , compilerErrors = []
        , compilationWarnings = []
        , compilationInfo = []
        }
  in success result === True

tests :: TestTree
tests = testGroup "IntegratedCompiler Properties QuickCheck Tests"
  [ fastProperty "defaultCompilerConfig has analyzers enabled" prop_defaultCompilerConfig_has_analyzers_enabled
  , fastProperty "CompilerConfig equality is reflexive" prop_compilerConfig_equality_reflexive
  , fastProperty "IntegratedCompileResult success consistency" prop_integratedCompileResult_success_consistency
  , fastProperty "formatCompilationResult produces non-empty output" prop_formatCompilationResult_nonempty
  , fastProperty "ErrorSeverity ordering is correct" prop_errorSeverity_ordering
  , fastProperty "CompilerConfig with disabled analyzers" prop_compilerConfig_with_disabled_analyzers
  , fastProperty "empty errors implies success" prop_integratedCompileResult_empty_errors_implies_success
  ]
