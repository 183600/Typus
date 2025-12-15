{-# LANGUAGE CPP #-}

module Test.Unit.CompilerUtilsPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import System.IO.Silently (capture_)

import CompilerUtils
import GoToolchain (defaultGoExecutor)

prop_defaultLogger_executes :: Property
prop_defaultLogger_executes =
  ioProperty $ do
    let logger = defaultLogger
    _ <- capture_ $ logInfo logger "test"
    return (property True)

prop_silentLogger_executes :: Property
prop_silentLogger_executes =
  ioProperty $ do
    let logger = silentLogger
    logInfo logger "test"
    logDebug logger "test"
    logWarning logger "test"
    return (property True)

prop_newCompilerContext_has_logger :: Property
prop_newCompilerContext_has_logger =
  let ctx = newCompilerContext defaultLogger
  in property True

prop_newCompilerContextWithExecutor_has_executor :: Property
prop_newCompilerContextWithExecutor_has_executor =
  ioProperty $ do
    executor <- defaultGoExecutor (\_ -> return ())
    let ctx = newCompilerContextWithExecutor defaultLogger executor
    return (property True)

prop_logger_info_nonempty :: Property
prop_logger_info_nonempty =
  forAll (listOf1 (elements ['a'..'z'])) $ \msg ->
  ioProperty $ do
    let logger = defaultLogger
    _ <- capture_ $ logInfo logger msg
    return (property True)

prop_logger_debug_nonempty :: Property
prop_logger_debug_nonempty =
  forAll (listOf1 (elements ['a'..'z'])) $ \msg ->
  ioProperty $ do
    let logger = defaultLogger
    _ <- capture_ $ logDebug logger msg
    return (property True)

prop_logger_warning_nonempty :: Property
prop_logger_warning_nonempty =
  forAll (listOf1 (elements ['a'..'z'])) $ \msg ->
  ioProperty $ do
    let logger = defaultLogger
    _ <- capture_ $ logWarning logger msg
    return (property True)

prop_compilerContext_consistency :: Property
prop_compilerContext_consistency =
  ioProperty $ do
    let logger = defaultLogger
    executor <- defaultGoExecutor (\_ -> return ())
    let ctx = newCompilerContextWithExecutor logger executor
    return (property True)

tests :: TestTree
tests = testGroup "CompilerUtils Properties QuickCheck Tests"
  [ fastProperty "defaultLogger executes" prop_defaultLogger_executes
  , fastProperty "silentLogger executes" prop_silentLogger_executes
  , fastProperty "newCompilerContext has logger" prop_newCompilerContext_has_logger
  , fastProperty "newCompilerContextWithExecutor has executor" prop_newCompilerContextWithExecutor_has_executor
  , fastProperty "logger info handles non-empty messages" prop_logger_info_nonempty
  , fastProperty "logger debug handles non-empty messages" prop_logger_debug_nonempty
  , fastProperty "logger warning handles non-empty messages" prop_logger_warning_nonempty
  , fastProperty "compiler context consistency" prop_compilerContext_consistency
  ]
