{-# LANGUAGE DeriveGeneric #-}
module Test.Unit.ErrorHandlerQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import GHC.Generics (Generic)
import qualified Data.Text as T

import ErrorHandler
import Compiler.Errors.Core

-- Test data generators
generateErrorSeverity :: Int -> ErrorSeverity
generateErrorSeverity n = case n `mod` 3 of
  0 -> Error
  1 -> Warning
  2 -> Info

generateErrorLocation :: Int -> ErrorLocation
generateErrorLocation n = ErrorLocation
  { filePath = Just $ "file" ++ show n ++ ".go"
  , line = n `mod` 100 + 1
  , column = n `mod` 80 + 1
  , endLine = Just $ n `mod` 100 + 1
  , endColumn = Just $ (n `mod` 80 + 1) + 5
  }

generateTypeError :: Int -> TypeError
generateTypeError n = TypeError
  { errorId = "ERR" ++ show n
  , severity = generateErrorSeverity n
  , errorMessage = T.pack $ "Error message " ++ show n
  , errorLocation = Just $ generateErrorLocation n
  , errorContext = Just $ T.pack $ "Context " ++ show n
  }

generateErrorHandler :: Int -> ErrorHandler
generateErrorHandler n = take (n `mod` 10) [generateTypeError i | i <- [1..20]]

-- QuickCheck properties
prop_error_handler_creation :: Property
prop_error_handler_creation =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
    in property $ length handler == min (n `mod` 10) 20

prop_handle_error :: Property
prop_handle_error =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let handler = generateErrorHandler n
        error = generateTypeError m
        newHandler = handleError handler error
    in property $ 
      length newHandler == length handler + 1 &&
      head newHandler == error

prop_handle_errors :: Property
prop_handle_errors =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let handler = generateErrorHandler n
        errors = take (m `mod` 5) [generateTypeError i | i <- [100..110]]
        newHandler = handleErrors handler errors
    in property $ 
      length newHandler == length handler + length errors &&
      take (length errors) newHandler == reverse errors

prop_handle_errors_list :: Property
prop_handle_errors_list =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let handler = generateErrorHandler n
        errors = take (m `mod` 5) [generateTypeError i | i <- [100..110]]
        newHandler = handleErrorsList handler errors
    in property $ 
      length newHandler == length handler + length errors &&
      take (length errors) newHandler == reverse errors

prop_create_error :: Property
prop_create_error =
  forAll arbitrary $ \n ->
    let errId = "ERR" ++ show n
        msg = T.pack $ "Error message " ++ show n
        loc = generateErrorLocation n
        error = createError errId msg loc
    in property $
      errorId error == errId &&
      errorMessage error == msg &&
      errorLocation error == Just loc &&
      severity error == Error

prop_create_warning :: Property
prop_create_warning =
  forAll arbitrary $ \n ->
    let errId = "WARN" ++ show n
        msg = T.pack $ "Warning message " ++ show n
        loc = generateErrorLocation n
        warning = createWarning errId msg loc
    in property $
      errorId warning == errId &&
      errorMessage warning == msg &&
      errorLocation warning == Just loc &&
      severity warning == Warning

prop_create_info :: Property
prop_create_info =
  forAll arbitrary $ \n ->
    let errId = "INFO" ++ show n
        msg = T.pack $ "Info message " ++ show n
        loc = generateErrorLocation n
        info = createInfo errId msg loc
    in property $
      errorId info == errId &&
      errorMessage info == msg &&
      errorLocation info == Just loc &&
      severity info == Info

prop_error_count :: Property
prop_error_count =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        expectedCount = length $ filter (\e -> severity e == Error) handler
        actualCount = errorCount handler
    in property $ expectedCount == actualCount

prop_warning_count :: Property
prop_warning_count =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        expectedCount = length $ filter (\e -> severity e == Warning) handler
        actualCount = warningCount handler
    in property $ expectedCount == actualCount

prop_info_count :: Property
prop_info_count =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        expectedCount = length $ filter (\e -> severity e == Info) handler
        actualCount = infoCount handler
    in property $ expectedCount == actualCount

prop_has_infos :: Property
prop_has_infos =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        hasInfoMessages = any (\e -> severity e == Info) handler
        hasInfoResult = hasInfos handler
    in property $ hasInfoResult == hasInfoMessages

prop_get_infos :: Property
prop_get_infos =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        infos = getInfos handler
        expectedInfos = filter (\e -> severity e == Info) handler
    in property $ infos == expectedInfos

prop_clear_errors :: Property
prop_clear_errors =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        cleared = clearErrors handler
        expectedCleared = filter (\e -> severity e /= Error) handler
    in property $ cleared == expectedCleared

prop_clear_warnings :: Property
prop_clear_warnings =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        cleared = clearWarnings handler
        expectedCleared = filter (\e -> severity e /= Warning) handler
    in property $ cleared == expectedCleared

prop_clear_infos :: Property
prop_clear_infos =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        cleared = clearInfos handler
        expectedCleared = filter (\e -> severity e /= Info) handler
    in property $ cleared == expectedCleared

prop_merge_handlers :: Property
prop_merge_handlers =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let handler1 = generateErrorHandler n
        handler2 = generateErrorHandler m
        merged = mergeHandlers handler1 handler2
    in property $ 
      length merged == length handler1 + length handler2 &&
      take (length handler1) merged == handler1 &&
      drop (length handler1) merged == handler2

prop_filter_by_severity :: Property
prop_filter_by_severity =
  forAll arbitrary $ \n ->
  forAll arbitrary $ \m ->
    let handler = generateErrorHandler n
        sev = generateErrorSeverity m
        filtered = filterBySeverityForTests sev handler
        expectedFiltered = filter (\e -> severity e == sev) handler
    in property $ filtered == expectedFiltered

prop_render_errors :: Property
prop_render_errors =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        rendered = renderErrors handler
        lines = lines rendered
    in property $ 
      length lines == length handler &&
      all (not . null) lines

prop_sort_by_severity :: Property
prop_sort_by_severity =
  forAll arbitrary $ \n ->
    let handler = generateErrorHandler n
        sorted = sortBySeverity handler
        severities = map severity sorted
    in property $ severities == sort severities

-- Test suite
testSuite :: TestTree
testSuite = testGroup "ErrorHandler QuickCheck Tests"
  [ testProperty "error handler creation" prop_error_handler_creation
  , testProperty "handle error" prop_handle_error
  , testProperty "handle errors" prop_handle_errors
  , testProperty "handle errors list" prop_handle_errors_list
  , testProperty "create error" prop_create_error
  , testProperty "create warning" prop_create_warning
  , testProperty "create info" prop_create_info
  , testProperty "error count" prop_error_count
  , testProperty "warning count" prop_warning_count
  , testProperty "info count" prop_info_count
  , testProperty "has infos" prop_has_infos
  , testProperty "get infos" prop_get_infos
  , testProperty "clear errors" prop_clear_errors
  , testProperty "clear warnings" prop_clear_warnings
  , testProperty "clear infos" prop_clear_infos
  , testProperty "merge handlers" prop_merge_handlers
  , testProperty "filter by severity" prop_filter_by_severity
  , testProperty "render errors" prop_render_errors
  , testProperty "sort by severity" prop_sort_by_severity
  ]

-- Unit tests for specific edge cases
unitTests :: TestTree
unitTests = testGroup "ErrorHandler Unit Tests"
  [ testCase "empty error handler" $ do
      let handler = [] :: ErrorHandler
      assertEqual "Empty handler has no errors" 0 (errorCount handler)
      assertEqual "Empty handler has no warnings" 0 (warningCount handler)
      assertEqual "Empty handler has no infos" 0 (infoCount handler)
      assertBool "Empty handler has no infos" $ not (hasInfos handler)

  , testCase "handler with single error" $ do
      let loc = generateErrorLocation 1
          error = createError "ERR001" (T.pack "Test error") loc
          handler = [error]
      assertEqual "Handler has one error" 1 (errorCount handler)
      assertEqual "Handler has no warnings" 0 (warningCount handler)
      assertEqual "Handler has no infos" 0 (infoCount handler)

  , testCase "handler with mixed severities" $ do
      let loc = generateErrorLocation 1
          error = createError "ERR001" (T.pack "Test error") loc
          warning = createWarning "WARN001" (T.pack "Test warning") loc
          info = createInfo "INFO001" (T.pack "Test info") loc
          handler = [error, warning, info]
      assertEqual "Handler has one error" 1 (errorCount handler)
      assertEqual "Handler has one warning" 1 (warningCount handler)
      assertEqual "Handler has one info" 1 (infoCount handler)
      assertBool "Handler has infos" $ hasInfos handler

  , testCase "clear functions" $ do
      let loc = generateErrorLocation 1
          error = createError "ERR001" (T.pack "Test error") loc
          warning = createWarning "WARN001" (T.pack "Test warning") loc
          info = createInfo "INFO001" (T.pack "Test info") loc
          handler = [error, warning, info]
          clearedErrors = clearErrors handler
          clearedWarnings = clearWarnings handler
          clearedInfos = clearInfos handler
      assertEqual "Clear errors removes only errors" 0 (errorCount clearedErrors)
      assertEqual "Clear errors preserves warnings" 1 (warningCount clearedErrors)
      assertEqual "Clear errors preserves infos" 1 (infoCount clearedErrors)
      assertEqual "Clear warnings removes only warnings" 1 (errorCount clearedWarnings)
      assertEqual "Clear warnings removes warnings" 0 (warningCount clearedWarnings)
      assertEqual "Clear warnings preserves infos" 1 (infoCount clearedWarnings)
      assertEqual "Clear infos removes only infos" 1 (errorCount clearedInfos)
      assertEqual "Clear infos preserves warnings" 1 (warningCount clearedInfos)
      assertEqual "Clear infos removes infos" 0 (infoCount clearedInfos)

  , testCase "merge handlers" $ do
      let loc = generateErrorLocation 1
          error1 = createError "ERR001" (T.pack "Test error 1") loc
          error2 = createError "ERR002" (T.pack "Test error 2") loc
          handler1 = [error1]
          handler2 = [error2]
          merged = mergeHandlers handler1 handler2
      assertEqual "Merged handler has two errors" 2 (errorCount merged)

  , testCase "filter by severity" $ do
      let loc = generateErrorLocation 1
          error = createError "ERR001" (T.pack "Test error") loc
          warning = createWarning "WARN001" (T.pack "Test warning") loc
          info = createInfo "INFO001" (T.pack "Test info") loc
          handler = [error, warning, info]
          errorsOnly = filterBySeverityForTests Error handler
          warningsOnly = filterBySeverityForTests Warning handler
          infosOnly = filterBySeverityForTests Info handler
      assertEqual "Filter errors returns only errors" 1 (length errorsOnly)
      assertEqual "Filter warnings returns only warnings" 1 (length warningsOnly)
      assertEqual "Filter infos returns only infos" 1 (length infosOnly)
  ]

-- Combined test suite
tests :: TestTree
tests = testGroup "ErrorHandler Tests"
  [ testSuite
  , unitTests
  ]