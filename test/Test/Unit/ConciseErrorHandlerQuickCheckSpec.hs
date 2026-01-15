{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.ConciseErrorHandlerQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter)
import ErrorHandler (ErrorHandler, ErrorSeverity(..), ErrorMessage(..), ErrorContext(..),
                    handleError, handleErrors, createError, createWarning, createInfo,
                    errorCount, warningCount, infoCount, hasErrors, hasWarnings, hasInfos,
                    getErrors, getWarnings, getInfos, clearErrors, clearWarnings, clearInfos,
                    mergeHandlers, filterBySeverity, sortBySeverity, renderErrors)

-- Helper generators for ErrorHandler tests
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Error, Warning, Info]

genErrorMessage :: Gen ErrorMessage
genErrorMessage = do
  severity <- genErrorSeverity
  msg <- elements ["Syntax error", "Type error", "Runtime error", "Warning message", "Info message"]
  line <- choose (1, 100)
  col <- choose (1, 100)
  source <- elements ["file1.typus", "file2.typus", "module1", "module2"]
  return $ ErrorMessage severity msg line col source

genErrorContext :: Gen ErrorContext
genErrorContext = do
  name <- elements ["parsing", "type checking", "code generation", "optimization"]
  details <- listOf $ elements ["detail1", "detail2", "detail3"]
  return $ ErrorContext name details

genErrorHandler :: Gen ErrorHandler
genErrorHandler = do
  numErrors <- choose (0, 5)
  numWarnings <- choose (0, 5)
  numInfos <- choose (0, 5)
  
  errors <- vectorOf numErrors genErrorMessage
  warnings <- vectorOf numWarnings genErrorMessage
  infos <- vectorOf numInfos genErrorMessage
  
  return $ foldl (\handler err -> handleError handler err) 
                  (foldl (\handler warn -> handleWarnings handler warn) 
                          (foldl (\handler info -> handleInfos handler info) 
                                  (ErrorHandler [] [] []) infos) warnings) errors

handleWarnings :: ErrorHandler -> ErrorMessage -> ErrorHandler
handleWarnings handler msg = handleError handler msg

handleInfos :: ErrorHandler -> ErrorMessage -> ErrorHandler
handleInfos handler msg = handleError handler msg

-- Test properties for ErrorHandler module

-- Basic error handling tests
prop_handle_error_increases_count :: ErrorHandler -> ErrorMessage -> Property
prop_handle_error_increases_count handler msg = 
  let newHandler = handleError handler msg
      oldCount = errorCount handler
      newCount = errorCount newHandler
  in case errorSeverity msg of
       Error -> newCount === oldCount + 1
       Warning -> newCount === oldCount
       Info -> newCount === oldCount

prop_handle_warning_increases_warning_count :: ErrorHandler -> ErrorMessage -> Property
prop_handle_warning_increases_warning_count handler msg = 
  let newHandler = handleError handler msg
      oldCount = warningCount handler
      newCount = warningCount newHandler
  in case errorSeverity msg of
       Warning -> newCount === oldCount + 1
       Error -> newCount === oldCount
       Info -> newCount === oldCount

prop_handle_info_increases_info_count :: ErrorHandler -> ErrorMessage -> Property
prop_handle_info_increases_info_count handler msg = 
  let newHandler = handleError handler msg
      oldCount = infoCount handler
      newCount = infoCount newHandler
  in case errorSeverity msg of
       Info -> newCount === oldCount + 1
       Error -> newCount === oldCount
       Warning -> newCount === oldCount

-- Error creation tests
prop_create_error_has_error_severity :: String -> Property
prop_create_error_has_error_severity msg = 
  not (null msg) ==>
  let err = createError msg
  in errorSeverity err === Error

prop_create_warning_has_warning_severity :: String -> Property
prop_create_warning_has_warning_severity msg = 
  not (null msg) ==>
  let warn = createWarning msg
  in errorSeverity warn === Warning

prop_create_info_has_info_severity :: String -> Property
prop_create_info_has_info_severity msg = 
  not (null msg) ==>
  let info = createInfo msg
  in errorSeverity info === Info

-- Error query tests
prop_has_errors_detection :: ErrorHandler -> Property
prop_has_errors_detection handler = 
  let hasErrs = hasErrors handler
      hasErrs' = errorCount handler > 0
  in hasErrs === hasErrs'

prop_has_warnings_detection :: ErrorHandler -> Property
prop_has_warnings_detection handler = 
  let hasWarns = hasWarnings handler
      hasWarns' = warningCount handler > 0
  in hasWarns === hasWarns'

prop_has_infos_detection :: ErrorHandler -> Property
prop_has_infos_detection handler = 
  let hasInfos = hasInfos handler
      hasInfos' = infoCount handler > 0
  in hasInfos === hasInfos'

prop_get_errors_returns_only_errors :: ErrorHandler -> Property
prop_get_errors_returns_only_errors handler = 
  let errs = getErrors handler
  in all (\e -> errorSeverity e == Error) errs

prop_get_warnings_returns_only_warnings :: ErrorHandler -> Property
prop_get_warnings_returns_only_warnings handler = 
  let warns = getWarnings handler
  in all (\w -> errorSeverity w == Warning) warns

prop_get_infos_returns_only_infos :: ErrorHandler -> Property
prop_get_infos_returns_only_infos handler = 
  let infos = getInfos handler
  in all (\i -> errorSeverity i == Info) infos

-- Error clearing tests
prop_clear_errors_removes_all_errors :: ErrorHandler -> Property
prop_clear_errors_removes_all_errors handler = 
  let cleared = clearErrors handler
  in errorCount cleared === 0

prop_clear_warnings_removes_all_warnings :: ErrorHandler -> Property
prop_clear_warnings_removes_all_warnings handler = 
  let cleared = clearWarnings handler
  in warningCount cleared === 0

prop_clear_infos_removes_all_infos :: ErrorHandler -> Property
prop_clear_infos_removes_all_infos handler = 
  let cleared = clearInfos handler
  in infoCount cleared === 0

-- Error filtering tests
prop_filter_by_severity_only_returns_matching :: ErrorHandler -> ErrorSeverity -> Property
prop_filter_by_severity_only_returns_matching handler severity = 
  let filtered = filterBySeverity handler severity
  in all (\e -> errorSeverity e == severity) filtered

-- Error merging tests
prop_merge_handlers_combines_counts :: ErrorHandler -> ErrorHandler -> Property
prop_merge_handlers_combines_counts handler1 handler2 = 
  let merged = mergeHandlers handler1 handler2
  in errorCount merged === errorCount handler1 + errorCount handler2 &&
     warningCount merged === warningCount handler1 + warningCount handler2 &&
     infoCount merged === infoCount handler1 + infoCount handler2

-- Error rendering tests
prop_render_errors_no_crash :: ErrorHandler -> Property
prop_render_errors_no_crash handler = 
  let rendered = renderErrors handler
  in property $ length rendered >= 0

prop_render_errors_contains_messages :: ErrorHandler -> Property
prop_render_errors_contains_messages handler = 
  let rendered = renderErrors handler
      allMsgs = map errorMessage $ getErrors handler ++ getWarnings handler ++ getInfos handler
  in if null allMsgs then property True else all (`isInfixOf` rendered) allMsgs

tests :: TestTree
tests = testGroup "Concise ErrorHandler QuickCheck Tests"
  [ testProperties "Basic Error Handling Tests"
    [ ("handle error increases count", prop_handle_error_increases_count)
    , ("handle warning increases warning count", prop_handle_warning_increases_warning_count)
    , ("handle info increases info count", prop_handle_info_increases_info_count)
    ]
  , testProperties "Error Creation Tests"
    [ ("create error has error severity", prop_create_error_has_error_severity)
    , ("create warning has warning severity", prop_create_warning_has_warning_severity)
    , ("create info has info severity", prop_create_info_has_info_severity)
    ]
  , testProperties "Error Query Tests"
    [ ("has errors detection", prop_has_errors_detection)
    , ("has warnings detection", prop_has_warnings_detection)
    , ("has infos detection", prop_has_infos_detection)
    , ("get errors returns only errors", prop_get_errors_returns_only_errors)
    , ("get warnings returns only warnings", prop_get_warnings_returns_only_warnings)
    , ("get infos returns only infos", prop_get_infos_returns_only_infos)
    ]
  , testProperties "Error Clearing Tests"
    [ ("clear errors removes all errors", prop_clear_errors_removes_all_errors)
    , ("clear warnings removes all warnings", prop_clear_warnings_removes_all_warnings)
    , ("clear infos removes all infos", prop_clear_infos_removes_all_infos)
    ]
  , testProperties "Error Filtering Tests"
    [ ("filter by severity only returns matching", prop_filter_by_severity_only_returns_matching)
    ]
  , testProperties "Error Merging Tests"
    [ ("merge handlers combines counts", prop_merge_handlers_combines_counts)
    ]
  , testProperties "Error Rendering Tests"
    [ ("render errors no crash", prop_render_errors_no_crash)
    , ("render errors contains messages", prop_render_errors_contains_messages)
    ]
  ]