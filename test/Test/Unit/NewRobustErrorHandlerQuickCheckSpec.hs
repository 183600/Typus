{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewRobustErrorHandlerQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import ErrorHandler
import Compiler.Errors.Core (ErrorLocation(..), ErrorSeverity(..), ErrorMessage(..))
import SourceLocation (SourcePos(..), SourceSpan(..), posAt, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing)
import Data.List (isPrefixOf, isInfixOf)

-- Test error creation properties
prop_error_location_valid :: Int -> Int -> Bool
prop_error_location_valid line col = 
  let pos = posAt line col
      errorLoc = toErrorLocation pos
  in line errorLocation == line && column errorLocation == col &&
     isJust (filePath errorLoc) && isNothing (endLine errorLoc) &&
     isNothing (endColumn errorLoc)

prop_error_message_non_empty :: String -> Property
prop_error_message_non_empty msg = 
  not (null msg) ==> 
  let errorMsg = createErrorMessage msg
  in not (T.null (messageText errorMsg))

prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_error_severity_ordering sev1 sev2 = 
  compare sev1 sev2 == compare (severityToInt sev1) (severityToInt sev2)
  where
    severityToInt ErrorWarning = 0
    severityToInt ErrorError = 1
    severityToInt ErrorFatal = 2

-- Test error handling chain properties
prop_error_chain_preserves_order :: [String] -> Bool
prop_error_chain_preserves_order msgs = 
  let errors = map createErrorMessage msgs
      chain = createErrorChain errors
      extracted = extractErrorMessages chain
  in length extracted == length errors &&
     all (\(i, msg) -> messageText (extracted !! i) == messageText (errors !! i)) 
         (zip [0..] msgs)

prop_error_chain_empty :: Bool
prop_error_chain_empty = 
  let chain = createErrorChain []
      extracted = extractErrorMessages chain
  in null extracted

-- Test error recovery properties
prop_error_recovery_attempts_non_negative :: Int -> Bool
prop_error_recovery_attempts_non_negative attempts = 
  let recovery = ErrorRecovery attempts
  in recoveryAttempts recovery >= 0

prop_error_recovery_increments :: ErrorRecovery -> Bool
prop_error_recovery_increments recovery = 
  let incremented = incrementRecovery recovery
  in recoveryAttempts incremented == recoveryAttempts recovery + 1

prop_error_recovery_limits :: Int -> Bool
prop_error_recovery_limits maxAttempts = 
  maxAttempts >= 0 ==> 
  let recovery = ErrorRecovery 0
      final = iterate incrementRecovery recovery !! maxAttempts
  in recoveryAttempts final == maxAttempts

-- Test error context properties
prop_error_context_adds_information :: String -> String -> Bool
prop_error_context_adds_information baseMsg context = 
  let baseError = createErrorMessage baseMsg
      contextedError = addErrorContext context baseError
      baseText = messageText baseError
      contextedText = messageText contextedError
  in T.length contextedText >= T.length baseText &&
     T.pack context `T.isInfixOf` contextedText

prop_error_context_preserves_severity :: ErrorSeverity -> String -> String -> Bool
prop_error_context_preserves_severity severity baseMsg context = 
  let baseError = ErrorMessage (T.pack baseMsg) severity
      contextedError = addErrorContext context baseError
  in messageSeverity contextedError == severity

-- Test error filtering properties
prop_error_filter_by_severity :: [ErrorSeverity] -> ErrorSeverity -> Bool
prop_error_filter_by_severity severities targetSeverity = 
  let errors = map (\sev -> ErrorMessage (T.pack "test") sev) severities
      filtered = filterErrorsBySeverity targetSeverity errors
  in all (\err -> messageSeverity err == targetSeverity) filtered

prop_error_filter_empty_list :: ErrorSeverity -> Bool
prop_error_filter_empty_list severity = 
  let filtered = filterErrorsBySeverity severity []
  in null filtered

-- Test error aggregation properties
prop_error_aggregation_counts_by_severity :: [ErrorSeverity] -> Bool
prop_error_aggregation_counts_by_severity severities = 
  let errors = map (\sev -> ErrorMessage (T.pack "test") sev) severities
      aggregated = aggregateErrorsBySeverity errors
      totalErrors = sum aggregated
  in totalErrors == length errors

prop_error_aggregation_empty :: Bool
prop_error_aggregation_empty = 
  let aggregated = aggregateErrorsBySeverity []
  in all (== 0) aggregated

-- Test error formatting properties
prop_error_format_includes_location :: String -> Int -> Int -> Bool
prop_error_format_includes_location msg line col = 
  let pos = posAt line col
      errorLoc = toErrorLocation pos
      error = ErrorMessage (T.pack msg) ErrorError
      formatted = formatError errorLoc error
  in show line `isInfixOf` formatted && show col `isInfixOf` formatted

prop_error_format_includes_message :: String -> Bool
prop_error_format_includes_message msg = 
  let error = ErrorMessage (T.pack msg) ErrorError
      errorLoc = ErrorLocation Nothing 1 1 Nothing Nothing
      formatted = formatError errorLoc error
  in msg `isInfixOf` formatted

-- Helper functions (these would need to be implemented in ErrorHandler module)
data ErrorRecovery = ErrorRecovery { recoveryAttempts :: Int }
  deriving (Show, Eq)

createErrorMessage :: String -> ErrorMessage
createErrorMessage msg = ErrorMessage (T.pack msg) ErrorError

toErrorLocation :: SourcePos -> ErrorLocation
toErrorLocation pos = ErrorLocation Nothing (posLine pos) (posColumn pos) Nothing Nothing

createErrorChain :: [ErrorMessage] -> [ErrorMessage]
createErrorChain = id

extractErrorMessages :: [ErrorMessage] -> [ErrorMessage]
extractErrorMessages = id

incrementRecovery :: ErrorRecovery -> ErrorRecovery
incrementRecovery (ErrorRecovery n) = ErrorRecovery (n + 1)

addErrorContext :: String -> ErrorMessage -> ErrorMessage
addErrorContext context (ErrorMessage msg sev) = 
  ErrorMessage (T.pack context `T.append` T.pack ": " `T.append` msg) sev

filterErrorsBySeverity :: ErrorSeverity -> [ErrorMessage] -> [ErrorMessage]
filterErrorsBySeverity targetSeverity = filter (\err -> messageSeverity err == targetSeverity)

aggregateErrorsBySeverity :: [ErrorMessage] -> [Int]
aggregateErrorsBySeverity errors = 
  let warnings = length $ filter (\err -> messageSeverity err == ErrorWarning) errors
      errors' = length $ filter (\err -> messageSeverity err == ErrorError) errors
      fatals = length $ filter (\err -> messageSeverity err == ErrorFatal) errors
  in [warnings, errors', fatals]

formatError :: ErrorLocation -> ErrorMessage -> String
formatError loc err = 
  let locStr = case filePath loc of
        Just path -> path ++ ":" ++ show (line loc) ++ ":" ++ show (column loc)
        Nothing -> show (line loc) ++ ":" ++ show (column loc)
      severityStr = case messageSeverity err of
        ErrorWarning -> "Warning"
        ErrorError -> "Error"
        ErrorFatal -> "Fatal"
  in locStr ++ ": " ++ severityStr ++ ": " ++ T.unpack (messageText err)

-- Arbitrary instances
instance Arbitrary ErrorSeverity where
  arbitrary = elements [ErrorWarning, ErrorError, ErrorFatal]

instance Arbitrary ErrorMessage where
  arbitrary = do
    msg <- arbitrary
    severity <- arbitrary
    return $ ErrorMessage (T.pack msg) severity

instance Arbitrary ErrorLocation where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    endLine <- arbitrary
    endCol <- arbitrary
    file <- arbitrary
    return $ ErrorLocation file line col endLine endCol

instance Arbitrary ErrorRecovery where
  arbitrary = do
    attempts <- choose (0, 10)
    return $ ErrorRecovery attempts

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests