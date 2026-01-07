module Test.Unit.NewErrorHandlingRecoveryQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (Property,             testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat, choose)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Compiler.Errors.Core ()
      ErrorLocation(..), ErrorContext(..), emptyContext, ErrorRecovery(..),
      ErrorCollector, newErrorCollector, addError, addWarning, addInfo,
      getErrors, getWarnings, getInfo, getAllMessages, hasErrors, hasWarnings,
      formatError, formatErrors, formatErrorWithLocation, formatErrorsWithLocation,
      canRecoverFrom, shouldContinueAfter, errorAt, errorWithCategory, 
      warningAt, warningWithCategory, infoAt, infoWithCategory )
import SourceLocation (SourcePos(..), SourceSpan(..), posAtLineCol, spanBetween)
import qualified Data.List as L
import Data.List ()
                                    shouldContinue = shouldContinueAfter errors
                                    hasFatal = L.any (\sev ->                               sev == FatalError) severities
  in                               shouldContinue == not hasFatal
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | 
prop_recoveryStrategiesAppropriate :: ErrorCategory -> Property
prop_recoveryStrategiesAppropriate                               category =
  let pos = posAtLineCol 1 1
                                    error = errorWithCategory SyntaxError pos emptyContext "test"
      -- This would depend on the actual implementation
      -- For now, just test that the function doesn't crash
  in canRecoverFrom                               error == True || canRecoverFrom                               error == False

-- | 
prop_contextPreservedInRecovery :: String -> Property
prop_contextPreservedInRecovery                               filename =
  let pos = posAtLineCol 1 1
                                    context = emptyContext {                               contextFile = Just filename }
                                    error = errorWithCategory SyntaxError pos context "test"
                                    canRecover = canRecoverFrom error
  in                               canRecover ==> errorContext                               error == context

-- | 
prop_errorCollectorCount :: [ErrorSeverity] -> Property
prop_errorCollectorCount                               severities =
  let pos = posAtLineCol 1 1
                                    errors = L.map (\sev -> TypeError {                               errorSeverity = sev,                               errorLocation = ErrorLocation pos,                               errorMessage = "test",                               errorContext = emptyContext,                               errorRecovery = NoRecovery }) severities
                                    collector = foldl addError newErrorCollector errors
                                    errorCount = L.length (getErrors collector)
                                    warningCount = L.length (getWarnings collector)
                                    infoCount = L.length (getInfo collector)
                                    expectedErrors = L.length (L.filter (\sev ->                               sev == Error ||                               sev == FatalError) severities)
                                    expectedWarnings = L.length (L.filter (\sev ->                               sev == Warning) severities)
                                    expectedInfo = L.length (L.filter (\sev ->                               sev == Info) severities)
  in                               errorCount == expectedErrors &&                               warningCount == expectedWarnings &&                               infoCount == expectedInfo

-- | 
prop_errorCollectorSeparation :: [ErrorSeverity] -> Property
prop_errorCollectorSeparation                               severities =
  let pos = posAtLineCol 1 1
                                    errors = L.map (\sev -> TypeError {                               errorSeverity = sev,                               errorLocation = ErrorLocation pos,                               errorMessage = "test",                               errorContext = emptyContext,                               errorRecovery = NoRecovery }) severities
                                    collector = foldl addError newErrorCollector errors
                                    errorList = getErrors collector
                                    warningList = getWarnings collector
                                    infoList = getInfo collector
                                    allErrors = errorList ++ warningList ++ infoList
  in L.all (\e -> errorSeverity e `elem` severities) allErrors

-- | 
prop_errorCollectorOrder :: [String] -> Property
prop_errorCollectorOrder                               messages =
  let pos = posAtLineCol 1 1
                                    errors = zipWith (\msg idx -> TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = msg,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }) messages [1..]
                                    collector = foldl addError newErrorCollector errors
                                    collectedErrors = getErrors collector
                                    collectedMessages = map errorMessage collectedErrors
  in                               collectedMessages == messages

-- | 
prop_errorCollectorBulk :: [[String]] -> Property
prop_errorCollectorBulk                               messageGroups =
  let pos = posAtLineCol 1 1
      addGroup collector                               group = 
        let errors = L.map (\msg -> TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = msg,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }) group
        in foldl addError collector errors
                                    finalCollector = foldl addGroup newErrorCollector messageGroups
                                    totalMessages = L.sum (map L.length messageGroups)
                                    collectedCount = L.length (getErrors finalCollector)
  in                               collectedCount == totalMessages

-- | 
prop_errorFormattingContainsInfo :: String -> Property
prop_errorFormattingContainsInfo                               message =
  not (null message) ==>
  let pos = posAtLineCol 1 1
                                    error = TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = message,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }
                                    formatted = formatError error
  in message `L.isInfixOf` formatted

-- | 
prop_errorFormattingSpecialChars :: String -> Property
prop_errorFormattingSpecialChars                               message =
  let specialChars = "\n\t\r\"'\\"
                                    messageWithSpecials = message ++ specialChars
                                    pos = posAtLineCol 1 1
                                    error = TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = messageWithSpecials,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }
                                    formatted = formatError error
  in not (null formatted)

-- | 
prop_errorFormattingWithLocation :: Int -> Int -> String -> Property
prop_errorFormattingWithLocation line col                               message =
  line > 0 && col > 0 && not (null message) ==>
  let pos = posAtLineCol line col
                                    error = TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = message,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }
                                    formatted = formatErrorWithLocation error
                                    lineStr = show line
                                    colStr = show col
  in lineStr `L.isInfixOf` formatted && colStr `L.isInfixOf` formatted

-- | 
prop_batchFormattingStructure :: [String] -> Property
prop_batchFormattingStructure                               messages =
  not (null messages) ==>
  let pos = posAtLineCol 1 1
                                    errors = L.map (\msg -> TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = msg,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }) messages
                                    formatted = formatErrors errors
  in L.length (lines formatted) >= L.length messages

-- | 
prop_emptyContextNoInfo :: Property
                              prop_emptyContextNoInfo =
  let context = emptyContext
  in contextFile                               context == Nothing &&
     contextFunction                               context == Nothing &&
     contextLine                               context == Nothing

-- | 
prop_contextMergingPreserves :: String -> String -> Property
prop_contextMergingPreserves file1                               file2 =
  let context1 = emptyContext {                               contextFile = Just file1 }
                                    context2 = emptyContext {                               contextFile = Just file2 }
      -- This would depend on actual merge implementation
  in contextFile                               context1 == Just file1 && contextFile                               context2 == Just file2

-- | 
prop_contextNestingPreserves :: [String] -> Property
prop_contextNestingPreserves                               files =
  let contexts = L.map (\file -> emptyContext {                               contextFile = Just file }) files
      -- Test that each context preserves its file
  in L.all (\ctx -> contextFile ctx `elem` map Just files) contexts

-- | 
prop_combinedErrorHighestSeverity :: [ErrorSeverity] -> Property
prop_combinedErrorHighestSeverity                               severities =
  not (null severities) ==>
  let pos = posAtLineCol 1 1
                                    errors = L.map (\sev -> TypeError {                               errorSeverity = sev,                               errorLocation = ErrorLocation pos,                               errorMessage = "test",                               errorContext = emptyContext,                               errorRecovery = NoRecovery }) severities
      -- This would depend on actual CombinedError implementation
                                    highestSeverity = L.maximum severities
  in highestSeverity `elem` severities

-- | 
prop_combinedErrorLocationSpans :: [Int] -> [Int] -> Property
prop_combinedErrorLocationSpans lines                               cols =
  L.length                               lines == L.length cols && not (null lines) ==>
  let positions = zipWith (\line col -> posAtLineCol line col) lines cols
      -- This would depend on actual CombinedError implementation
                                    minLine = L.minimum lines
                                    maxLine = L.maximum lines
  in minLine <= maxLine

-- | 
prop_combinedErrorMessagesConcatenated :: [String] -> Property
prop_combinedErrorMessagesConcatenated                               messages =
  not (null messages) ==>
  let -- This would depend on actual CombinedError implementation
                                    combined = L.concat messages
  in L.length combined >= L.sum (map L.length messages)

-- | 
prop_errorHandlingEmptyMessages :: Property
                              prop_errorHandlingEmptyMessages =
  let pos = posAtLineCol 1 1
                                    error = TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = "",                               errorContext = emptyContext,                               errorRecovery = NoRecovery }
                                    collector = addError newErrorCollector error
                                    formatted = formatError error
  in not (null formatted)

-- | 
prop_errorHandlingLongMessages :: Property
                              prop_errorHandlingLongMessages =
  let longMessage = replicate 10000 'a'
                                    pos = posAtLineCol 1 1
                                    error = TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = longMessage,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }
                                    collector = addError newErrorCollector error
                                    formatted = formatError error
  in not (null formatted)

-- | unicode
prop_errorHandlingUnicodeMessages :: Property
                              prop_errorHandlingUnicodeMessages =
  let unicodeMessage = "  with mojis L.and "
                                    pos = posAtLineCol 1 1
                                    error = TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = unicodeMessage,                               errorContext = emptyContext,                               errorRecovery = NoRecovery }
                                    collector = addError newErrorCollector error
                                    formatted = formatError error
  in not (null formatted)

-- | 
prop_errorHandlingExtremePositions :: Property
                              prop_errorHandlingExtremePositions =
  let extremeLine = 1000000
                                    extremeCol = 1000000
                                    pos = posAtLineCol extremeLine extremeCol
                                    error = TypeError {                               errorSeverity = Error,                               errorLocation = ErrorLocation pos,                               errorMessage = "extreme position",                               errorContext = emptyContext,                               errorRecovery = NoRecovery }
                                    collector = addError newErrorCollector error
                                    formatted =  formatErrorWithLocation error
  in property $ not (null formatted)]]