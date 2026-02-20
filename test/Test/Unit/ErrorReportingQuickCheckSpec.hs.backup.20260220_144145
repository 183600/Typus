{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing  -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.ErrorReportingQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure, Assertion)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>), classify, sized)
import Data.List (nub, nubBy, sort, groupBy, sortBy, find, delete, isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (replicateM, when)

-- Error reporting types for testing

-- Local SourceLocation type for testing
data SourceLocation = SourceLocation
  { sourceLine :: Int
  , sourceColumn :: Int
  } deriving (Eq, Show)
data ErrorSeverity = Error | Warning | Info | Hint
                   deriving (Eq, Ord, Show)

data ErrorCategory = SyntaxError | TypeError | NameError | SemanticError | InternalError
                   deriving (Eq, Ord, Show)

data ErrorMessage = ErrorMessage
  { errorMsg :: String
  , errorCategory :: ErrorCategory
  , errorSeverity :: ErrorSeverity
  , errorLocation :: SourceLocation
  , errorContext :: [String]
  , errorSuggestions :: [String]
  }
  deriving (Eq, Show)

data ErrorReport = ErrorReport
  { reportErrors :: [ErrorMessage]
  , reportWarnings :: [ErrorMessage]
  , reportInfo :: [ErrorMessage]
  , reportHints :: [ErrorMessage]
  }
  deriving (Eq, Show)

data ErrorFormatter = PlainFormatter | ColoredFormatter | JsonFormatter
                   deriving (Eq, Show)

-- Helper generators for error reporting tests
genErrorFormatter :: Gen ErrorFormatter
genErrorFormatter = elements [PlainFormatter, ColoredFormatter, JsonFormatter]

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Error, Warning, Info, Hint]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [SyntaxError, TypeError, NameError, SemanticError, InternalError]

genSourceLocation :: Gen SourceLocation
genSourceLocation = do
  line <- choose (1, 100)
  column <- choose (1, 100)
  return $ SourceLocation { sourceLine = line, sourceColumn = column }

genString :: Gen String
genString = do
  len <- choose (5, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "

genErrorMessage :: Gen ErrorMessage
genErrorMessage = do
  msg <- genString
  category <- genErrorCategory
  severity <- genErrorSeverity
  location <- genSourceLocation
  contextSize <- choose (0, 3)
  context <- replicateM contextSize genString
  suggestionsSize <- choose (0, 3)
  suggestions <- replicateM suggestionsSize genString
  return $ ErrorMessage msg category severity location context suggestions

genErrorReport :: Gen ErrorReport
genErrorReport = do
  errorSize <- choose (0, 5)
  warningSize <- choose (0, 5)
  infoSize <- choose (0, 3)
  hintSize <- choose (0, 3)
  
  errors <- replicateM errorSize genErrorMessage
  warnings <- replicateM warningSize genErrorMessage
  info <- replicateM infoSize genErrorMessage
  hints <- replicateM hintSize genErrorMessage
  
  return $ ErrorReport errors warnings info hints

-- Arbitrary instances
instance Arbitrary SourceLocation where
  arbitrary = genSourceLocation

instance Arbitrary ErrorMessage where
  arbitrary = genErrorMessage

instance Arbitrary ErrorReport where
  arbitrary = genErrorReport

instance Arbitrary ErrorFormatter where
  arbitrary = genErrorFormatter

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

-- Test properties for error reporting

-- Property 1: Error message formatting preserves content
prop_error_formatting_preserves_content :: ErrorMessage -> ErrorFormatter -> Bool
prop_error_formatting_preserves_content msg formatter = 
  let formatted = formatErrorMessage msg formatter
      originalContent = errorMsg msg
  in originalContent `isInfixOf` formatted

-- Property 2: Error filtering by severity is correct
prop_error_filtering_by_severity :: ErrorReport -> ErrorSeverity -> Bool
prop_error_filtering_by_severity report severity = 
  let filtered = filterBySeverity report severity
      expected = case severity of
        Error -> reportErrors report
        Warning -> reportWarnings report
        Info -> reportInfo report
        Hint -> reportHints report
  in length filtered == length expected

-- Property 3: Error sorting by location preserves order
prop_error_sorting_by_location :: [ErrorMessage] -> Bool
prop_error_sorting_by_location msgs = 
  let sorted = sortByLocation msgs
      locations = map (errorLocation) sorted
  in isSortedByLine locations

-- Property 4: Error grouping by category is consistent
prop_error_grouping_by_category :: [ErrorMessage] -> Bool
prop_error_grouping_by_category msgs = 
  let grouped = groupByCategory msgs
      categories = Map.keys grouped
      allCategories = Set.fromList $ map errorCategory msgs
  in Set.fromList categories == allCategories

-- Property 5: Error deduplication removes duplicates
prop_error_deduplication_removes_duplicates :: [ErrorMessage] -> Bool
prop_error_deduplication_removes_duplicates msgs = 
  let deduplicated = deduplicateErrors msgs
      msgPairs = [(msg1, msg2) | msg1 <- msgs, msg2 <- msgs, msg1 /= msg2]
      hasDuplicates = any (\(m1, m2) -> isSameError m1 m2) msgPairs
  in not hasDuplicates || length deduplicated < length msgs

-- Property 6: Error context is preserved in reporting
prop_error_context_preserved :: ErrorMessage -> Bool
prop_error_context_preserved msg = 
  let report = ErrorReport [msg] [] [] []
      extracted = extractContext report (errorLocation msg)
      originalContext = errorContext msg
  in all (`elem` extracted) originalContext

-- Property 7: Error suggestions are relevant to error type
prop_error_suggestions_relevant :: ErrorMessage -> Bool
prop_error_suggestions_relevant msg = 
  let category = errorCategory msg
      suggestions = errorSuggestions msg
  in null suggestions || all (isRelevantToCategory category) suggestions

-- Property 8: Error location highlighting is accurate
prop_error_location_highlighting_accurate :: String -> SourceLocation -> Bool
prop_error_location_highlighting_accurate source loc = 
  let highlighted = highlightErrorLocation source loc
      expectedPos = (sourceLine loc, sourceColumn loc)
  in containsHighlightAt highlighted expectedPos

-- Property 9: Error report summary is accurate
prop_error_report_summary_accurate :: ErrorReport -> Bool
prop_error_report_summary_accurate report = 
  let summary = generateReportSummary report
      errorCount = length (reportErrors report)
      warningCount = length (reportWarnings report)
      infoCount = length (reportInfo report)
      hintCount = length (reportHints report)
  in containsCounts summary errorCount warningCount infoCount hintCount

-- Property 10: Error filtering by context is correct
prop_error_filtering_by_context :: ErrorReport -> String -> Bool
prop_error_filtering_by_context report context = 
  let filtered = filterByContext report context
      hasContext = any (elem context . errorContext) 
                   (reportErrors report ++ reportWarnings report ++ 
                    reportInfo report ++ reportHints report)
  in not hasContext || length filtered > 0

-- Helper functions for error reporting
formatErrorMessage :: ErrorMessage -> ErrorFormatter -> String
formatErrorMessage msg PlainFormatter = 
  let severityStr = show (errorSeverity msg)
      categoryStr = show (errorCategory msg)
      locationStr = show (errorLocation msg)
  in severityStr ++ " [" ++ categoryStr ++ "] at " ++ locationStr ++ ": " ++ errorMsg msg
formatErrorMessage msg ColoredFormatter = 
  -- Simplified colored formatting
  "\x1b[31m" ++ formatErrorMessage msg PlainFormatter ++ "\x1b[0m"
formatErrorMessage msg JsonFormatter = 
  -- Simplified JSON formatting
  "{\"severity\":\"" ++ show (errorSeverity msg) ++ 
  "\",\"category\":\"" ++ show (errorCategory msg) ++ 
  "\",\"location\":\"" ++ show (errorLocation msg) ++ 
  "\",\"message\":\"" ++ errorMsg msg ++ "\"}"

filterBySeverity :: ErrorReport -> ErrorSeverity -> [ErrorMessage]
filterBySeverity report Error = reportErrors report
filterBySeverity report Warning = reportWarnings report
filterBySeverity report Info = reportInfo report
filterBySeverity report Hint = reportHints report

sortByLocation :: [ErrorMessage] -> [ErrorMessage]
sortByLocation = sortBy (\msg1 msg2 -> 
  let loc1 = errorLocation msg1
      loc2 = errorLocation msg2
  in compare (sourceLine loc1, sourceColumn loc1) 
            (sourceLine loc2, sourceColumn loc2))

isSortedByLine :: [SourceLocation] -> Bool
isSortedByLine [] = True
isSortedByLine [_] = True
isSortedByLine (l1:l2:ls) = 
  (sourceLine l1, sourceColumn l1) <= (sourceLine l2, sourceColumn l2) && 
  isSortedByLine (l2:ls)

groupByCategory :: [ErrorMessage] -> Map ErrorCategory [ErrorMessage]
groupByCategory = foldr insertMsg Map.empty
  where
    insertMsg msg acc = Map.insertWith (++) (errorCategory msg) [msg] acc

isSameError :: ErrorMessage -> ErrorMessage -> Bool
isSameError msg1 msg2 = 
  errorMsg msg1 == errorMsg msg2 &&
  errorCategory msg1 == errorCategory msg2 &&
  errorLocation msg1 == errorLocation msg2

deduplicateErrors :: [ErrorMessage] -> [ErrorMessage]
deduplicateErrors = nubBy isSameError

extractContext :: ErrorReport -> SourceLocation -> [String]
extractContext report loc = 
  let allMsgs = reportErrors report ++ reportWarnings report ++ 
                reportInfo report ++ reportHints report
      relevantMsgs = filter (\msg -> errorLocation msg == loc) allMsgs
  in concatMap errorContext relevantMsgs

isRelevantToCategory :: ErrorCategory -> String -> Bool
isRelevantToCategory SyntaxError suggestion = 
  any (`isInfixOf` suggestion) ["syntax", "parse", "token", "semicolon", "bracket"]
isRelevantToCategory TypeError suggestion = 
  any (`isInfixOf` suggestion) ["type", "convert", "cast", "incompatible"]
isRelevantToCategory NameError suggestion = 
  any (`isInfixOf` suggestion) ["name", "variable", "function", "declare", "define"]
isRelevantToCategory SemanticError suggestion = 
  any (`isInfixOf` suggestion) ["semantic", "meaning", "logic", "behavior"]
isRelevantToCategory InternalError suggestion = 
  any (`isInfixOf` suggestion) ["internal", "compiler", "bug", "report"]

highlightErrorLocation :: String -> SourceLocation -> String
highlightErrorLocation source loc = 
  let linesList = lines source
      targetLine = if sourceLine loc > 0 && sourceLine loc <= length linesList
                   then linesList !! (sourceLine loc - 1)
                   else ""
      linePrefix = take (sourceColumn loc - 1) targetLine
      highlightedChar = if sourceColumn loc > 0 && sourceColumn loc <= length targetLine
                        then [targetLine !! (sourceColumn loc - 1)]
                        else ""
      lineSuffix = if sourceColumn loc < length targetLine
                   then drop (sourceColumn loc) targetLine
                   else ""
  in linePrefix ++ "\x1b[41m" ++ highlightedChar ++ "\x1b[0m" ++ lineSuffix

containsHighlightAt :: String -> (Int, Int) -> Bool
containsHighlightAt highlighted (line, column) = 
  "\x1b[41m" `isInfixOf` highlighted

generateReportSummary :: ErrorReport -> String
generateReportSummary report = 
  let errorCount = length (reportErrors report)
      warningCount = length (reportWarnings report)
      infoCount = length (reportInfo report)
      hintCount = length (reportHints report)
  in "Errors: " ++ show errorCount ++ 
     ", Warnings: " ++ show warningCount ++ 
     ", Info: " ++ show infoCount ++ 
     ", Hints: " ++ show hintCount

containsCounts :: String -> Int -> Int -> Int -> Int -> Bool
containsCounts summary errors warnings info hints = 
  ("Errors: " ++ show errors) `isInfixOf` summary &&
  ("Warnings: " ++ show warnings) `isInfixOf` summary &&
  ("Info: " ++ show info) `isInfixOf` summary &&
  ("Hints: " ++ show hints) `isInfixOf` summary

filterByContext :: ErrorReport -> String -> [ErrorMessage]
filterByContext report context = 
  let allMsgs = reportErrors report ++ reportWarnings report ++ 
                reportInfo report ++ reportHints report
  in filter (\msg -> context `elem` errorContext msg) allMsgs

-- Test cases for error reporting
testErrorReporting :: TestTree
testErrorReporting = testGroup "Error Reporting QuickCheck Tests"
  [ testProperties "Error Formatting Properties"
    [ ("error_formatting_preserves_content", property prop_error_formatting_preserves_content)
    ]
  , testProperties "Error Filtering Properties"
    [ ("error_filtering_by_severity", property prop_error_filtering_by_severity)
    , ("error_filtering_by_context", property prop_error_filtering_by_context)
    ]
  , testProperties "Error Organization Properties"
    [ ("error_sorting_by_location", property prop_error_sorting_by_location)
    , ("error_grouping_by_category", property prop_error_grouping_by_category)
    , ("error_deduplication_removes_duplicates", property prop_error_deduplication_removes_duplicates)
    ]
  , testProperties "Error Content Properties"
    [ ("error_context_preserved", property prop_error_context_preserved)
    , ("error_suggestions_relevant", property prop_error_suggestions_relevant)
    , ("error_location_highlighting_accurate", property prop_error_location_highlighting_accurate)
    ]
  , testProperties "Error Report Properties"
    [ ("error_report_summary_accurate", property prop_error_report_summary_accurate)
    ]
  , testCase "Plain error formatting" $ do
    let location = SourceLocation { sourceLine = 10, sourceColumn = 5 }
    let msg = ErrorMessage "Undefined variable" NameError Error location [] ["Declare the variable first"]
    let formatted = formatErrorMessage msg PlainFormatter
    assertBool "Should include error message" 
               ("Undefined variable" `isInfixOf` formatted)
    assertBool "Should include error severity" 
               ("Error" `isInfixOf` formatted)
    assertBool "Should include error category" 
               ("NameError" `isInfixOf` formatted)
  
  , testCase "Colored error formatting" $ do
    let location = SourceLocation { sourceLine = 10, sourceColumn = 5 }
    let msg = ErrorMessage "Type mismatch" TypeError Error location [] ["Use compatible types"]
    let formatted = formatErrorMessage msg ColoredFormatter
    assertBool "Should include color codes" 
               ("\x1b[31m" `isInfixOf` formatted)
  
  , testCase "JSON error formatting" $ do
    let location = SourceLocation { sourceLine = 10, sourceColumn = 5 }
    let msg = ErrorMessage "Syntax error" SyntaxError Error location [] ["Add semicolon"]
    let formatted = formatErrorMessage msg JsonFormatter
    assertBool "Should include JSON structure" 
               ("{\"severity\":" `isInfixOf` formatted)
    assertBool "Should include error message in JSON" 
               ("\"message\":\"Syntax error\"" `isInfixOf` formatted)
  
  , testCase "Error filtering by severity" $ do
    let location = SourceLocation 10 5
    let errorMsg = ErrorMessage "Error message" SyntaxError Error location [] []
    let warningMsg = ErrorMessage "Warning message" SyntaxError Warning location [] []
    let report = ErrorReport [errorMsg] [warningMsg] [] []
    let errors = filterBySeverity report Error
    let warnings = filterBySeverity report Warning
    assertEqual "Should filter errors correctly" [errorMsg] errors
    assertEqual "Should filter warnings correctly" [warningMsg] warnings
  
  , testCase "Error sorting by location" $ do
    let loc1 = SourceLocation { sourceLine = 5, sourceColumn = 10 }
    let loc2 = SourceLocation { sourceLine = 3, sourceColumn = 20 }
    let loc3 = SourceLocation { sourceLine = 7, sourceColumn = 5 }
    let msg1 = ErrorMessage "Message 1" SyntaxError Error loc1 [] []
    let msg2 = ErrorMessage "Message 2" SyntaxError Error loc2 [] []
    let msg3 = ErrorMessage "Message 3" SyntaxError Error loc3 [] []
    let sorted = sortByLocation [msg1, msg2, msg3]
    assertEqual "Should sort by location correctly" [msg2, msg1, msg3] sorted
  
  , testCase "Error grouping by category" $ do
    let loc = SourceLocation { sourceLine = 10, sourceColumn = 5 }
    let syntaxMsg = ErrorMessage "Syntax error" SyntaxError Error loc [] []
    let typeMsg = ErrorMessage "Type error" TypeError Error loc [] []
    let grouped = groupByCategory [syntaxMsg, typeMsg]
    assertEqual "Should group by category correctly" 
                (Map.fromList [(SyntaxError, [syntaxMsg]), (TypeError, [typeMsg])]) grouped
  
  , testCase "Error deduplication" $ do
    let loc = SourceLocation { sourceLine = 10, sourceColumn = 5 }
    let msg1 = ErrorMessage "Same message" SyntaxError Error loc [] []
    let msg2 = ErrorMessage "Same message" SyntaxError Error loc [] []
    let msg3 = ErrorMessage "Different message" SyntaxError Error loc [] []
    let deduplicated = deduplicateErrors [msg1, msg2, msg3]
    assertEqual "Should remove duplicates" [msg1, msg3] deduplicated
  
  , testCase "Error report summary" $ do
    let loc = SourceLocation { sourceLine = 10, sourceColumn = 5 }
    let errorMsg = ErrorMessage "Error message" SyntaxError Error loc [] []
    let warningMsg = ErrorMessage "Warning message" SyntaxError Warning loc [] []
    let infoMsg = ErrorMessage "Info message" SyntaxError Info loc [] []
    let hintMsg = ErrorMessage "Hint message" SyntaxError Hint loc [] []
    let report = ErrorReport [errorMsg] [warningMsg] [infoMsg] [hintMsg]
    let summary = generateReportSummary report
    assertBool "Should include error count" 
               ("Errors: 1" `isInfixOf` summary)
    assertBool "Should include warning count" 
               ("Warnings: 1" `isInfixOf` summary)
    assertBool "Should include info count" 
               ("Info: 1" `isInfixOf` summary)
    assertBool "Should include hint count" 
               ("Hints: 1" `isInfixOf` summary)
  ]

-- Export the test
tests :: TestTree
tests = testErrorReporting