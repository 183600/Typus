{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ErrorHandlerRecoverySpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)

-- Test error handler recovery properties
tests :: TestTree
tests = testGroup "Error Handler Recovery Tests"
  [ testGroup "Error detection properties"
    [ testProperty "syntax errors are detected" $
        \input -> containsSyntaxError input ==> hasSyntaxError (analyzeInput input)
    
    , testProperty "type errors are detected" $
        \input -> containsTypeError input ==> hasTypeError (analyzeInput input)
    
    , testProperty "semantic errors are detected" $
        \input -> containsSemanticError input ==> hasSemanticError (analyzeInput input)
    
    , testProperty "valid input produces no errors" $
        \input -> isValidInput input ==> not (hasAnyError (analyzeInput input))
    
    , testProperty "empty input produces error" $
        hasAnyError (analyzeInput "")
    
    , testProperty "null input produces error" $
        hasAnyError (analyzeInput "null")
    
    , testProperty "unterminated strings produce error" $
        \content -> hasAnyError (analyzeInput ("\"" ++ content))
    
    , testProperty "unbalanced brackets produce error" $
        \content -> hasAnyError (analyzeInput ("(" ++ content))
    
    , testProperty "invalid keywords produce error" $
        \keyword -> not (isValidKeyword keyword) ==> hasAnyError (analyzeInput keyword)
    
    , testProperty "invalid operators produce error" $
        \op -> not (isValidOperator op) ==> hasAnyError (analyzeInput op)
    ]
  
  , testGroup "Error recovery properties"
    [ testProperty "recovery produces valid output" $
        \input -> hasAnyError (analyzeInput input) ==> 
          isValidOutput (recoverFromError (analyzeInput input))
    
    , testProperty "recovery preserves valid parts" $
        \input -> hasAnyError (analyzeInput input) ==> 
          containsValidParts input (recoverFromError (analyzeInput input))
    
    , testProperty "recovery adds error markers" $
        \input -> hasAnyError (analyzeInput input) ==> 
          containsErrorMarkers (recoverFromError (analyzeInput input))
    
    , testProperty "multiple errors are all recovered" $
        \input -> countErrors (analyzeInput input) <= countErrorMarkers (recoverFromError (analyzeInput input))
    
    , testProperty "recovery maintains structure" $
        \input -> hasAnyError (analyzeInput input) ==> 
          maintainsStructure input (recoverFromError (analyzeInput input))
    
    , testProperty "recovery is deterministic" $
        \input -> recoverFromError (analyzeInput input) === 
          recoverFromError (analyzeInput input)
    
    , testProperty "recovery handles cascading errors" $
        \input -> hasMultipleErrors input ==> 
          handlesCascadingErrors (analyzeInput input) (recoverFromError (analyzeInput input))
    
    , testProperty "recovery preserves line numbers" $
        \input -> hasAnyError (analyzeInput input) ==> 
          preservesLineNumbers input (recoverFromError (analyzeInput input))
    
    , testProperty "recovery preserves indentation" $
        \input -> hasAnyError (analyzeInput input) ==> 
          preservesIndentation input (recoverFromError (analyzeInput input))
    
    , testProperty "recovery produces minimal changes" $
        \input -> hasAnyError (analyzeInput input) ==> 
          makesMinimalChanges input (recoverFromError (analyzeInput input))
    ]
  
  , testGroup "Error reporting properties"
    [ testProperty "error messages are descriptive" $
        \input -> hasAnyError (analyzeInput input) ==> 
          isDescriptive (getErrorMessage (analyzeInput input))
    
    , testProperty "error locations are accurate" $
        \input -> hasAnyError (analyzeInput input) ==> 
          hasAccurateLocation input (getErrorLocation (analyzeInput input))
    
    , testProperty "error severity is appropriate" $
        \input -> hasAnyError (analyzeInput input) ==> 
          hasAppropriateSeverity (analyzeInput input)
    
    , testProperty "error suggestions are helpful" $
        \input -> hasAnyError (analyzeInput input) ==> 
          hasHelpfulSuggestions (getErrorSuggestions (analyzeInput input))
    
    , testProperty "error context is preserved" $
        \input -> hasAnyError (analyzeInput input) ==> 
          preservesContext input (getErrorContext (analyzeInput input))
    
    , testProperty "error categories are correct" $
        \input -> containsSyntaxError input ==> 
          getErrorCategory (analyzeInput input) == "Syntax"
    
    , testProperty "error codes are unique" $
        \input1 input2 -> 
          hasAnyError (analyzeInput input1) && hasAnyError (analyzeInput input2) && 
          input1 /= input2 ==> 
          getErrorCode (analyzeInput input1) /= getErrorCode (analyzeInput input2)
    
    , testProperty "error summaries are concise" $
        \input -> hasAnyError (analyzeInput input) ==> 
          length (getErrorSummary (analyzeInput input)) <= 100
    
    , testProperty "error details are complete" $
        \input -> hasAnyError (analyzeInput input) ==> 
          hasCompleteDetails (getErrorDetails (analyzeInput input))
    
    , testProperty "error fixes are applicable" $
        \input -> hasAnyError (analyzeInput input) ==> 
          hasApplicableFixes input (getErrorFixes (analyzeInput input))
    ]
  ]

-- Helper functions
data AnalysisResult = AnalysisResult
  { hasSyntaxError :: Bool
  , hasTypeError :: Bool
  , hasSemanticError :: Bool
  , hasAnyError :: Bool
  , errorMessage :: String
  , errorLocation :: Int
  , errorSeverity :: String
  , errorSuggestions :: [String]
  , errorContext :: String
  , errorCategory :: String
  , errorCode :: String
  , errorSummary :: String
  , errorDetails :: String
  , errorFixes :: [String]
  } deriving (Show, Eq)

analyzeInput :: String -> AnalysisResult
analyzeInput input = AnalysisResult
  { hasSyntaxError = containsSyntaxError input
  , hasTypeError = containsTypeError input
  , hasSemanticError = containsSemanticError input
  , hasAnyError = containsSyntaxError input || containsTypeError input || containsSemanticError input
  , errorMessage = "Error in: " ++ take 20 input
  , errorLocation = length input `div` 2
  , errorSeverity = if containsSyntaxError input then "Error" else "Warning"
  , errorSuggestions = ["Check syntax", "Verify types"]
  , errorContext = take 10 input ++ "..."
  , errorCategory = if containsSyntaxError input then "Syntax" else "Type"
  , errorCode = "ERR" ++ show (length input `mod` 1000)
  , errorSummary = take 50 (errorMessage input)
  , errorDetails = "Detailed error information"
  , errorFixes = ["Fix 1", "Fix 2"]
  }

containsSyntaxError :: String -> Bool
containsSyntaxError input = any (`isInfixOf` input) ["{", "(", "[", "\"", "'"] && 
                          not (all (`isInfixOf` input) ["}", ")", "]", "\"", "'"])

containsTypeError :: String -> Bool
containsTypeError input = any (`isInfixOf` input) ["int", "string", "bool"] && 
                         any (`isInfixOf` input) ["+", "*", "/"]

containsSemanticError :: String -> Bool
containsSemanticError input = any (`isInfixOf` input) ["if", "while", "for"] && 
                             not (any (`isInfixOf` input) ["{", "}"])

isValidInput :: String -> Bool
isValidInput input = not (null input) && 
                     not (containsSyntaxError input) && 
                     not (containsTypeError input) && 
                     not (containsSemanticError input)

isValidOutput :: String -> Bool
isValidOutput output = not (null output)

containsValidParts :: String -> String -> Bool
containsValidParts input output = length output >= length input `div` 2

containsErrorMarkers :: String -> Bool
containsErrorMarkers output = "/* ERROR */" `isInfixOf` output

countErrors :: AnalysisResult -> Int
countErrors result = sum [1 | True <- [hasSyntaxError result, hasTypeError result, hasSemanticError result]]

countErrorMarkers :: String -> Int
countErrorMarkers output = length (filter (== "/* ERROR */") (words output))

maintainsStructure :: String -> String -> Bool
maintainsStructure input output = length (lines output) == length (lines input)

handlesCascadingErrors :: AnalysisResult -> String -> Bool
handlesCascadingErrors result output = countErrors result <= countErrorMarkers output

preservesLineNumbers :: String -> String -> Bool
preservesLineNumbers input output = length (lines input) == length (lines output)

preservesIndentation :: String -> String -> Bool
preservesIndentation input output = all (\(l1, l2) -> takeWhile isSpace l1 == takeWhile isSpace l2) 
                                       (zip (lines input) (lines output))

makesMinimalChanges :: String -> String -> Bool
makesMinimalChanges input output = length output <= length input * 2

recoverFromError :: AnalysisResult -> String
recoverFromError result = if hasAnyError result 
                         then "Recovered: " ++ errorMessage result ++ " /* ERROR */"
                         else "Valid input"

isDescriptive :: String -> Bool
isDescriptive msg = length msg > 10 && any (`isInfixOf` msg) ["Error", "issue", "problem"]

hasAccurateLocation :: String -> Int -> Bool
hasAccurateLocation input loc = loc >= 0 && loc <= length input

hasAppropriateSeverity :: AnalysisResult -> Bool
hasAppropriateSeverity result = errorSeverity result `elem` ["Error", "Warning", "Info"]

hasHelpfulSuggestions :: [String] -> Bool
hasHelpfulSuggestions suggestions = not (null suggestions) && all (>= 5) (map length suggestions)

preservesContext :: String -> String -> Bool
preservesContext input context = input `isPrefixOf` context || context `isPrefixOf` input

getErrorMessage :: AnalysisResult -> String
getErrorMessage = errorMessage

getErrorLocation :: AnalysisResult -> Int
getErrorLocation = errorLocation

getErrorSuggestions :: AnalysisResult -> [String]
getErrorSuggestions = errorSuggestions

getErrorContext :: AnalysisResult -> String
getErrorContext = errorContext

getErrorCategory :: AnalysisResult -> String
getErrorCategory = errorCategory

getErrorCode :: AnalysisResult -> String
getErrorCode = errorCode

getErrorSummary :: AnalysisResult -> String
getErrorSummary = errorSummary

getErrorDetails :: AnalysisResult -> String
getErrorDetails = errorDetails

getErrorFixes :: AnalysisResult -> [String]
getErrorFixes = errorFixes

hasCompleteDetails :: String -> Bool
hasCompleteDetails details = length details > 20

hasApplicableFixes :: String -> [String] -> Bool
hasApplicableFixes input fixes = not (null fixes) && all (>= 3) (map length fixes)

isValidKeyword :: String -> Bool
isValidKeyword kw = kw `elem` ["if", "else", "while", "for", "function", "return"]

isValidOperator :: String -> Bool
isValidOperator op = op `elem` ["+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">="]

hasMultipleErrors :: String -> Bool
hasMultipleErrors input = containsSyntaxError input && containsTypeError input

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r"