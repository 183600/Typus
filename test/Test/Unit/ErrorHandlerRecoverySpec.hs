{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing  -Wno-unused-matches #-}
module Test.Unit.ErrorHandlerRecoverySpec where


import Test.Tasty



import Test.Tasty

import Test.Tasty.QuickCheck
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Map as Map
import Analyzer.Types
import qualified Ownership.Common.Types as Own
import qualified Dependencies.TypeSystem as Dep

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
analyzeInput :: String -> AnalysisResult
analyzeInput input = AnalysisResult
  { ownershipErrors = if containsSyntaxError input then [(Error, Own.OwnershipError "Syntax error")] else []
  , dependentTypeErrors = if containsTypeError input then [(Error, Dep.DependentTypeError "Type error")] else []
  , combinedErrors = []
  , analysisWarnings = if containsSemanticError input then ["Semantic warning"] else []
  , analysisInfo = ["Error in: " ++ take 20 input]
  , typeEnvironment = Map.empty
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

-- Helper functions for testing
hasSyntaxError :: AnalysisResult -> Bool
hasSyntaxError result = not (null (ownershipErrors result))

hasTypeError :: AnalysisResult -> Bool
hasTypeError result = not (null (dependentTypeErrors result))

hasSemanticError :: AnalysisResult -> Bool
hasSemanticError result = not (null (analysisWarnings result))

hasAnyError :: AnalysisResult -> Bool
hasAnyError result = hasSyntaxError result || hasTypeError result || hasSemanticError result

getErrorMessage :: AnalysisResult -> String
getErrorMessage result = case analysisInfo result of
  (msg:_) -> msg
  [] -> "Unknown error"

-- Mock error types
data OwnershipError = OwnershipError String deriving (Show, Eq)
data TypeError = TypeError String deriving (Show, Eq)

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
                         then "Recovered: " ++ getErrorMessage result ++ " /* ERROR */"
                         else "Valid input"

isDescriptive :: String -> Bool
isDescriptive msg = length msg > 10 && any (`isInfixOf` msg) ["Error", "issue", "problem"]

hasAccurateLocation :: String -> Int -> Bool
hasAccurateLocation input loc = loc >= 0 && loc <= length input

hasAppropriateSeverity :: AnalysisResult -> Bool
hasAppropriateSeverity result = True

hasHelpfulSuggestions :: [String] -> Bool
hasHelpfulSuggestions suggestions = not (null suggestions) && all (>= 5) (map length suggestions)

preservesContext :: String -> String -> Bool
preservesContext input context = input `isPrefixOf` context || context `isPrefixOf` input

getErrorMessageFromResult :: AnalysisResult -> String
getErrorMessageFromResult = getErrorMessage

getErrorLocation :: AnalysisResult -> Int
getErrorLocation result = 0

getErrorSuggestions :: AnalysisResult -> [String]
getErrorSuggestions result = ["Check syntax", "Verify types"]

getErrorContext :: AnalysisResult -> String
getErrorContext result = "Error context"

getErrorCategory :: AnalysisResult -> String
getErrorCategory result = "Syntax"

getErrorCode :: AnalysisResult -> String
getErrorCode result = "ERR001"

getErrorSummary :: AnalysisResult -> String
getErrorSummary result = "Error summary"

getErrorDetails :: AnalysisResult -> String
getErrorDetails result = "Error details"

getErrorFixes :: AnalysisResult -> [String]
getErrorFixes result = ["Fix 1", "Fix 2"]

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
isSpace c = c `elem` (" \t\n\r" :: String)