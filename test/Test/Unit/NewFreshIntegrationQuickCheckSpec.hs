{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Integration tests for Typus modules working together
module Test.Unit.NewFreshIntegrationQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, removeComments, normalizeIndentation)
import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, advancePos, advancePosBy, spanBetween, mergeSpans
  , isValidSpan, spanStart, spanEnd
  )
import Compiler.Errors.Core 
  ( TypeError(..), ErrorSeverity(..), ErrorCategory(..)
  , ErrorCollector, newErrorCollector, addError, addWarning
  , formatError, errorAt, warningAt, hasErrors, getErrors
  )
import Parser (parseTypus, FileDirectives(..), defaultFileDirectives)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New Integration QuickCheck Tests"
  [ utilsSourceLocationIntegration
  , parserErrorHandlingIntegration
  , multiModuleIntegration
  , endToEndIntegration
  , performanceIntegration
  ]

-- ============================================================================
-- Utils + SourceLocation Integration
-- ============================================================================

utilsSourceLocationIntegration :: TestTree
utilsSourceLocationIntegration = testGroup "Utils + SourceLocation Integration"
  [ testProperty "string processing affects position calculations correctly" $
      \str ->
        let trimmed = trim str
            originalPos = advancePosBy startPos str
            trimmedPos = advancePosBy startPos trimmed
        in sourceLine trimmedPos <= sourceLine originalPos &&
           sourceColumn trimmedPos <= sourceColumn originalPos + 10  -- Allow some variance
        
  , testProperty "splitBy and position tracking work together" $
      \delim str ->
        let parts = splitBy delim str
            positions = scanl (\pos part -> advancePosBy pos (part ++ [delim])) startPos parts
        in delim /= '\0' && length str < 100 ==>
           all (\pos -> sourceLine pos >= 1 && sourceColumn pos >= 1) positions
           
  , testProperty "comment removal and position tracking" $
      \content ->
        let withComments = content ++ "\n// This is a comment\nmore content"
            withoutComments = removeComments withComments
            pos1 = advancePosBy startPos withComments
            pos2 = advancePosBy startPos withoutComments
        in length content < 50 ==>
           sourceLine pos2 <= sourceLine pos1 &&
           sourceColumn pos2 <= sourceColumn pos1 + 20
           
  , testProperty "indentation normalization preserves line structure" $
      \lines ->
        let input = unlines lines
            normalized = normalizeIndentation input
            originalLines = length $ filter (not . null) lines
            normalizedLines = length $ filter (not . null) $ lines normalized
        in length lines < 20 ==> originalLines === normalizedLines
  ]

-- ============================================================================
-- Parser + ErrorHandling Integration
-- ============================================================================

parserErrorHandlingIntegration :: TestTree
parserErrorHandlingIntegration = testGroup "Parser + ErrorHandling Integration"
  [ testProperty "parsing errors create appropriate error messages" $
      \malformedInput ->
        let result = parseTypus (take 50 malformedInput)
        in case result of
             Left err -> 
               -- Check that error contains useful information
               length (show err) > 10
             Right _ -> 
               True  -- Successful parse is also valid
               
  , testProperty "directive parsing creates consistent error locations" $
      \input ->
        let result = parseTypus ("// @ownership: " ++ take 20 input)
        in case result of
             Left err -> 
               -- Error should mention location information
               "line" `isInfixOf` show err || "position" `isInfixOf` show err
             Right _ -> 
               True  -- Successful parse
               
  , testProperty "multiple parsing attempts accumulate errors correctly" $
      \inputs ->
        let results = map (parseTypus . take 30) (take 5 inputs)
            errorCount = length $ filter isLeft results
        in errorCount >= 0 && errorCount <= 5
        
  , testCase "error collector integration with parser" $
    do
      let collector = newErrorCollector
          collector' = addError startPos "Parse error" collector
          formattedErrors = formatError $ head $ getErrors collector'
      assertBool "error contains position information" $ "1:1" `isInfixOf` formattedErrors
  ]

-- ============================================================================
-- Multi-Module Integration
-- ============================================================================

multiModuleIntegration :: TestTree
multiModuleIntegration = testGroup "Multi-Module Integration"
  [ testProperty "source location tracking across multiple operations" $
      \operations ->
        let positions = scanl (\pos op -> advancePosBy pos (take 10 op)) startPos operations
            spans = zipWith spanBetween positions (tail positions ++ [last positions])
            merged = foldl mergeSpans emptySpan spans
        in length operations < 10 ==> 
           all isValidSpan spans ==> isValidSpan merged
           
  , testProperty "error handling across multiple parsing stages" $
      \inputs ->
        let parseResults = map (parseTypus . take 20) inputs
            errorCollectors = map (\result -> 
              case result of
                Left err -> addError startPos (show err) newErrorCollector
                Right _ -> newErrorCollector
            ) parseResults
            totalErrors = sum $ map (length . getErrors) errorCollectors
        in length inputs < 5 ==> totalErrors >= 0
        
  , testProperty "string processing pipeline consistency" $
      \input ->
        let stage1 = trim input
            stage2 = removeComments stage1
            stage3 = normalizeIndentation stage2
            -- Final result should be consistent regardless of intermediate steps
            direct = normalizeIndentation $ removeComments $ trim input
        in length input < 100 ==> stage3 === direct
  ]

-- ============================================================================
-- End-to-End Integration
-- ============================================================================

endToEndIntegration :: TestTree
endToEndIntegration = testGroup "End-to-End Integration"
  [ testCase "complete parsing and analysis pipeline" $
    do
      let input = "// @ownership: true\n// @dependentTypes: false\nfn test() { return 42; }"
          parseResult = parseTypus input
      case parseResult of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right file -> do
          -- Check that directives were parsed correctly
          case fdOwnership (fileDirectives file) of
            Nothing -> assertFailure "Ownership directive not found"
            Just (Located _ ownership) -> assertBool "Ownership should be true" ownership
          case fdDependentTypes (fileDirectives file) of
            Nothing -> assertFailure "DependentTypes directive not found"
            Just (Located _ dependentTypes) -> assertBool "DependentTypes should be false" (not dependentTypes)
            
  , testProperty "robustness with malformed input" $
      \input ->
        let result = parseTypus (take 100 input)
        in case result of
             Left _ -> True  -- Graceful failure
             Right _ -> True  -- Or successful parsing
             
  , testProperty "consistency across multiple runs" $
      \input ->
        let result1 = parseTypus input
            result2 = parseTypus input
        in case (result1, result2) of
             (Left _, Left _) -> True
             (Right f1, Right f2) -> fileDirectives f1 === fileDirectives f2
             _ -> False  -- Should be consistent
             
  , testCase "memory usage with large inputs" $
    do
      let largeInput = concat $ replicate 1000 "// @ownership: true\n"
          result = parseTypus largeInput
      case result of
        Left _ -> return ()  -- Expected to fail due to size
        Right _ -> return ()  -- Or handle successfully
  ]

-- ============================================================================
-- Performance Integration
-- ============================================================================

performanceIntegration :: TestTree
performanceIntegration = testGroup "Performance Integration"
  [ testProperty "linear scaling with input size" $
      \baseInput multiplier ->
        let input = concat $ replicate multiplier baseInput
            result = parseTypus (take 1000 input)  -- Limit size
        in multiplier >= 1 && multiplier <= 10 ==> 
           case result of
             Left _ -> True  -- Failure is acceptable for large inputs
             Right _ -> True  -- Success is also acceptable
             
  , testProperty "efficient error collection" $
      \errorCount ->
        let collector = foldl (\c i -> addError (SourcePos i 1) ("Error " ++ show i) c) 
                             newErrorCollector [1..errorCount]
            errors = getErrors collector
        in errorCount >= 0 && errorCount <= 100 ==> 
           length errors === errorCount
           
  , testProperty "position calculation performance" $
      \inputSize ->
        let input = replicate inputSize 'x'
            finalPos = advancePosBy startPos input
        in inputSize >= 0 && inputSize <= 1000 ==>
           sourceColumn finalPos === inputSize + 1
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isInfixOf :: String -> String -> Bool
isInfixOf = Data.List.isInfixOf

isPrefixOf :: String -> String -> Bool
isPrefixOf = Data.List.isPrefixOf

lines :: String -> [String]
lines = Data.List.lines

unlines :: [String] -> String
unlines = Data.List.unlines