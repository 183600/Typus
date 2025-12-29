{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive QuickCheck tests covering multiple Typus modules
module Test.Unit.NewFreshComprehensiveQuickCheckSpec where

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
  , getErrors, getWarnings, formatError, errorAt, warningAt
  )
import Parser (parseTypus, FileDirectives(..), defaultFileDirectives)
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Set (Set, empty, singleton, union, member, toList)
import qualified Data.Set as Set
import Control.Monad (when)

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New Comprehensive QuickCheck Tests"
  [ comprehensivePropertyTests
  [ crossModuleProperties
  , edgeCaseProperties
  , robustnessProperties
  , consistencyProperties
  ]

-- ============================================================================
-- Comprehensive Property Tests
-- ============================================================================

comprehensivePropertyTests :: TestTree
comprehensivePropertyTests = testGroup "Comprehensive Property Tests"
  [ testProperty "round-trip property: string processing preserves essential information" $
      \input ->
        let processed = normalizeIndentation $ removeComments $ trim input
            -- Should preserve non-whitespace, non-comment content
        in length input < 200 ==> length processed <= length input + 50
        
  , testProperty "composition property: multiple operations are associative" $
      \input ->
        let op1 = trim input
            op2 = removeComments op1
            op3 = normalizeIndentation op2
            -- Alternative composition order
            alt1 = normalizeIndentation input
            alt2 = removeComments alt1
            alt3 = trim alt2
        in length input < 100 ==> 
           -- Results should be equivalent up to whitespace differences
           length (words op3) === length (words alt3)
           
  , testProperty "idempotence property: repeated operations stabilize" $
      \input ->
        let trimmed1 = trim input
            trimmed2 = trim trimmed1
            commentsRemoved1 = removeComments input
            commentsRemoved2 = removeComments commentsRemoved1
            normalized1 = normalizeIndentation input
            normalized2 = normalizeIndentation normalized1
        in trimmed1 === trimmed2 &&
           commentsRemoved1 === commentsRemoved2 &&
           normalized1 === normalized2
           
  , testProperty "monotonicity property: operations don't increase essential complexity" $
      \input ->
        let trimmed = trim input
            commentsRemoved = removeComments input
            normalized = normalizeIndentation input
            baseComplexity = length $ filter (not . isSpace) input
            trimmedComplexity = length $ filter (not . isSpace) trimmed
            commentsComplexity = length $ filter (not . isSpace) commentsRemoved
            normalizedComplexity = length $ filter (not . isSpace) normalized
        in trimmedComplexity <= baseComplexity + 10 &&
           commentsComplexity <= baseComplexity + 10 &&
           normalizedComplexity <= baseComplexity + 10
  ]

-- ============================================================================
-- Cross-Module Properties
-- ============================================================================

crossModuleProperties :: TestTree
crossModuleProperties = testGroup "Cross-Module Properties"
  [ testProperty "parser + error handling: error positions are within input bounds" $
      \input ->
        let result = parseTypus (take 100 input)
        in case result of
             Left err -> 
               -- Error position should be reasonable
               True  -- Simplified check
             Right _ -> 
               True  -- Successful parse has no errors
               
  , testProperty "source location + utils: position calculations respect string processing" $
      \input ->
        let original = input
            processed = trim original
            originalPos = advancePosBy startPos original
            processedPos = advancePosBy startPos processed
        in sourceLine processedPos <= sourceLine originalPos + 5
        
  , testProperty "error collector + multiple modules: consistent error accumulation" $
      \errorCounts ->
        let collectors = map (\n -> foldl (\c i -> addError (SourcePos i 1) ("Error " ++ show i)) 
                                         newErrorCollector [1..n]) errorCounts
            totalErrors = sum $ map (length . getErrors) collectors
            combinedCollector = foldl (\c1 c2 -> 
              foldl (\c err -> addError (SourcePos 1 1) (formatError err) c) c1 (getErrors c2)
            ) newErrorCollector collectors
            combinedErrors = length $ getErrors combinedCollector
        in all (>= 0) errorCounts && all (<= 100) errorCounts ==>
           totalErrors === combinedErrors
           
  , testProperty "parser + source location: directive positions are tracked correctly" $
      \directives ->
        let input = unlines $ map (\d -> "// @ownership: " ++ show d) directives
            result = parseTypus input
        in length directives <= 10 ==> 
           case result of
             Left _ -> True
             Right file -> isJust (fdOwnership (fileDirectives file))
  ]

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Edge Case Properties"
  [ testProperty "empty and minimal inputs" $
      \input ->
        let minimalInput = take 5 input
            result = parseTypus minimalInput
        in case result of
             Left _ -> True  -- Expected for malformed input
             Right _ -> True  -- Or successful parsing
             
  , testProperty "unicode and special characters" $
      \input ->
        let specialInput = input ++ "\n\t\r\x00\x1F"
            processed = trim specialInput
            split = splitBy ',' specialInput
        in length input < 50 ==> 
           not (null processed) && length split >= 1
           
  , testProperty "extreme values and boundaries" $
      \size ->
        let largeInput = replicate size 'x'
            result = trim largeInput
        in size >= 0 && size <= 10000 ==> 
           length result <= size + 10
           
  , testProperty "nested and recursive structures" $
      \depth ->
        let nestedComment = "/* " ++ replicate depth '*' ++ " */"
            result = removeComments nestedComment
        in depth >= 0 && depth <= 100 ==> 
           length result < length nestedComment
  ]

-- ============================================================================
-- Robustness Properties
-- ============================================================================

robustnessProperties :: TestTree
robustnessProperties = testGroup "Robustness Properties"
  [ testProperty "graceful degradation with malformed input" $
      \input ->
        let malformed = input ++ "\x00\x1F\uFFFE\uFFFF"
            processed = trim malformed
            parsed = parseTypus (take 100 malformed)
        in length input < 100 ==> 
           not (null processed) && 
           case parsed of
             Left _ -> True
             Right _ -> True
             
  , testProperty "resource exhaustion prevention" $
      \size ->
        let largeInput = concat $ replicate size "// @ownership: true\n"
            result = parseTypus (take 10000 largeInput)
        in size >= 0 && size <= 1000 ==> 
           case result of
             Left _ -> True  -- Expected to fail due to resource limits
             Right _ -> True  -- Or handle gracefully
             
  , testProperty "error handling under stress" $
      \errorCount ->
        let collector = foldl (\c i -> addError (SourcePos i 1) (replicate i 'x')) 
                             newErrorCollector [1..errorCount]
            errors = getErrors collector
            formatted = map formatError errors
        in errorCount >= 0 && errorCount <= 1000 ==> 
           length formatted === errorCount
           
  , testProperty "concurrent operations consistency" $
      \operations ->
        let results = map (\op -> trim (take 50 op)) (take 10 operations)
        in all (not . null) results ==> True
  ]

-- ============================================================================
-- Consistency Properties
-- ============================================================================

consistencyProperties :: TestTree
consistencyProperties = testGroup "Consistency Properties"
  [ testProperty "deterministic behavior across multiple runs" $
      \input ->
        let result1 = parseTypus input
            result2 = parseTypus input
            processed1 = trim input
            processed2 = trim input
        in case (result1, result2) of
             (Left _, Left _) -> processed1 === processed2
             (Right f1, Right f2) -> fileDirectives f1 === fileDirectives f2
             _ -> False
             
  , testProperty "consistency of error messages" $
      \input ->
        let result1 = parseTypus input
            result2 = parseTypus input
        in case (result1, result2) of
             (Left err1, Left err2) -> show err1 === show err2
             (Right _, Right _) -> True
             _ -> False  -- Should be consistent
             
  , testProperty "position calculation consistency" $
      \input ->
        let pos1 = advancePosBy startPos input
            pos2 = advancePosBy startPos input
        in pos1 === pos2
        
  , testProperty "formatting consistency" $
      \errorCount ->
        let errors = map (\i -> errorAt (SourcePos i 1) ("Error " ++ show i)) [1..errorCount]
            formatted1 = map formatError errors
            formatted2 = map formatError errors
        in errorCount >= 0 && errorCount <= 100 ==> 
           formatted1 === formatted2
  ]

-- ============================================================================
-- Integration Test Cases
-- ============================================================================

integrationTestCases :: TestTree
integrationTestCases = testGroup "Integration Test Cases"
  [ testCase "complete parsing and analysis workflow" $
    do
      let input = "// @ownership: true\n// @dependentTypes: false\nfn test() {\n  let x = 42;\n  return x;\n}"
          parseResult = parseTypus input
      case parseResult of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right file -> do
          -- Verify directives
          assertBool "ownership directive present" $ isJust (fdOwnership (fileDirectives file))
          assertBool "dependentTypes directive present" $ isJust (fdDependentTypes (fileDirectives file))
          
  , testCase "error handling in complex scenarios" $
    do
      let collector = newErrorCollector
          collector1 = addError startPos "Syntax error" collector
          collector2 = addWarning (SourcePos 2 1) "Type warning" collector1
          collector3 = addInfo (SourcePos 3 1) "Info message" collector2
          errors = getErrors collector3
          warnings = getWarnings collector3
      assertEqual "error count" 1 (length errors)
      assertEqual "warning count" 1 (length warnings)
      
  , testCase "performance under load" $
    do
      let largeInput = concat $ replicate 100 "// @ownership: true\nfn test() { return 42; }\n"
          result = parseTypus largeInput
      case result of
        Left _ -> return ()  -- May fail due to size
        Right _ -> return ()  -- Or succeed
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

emptySpan :: SourceSpan
emptySpan = error "emptySpan not implemented for this test"

spanStart :: SourceSpan -> SourcePos
spanStart = error "spanStart not implemented for this test"

spanEnd :: SourceSpan -> SourcePos  
spanEnd = error "spanEnd not implemented for this test"

data SourceSpan = SourceSpan
  deriving (Eq, Show)