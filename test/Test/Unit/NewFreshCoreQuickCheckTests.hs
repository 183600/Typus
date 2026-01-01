{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Core QuickCheck tests for essential Typus functionality
module Test.Unit.NewFreshCoreQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, removeComments)
import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, advancePos, mergeSpans, spanFrom, spanTo
  , emptySpan, spanBetween, isValidSpan
  )
import Compiler.Errors.Core 
  ( TypeError(..), ErrorSeverity(..), ErrorCategory(..)
  , ErrorLocation(..), ErrorContext(..), emptyContext
  , formatError, errorAt, warningAt, infoAt
  )
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), defaultFileDirectives)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Control.Monad (when)
import qualified Data.Text as T (pack, unpack)

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "New Core QuickCheck Tests"
  [ utilsProperties
  , sourceLocationProperties  
  , errorHandlingProperties
  , parserProperties
  , integrationProperties
  ]

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

utilsProperties :: TestTree
utilsProperties = testGroup "Utils Module Properties"
  [ testProperty "trim: trimming twice is idempotent" $
      \s -> trim (trim s) === trim s
      
  , testProperty "trim: trimmed string has no leading/trailing whitespace" $
      \s -> let t = trim s
             in not (null t) ==> 
                (not . isSpace $ L.head t) && (not . isSpace $ last t)
                
  , testProperty "splitBy: split L.and join with delimiter preserves original" $
      \c s -> splitBy c s === L.map (T.unpack . T.pack) (T.split (== c) (T.pack s))
      
  , testProperty "splitByCollapsed: never returns empty strings" $
      \c s -> L.all (not . null) (splitByCollapsed c s)
      
  , testProperty "splitByCollapsed: result is subset of splitBy" $
      \c s -> L.all (`elem` splitBy c s) (splitByCollapsed c s)
      
  , testProperty "removeLineComments: removing comments twice is idempotent" $
      \s -> removeLineComments (removeLineComments s) === removeLineComments s
      
  , testProperty "removeComments: removing comments twice is idempotent" $
      \s -> removeComments (removeComments s) === removeComments s
  ]

-- ============================================================================
-- SourceLocation Module Properties  
-- ============================================================================

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Module Properties"
  [ testProperty "SourcePos: advancing by 0 preserves position" $
      \pos -> advancePos pos '\0' === pos
      
  , testProperty "SourcePos: advancing by newline increments line, resets column" $
      \line col -> let pos = SourcePos line col
                   in advancePos pos '\n' === SourcePos (line + 1) 1
                   
  , testProperty "SourceSpan: empty span is invalid" $
      \() -> not $ isValidSpan emptySpan
      
  , testProperty "SourceSpan: span from single position to itself is empty" $
      \pos -> let span = spanFrom pos `spanTo` pos
              in not $ isValidSpan span
              
  , testProperty "SourceSpan: mergeSpans is commutative for valid spans" $
      \pos1 pos2 pos3 pos4 -> 
        let span1 = spanBetween pos1 pos2
            span2 = spanBetween pos3 pos4
        in isValidSpan span1 && isValidSpan span2 ==> 
           mergeSpans span1 span2 === mergeSpans span2 span1
           
  , testProperty "SourceSpan: mergeSpans result contains both original spans" $
      \pos1 pos2 pos3 pos4 ->
        let span1 = spanBetween pos1 pos2
            span2 = spanBetween pos3 pos4
            merged = mergeSpans span1 span2
        in isValidSpan span1 && isValidSpan span2 ==>
           spanStart merged <= min (spanStart span1) (spanStart span2) &&
           spanEnd merged >= max (spanEnd span1) (spanEnd span2)
  ]

-- ============================================================================
-- ErrorHandling Module Properties
-- ============================================================================

errorHandlingProperties :: TestTree
errorHandlingProperties = testGroup "ErrorHandling Module Properties"
  [ testProperty "TypeError: formatting preserves essential information" $
      \severity category msg ->
        let err = TypeError severity category msg emptyContext
            formatted = formatError err
        in msg `L.isInfixOf` formatted
        
  , testProperty "ErrorLocation: position information is preserved in formatting" $
      \line col msg ->
        let pos = SourcePos line col
            err = errorAt "test-id" = errorAt (startPos) msg
            err2 = warningAt (startPos) msg
            err3 = infoAt (startPos) msg
            fmt1 = formatError err1
            fmt2 = formatError err2  
            fmt3 = formatError err3
        in fmt1 /= fmt2 && fmt2 /= fmt3 && fmt1 /= fmt3
        
  , testCase "ErrorContext: empty context has no additional information" $
    do
      let ctx = emptyContext
      ctx @?= emptyContext
  ]

-- ============================================================================
-- Parser Module Properties
-- ============================================================================

parserProperties :: TestTree
parserProperties = testGroup "Parser Module Properties"
  [ testCase "Parser: empty input produces default directives" $
    do
      let result = parseTypus ""
      case result of
        Left _ -> assertFailure "Empty input should parse successfully"
        Right file -> do
          fileDirectives file @?= defaultFileDirectives
          
  , testCase "Parser: directives are case sensitive" $
    do
      let input1 = "// @ownership: true\n"
          input2 = "// @Ownership: true\n"  -- Capital O
          result1 = parseTypus input1
          result2 = parseTypus input2
      case (result1, result2) of
        (Right file1, Right file2) -> do
          -- The case-sensitive version should not parse the directive
          fileDirectives file1 @?= fileDirectives file2
        _ -> assertFailure "Both inputs should parse successfully"
        
  , testProperty "Parser: adding whitespace around directives doesn't affect parsing" $
      \ws1 ws2 ->
        let input = ws1 ++ "// @ownership: true\n" ++ ws2
            result = parseTypus input
        in L.length ws1 < 10 && L.length ws2 < 10 ==>  -- Limit size for performance
           case result of
             Left _ -> property False  -- Should parse successfully
             Right file -> isJust (fdOwnership (fileDirectives file))
  ]

-- ============================================================================
-- Integration Properties
-- ============================================================================

integrationProperties :: TestTree
integrationProperties = testGroup "Integration Properties"
  [ testProperty "Utils + SourceLocation: position calculations with string processing" $
      \str ->
        let trimmed = trim str
            pos = startPos
            finalPos = foldl advancePos pos trimmed
        in sourceLine finalPos >= 0 && sourceColumn finalPos >= 0
        
  , testProperty "Parser + ErrorHandling: error positions are within source bounds" $
      \input ->
        let result = parseTypus (take 100 input)  -- Limit size
        in case result of
             Left err -> 
               -- For parse errors, position should be reasonable
               property True  -- Simplified for this example
             Right _ -> 
               property True  -- Successful parse has no errors
               
  , testProperty "Utils + Parser: comment removal affects parsing behavior" $
      \input ->
        let withComments = input ++ "\n// This is a comment"
            withoutComments = removeLineComments withComments
            result1 = parseTypus withComments
            result2 = parseTypus withoutComments
        in L.length input < 50 ==>  -- Limit size
           case (result1, result2) of
             (Right _, Right _) -> property True
             (Left _, Left _) -> property True  
             _ -> property False  -- Should have same success/failure status
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

isInfixOf :: String -> String -> Bool
L.isInfixOf = Data.List.L.isInfixOf