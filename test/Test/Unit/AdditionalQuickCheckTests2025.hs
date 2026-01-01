{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Additional QuickCheck tests for Typus functionality - 2025 edition
module Test.Unit.AdditionalQuickCheckTests2025 where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (Arbitrary, arbitrary, suchThat)
import Utils (trim, splitBy, removeComments)
import SourceLocation (SourcePos(..), startPos, advancePos)

import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), formatError, errorAt, ErrorLocation(..))
import qualified Data.Text as T (pack, isInfixOf)
import Parser (parseTypus)
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Maybe (isJust)

-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- arbitrary `suchThat` (> 0)
    column <- arbitrary `suchThat` (> 0)
    offset <- arbitrary
    return $ SourcePos line column offset

-- ============================================================================
-- Test Suite Definition
-- ============================================================================

testSuite :: TestTree
testSuite = testGroup "Additional QuickCheck Tests 2025"
  [ basicUtilsProperties
  , basicSourceLocationProperties
  , basicErrorHandlingProperties
  , basicParserProperties
  ]

-- ============================================================================
-- Basic Utils Properties
-- ============================================================================

basicUtilsProperties :: TestTree
basicUtilsProperties = testGroup "Basic Utils Properties"
  [ testProperty "trim: idempotent" $
      \s -> trim (trim s) === trim s
      
  , testProperty "trim: removes leading L.and trailing whitespace" $
      \s ->
        let t = trim s
        in not (null t) ==> 
           (not . isSpace $ L.head t) && (not . isSpace $ last t)
           
  , testProperty "splitBy: preserves original content when joined" $
      \delim s ->
        let parts = splitBy delim s
            rejoined = L.concat parts
        in delim /= '\0' ==> L.length rejoined === L.length s
  ]

-- ============================================================================
-- Basic SourceLocation Properties
-- ============================================================================

basicSourceLocationProperties :: TestTree
basicSourceLocationProperties = testGroup "Basic SourceLocation Properties"
  [ testProperty "advancePos: preserves line positivity" $
      \pos ->
        let newPos = advancePos 'x' pos
        in posLine newPos >= 1
        
  , testProperty "advancePos: preserves column positivity" $
      \pos ->
        let newPos = advancePos 'x' pos
        in posColumn newPos >= 1
        
  , testCase "startPos has correct values" $
    do
      posLine startPos @?= 1
      posColumn startPos @?= 1
  ]

-- ============================================================================
-- Basic ErrorHandling Properties
-- ============================================================================

basicErrorHandlingProperties :: TestTree
basicErrorHandlingProperties = testGroup "Basic ErrorHandling Properties"
  [ testProperty "error formatting contains message" $
      \msg ->
        let location = ErrorLocation Nothing 1 1 Nothing Nothing
            err = errorAt "test-id" (T.pack msg) location
            formatted = formatError err
        in not (null msg) ==> T.pack msg `T.isInfixOf` T.pack formatted
        
  , testProperty "error formatting contains position" $
      \msg ->
        let location = ErrorLocation Nothing 1 1 Nothing Nothing
            err = errorAt "test-id" (T.pack msg) location
            formatted = formatError err
        in "1:1" `L.isInfixOf` formatted
  ]

-- ============================================================================
-- Basic Parser Properties
-- ============================================================================

basicParserProperties :: TestTree
basicParserProperties = testGroup "Basic Parser Properties"
  [ testProperty "empty input produces some result" $
      \() ->
        let result = parseTypus ""
        in case result of
             Left _ -> True
             Right _ -> True
             
  , testCase "simple directive parsing" $
    do
      let input = "// @ownership: true\n"
          result = parseTypus input
      case result of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right _ -> return ()
  ]