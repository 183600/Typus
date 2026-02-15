{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.NewComprehensiveQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.QuickCheck (fastProperty, memoryEfficientProperty, ultraMemoryEfficientProperty)
import TestSupport.Arbitrary
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, sort, nub)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad (when, unless)
import Data.Either (isLeft, isRight)

-- Import Typus modules to test
import Parser
  ( parseTypus
  , parseTypusFile
  , parseExpression
  , parseDeclaration
  , Declaration(..)
  , Expression(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , tfContents
  , defaultFileDirectives
  , defaultBlockDirectives
  , isIdentifierChar
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  )

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Escape special characters in a string
escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '\"' = "\\\""
    escapeChar c = [c]

-- | Unescape special characters in a string
unescapeString :: String -> String
unescapeString = unescape []
  where
    unescape acc [] = reverse acc
    unescape acc ('\\':n:rest) | n == 'n' = unescape ('\n':acc) rest
    unescape acc ('\\':t:rest) | t == 't' = unescape ('\t':acc) rest
    unescape acc ('\\':r:rest) | r == 'r' = unescape ('\r':acc) rest
    unescape acc ('\\':'\\':rest) = unescape ('\\':acc) rest
    unescape acc ('\\':'"':rest) = unescape ('"':acc) rest
    unescape acc (c:rest) = unescape (c:acc) rest

-- ============================================================================
-- Test Properties
-- ============================================================================

-- | Property: Valid identifiers should be parsed correctly
prop_valid_identifier_parsing :: String -> Property
prop_valid_identifier_parsing s = 
  let isKeyword s = s `elem` ["func", "type", "import", "if", "else", "for", "while", "return"]
      isValid = all isIdentifierChar s && not (null s) && not (isKeyword s)
  in if isValid
      then property $ isRight $ parseExpression s
      else property True -- Skip invalid identifiers

-- | Property: Empty code blocks should be valid
prop_empty_code_block_valid :: Property
prop_empty_code_block_valid = 
  let emptyCode = ""
  in property $ isRight $ parseTypusFile emptyCode

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive QuickCheck Tests"
  [ testGroup "Parser Tests"
    [ fastProperty "valid identifier parsing" prop_valid_identifier_parsing
    , fastProperty "empty code block valid" prop_empty_code_block_valid
    ]
  ]
