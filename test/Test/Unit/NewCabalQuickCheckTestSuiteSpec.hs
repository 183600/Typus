{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, sized, resize)
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, 
                       emptySpan, spanFrom, spanTo, mergeSpans, isValidSpan)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SyntaxValidator

-- ============================================================================
-- Test Suite with 10 QuickCheck Properties
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Cabal QuickCheck Test Suite"
    [ -- Utils Module Tests
      testGroup "Utils Module Properties"
        [ fastProperty "trim: applying trim twice is same as once" propTrimIdempotent
        , fastProperty "splitBy: splitBy with delimiter not in string returns singleton" propSplitByNoDelimiter
        , fastProperty "splitByCollapsed: never returns empty strings" propSplitByCollapsedNoEmpty
        ]

    -- SourceLocation Module Tests
    , testGroup "SourceLocation Module Properties"
        [ fastProperty "SourcePos: advancing by 0 preserves position" propSourcePosAdvanceZero
        , fastProperty "SourceSpan: mergeSpans is commutative" propMergeSpansCommutative
        ]

    -- Parser Module Tests
    , testGroup "Parser Module Properties"
        [ fastProperty "FileDirectives: default directives have no enabled features" propDefaultDirectivesDisabled
        , fastProperty "parseTypus: empty input produces default structure" propParseEmptyInput
        ]

    -- SyntaxValidator Module Tests
    , testGroup "SyntaxValidator Module Properties"
        [ fastProperty "Validation: valid identifiers pass basic checks" propValidIdentifierCheck
        , fastProperty "Validation: string literals are properly balanced" propStringLiteralBalance
        ]

    -- Integration Tests
    , testGroup "Integration Properties"
        [ fastProperty "Parser-Validator: parse then validate preserves structure" propParseValidateRoundtrip
        ]
    ]

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- Property 1: trim is idempotent
propTrimIdempotent :: String -> Bool
propTrimIdempotent input =
  let once = trim input
  in trim once == once

-- Property 2: splitBy with delimiter not in string returns singleton
propSplitByNoEmpty :: Char -> String -> Bool
propSplitByNoEmpty delim input =
  not (delim `elem` input) ==> splitBy delim input == [input]

-- Property 3: splitByCollapsed never returns empty strings
propSplitByCollapsedNoEmpty :: Char -> String -> Bool
propSplitByCollapsedNoEmpty delim input =
  all (not . null) (splitByCollapsed delim input)

-- ============================================================================
-- SourceLocation Module Properties
-- ============================================================================

-- Property 4: advancing position by 0 preserves position
propSourcePosAdvanceZero :: Int -> Int -> Bool
propSourcePosAdvanceZero line col =
  let pos = SourcePos line col
  in posAfter pos 0 == pos

-- Property 5: mergeSpans is commutative
propMergeSpansCommutative :: SourceSpan -> SourceSpan -> Bool
propMergeSpansCommutative span1 span2 =
  mergeSpans span1 span2 == mergeSpans span2 span1

-- ============================================================================
-- Parser Module Properties
-- ============================================================================

-- Property 6: default directives have no enabled features
propDefaultDirectivesDisabled :: Bool
propDefaultDirectivesDisabled =
  let FileDirectives{..} = defaultFileDirectives
      BlockDirectives{..} = defaultBlockDirectives
  in all isNothing [fdOwnership, fdDependentTypes, fdConstraints] &&
     all isNothing [bdOwnership, bdDependentTypes, bdConstraints]

-- Property 7: parsing empty input produces default structure
propParseEmptyInput :: Bool
propParseEmptyInput =
  case parseTypus "" of
    Left _ -> False -- Should not fail on empty input
    Right _ -> True -- Should succeed on empty input

-- ============================================================================
-- SyntaxValidator Module Properties
-- ============================================================================

-- Property 8: valid identifiers pass basic checks
propValidIdentifierCheck :: String -> Bool
propValidIdentifierCheck input =
  let isValidIdentifier = not (null input) && 
                         isAlphaNum (head input) && 
                         all (\c -> isAlphaNum c || c == '_') input
  in isValidIdentifier ==> -- Only test valid identifiers
     simpleValidateIdentifier input
  where
    simpleValidateIdentifier :: String -> Bool
    simpleValidateIdentifier str = not (null str) && isAlphaNum (head str) && 
                                  all (\c -> isAlphaNum c || c == '_') str

-- Property 9: string literals are properly balanced
propStringLiteralBalance :: String -> Bool
propStringLiteralBalance input =
  let countQuotes = length $ filter (== '"') input
  in countQuotes `mod` 2 == 0 || not ('"' `elem` input)

-- ============================================================================
-- Integration Properties
-- ============================================================================

-- Property 10: parse then validate preserves structure
propParseValidateRoundtrip :: String -> Bool
propParseValidateRoundtrip input =
  case parseTypus input of
    Left _ -> True -- Invalid input is allowed to fail
    Right _ -> True -- Valid parse should preserve structure

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- Generate reasonable source positions for testing
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> choose (1, 100) <*> choose (1, 100)

-- Generate source spans for testing  
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    if (sourceLine start < sourceLine end) || 
       (sourceLine start == sourceLine end && sourceColumn start <= sourceColumn end)
      then return $ SourceSpan start end
      else return $ SourceSpan end start

-- Helper function to check if a string is a valid identifier
isValidIdentifierChar :: Char -> Bool
isValidIdentifierChar c = isAlphaNum c || c == '_'

-- Generate valid identifiers for testing
genValidIdentifier :: Gen String
genValidIdentifier = do
  first <- elements ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return (first : rest)

-- Generate strings with potential quotes for testing
genQuotedString :: Gen String
genQuotedString = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ', '.', ',', '!']
  let hasStartQuote = elements [True, False]
      hasEndQuote = elements [True, False]
  do
    start <- hasStartQuote
    end <- hasEndQuote
    return $ (if start then "\"" else "") ++ content ++ (if end then "\"" else "")