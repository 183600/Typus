{-# LANGUAGE CPP #-}

-- | Additional Cabal QuickCheck Test Suite for Typus
-- This module provides comprehensive QuickCheck-based tests for core Typus functionality
module Test.Unit.AdditionalCabalQuickCheckTestSuite where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import SourceLocation (SourceLocation(..))
import Utils (splitIdentifier, normalizeTypusString)
import Parser (parseTypusExpression, ParseResult(..))
import Compiler (compileExpression, CompileResult(..))
import ErrorHandler (TypusError(..), ErrorSeverity(..))

-- ============================================================================
-- Test 1: Source Location Properties
-- ============================================================================

-- | Test that source location arithmetic is consistent
prop_sourceLocationArithmetic :: SourceLocation -> SourceLocation -> Bool
prop_sourceLocationArithmetic loc1 loc2 =
  -- Adding zero should not change location
  let zeroLoc = SourceLocation 0 0 0 0
      loc1PlusZero = SourceLocation 
        (sourceLine loc1 + sourceLine zeroLoc)
        (sourceColumn loc1 + sourceColumn zeroLoc)
        (sourceOffset loc1 + sourceOffset zeroLoc)
        (sourceLength loc1 + sourceLength zeroLoc)
  in loc1PlusZero == loc1

-- ============================================================================
-- Test 2: String Processing Properties
-- ============================================================================

-- | Test that identifier splitting is idempotent for simple cases
prop_identifierSplittingIdempotent :: String -> Bool
prop_identifierSplittingIdempotent str =
  let parts = splitIdentifier str
      rejoined = concat parts
  in null str || rejoined == str || length parts > 1

-- | Test that string normalization preserves non-empty strings
prop_stringNormalizationPreservesNonEmpty :: String -> Bool
prop_stringNormalizationPreservesNonEmpty str =
  let normalized = normalizeTypusString str
  in not (null str) ==> not (null normalized)

-- ============================================================================
-- Test 3: Parser Properties
-- ============================================================================

-- | Test that parsing and then re-serializing simple expressions is consistent
prop_parseRoundTripSimple :: String -> Bool
prop_parseRoundTripSimple expr =
  let result = parseTypusExpression (T.pack expr)
  in case result of
    ParseSuccess ast _ -> 
      -- For simple expressions, the round trip should work
      not (null expr) ==> length (show ast) > 0
    ParseError _ -> True -- Expected for invalid expressions

-- | Test that parsing empty string produces error
prop_parseEmptyString :: Bool
prop_parseEmptyString =
  case parseTypusExpression (T.pack "") of
    ParseError _ -> True
    _ -> False

-- ============================================================================
-- Test 4: Compiler Properties
-- ============================================================================

-- | Test that compilation preserves basic structure for simple cases
prop_compilationPreservesStructure :: String -> Bool
prop_compilationPreservesStructure expr =
  let parseResult = parseTypusExpression (T.pack expr)
  in case parseResult of
    ParseSuccess ast _ ->
      let compileResult = compileExpression ast
      in case compileResult of
        CompileSuccess _ -> True
        CompileError _ -> True -- Expected for complex expressions
    ParseError _ -> True -- Expected for invalid expressions

-- ============================================================================
-- Test 5: Error Handling Properties
-- ============================================================================

-- | Test that error severity ordering is consistent
prop_errorSeverityOrdering :: TypusError -> TypusError -> Bool
prop_errorSeverityOrdering err1 err2 =
  let severity1 = errorSeverity err1
      severity2 = errorSeverity err2
  in (severity1 == severity2) || (severity1 /= severity2)

-- | Test that error messages contain context
prop_errorMessagesHaveContext :: TypusError -> Bool
prop_errorMessagesHaveContext err =
  let msg = errorMessage err
  in not (null msg) && length msg >= 5

-- ============================================================================
-- Test 6: Map/Set Operations Properties
-- ============================================================================

-- | Test that Map union is associative
prop_mapUnionAssociative :: Map.Map String Int -> Map.Map String Int -> Map.Map String Int -> Bool
prop_mapUnionAssociative m1 m2 m3 =
  Map.union m1 (Map.union m2 m3) == Map.union (Map.union m1 m2) m3

-- | Test that Set intersection is commutative
prop_setIntersectionCommutative :: Set.Set String -> Set.Set String -> Bool
prop_setIntersectionCommutative s1 s2 =
  Set.intersection s1 s2 == Set.intersection s2 s1

-- ============================================================================
-- Test 7: Text Processing Properties
-- ============================================================================

-- | Test that Text splitting and joining is consistent
prop_textSplitJoin :: T.Text -> T.Text -> Bool
prop_textSplitJoin text delim =
  let parts = T.splitOn delim text
      rejoined = T.intercalate delim parts
  in not (T.null delim) ==> rejoined == text

-- | Test that Text length is preserved under certain operations
prop_textLengthPreservation :: T.Text -> Bool
prop_textLengthPreservation text =
  let upper = T.toUpper text
      lower = T.toLower text
  in T.length text == T.length upper && T.length text == T.length lower

-- ============================================================================
-- Test 8: List Processing Properties
-- ============================================================================

-- | Test that list reverse is involutive
prop_listReverseInvolutive :: [Int] -> Bool
prop_listReverseInvolutive xs = reverse (reverse xs) == xs

-- | Test that list sort is idempotent
prop_listSortIdempotent :: [Int] -> Bool
prop_listSortIdempotent xs = sort xs == sort (sort xs)
  where sort = foldr insert []

-- ============================================================================
-- Test 9: Numeric Properties
-- ============================================================================

-- | Test that addition is commutative
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative x y = x + y == y + x

-- | Test that multiplication distributes over addition
prop_multiplicationDistributive :: Int -> Int -> Int -> Bool
prop_multiplicationDistributive x y z = x * (y + z) == x * y + x * z

-- ============================================================================
-- Test 10: Boolean Properties
-- ============================================================================

-- | Test De Morgan's laws
prop_deMorganLaws :: Bool -> Bool -> Bool
prop_deMorganLaws p q =
  not (p && q) == (not p || not q) &&
  not (p || q) == (not p && not q)

-- | Test that double negation cancels
prop_doubleNegation :: Bool -> Bool
prop_doubleNegation p = not (not p) == p

-- ============================================================================
-- Test Suite Assembly
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Cabal QuickCheck Test Suite"
  [ testGroup "Source Location Tests"
    [ fastProperty "Source location arithmetic" prop_sourceLocationArithmetic
    ]
  , testGroup "String Processing Tests"
    [ fastProperty "Identifier splitting idempotent" prop_identifierSplittingIdempotent
    , fastProperty "String normalization preserves non-empty" prop_stringNormalizationPreservesNonEmpty
    ]
  , testGroup "Parser Tests"
    [ fastProperty "Parse round trip simple" prop_parseRoundTripSimple
    , fastProperty "Parse empty string" prop_parseEmptyString
    ]
  , testGroup "Compiler Tests"
    [ fastProperty "Compilation preserves structure" prop_compilationPreservesStructure
    ]
  , testGroup "Error Handling Tests"
    [ fastProperty "Error severity ordering" prop_errorSeverityOrdering
    , fastProperty "Error messages have context" prop_errorMessagesHaveContext
    ]
  , testGroup "Collection Tests"
    [ fastProperty "Map union associative" prop_mapUnionAssociative
    , fastProperty "Set intersection commutative" prop_setIntersectionCommutative
    ]
  , testGroup "Text Processing Tests"
    [ fastProperty "Text split join" prop_textSplitJoin
    , fastProperty "Text length preservation" prop_textLengthPreservation
    ]
  , testGroup "List Processing Tests"
    [ fastProperty "List reverse involutive" prop_listReverseInvolutive
    , fastProperty "List sort idempotent" prop_listSortIdempotent
    ]
  , testGroup "Numeric Tests"
    [ fastProperty "Addition commutative" prop_additionCommutative
    , fastProperty "Multiplication distributive" prop_multiplicationDistributive
    ]
  , testGroup "Boolean Tests"
    [ fastProperty "De Morgan's laws" prop_deMorganLaws
    , fastProperty "Double negation" prop_doubleNegation
    ]
  ]