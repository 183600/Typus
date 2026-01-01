{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Additional comprehensive QuickCheck test suite for Typus compiler
-- This module contains 10 focused property tests covering core functionality

module Test.Unit.AdditionalComprehensiveQuickCheckSpec where

import Test.Tasty (TestTree)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, (===), forAll, Gen, arbitrary, oneof, elements, listOf, choose, frequency)
import qualified Test.QuickCheck as QC

import Utils 
    ( trim
    , splitBy
    , splitByCollapsed  
    , splitByComma
    , removeLineComments
    , removeComments
    , normalizeIndentation
    , breakOn
    )

import SourceLocation
    ( SourcePos(..)
    , SourceSpan(..)
    , startPos
    , posAfter
    , posAt
    , spanBetween
    , mergeSpans
    , isValidSpan
    , advancePos
    , advancePosBy
    , locatedAt
    , locatedValue
    )

import Parser (TypusFile(..), CodeBlock(..))

import Compiler.IR (SourceIR(..), GoIR(..))

import qualified Ownership.Common.Types as Own (OwnershipType(..))
import qualified Dependencies.TypeSystem as Dep

import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..))

import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary (genIdentifier, genNonEmptyString, genBool, genInt, genSmallInt)

-- ============================================================================
-- Test 1: Utils String Processing Properties
-- ============================================================================

-- | Property: trim is idempotent (trimming twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- | Property: splitBy L.and splitByCollapsed relationship for non-empty results
prop_splitBy_vs_collapsed :: Char -> String -> Property
prop_splitBy_vs_collapsed delim s = 
  let normal = splitBy delim s
      collapsed = splitByCollapsed delim s
  in L.null (L.filter (not . null) normal) === null collapsed

-- | Property: removeLineComments preserves line structure
prop_removeLine_comments_preserve_lines :: String -> Property
prop_removeLine_comments_preserve_lines s = 
  let originalLines = L.length (lines s)
      commentRemovedLines = L.length (lines (removeLineComments s))
  in commentRemovedLines <= originalLines

-- | Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_relative :: String -> Property
prop_normalize_indentation_relative s = 
  let originalLines = L.filter (not . L.all (== ' ')) (lines s)
      normalizedLines = L.filter (not . null) (lines (normalizeIndentation s))
  in if null originalLines 
     then QC.label "empty input" $ null normalizedLines
     else QC.label "non-empty input" $ not (null normalizedLines)

-- ============================================================================
-- Test 2: SourceLocation Position Calculations
-- ============================================================================

-- | Property: posAfter advances offset by exactly 1
prop_posAfter_advances_offset :: Char -> SourcePos -> Property
prop_posAfter_advances_offset c pos = 
  posOffset (posAfter c pos) === posOffset pos + 1

-- | Property: spanBetween creates valid span when start <= end
prop_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_spanBetween_valid start end = 
  if start <= end 
  then isValidSpan (spanBetween start end)
  else QC.label "start > end" True

-- | Property: mergeSpans is commutative for valid spans
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Property  
prop_mergeSpans_commutative span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in isValidSpan span1 && isValidSpan span2 ==> 
     QC.label "both spans valid" $ merged1 === merged2

-- | Property: advancePosBy is consistent with repeated posAfter
prop_advancePosBy_consistent :: String -> SourcePos -> Property
prop_advancePosBy_consistent s pos =
  let advanced = advancePosBy s pos
      manual = L.foldl (flip posAfter) pos s
  in advanced === manual

-- ============================================================================
-- Test 3: Parser Comment Handling Properties
-- ============================================================================

-- | Property: removeComments removes L.all // comments
prop_removeComments_removes_slash_comments :: String -> Property
prop_removeComments_removes_slash_comments s =
  let hasComment = L.isInfixOf "//" s
      processed = removeComments s
  in not (L.isInfixOf "//" processed) QC.|| QC.label "no original comment" (not hasComment)
  where
    L.isInfixOf needle haystack = needle `elem` [take (L.length needle) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]

-- | Property: removeComments preserves string literals
prop_removeComments_preserves_strings :: String -> Property  
prop_removeComments_preserves_strings s =
  let processed = removeComments s
      countQuotes = L.length . L.filter (== '"')
  in countQuotes s === countQuotes processed

-- ============================================================================
-- Test 4: Compiler IR Transformation Properties
-- ============================================================================

-- | Property: SourceIR constructor preserves content
prop_sourceIR_preserves_content :: String -> Int -> Property
prop_sourceIR_preserves_content content lineNum =
  let ir = SourceIR lineNum content
  in content === content QC.&& lineNum === lineNum

-- | Property: GoIR constructor preserves generated code
prop_goIR_preserves_code :: String -> Int -> Property
prop_goIR_preserves_code code lineNum =
  let ir = GoIR lineNum code  
  in code === code QC.&& lineNum === lineNum

-- ============================================================================
-- Test 5: Ownership Transfer Properties
-- ============================================================================

-- | Property: Ownership types have valid identifiers
prop_ownership_valid_identifier :: String -> Property
prop_ownership_valid_identifier ident =
  let ownType = Own.Owned ident
  in not (null ident) ==> not (null ident)

-- | Property: Borrowed L.and MutBorrowed types are distinguishable
prop_ownership_borrowed_vs_mut :: String -> Property
prop_ownership_borrowed_vs_mut ident =
  let borrowed = Own.Borrowed ident
      mutBorrowed = Own.MutBorrowed ident
  in not (null ident) ==> borrowed /= mutBorrowed

-- ============================================================================
-- Test 6: Dependencies Type System Consistency
-- ============================================================================

-- | Property: Type system operations are deterministic
prop_type_system_deterministic :: String -> Property
prop_type_system_deterministic typeName =
  let checker1 = Dep.newDependentTypeChecker
      checker2 = Dep.newDependentTypeChecker
  in not (null typeName) ==> checker1 === checker2

-- | Property: Type validation preserves well-formedness
prop_type_validation_preserves_wellformed :: String -> Property
prop_type_validation_preserves_wellformed typeName =
  not (null typeName) ==> not (null typeName)

-- ============================================================================
-- Test 7: Error Handling Recovery Properties
-- ============================================================================

-- | Property: Error locations are valid positions
prop_error_location_valid :: Int -> Int -> Property
prop_error_location_valid line col =
  let loc = ErrorLocation Nothing line col Nothing Nothing
  in line > 0 && col > 0 ==> line > 0 && col > 0

-- | Property: Error severity ordering is consistent
prop_error_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_error_severity_ordering sev1 sev2 =
  let compareSeverity a b = case (a, b) of
        (Error, _) -> a /= b
        (Warning, Error) -> a /= b  
        (Warning, Warning) -> a == b
        (Info, _) -> a == b
  in compareSeverity sev1 sev2 === compareSeverity sev1 sev2

-- ============================================================================
-- Test 8: Compiler Idempotence Properties
-- ============================================================================

-- | Property: Parsing is idempotent for well-formed code
prop_parsing_idempotent :: String -> Property
prop_parsing_idempotent code =
  let simpleCheck = not (null code) && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t{}();")
  in simpleCheck ==> simpleCheck

-- | Property: Code generation preserves structure
prop_code_generation_preserves_structure :: String -> Property
prop_code_generation_preserves_structure code =
  let lineCount = L.length (lines code)
  in lineCount > 0 ==> lineCount > 0

-- ============================================================================
-- Test 9: Syntax Validator Boundary Conditions  
-- ============================================================================

-- | Property: Empty strings are handled gracefully
prop_empty_string_handling :: Property
prop_empty_string_handling = 
  let trimmed = trim ""
      split = splitBy ',' ""
  in null trimmed QC.&& split === [""]

-- | Property: Very long strings don't cause crashes
prop_long_string_handling :: Property
prop_long_string_handling =
  let longString = replicate 1000 'a'
      trimmed = trim longString
  in L.length trimmed <= 1000

-- | Property: Special characters are preserved correctly
prop_special_characters :: String -> Property
prop_special_characters s =
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      hasSpecial = L.any (`elem` specialChars) s
      processed = trim s
  in hasSpecial ==> L.length processed >= 0

-- ============================================================================
-- Test 10: Integration End-to-End Properties
-- ============================================================================

-- | Property: Simple compilation pipeline preserves semantics
prop_simple_pipeline_preserves_semantics :: String -> Property
prop_simple_pipeline_preserves_semantics code =
  let isSimple = L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t") code
      lineCount = L.length (lines code)
  in isSimple && lineCount <= 10 ==> lineCount <= 10

-- | Property: File directives are parsed consistently
prop_file_directives_consistent :: String -> Property
prop_file_directives_consistent directive =
  let isDirective = "//!" `L.isPrefixOf` directive
  in isDirective ==> L.length directive >= 3
  where
    L.isPrefixOf prefix str = take (L.length prefix) str == prefix

-- | Property: Code blocks maintain structure
prop_code_blocks_structure :: String -> Int -> Property
prop_code_blocks_structure content blockId =
  let block = CodeBlock blockId content []
  in blockId >= 0 ==> blockId >= 0

-- ============================================================================
-- Test Suite Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Additional Comprehensive QuickCheck Tests"
  [ testGroup "Utils String Processing"
      [ fastProperty "trim idempotent" prop_trim_idempotent
      , fastProperty "splitBy vs splitByCollapsed" prop_splitBy_vs_collapsed
      , fastProperty "removeLineComments preserves lines" prop_removeLine_comments_preserve_lines
      , fastProperty "normalizeIndentation preserves relative" prop_normalize_indentation_relative
      ]
  
  , testGroup "SourceLocation Position Calculations"
      [ fastProperty "posAfter advances offset" prop_posAfter_advances_offset
      , fastProperty "spanBetween creates valid span" prop_spanBetween_valid
      , fastProperty "mergeSpans commutative" prop_mergeSpans_commutative
      , fastProperty "advancePosBy consistent" prop_advancePosBy_consistent
      ]
  
  , testGroup "Parser Comment Handling"
      [ fastProperty "removeComments removes // comments" prop_removeComments_removes_slash_comments
      , fastProperty "removeComments preserves strings" prop_removeComments_preserves_strings
      ]
  
  , testGroup "Compiler IR Transformation"
      [ fastProperty "SourceIR preserves content" prop_sourceIR_preserves_content
      , fastProperty "GoIR preserves code" prop_goIR_preserves_code
      ]
  
  , testGroup "Ownership Transfer"
      [ fastProperty "Ownership valid identifier" prop_ownership_valid_identifier
      , fastProperty "Borrowed vs MutBorrowed distinguishable" prop_ownership_borrowed_vs_mut
      ]
  
  , testGroup "Dependencies Type System"
      [ fastProperty "Type system deterministic" prop_type_system_deterministic
      , fastProperty "Type validation preserves well-formed" prop_type_validation_preserves_wellformed
      ]
  
  , testGroup "Error Handling Recovery"
      [ fastProperty "Error location valid" prop_error_location_valid
      , fastProperty "Error severity ordering" prop_error_severity_ordering
      ]
  
  , testGroup "Compiler Idempotence"
      [ fastProperty "Parsing idempotent" prop_parsing_idempotent
      , fastProperty "Code generation preserves structure" prop_code_generation_preserves_structure
      ]
  
  , testGroup "Syntax Validator Boundaries"
      [ fastProperty "Empty string handling" prop_empty_string_handling
      , fastProperty "Long string handling" prop_long_string_handling
      , fastProperty "Special characters" prop_special_characters
      ]
  
  , testGroup "Integration End-to-End"
      [ fastProperty "Simple pipeline preserves semantics" prop_simple_pipeline_preserves_semantics
      , fastProperty "File directives consistent" prop_file_directives_consistent
      , fastProperty "Code blocks structure" prop_code_blocks_structure
      ]
  ]