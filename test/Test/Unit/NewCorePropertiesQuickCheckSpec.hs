{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Core module properties QuickCheck tests
-- This module contains property-based tests for the core modules of the Typus compiler
module Test.Unit.NewCorePropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck ((==>), conjoin, counterexample)
import Utils
import SourceLocation
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import Data.Time (UTCTime, getCurrentTime)
import Data.List (sort, nub)
import Data.Ord (comparing)

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Check if a character is a valid identifier character
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- | Test trim function: trimming whitespace from both ends
prop_trim_roundtrip :: String -> Bool
prop_trim_roundtrip s = trim (trim s) == trim s

-- | Test splitBy function: splitting by delimiter and rejoining
prop_splitBy_join :: Char -> String -> Bool
prop_splitBy_join delim s = intercalate [delim] (splitBy delim s) == s

-- | Test splitByComma function: comma splitting
prop_splitByComma_consistency :: String -> Bool
prop_splitByComma_consistency s = splitByComma s == splitBy ',' s

-- | Test splitByCollapsed function: removing empty segments
prop_splitByCollapsed_noEmpty :: Char -> String -> Bool
prop_splitByCollapsed_noEmpty delim s = all (not . null) (splitByCollapsed delim s)

-- | Test breakOn function: empty pattern
prop_breakOn_emptyPattern :: String -> Bool
prop_breakOn_emptyPattern s = 
  let (before, after) = breakOn "" s
  in null before && after == s

-- | Test safeProcessString function: filtering control characters
prop_safeProcessString_removesControlChars :: String -> Bool
prop_safeProcessString_removesControlChars s =
  case safeProcessString s of
    Left _ -> True
    Right filtered -> all isValidChar filtered

-- | Test isValidChar function: valid characters
prop_isValidChar_valid :: Char -> Bool
prop_isValidChar_valid c = isValidChar c == (c >= ' ' || c == '\n' || c == '\r' || c == '\t')

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- | Test startPos: starting position should be (1,1,0)
prop_startPos_values :: Bool
prop_startPos_values = 
  posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

-- | Test posAfter: newline advances line number
prop_posAfter_newline :: Int -> Int -> Int -> Bool
prop_posAfter_newline line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\n' pos
  in posLine newPos == posLine pos + 1 && 
     posColumn newPos == 1 && 
     posOffset newPos == posOffset pos + 1

-- | Test posAfter: tab advances to next tab stop
prop_posAfter_tab :: Int -> Int -> Int -> Bool
prop_posAfter_tab line col offset = 
  let pos = SourcePos line col offset
      newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in posLine newPos == posLine pos && 
     posColumn newPos == expectedCol && 
     posOffset newPos == posOffset pos + 1

-- | Test emptySpan: span at position
prop_emptySpan_sameStartEnd :: Int -> Int -> Int -> Bool
prop_emptySpan_sameStartEnd line col offset = 
  let pos = SourcePos line col offset
      span = emptySpan pos
  in spanStart span == pos && spanEnd span == pos

-- | Test spanBetween: span between two positions
prop_spanBetween_correct :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_spanBetween_correct line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in posLine (spanStart span) == posLine pos1 && 
     posColumn (spanStart span) == posColumn pos1 &&
     posLine (spanEnd span) == posLine pos2 && 
     posColumn (spanEnd span) == posColumn pos2

-- | Test mergeSpans: merging spans
prop_mergeSpans_correct :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_mergeSpans_correct line1 col1 offset1 line2 col2 offset2 line3 col3 offset3 line4 col4 offset4 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      pos4 = SourcePos line4 col4 offset4
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos3 pos4
      merged = mergeSpans span1 span2
      
      -- Expected values
      expectedStartLine = min (posLine (spanStart span1)) (posLine (spanStart span2))
      expectedStartCol = min (posColumn (spanStart span1)) (posColumn (spanStart span2))
      expectedStartOffset = min (posOffset (spanStart span1)) (posOffset (spanStart span2))
      expectedEndLine = max (posLine (spanEnd span1)) (posLine (spanEnd span2))
      expectedEndCol = max (posColumn (spanEnd span1)) (posColumn (spanEnd span2))
      expectedEndOffset = max (posOffset (spanEnd span1)) (posOffset (spanEnd span2))
      
      -- Actual values
      actualStartLine = posLine (spanStart merged)
      actualStartCol = posColumn (spanStart merged)
      actualStartOffset = posOffset (spanStart merged)
      actualEndLine = posLine (spanEnd merged)
      actualEndCol = posColumn (spanEnd merged)
      actualEndOffset = posOffset (spanEnd merged)
      
      -- Helper to show comparison
      checkEq desc actual expected = 
        counterexample (desc ++ ": expected " ++ show expected ++ ", got " ++ show actual) (actual == expected)
  in conjoin 
     [ checkEq "start line" actualStartLine expectedStartLine
     , checkEq "start column" actualStartCol expectedStartCol
     , checkEq "start offset" actualStartOffset expectedStartOffset
     , checkEq "end line" actualEndLine expectedEndLine
     , checkEq "end column" actualEndCol expectedEndCol
     , checkEq "end offset" actualEndOffset expectedEndOffset
     ]

-- | Test isValidSpan: valid span has start <= end
prop_isValidSpan_correct :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_isValidSpan_correct line1 col1 offset1 line2 col2 offset2 = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
      valid = pos1 <= pos2
  in isValidSpan span == valid

-- | Test locatedAt: creating located value
prop_locatedAt_correct :: Int -> Int -> Int -> Int -> Bool
prop_locatedAt_correct line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
  in locValue located == value && 
     locPos located == pos && 
     locSpan located == emptySpan pos

-- | Test mapLocated: mapping over located value
prop_mapLocated_correct :: Int -> Int -> Int -> Int -> Bool
prop_mapLocated_correct line col offset value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
      mapped = mapLocated (*2) located
  in locValue mapped == value * 2 && 
     locPos mapped == pos && 
     locSpan mapped == emptySpan pos

-- | Test advancePosBy: advancing by multiple characters
prop_advancePosBy_consistent :: String -> Int -> Int -> Int -> Bool
prop_advancePosBy_consistent chars line col offset = 
  let pos = SourcePos line col offset
  in advancePosBy chars pos == foldl (flip advancePos) pos chars

-- | Test advancePosByText: advancing by text
prop_advancePosByText_consistent :: String -> Int -> Int -> Int -> Bool
prop_advancePosByText_consistent text line col offset = 
  let pos = SourcePos line col offset
  in advancePosByText (T.pack text) pos == advancePosBy text pos

-- | Test advancePosByLine: advancing by lines
prop_advancePosByLine_correct :: Int -> Int -> Int -> Int -> Bool
prop_advancePosByLine_correct line col offset numLines = 
  let pos = SourcePos line col offset
      newPos = advancePosByLine numLines pos
  in posLine newPos == posLine pos + numLines && 
     posColumn newPos == 1

-- ============================================================================
-- Error Handling Module Tests
-- ============================================================================

-- | Test errorAt: creating error at position
prop_errorAt_correct :: Int -> Int -> Int -> String -> Bool
prop_errorAt_correct line col offset message = 
  let pos = SourcePos line col offset
      error = errorAt "test-id" (T.pack message) (toErrorLocation pos)
      loc = location error
  in Error.line loc == posLine pos && 
     Error.column loc == posColumn pos &&
     severity error == Error

-- | Test errorWithCategory: creating error with category
prop_errorWithCategory_correct :: Int -> String -> Bool
prop_errorWithCategory_correct catIndex message = 
  let categories = [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]
      cat = categories !! (catIndex `mod` length categories)
      error = errorWithCategory "test-id" cat (T.pack message) (ErrorLocation Nothing 0 0 Nothing Nothing)
  in category error == cat

-- | Test warningAt: creating warning at position
prop_warningAt_correct :: Int -> Int -> Int -> String -> Bool
prop_warningAt_correct line col offset message = 
  let pos = SourcePos line col offset
      warning = warningAt "test-id" (T.pack message) (toErrorLocation pos)
      loc = location warning
  in Error.line loc == posLine pos && 
     Error.column loc == posColumn pos &&
     severity warning == Warning

-- | Test infoAt: creating info at position
prop_infoAt_correct :: Int -> Int -> Int -> String -> Bool
prop_infoAt_correct line col offset message = 
  let pos = SourcePos line col offset
      info = infoAt "test-id" (T.pack message) (toErrorLocation pos)
      loc = location info
  in Error.line loc == posLine pos && 
     Error.column loc == posColumn pos &&
     severity info == Info

-- | Test fatalError: creating fatal error
prop_fatalError_correct :: String -> Bool
prop_fatalError_correct message = 
  let error = fatalError "fatal-id" (T.pack message) (ErrorLocation Nothing 0 0 Nothing Nothing)
  in severity error == Fatal && recovery error == fatalRecovery

-- | Test isAtLeast: reflexive
prop_isAtLeast_reflexive :: Int -> Bool
prop_isAtLeast_reflexive sevIndex = 
  let severities = [Fatal, Error, Warning, Info]
      severity = severities !! (sevIndex `mod` length severities)
  in severity `isAtLeast` severity

-- | Test severityPriority: priority ordering
prop_severityPriority_ordering :: Int -> Int -> Bool
prop_severityPriority_ordering s1Index s2Index = 
  let severities = [Fatal, Error, Warning, Info]
      s1 = severities !! (s1Index `mod` length severities)
      s2 = severities !! (s2Index `mod` length severities)
      p1 = severityPriority s1
      p2 = severityPriority s2
  in (s1 `isAtLeast` s2) == (p1 >= p2)

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

-- | Test defaultFileDirectives: default values
prop_defaultFileDirectives_correct :: Bool
prop_defaultFileDirectives_correct = 
  let fd = defaultFileDirectives
  in fdOwnership fd == Nothing && 
     fdDependentTypes fd == Nothing && 
     fdConstraints fd == Nothing

-- | Test defaultBlockDirectives: default values
prop_defaultBlockDirectives_correct :: Bool
prop_defaultBlockDirectives_correct = 
  let bd = defaultBlockDirectives
  in bdOwnership bd == Nothing && 
     bdDependentTypes bd == Nothing && 
     bdConstraints bd == Nothing

-- | Test isIdentifierChar: special characters
prop_isIdentifierChar_special :: Bool
prop_isIdentifierChar_special = isIdentifierChar '_' && isIdentifierChar '-'

-- ============================================================================
-- Additional Property Tests
-- ============================================================================

-- | Test trim idempotency
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim s == trim (trim s)

-- | Test splitBy consistency with splitByComma
prop_splitByComma_splitBy :: String -> Bool
prop_splitByComma_splitBy s = splitByComma s == splitBy ',' s

-- | Test normalizeIndentation idempotency
prop_normalizeIndentation_idempotent :: String -> Bool
prop_normalizeIndentation_idempotent s = 
  let normalized = normalizeIndentation s
  in normalizeIndentation normalized == normalized

-- | Test posAfter preserves offset increment
prop_posAfter_offsetIncrement :: SourcePos -> Char -> Bool
prop_posAfter_offsetIncrement pos c = 
  let newPos = posAfter c pos
  in posOffset newPos == posOffset pos + 1

-- | Test emptySpan validity
prop_emptySpan_valid :: SourcePos -> Bool
prop_emptySpan_valid pos = isValidSpan (emptySpan pos)

-- | Test spanBetween validity
prop_spanBetween_valid :: SourcePos -> SourcePos -> Bool
prop_spanBetween_valid pos1 pos2 = 
  let span = spanBetween pos1 pos2
  in spanStart span == pos1 && spanEnd span == pos2

-- | Test mergeSpans commutativity
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_commutative span1 span2 = 
  mergeSpans span1 span2 == mergeSpans span2 span1

-- | Test mergeSpans associativity
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_associative span1 span2 span3 = 
  mergeSpans span1 (mergeSpans span2 span3) == mergeSpans (mergeSpans span1 span2) span3

-- | Test locatedAt creates valid span
prop_locatedAt_validSpan :: SourcePos -> Int -> Bool
prop_locatedAt_validSpan pos value = 
  isValidSpan (locSpan (locatedAt pos value))

-- | Test mapLocated preserves location
prop_mapLocated_preservesLocation :: SourcePos -> Int -> Bool
prop_mapLocated_preservesLocation pos value = 
  let located = locatedAt pos value
      mapped = mapLocated (*2) located
  in locPos mapped == locPos located && locSpan mapped == locSpan located

-- | Test advancePosBy with empty string
prop_advancePosBy_empty :: SourcePos -> Bool
prop_advancePosBy_empty pos = advancePosBy "" pos == pos

-- | Test advancePosByLine with zero lines
prop_advancePosByLine_zero :: SourcePos -> Bool
prop_advancePosByLine_zero pos = advancePosByLine 0 pos == pos

-- | Test error severity ordering
prop_severityOrdering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severityOrdering s1 s2 = 
  (s1 `isAtLeast` s2 && s2 `isAtLeast` s1) == (s1 == s2)

-- | Test filterByCategory preserves order
prop_filterByCategory_preservesOrder :: ErrorCategory -> [TypeError] -> Bool
prop_filterByCategory_preservesOrder cat errors = 
  let filtered = filterByCategory cat errors
      expected = filter (\e -> category e == cat) errors
  in filtered == expected

-- | Test filterBySeverity preserves order
prop_filterBySeverity_preservesOrder :: ErrorSeverity -> [TypeError] -> Bool
prop_filterBySeverity_preservesOrder sev errors = 
  let filtered = filterBySeverity sev errors
      expected = filter (\e -> severity e == sev) errors
  in filtered == expected

-- | Test combineErrors idempotency with same error
prop_combineErrors_idempotent :: Int -> Bool
prop_combineErrors_idempotent errorIndex = 
  let error = errorAt ("error-" ++ show errorIndex) (T.pack "test message") (ErrorLocation Nothing errorIndex 0 Nothing Nothing)
      combined = combineErrors [error, error]
  in length combined >= 1

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Core Properties QuickCheck Tests"
  [ -- Utils tests
    testProperty "trim roundtrip" prop_trim_roundtrip,
    testProperty "splitBy join" prop_splitBy_join,
    testProperty "splitByComma consistency" prop_splitByComma_consistency,
    testProperty "splitByCollapsed no empty" prop_splitByCollapsed_noEmpty,
    testProperty "breakOn empty pattern" prop_breakOn_emptyPattern,
    testProperty "safeProcessString removes control chars" prop_safeProcessString_removesControlChars,
    testProperty "isValidChar valid" prop_isValidChar_valid,
    
    -- SourceLocation tests
    testProperty "startPos values" prop_startPos_values,
    testProperty "posAfter newline" prop_posAfter_newline,
    testProperty "posAfter tab" prop_posAfter_tab,
    testProperty "emptySpan same start end" prop_emptySpan_sameStartEnd,
    testProperty "spanBetween correct" prop_spanBetween_correct,
    testProperty "mergeSpans correct" prop_mergeSpans_correct,
    testProperty "isValidSpan correct" prop_isValidSpan_correct,
    testProperty "locatedAt correct" prop_locatedAt_correct,
    testProperty "mapLocated correct" prop_mapLocated_correct,
    testProperty "advancePosBy consistent" prop_advancePosBy_consistent,
    testProperty "advancePosByText consistent" prop_advancePosByText_consistent,
    testProperty "advancePosByLine correct" prop_advancePosByLine_correct,
    
    -- Error handling tests
    testProperty "errorAt correct" prop_errorAt_correct,
    testProperty "errorWithCategory correct" prop_errorWithCategory_correct,
    testProperty "warningAt correct" prop_warningAt_correct,
    testProperty "infoAt correct" prop_infoAt_correct,
    testProperty "fatalError correct" prop_fatalError_correct,
    testProperty "isAtLeast reflexive" prop_isAtLeast_reflexive,
    testProperty "severityPriority ordering" prop_severityPriority_ordering,
    
    -- Parser tests
    testProperty "defaultFileDirectives correct" prop_defaultFileDirectives_correct,
    testProperty "defaultBlockDirectives correct" prop_defaultBlockDirectives_correct,
    testProperty "isIdentifierChar special" prop_isIdentifierChar_special,
    
    -- Additional tests
    testProperty "trim idempotent" prop_trim_idempotent,
    testProperty "splitByComma splitBy" prop_splitByComma_splitBy,
    testProperty "normalizeIndentation idempotent" prop_normalizeIndentation_idempotent
  ]

-- Helper function for intercalate
intercalate :: String -> [String] -> String
intercalate _ [] = []
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs