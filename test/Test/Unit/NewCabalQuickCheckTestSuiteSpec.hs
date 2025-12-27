{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalQuickCheckTestSuiteSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf, suchThat)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , spanBetween
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , locatedPos
  , mapLocated
  , advancePos
  , advancePosBy
  , advancePosByText
  , advancePosByLine
  , toErrorLocation
  , toErrorLocationWithSpan
  )

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    offset <- choose (0, 100000)
    return $ SourcePos line col offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endOffset <- choose (0, 100)
    let end = start { posOffset = posOffset start + endOffset
                   , posColumn = posColumn start + endOffset
                   }
    return $ SourceSpan start end

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    pos <- arbitrary
    span <- arbitrary
    return $ Located value pos span

-- ============================================================================
-- SourceLocation Tests
-- ============================================================================

-- Property: startPos is consistent
prop_sourcePos_startPos_consistent :: Property
prop_sourcePos_startPos_consistent =
  property $ startPos === SourcePos 1 1 0

-- Property: posAfter handles newline correctly
prop_sourcePos_posAfter_newline :: SourcePos -> Property
prop_sourcePos_posAfter_newline pos =
  let newPos = posAfter '\n' pos
  in property $ posLine newPos === posLine pos + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter handles tab correctly (8-space tabs)
prop_sourcePos_posAfter_tab :: SourcePos -> Property
prop_sourcePos_posAfter_tab pos =
  let newPos = posAfter '\t' pos
      expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
  in property $ posColumn newPos === expectedCol .&&.
     posOffset newPos === posOffset pos + 1

-- Property: posAfter handles regular characters correctly
prop_sourcePos_posAfter_regular :: SourcePos -> Char -> Property
prop_sourcePos_posAfter_regular pos char =
  char `notElem` "\n\t" ==>
  let newPos = posAfter char pos
  in property $ posLine newPos === posLine pos .&&.
     posColumn newPos === posColumn pos + 1 .&&.
     posOffset newPos === posOffset pos + 1

-- Property: spanBetween creates valid span
prop_sourceSpan_spanBetween_valid :: SourcePos -> SourcePos -> Property
prop_sourceSpan_spanBetween_valid start end =
  let span = spanBetween start end
  in property $ spanStart span === start .&&. spanEnd span === end

-- Property: mergeSpans contains both original spans
prop_sourceSpan_mergeSpans_contains :: SourceSpan -> SourceSpan -> Property
prop_sourceSpan_mergeSpans_contains span1 span2 =
  let merged = mergeSpans span1 span2
  in property $ spanStart merged <= spanStart span1 .&&.
     spanEnd merged >= spanEnd span1 .&&.
     spanStart merged <= spanStart span2 .&&.
     spanEnd merged >= spanEnd span2

-- Property: advancePosBy advances correctly
prop_sourcePos_advancePosBy :: SourcePos -> String -> Property
prop_sourcePos_advancePosBy pos chars =
  let finalPos = advancePosBy chars pos
      expectedOffset = posOffset pos + length chars
  in property $ posOffset finalPos === expectedOffset

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Property: Default directives are consistent
prop_parser_default_directives :: Property
prop_parser_default_directives =
  property $ defaultFileDirectives === FileDirectives Nothing Nothing Nothing .&&.
     defaultBlockDirectives === BlockDirectives Nothing Nothing Nothing

-- Property: Parsing empty content produces minimal structure
prop_parser_parse_empty :: Property
prop_parser_parse_empty =
  let result = parseTypus "" 
  in case result of
    Left _ -> property $ False
    Right typusFile -> property $ 
      tfDirectives typusFile === defaultFileDirectives .&&.
      null (tfBuildTags typusFile) .&&.
      null (tfBlocks typusFile)

-- Property: Parsing content with only directives preserves directives
prop_parser_parse_directives_only :: String -> Property
prop_parser_parse_directives_only content =
  not (null content) && not (any (`elem` "\n\r") content) ==>
  let directiveContent = "//! ownership=true, dependent-types=false\n" ++ content
      result = parseTypus directiveContent
  in case result of
    Left _ -> property $ False
    Right typusFile -> property $ 
      isJust (fdOwnership (tfDirectives typusFile))

-- ============================================================================
-- Utils Tests
-- ============================================================================

-- Property: trim removes only leading/trailing whitespace
prop_utils_trim_preserves_internal :: String -> String -> String -> Property
prop_utils_trim_preserves_internal before middle after =
  not (null middle) ==>
  let content = before ++ middle ++ after
      trimmed = trim content
      hasLeading = any isSpace before
      hasTrailing = any isSpace after
      noLeadingSpace = null trimmed || not (isSpace (head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace .&&.
     middle `isInfixOf` trimmed

-- Property: splitBy preserves order
prop_utils_splitBy_preserves_order :: String -> Char -> Property
prop_utils_splitBy_preserves_order str delim =
  let parts = splitBy delim str
      rejoined = L.intercalate [delim] parts
  in property $ rejoined === str

-- Property: splitByCollapsed removes empty segments
prop_utils_splitByCollapsed_no_empty :: Char -> String -> Property
prop_utils_splitByCollapsed_no_empty delim str =
  let parts = splitByCollapsed delim str
  in property $ all (not . null) parts

-- Property: removeLineComments removes line comments
prop_utils_removeLineComments_basic :: String -> String -> Property
prop_utils_removeLineComments_basic code comment =
  not ('"' `elem` code) && not ('\'' `elem` code) ==>
  let lineWithComment = code ++ " // " ++ comment
      cleaned = removeLineComments lineWithComment
  in property $ cleaned === (code ++ " ")

-- Property: normalizeIndentation removes common leading whitespace
prop_utils_normalizeIndentation_removes_common :: String -> Property
prop_utils_normalizeIndentation_removes_common content =
  not (null content) && not (any (`elem` "\n\r") content) ==>
  let indented = "    " ++ content ++ "\n    " ++ content ++ "\n"
      normalized = normalizeIndentation indented
      lines' = lines normalized
  in property $ all (not . L.isPrefixOf "    ") (filter (not . null) lines')

-- Property: breakOn finds first occurrence
prop_utils_breakOn_first :: String -> String -> String -> Property
prop_utils_breakOn_first prefix delimiter suffix =
  not (null delimiter) ==>
  let full = prefix ++ delimiter ++ suffix ++ delimiter ++ "extra"
      (before, after) = breakOn delimiter full
  in property $ before === prefix ++ delimiter ++ suffix .&&. after === "extra"

-- ============================================================================
-- Integration Tests
-- ============================================================================

-- Property: Source location tracking is consistent through parsing
prop_integration_source_location_consistency :: String -> Property
prop_integration_source_location_consistency content =
  length content <= 100 ==> -- Limit for performance
  let result = parseTypus content
  in case result of
    Left _ -> property $ True -- Parse errors are acceptable for arbitrary input
    Right typusFile -> property $
      -- Check that all blocks have valid spans
      all (isValidSpan . cbSpan) (tfBlocks typusFile)

-- Property: Comment removal preserves code structure
prop_integration_comment_preservation :: String -> Property
prop_integration_comment_preservation code =
  not (null code) && length code <= 200 ==> -- Limit for performance
  let withComments = code ++ " // comment\n/* block comment */" ++ code
      withoutComments = removeComments withComments
  in property $ code `isInfixOf` withoutComments

-- Property: String processing pipeline is idempotent
prop_integration_pipeline_idempotent :: String -> Property
prop_integration_pipeline_idempotent content =
  length content <= 100 ==> -- Limit for performance
  let pipeline = content |> removeComments |> trim |> normalizeIndentation
      pipelineTwice = pipeline |> removeComments |> trim |> normalizeIndentation
  in property $ pipeline === pipelineTwice

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Suite"
  [ testGroup "SourceLocation"
    [ fastProperty "startPos is consistent" prop_sourcePos_startPos_consistent
    , fastProperty "posAfter handles newline correctly" prop_sourcePos_posAfter_newline
    , fastProperty "posAfter handles tab correctly" prop_sourcePos_posAfter_tab
    , fastProperty "posAfter handles regular characters correctly" prop_sourcePos_posAfter_regular
    , fastProperty "spanBetween creates valid span" prop_sourceSpan_spanBetween_valid
    , fastProperty "mergeSpans contains both original spans" prop_sourceSpan_mergeSpans_contains
    , fastProperty "advancePosBy advances correctly" prop_sourcePos_advancePosBy
    ]
  , testGroup "Parser"
    [ fastProperty "Default directives are consistent" prop_parser_default_directives
    , fastProperty "Parsing empty content produces minimal structure" prop_parser_parse_empty
    , fastProperty "Parsing content with only directives preserves directives" prop_parser_parse_directives_only
    ]
  , testGroup "Utils"
    [ fastProperty "trim removes only leading/trailing whitespace" prop_utils_trim_preserves_internal
    , fastProperty "splitBy preserves order" prop_utils_splitBy_preserves_order
    , fastProperty "splitByCollapsed removes empty segments" prop_utils_splitByCollapsed_no_empty
    , fastProperty "removeLineComments removes line comments" prop_utils_removeLineComments_basic
    , fastProperty "normalizeIndentation removes common leading whitespace" prop_utils_normalizeIndentation_removes_common
    , fastProperty "breakOn finds first occurrence" prop_utils_breakOn_first
    ]
  , testGroup "Integration"
    [ fastProperty "Source location tracking is consistent through parsing" prop_integration_source_location_consistency
    , fastProperty "Comment removal preserves code structure" prop_integration_comment_preservation
    , fastProperty "String processing pipeline is idempotent" prop_integration_pipeline_idempotent
    ]
  ]

-- Helper operator for pipeline testing
(|>) :: a -> (a -> b) -> b
x |> f = f
infixl 0 |>

-- Additional QuickCheck generators for more complex testing

-- Generate valid identifier strings
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate valid comment content
genCommentContent :: Gen String
genCommentContent = do
  length' <- choose (1, 50)
  chars <- vectorOf length' $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "
  return $ filter (not . (`elem` "\n\r")) chars

-- Generate valid code content (without quotes to avoid string literal issues)
genCodeContent :: Gen String
genCodeContent = do
  length' <- choose (1, 50)
  chars <- vectorOf length' $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " +-*/=(){}[];:"
  return $ filter (not . (`elem` "\"'")) chars
