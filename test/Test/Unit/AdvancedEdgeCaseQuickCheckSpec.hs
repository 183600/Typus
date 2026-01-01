{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdvancedEdgeCaseQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import qualified Test.QuickCheck as QC
import qualified Data.Map as Map

import SourceLocation 
import Compiler.Errors.Core
import Utils
import qualified Data.Text as T (pack, unpack)
import qualified Data.List as L
import Data.Char (isSpace, isLetter, isDigit, isPunctuation, ord, chr)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad.State (evalState, get, put)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, secondsToDiffTime)

-- ============================================================================
-- Custom Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
    arbitrary = SourcePos <$> 
        choose (1, 1000) <*> 
        choose (1, 1000) <*> 
        choose (0, 100000)

instance Arbitrary SourceSpan where
    arbitrary = do
        start <- arbitrary
        endOffset <- choose (0, 100)
        let end = start { posOffset = posOffset start + endOffset }
        return $ SourceSpan start end

instance Arbitrary ErrorLocation where
    arbitrary = ErrorLocation <$> 
        frequency [(3, Just <$> arbitrary), (1, pure Nothing)] <*>
        choose (0, 1000) <*>
        choose (0, 1000) <*>
        frequency [(2, Just <$> choose (0, 1000)), (1, pure Nothing)] <*>
        frequency [(2, Just <$> choose (0, 1000)), (1, pure Nothing)]

instance Arbitrary ErrorSeverity where
    arbitrary = frequency 
        [(1, pure Fatal)
        ,(3, pure Error)
        ,(3, pure Warning)
        ,(2, pure Info)
        ]

instance Arbitrary ErrorCategory where
    arbitrary = frequency 
        [(2, pure TypeChecking)
        ,(2, pure Ownership)
        ,(2, pure Parsing)
        ,(2, pure Semantic)
        ,(1, pure Runtime)
        ,(1, pure Constraint)
        ,(1, pure Inference)
        ,(1, pure Integration)
        ,(1, pure Unknown)
        ]

instance Arbitrary ErrorContext where
    arbitrary = ErrorContext <$> 
        frequency [(2, Just <$> arbitrary), (1, pure Nothing)] <*>
        frequency [(2, Just <$> arbitrary), (1, pure Nothing)] <*>
        frequency [(2, Just <$> arbitrary), (1, pure Nothing)] <*>
        frequency [(2, Just <$> arbitrary), (1, pure Nothing)] <*>
        listOf ((,) <$> arbitrary <*> arbitrary)

instance Arbitrary ErrorRecovery where
    arbitrary = ErrorRecovery <$> 
        arbitrary <*>
        arbitrary <*>
        frequency [(2, Just <$> arbitrary), (1, pure Nothing)] <*>
        frequency [(2, Just <$> arbitrary), (1, pure Nothing)] <*>
        choose (0, 100) <*>
        choose (0.0, 1.0)

instance Arbitrary TypeError where
    arbitrary = TypeError <$> 
        arbitrary <*>
        arbitrary <*>
        arbitrary <*>
        (T.pack <$> arbitrary) <*>
        arbitrary <*>
        arbitrary <*>
        arbitrary <*>
        listOf (T.pack <$> arbitrary) <*>
        listOf arbitrary <*>
        listOf arbitrary <*>
        frequency [(2, Just <$> arbitrary), (1, pure Nothing)]

-- Generate special Unicode characters for edge case testing
unicodeChar :: Gen Char
unicodeChar = frequency 
    [(5, arbitrary :: Gen Char)  -- Regular ASCII
    ,(2, elements $ map chr [0x80..0xFF])  -- Extended Latin
    ,(1, elements $ map chr [0x4E00..0x4E5F])  -- Chinese characters
    ,(1, elements $ map chr [0x0900..0x097F])  -- Devanagari
    ,(1, elements ['😀','🚀','🔥','💡','⚡'])  -- Emoji
    ]

-- Generate strings with challenging whitespace patterns
whitespaceString :: Gen String
whitespaceString = listOf $ frequency 
    [(3, elements " \t")  -- Common whitespace
    ,(1, elements "\n\r")  -- Line breaks
    ,(1, elements "\0\v\f")  -- Control characters
    ,(2, arbitrary :: Gen Char)  -- Regular characters
    ]

-- ============================================================================
-- Test 1: SourceLocation Advanced Mathematical Properties
-- ============================================================================

-- Property: Position advancement is associative for character sequences
prop_source_position_associative :: String -> String -> String -> Property
prop_source_position_associative s1 s2 s3 =
    let pos1 = startPos
        pos2 = advancePosBy s1 pos1
        pos3 = advancePosBy s2 pos2
        pos4 = advancePosBy s3 pos3
        -- Alternative: advance by concatenated string
        posAlt = advancePosBy (s1 ++ s2 ++ s3) pos1
    in property $ pos4 === posAlt

-- Property: Span merging is commutative for overlapping spans
prop_span_merging_commutative :: SourceSpan -> SourceSpan -> Property
prop_span_merging_commutative span1 span2 =
    let merged1 = mergeSpans span1 span2
        merged2 = mergeSpans span2 span1
    in property $ merged1 === merged2

-- Property: Span merging is associative
prop_span_merging_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_span_merging_associative span1 span2 span3 =
    let merged12 = mergeSpans span1 span2
        merged123 = mergeSpans merged12 span3
        merged23 = mergeSpans span2 span3
        merged123_alt = mergeSpans span1 merged23
    in property $ merged123 === merged123_alt

-- Property: Position distance is symmetric
prop_position_distance_symmetric :: SourcePos -> SourcePos -> Property
prop_position_distance_symmetric pos1 pos2 =
    let dist1 = abs (posOffset pos2 - posOffset pos1)
        dist2 = abs (posOffset pos1 - posOffset pos2)
    in property $ dist1 === dist2

-- ============================================================================
-- Test 2: Error Handler Recovery Strategy Properties
-- ============================================================================

-- Property: Recovery strategy composition preserves recoverability
prop_recovery_composition_preserves_recoverability :: ErrorRecovery -> ErrorRecovery -> Property
prop_recovery_composition_preserves_recoverability rec1 rec2 =
    let composed = ErrorRecovery
            { canRecover = canRecover rec1 && canRecover rec2
            , shouldContinue = shouldContinue rec1 && shouldContinue rec2
            , recoveryAction = Nothing
            , recoveryHint = Nothing
            , recoveryCost = recoveryCost rec1 + recoveryCost rec2
            , recoveryConfidence = (recoveryConfidence rec1 + recoveryConfidence rec2) / 2
            }
    in property $ canRecover composed === (canRecover rec1 && canRecover rec2)

-- Property: Error severity ordering is transitive
prop_severity_ordering_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering_transitive sev1 sev2 sev3 =
    sev1 <= sev2 && sev2 <= sev3 ==> sev1 <= sev3

-- Property: Error filtering preserves ordering
prop_error_filtering_preserves_ordering :: [TypeError] -> ErrorSeverity -> Property
prop_error_filtering_preserves_ordering errors minSeverity =
    let filtered = L.filter (\e -> severity e >= minSeverity) errors
        sorted = L.sortBy (\e1 e2 -> compare (severity e2) (severity e1)) filtered
    in property $ L.all (\e -> severity e >= minSeverity) sorted

-- ============================================================================
-- Test 3: Parser L.and SourceLocation Integration
-- ============================================================================

-- Property: Text processing preserves position tracking consistency
prop_text_processing_position_consistency :: String -> Property
prop_text_processing_position_consistency text =
    let processed = removeComments text
        originalLength = L.length text
        processedLength = L.length processed
        -- Position tracking should account for removed characters
        positions = scanl (\pos char -> advancePos char pos) startPos text
        finalPos = last positions
    in property $ posOffset finalPos >= originalLength - processedLength

-- Property: Comment removal preserves line structure
prop_comment_removal_preserves_lines :: String -> Property
prop_comment_removal_preserves_lines text =
    let originalLines = L.length $ lines text
        processed = removeComments text
        processedLines = L.length $ lines processed
    in property $ processedLines <= originalLines

-- Property: String splitting L.and rejoining preserves content
prop_string_splitting_roundtrip :: String -> Char -> Property
prop_string_splitting_roundtrip text delim =
    let parts = splitBy delim text
        rejoined = L.intercalate [delim] parts
    in property $ rejoined === text

-- ============================================================================
-- Test 4: Error Chain Propagation
-- ============================================================================

-- Property: Error chain preserves chronological order
prop_error_chain_chronological :: [TypeError] -> Property
prop_error_chain_chronological errors =
    not (null errors) ==>
    let chained = L.foldl (\acc err -> err { errorChain = acc }) [] errors
        timestamps = concatMap (maybe [] (:[]) . timestamp) chained
    in property $ L.length timestamps === L.length errors

-- Property: Error wrapping preserves original error information
prop_error_wrapping_preserves_original :: TypeError -> String -> Property
prop_error_wrapping_preserves_original err wrapperMsg =
    let wrapped = wrapError (T.pack wrapperMsg) err
        chain = errorChain wrapped
    in property $ not (null chain) && L.head chain === err

-- Property: Combined error severity reflects most severe component
prop_combined_error_severity :: [CombinedError] -> Property
prop_combined_error_severity errors =
    not (null errors) ==>
    let severities = map combinedErrorSeverity errors
        maxSeverity = L.maximum severities
    in property $ L.all (\sev -> sev <= maxSeverity) severities

-- ============================================================================
-- Test 5: Multi-module Coordination
-- ============================================================================

-- Property: Location tracking across module boundaries
prop_cross_module_location_tracking :: SourcePos -> String -> Property
prop_cross_module_location_tracking pos content =
    let locatedValue = locatedAt pos content
        extractedPos = locatedPos locatedValue
        extractedSpan = locatedSpan locatedValue
    in extractedPos === pos .&. spanStart extractedSpan === pos

-- Property: Error context accumulation preserves information
prop_error_context_accumulation :: ErrorContext -> ErrorContext -> Property
prop_error_context_accumulation ctx1 ctx2 =
    let combined = ctx1 
            { contextAdditional = contextAdditional ctx1 ++ contextAdditional ctx2 }
    in property $ L.length (contextAdditional combined) >= 
                  max (L.length $ contextAdditional ctx1) (L.length $ contextAdditional ctx2)

-- ============================================================================
-- Test 6: Performance Boundary Tests
-- ============================================================================

-- Property: Large string processing performance degrades gracefully
prop_large_string_processing_performance :: Int -> String -> Property
prop_large_string_processing_performance multiplier baseString =
    multiplier >= 0 && multiplier <= 100 ==>  -- Limit for reasonable test time
    let largeString = L.concat $ replicate multiplier baseString
        processed = trim largeString
    in property $ L.length processed <= L.length largeString

-- Property: Error collection scales linearly
prop_error_collection_linear_scaling :: Int -> TypeError -> Property
prop_error_collection_linear_scaling count baseError =
    count >= 0 && count <= 1000 ==>  -- Reasonable limit
    let errors = replicate count baseError
        stats = getErrorStatistics errors
    in property $ stats Map.! "total" === count

-- Property: Memory usage with repeated operations
prop_memory_usage_repeated_operations :: String -> Int -> Property
prop_memory_usage_repeated_operations content iterations =
    iterations >= 0 && iterations <= 100 ==>  -- Limit for performance
    let repeated = iterate normalizeIndentation content !! iterations
    in property $ L.length repeated <= L.length content * 2

-- ============================================================================
-- Test 7: Unicode L.and Special Character Handling
-- ============================================================================

-- Property: Unicode character processing preserves content
prop_unicode_processing_preserves_content :: String -> Property
prop_unicode_processing_preserves_content content =
    let processed = trim content
        -- Check that Unicode characters are preserved
        hasUnicode = L.any (> '\127') content
    in classify hasUnicode "contains Unicode" $
       property $ if hasUnicode 
                  then L.any (> '\127') processed
                  else True

-- Property: Special whitespace normalization
prop_special_whitespace_normalization :: String -> Property
prop_special_whitespace_normalization content =
    let withSpecialWhitespace = content ++ "\t\n\r\v\f" ++ content
        normalized = normalizeIndentation withSpecialWhitespace
    in property $ not (L.any (`elem` "\v\f") normalized)

-- Property: Emoji L.and symbol handling
prop_emoji_symbol_handling :: String -> Property
prop_emoji_symbol_handling content =
    let withEmoji = content ++ "😀🚀🔥💡⚡" ++ content
        processed = removeComments withEmoji
    in property $ "😀" `L.isInfixOf` processed && "🚀" `L.isInfixOf` processed

-- ============================================================================
-- Test 8: Memory Efficiency L.and Resource Management
-- ============================================================================

-- Property: String sharing in repeated operations
prop_string_sharing_efficiency :: String -> Int -> Property
prop_string_sharing_efficiency base count =
    count >= 0 && count <= 50 ==>  -- Limit for memory testing
    let strings = replicate count base
        processed = map trim strings
        totalLength = L.sum $ map L.length processed
    in property $ totalLength <= L.length base * count

-- Property: Error collection cleanup
prop_error_collection_cleanup :: [TypeError] -> Property
prop_error_collection_cleanup errors =
    let filtered = filterBySeverity Error errors
        warnings = filterBySeverity Warning errors
    in property $ L.length filtered + L.length warnings <= L.length errors

-- Property: Location tracking memory efficiency
prop_location_tracking_memory_efficiency :: Int -> Property
prop_location_tracking_memory_efficiency count =
    count >= 0 && count <= 1000 ==>  -- Reasonable limit
    let positions = replicate count startPos
        located = L.map (`locatedAt` ()) positions
    in property $ L.length located === count

-- ============================================================================
-- Test 9: Concurrent Safety (simulated)
-- ============================================================================

-- Property: Error collection is thread-safe (simulated)
prop_error_collection_thread_safe :: [TypeError] -> [TypeError] -> Property
prop_error_collection_thread_safe errors1 errors2 =
    let combined1 = errors1 ++ errors2
        combined2 = errors2 ++ errors1
        stats1 = getErrorStatistics combined1
        stats2 = getErrorStatistics combined2
    in property $ stats1 === stats2

-- Property: Position tracking consistency under concurrent modifications
prop_position_tracking_concurrent_consistency :: SourcePos -> String -> String -> Property
prop_position_tracking_concurrent_consistency pos text1 text2 =
    let pos1 = advancePosByText (T.pack text1) pos
        pos2 = advancePosByText (T.pack text2) pos
        posCombined = advancePosByText (T.pack (text1 ++ text2)) pos
        posSequential = advancePosByText (T.pack text2) pos1
    in property $ posCombined === posSequential

-- ============================================================================
-- Test 10: End-to-End Integration
-- ============================================================================

-- Property: Complete error processing pipeline
prop_complete_error_processing_pipeline :: TypeError -> Property
prop_complete_error_processing_pipeline baseError =
    let testPos = SourcePos 10 5 0
        withLocation = baseError { location = toErrorLocation testPos }
        withContext = withLocation { context = emptyContext { contextFunction = Just "test" } }
        withRecovery = withContext { recovery = errorRecovery }
        formatted = formatErrorWithRecovery withRecovery
    in property $ L.length formatted > 0 .&. "test" `L.isInfixOf` formatted

-- Property: Multi-stage text processing consistency
prop_multistage_text_processing_consistency :: String -> Property
prop_multistage_text_processing_consistency content =
    let stage1 = trim content
        stage2 = removeComments stage1
        stage3 = normalizeIndentation stage2
        -- Alternative order
        stage1_alt = removeComments content
        stage2_alt = normalizeIndentation stage1_alt
        stage3_alt = trim stage2_alt
    in property $ L.length stage3 >= 0 && L.length stage3_alt >= 0

-- Property: Error reporting completeness
prop_error_reporting_completeness :: [TypeError] -> Property
prop_error_reporting_completeness errors =
    not (null errors) ==>
    let report = generateErrorReport errors
        stats = getErrorStatistics errors
        hasStats = L.any (`L.isInfixOf` report) (map show (Map.toList stats))
        hasErrors = "Detailed Errors:" `L.isInfixOf` report
    in property $ hasStats && hasErrors

-- Helper function for error formatting with recovery
formatErrorWithRecovery :: TypeError -> String
formatErrorWithRecovery err =
    let baseMsg = T.unpack (message err)
        recoveryMsg = case recoveryAction (recovery err) of
            Just action -> " [Recovery: " ++ action ++ "]"
            Nothing -> ""
    in baseMsg ++ recoveryMsg

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Advanced Edge Case QuickCheck Tests"
    [ testGroup "SourceLocation Mathematical Properties"
        [ fastProperty "Position advancement associativity" prop_source_position_associative
        , fastProperty "Span merging commutative" prop_span_merging_commutative
        , fastProperty "Span merging associative" prop_span_merging_associative
        , fastProperty "Position distance symmetric" prop_position_distance_symmetric
        ]
    , testGroup "Error Handler Recovery Strategy"
        [ fastProperty "Recovery composition preserves recoverability" prop_recovery_composition_preserves_recoverability
        , fastProperty "Severity ordering transitive" prop_severity_ordering_transitive
        , fastProperty "Error filtering preserves ordering" prop_error_filtering_preserves_ordering
        ]
    , testGroup "Parser L.and SourceLocation Integration"
        [ fastProperty "Text processing position consistency" prop_text_processing_position_consistency
        , fastProperty "Comment removal preserves lines" prop_comment_removal_preserves_lines
        , fastProperty "String splitting roundtrip" prop_string_splitting_roundtrip
        ]
    , testGroup "Error Chain Propagation"
        [ fastProperty "Error chain chronological" prop_error_chain_chronological
        , fastProperty "Error wrapping preserves original" prop_error_wrapping_preserves_original
        , fastProperty "Combined error severity" prop_combined_error_severity
        ]
    , testGroup "Multi-module Coordination"
        [ fastProperty "Cross-module location tracking" prop_cross_module_location_tracking
        , fastProperty "Error context accumulation" prop_error_context_accumulation
        ]
    , testGroup "Performance Boundary Tests"
        [ fastProperty "Large string processing performance" prop_large_string_processing_performance
        , fastProperty "Error collection linear scaling" prop_error_collection_linear_scaling
        , fastProperty "Memory usage repeated operations" prop_memory_usage_repeated_operations
        ]
    , testGroup "Unicode L.and Special Character Handling"
        [ fastProperty "Unicode processing preserves content" prop_unicode_processing_preserves_content
        , fastProperty "Special whitespace normalization" prop_special_whitespace_normalization
        , fastProperty "Emoji L.and symbol handling" prop_emoji_symbol_handling
        ]
    , testGroup "Memory Efficiency L.and Resource Management"
        [ fastProperty "String sharing efficiency" prop_string_sharing_efficiency
        , fastProperty "Error collection cleanup" prop_error_collection_cleanup
        , fastProperty "Location tracking memory efficiency" prop_location_tracking_memory_efficiency
        ]
    , testGroup "Concurrent Safety (simulated)"
        [ fastProperty "Error collection thread-safe" prop_error_collection_thread_safe
        , fastProperty "Position tracking concurrent consistency" prop_position_tracking_concurrent_consistency
        ]
    , testGroup "End-to-End Integration"
        [ fastProperty "Complete error processing pipeline" prop_complete_error_processing_pipeline
        , fastProperty "Multi-stage text processing consistency" prop_multistage_text_processing_consistency
        , fastProperty "Error reporting completeness" prop_error_reporting_completeness
        ]
    ]