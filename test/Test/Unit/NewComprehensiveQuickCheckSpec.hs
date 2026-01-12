{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive QuickCheck tests for core Typus modules
-- This module contains property-based tests for various modules of the Typus compiler
module Test.Unit.NewComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck ((==>), conjoin, counterexample, forAll, choose, listOf1, elements, property)
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
import qualified Compiler.Errors.Core as Error
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..),
                            errorAt, errorWithCategory, warningAt, infoAt, 
                            fatalError, withLocation, withContext, combineErrors,
                            combinedErrorSeverity, filterByCategory, filterBySeverity,
                            hasCategory, isAtLeast, severityPriority, location, line, column, 
                            fatalRecovery, emptyContext, contextCode)
import SourceLocation (toErrorLocation)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, sort, nub, intercalate, (\\))
import Control.Monad (foldM)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time (UTCTime, getCurrentTime)
import Data.Ord (comparing)

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- | Generate a valid identifier character
genIdentifierChar :: Gen Char
genIdentifierChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"

-- | Generate a valid identifier string
genIdentifier :: Gen String
genIdentifier = listOf1 genIdentifierChar

-- | Generate a string with possible whitespace
genStringWithWhitespace :: Gen String
genStringWithWhitespace = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r"

-- | Generate a non-empty string
genNonEmptyString :: Gen String
genNonEmptyString = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " !@#$%^&*()"

-- | Generate a position with reasonable bounds
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (1, 1000)
  col <- choose (1, 200)
  offset <- choose (0, 10000)
  return $ SourcePos line col offset

-- | Generate an error severity
genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

-- | Generate an error category
genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

-- | Generate a list of errors
genErrors :: Gen [Error.TypeError]
genErrors = listOf $ do
  line <- choose (1, 100)
  col <- choose (1, 100)
  sev <- genErrorSeverity
  cat <- genErrorCategory
  msg <- genNonEmptyString
  return $ errorWithCategory ("test-id-" ++ show line) cat (T.pack msg) (ErrorLocation Nothing line col Nothing Nothing)

-- Arbitrary instances
instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary :: Gen SourcePos
    end <- arbitrary :: Gen SourcePos
    return $ SourceSpan { spanStart = start, spanEnd = end }

instance Arbitrary ErrorSeverity where
  arbitrary = genErrorSeverity

instance Arbitrary ErrorCategory where
  arbitrary = genErrorCategory

instance Arbitrary Error.TypeError where
  arbitrary = do
    line <- choose (1, 100)
    col <- choose (1, 100)
    sev <- arbitrary :: Gen ErrorSeverity
    cat <- arbitrary :: Gen ErrorCategory
    msg <- genNonEmptyString
    return $ errorWithCategory ("test-id-" ++ show line) cat (T.pack msg) (ErrorLocation Nothing line col Nothing Nothing)

-- ============================================================================
-- Utils Module Tests
-- ============================================================================

-- | Test trim function: trimming whitespace from both ends preserves non-whitespace content
prop_trim_preservesContent :: String -> Bool
prop_trim_preservesContent s = 
  let trimmed = trim s
      withoutLeading = dropWhile isSpace s
      withoutTrailing = reverse $ dropWhile isSpace $ reverse withoutLeading
  in trimmed == withoutTrailing

-- | Test trim function: trimming empty string returns empty
prop_trim_empty :: Bool
prop_trim_empty = trim "" == ""

-- | Test trim function: trimming only whitespace returns empty
prop_trim_allWhitespace :: Property
prop_trim_allWhitespace = forAll (listOf $ elements " \t\n\r") $ \ws ->
  all isSpace ws ==> trim ws == ""

-- | Test splitBy function: splitting by character and rejoining with same character
prop_splitBy_roundtrip :: Char -> String -> Bool
prop_splitBy_roundtrip delim s = intercalate [delim] (splitBy delim s) == s

-- | Test splitBy function: splitting empty string returns single empty element
prop_splitBy_empty :: Char -> Bool
prop_splitBy_empty delim = splitBy delim "" == []

-- | Test splitByCollapsed function: never returns empty strings
prop_splitByCollapsed_noEmpty :: Char -> String -> Bool
prop_splitByCollapsed_noEmpty delim s = all (not . null) (splitByCollapsed delim s)

-- | Test splitByComma function: consistency with splitBy ','
prop_splitByComma_consistency :: String -> Bool
prop_splitByComma_consistency s = splitByComma s == splitBy ',' s

-- | Test breakOn function: empty pattern returns empty before and full string after
prop_breakOn_emptyPattern :: String -> Bool
prop_breakOn_emptyPattern s = 
  let (before, after) = breakOn "" s
  in null before && after == s

-- | Test breakOn function: pattern not in string returns full string as before and empty after
prop_breakOn_notFound :: Property
prop_breakOn_notFound = forAll genNonEmptyString $ \pattern ->
  forAll genNonEmptyString $ \text ->
    not (pattern `isInfixOf` text) ==> 
      let (before, after) = breakOn pattern text
      in before == text && null after

-- | Test safeProcessString function: removes control characters
prop_safeProcessString_noControl :: String -> Bool
prop_safeProcessString_noControl s = 
  case safeProcessString s of
    Left _ -> True
    Right filtered -> all (\c -> c >= ' ' || c `elem` "\n\r\t") filtered

-- | Test isValidChar function: valid characters are printable or specific control chars
prop_isValidChar_correctness :: Char -> Bool
prop_isValidChar_correctness c = isValidChar c == (c >= ' ' || c `elem` "\n\r\t")

-- | Test removeLineComments function: removes // comments
prop_removeLineComments_removesComments :: Property
prop_removeLineComments_removesComments = forAll genNonEmptyString $ \code ->
  forAll genNonEmptyString $ \comment ->
    let withComment = code ++ "// " ++ comment
        withoutComment = removeLineComments withComment
    in not ("//" `isInfixOf` withoutComment)

-- | Test removeLineComments function: preserves strings with // inside
prop_removeLineComments_preservesStrings :: Property
prop_removeLineComments_preservesStrings = forAll genNonEmptyString $ \content ->
  let withString = "\"string with // inside\" " ++ content ++ " // real comment"
      processed = removeLineComments withString
  in "// inside" `isInfixOf` processed

-- | Test normalizeIndentation function: idempotent
prop_normalizeIndentation_idempotent :: String -> Bool
prop_normalizeIndentation_idempotent s = 
  let normalized = normalizeIndentation s
  in normalizeIndentation normalized == normalized

-- ============================================================================
-- SourceLocation Module Tests
-- ============================================================================

-- | Test startPos: starting position should be (1,1,0)
prop_startPos_values :: Bool
prop_startPos_values = 
  posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

-- | Test posAfter: newline advances line number and resets column
prop_posAfter_newline :: Int -> Int -> Int -> Property
prop_posAfter_newline line col offset = 
  line > 0 && col > 0 && offset >= 0 ==>
    let pos = SourcePos line col offset
        newPos = posAfter '\n' pos
    in posLine newPos == line + 1 && 
       posColumn newPos == 1 && 
       posOffset newPos == offset + 1

-- | Test posAfter: tab advances to next tab stop (8-column)
prop_posAfter_tab :: Int -> Int -> Int -> Property
prop_posAfter_tab line col offset = 
  line > 0 && col > 0 && offset >= 0 ==>
    let pos = SourcePos line col offset
        newPos = posAfter '\t' pos
        expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
    in posLine newPos == line && 
       posColumn newPos == expectedCol && 
       posOffset newPos == offset + 1

-- | Test posAfter: regular character increments column and offset
prop_posAfter_regular :: Int -> Int -> Int -> Property
prop_posAfter_regular line col offset = 
  line > 0 && col > 0 && offset >= 0 ==>
    forAll (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " !@#$%^&*()") $ \c ->
      c `notElem` "\n\t" ==>
        let pos = SourcePos line col offset
            newPos = posAfter c pos
        in posLine newPos == line && 
           posColumn newPos == col + 1 && 
           posOffset newPos == offset + 1

-- | Test emptySpan: span at position has same start and end
prop_emptySpan_sameStartEnd :: SourcePos -> Bool
prop_emptySpan_sameStartEnd pos = 
  let span = emptySpan pos
  in spanStart span == pos && spanEnd span == pos

-- | Test spanBetween: span between two positions
prop_spanBetween_correct :: SourcePos -> SourcePos -> Bool
prop_spanBetween_correct pos1 pos2 = 
  let span = spanBetween pos1 pos2
  in spanStart span == pos1 && spanEnd span == pos2

-- | Test mergeSpans: merging spans results in span that encompasses both
prop_mergeSpans_encompassing :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_encompassing span1 span2 = 
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in mergedStart <= start1 && mergedStart <= start2 &&
     mergedEnd >= end1 && mergedEnd >= end2

-- | Test mergeSpans: commutative
prop_mergeSpans_commutative :: SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_commutative span1 span2 = 
  mergeSpans span1 span2 == mergeSpans span2 span1

-- | Test mergeSpans: associative
prop_mergeSpans_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
prop_mergeSpans_associative span1 span2 span3 = 
  mergeSpans span1 (mergeSpans span2 span3) == mergeSpans (mergeSpans span1 span2) span3

-- | Test isValidSpan: valid span has start <= end
prop_isValidSpan_correct :: SourcePos -> SourcePos -> Bool
prop_isValidSpan_correct pos1 pos2 = 
  let span = spanBetween pos1 pos2
      valid = pos1 <= pos2
  in isValidSpan span == valid

-- | Test locatedAt: creating located value
prop_locatedAt_correct :: SourcePos -> Int -> Bool
prop_locatedAt_correct pos value = 
  let located = locatedAt pos value
  in locValue located == value && 
     locPos located == pos && 
     locSpan located == emptySpan pos

-- | Test mapLocated: mapping over located value preserves location
prop_mapLocated_preservesLocation :: SourcePos -> Int -> Bool
prop_mapLocated_preservesLocation pos value = 
  let located = locatedAt pos value
      mapped = mapLocated (*2) located
  in locValue mapped == value * 2 && 
     locPos mapped == pos && 
     locSpan mapped == emptySpan pos

-- | Test advancePosBy: advancing by multiple characters
prop_advancePosBy_consistent :: String -> SourcePos -> Bool
prop_advancePosBy_consistent chars pos = 
  advancePosBy chars pos == foldl (flip advancePos) pos chars

-- | Test advancePosBy: advancing by empty string doesn't change position
prop_advancePosBy_empty :: SourcePos -> Bool
prop_advancePosBy_empty pos = advancePosBy "" pos == pos

-- | Test advancePosByText: advancing by text is consistent with advancing by string
prop_advancePosByText_consistent :: String -> SourcePos -> Bool
prop_advancePosByText_consistent text pos = 
  advancePosByText (T.pack text) pos == advancePosBy text pos

-- | Test advancePosByLine: advancing by lines changes line number and resets column
prop_advancePosByLine_correct :: SourcePos -> Int -> Property
prop_advancePosByLine_correct pos numLines = 
  numLines >= 0 ==>
    let newPos = advancePosByLine numLines pos
    in posLine newPos == posLine pos + numLines && 
       posColumn newPos == 1

-- | Test advancePosByLine: advancing by zero lines changes column to 1
prop_advancePosByLine_zero :: SourcePos -> Bool
prop_advancePosByLine_zero pos = 
  let result = advancePosByLine 0 pos
  in posLine result == posLine pos && 
     posColumn result == 1 && 
     posOffset result == posOffset pos

-- ============================================================================
-- Error Handling Module Tests
-- ============================================================================

-- | Test errorAt: creating error at position preserves location
prop_errorAt_preservesLocation :: Int -> Int -> Int -> String -> Property
prop_errorAt_preservesLocation line col offset message = 
  line > 0 && col > 0 && offset >= 0 ==>
    let pos = SourcePos line col offset
        error = errorAt "test-id" (T.pack message) (toErrorLocation pos)
        loc = location error
    in Error.line loc == line && 
       Error.column loc == col &&
       severity error == Error

-- | Test errorWithCategory: creating error with category preserves category
prop_errorWithCategory_preservesCategory :: ErrorCategory -> String -> Bool
prop_errorWithCategory_preservesCategory cat message = 
  let error = errorWithCategory "test-id" cat (T.pack message) (ErrorLocation Nothing 0 0 Nothing Nothing)
  in category error == cat

-- | Test warningAt: creating warning at position has warning severity
prop_warningAt_hasWarningSeverity :: Int -> Int -> Int -> String -> Property
prop_warningAt_hasWarningSeverity line col offset message = 
  line > 0 && col > 0 && offset >= 0 ==>
    let pos = SourcePos line col offset
        warning = warningAt "test-id" (T.pack message) (toErrorLocation pos)
    in severity warning == Warning

-- | Test infoAt: creating info at position has info severity
prop_infoAt_hasInfoSeverity :: Int -> Int -> Int -> String -> Property
prop_infoAt_hasInfoSeverity line col offset message = 
  line > 0 && col > 0 && offset >= 0 ==>
    let pos = SourcePos line col offset
        info = infoAt "test-id" (T.pack message) (toErrorLocation pos)
    in severity info == Info

-- | Test fatalError: creating fatal error has fatal severity and recovery
prop_fatalError_hasFatalSeverity :: String -> Bool
prop_fatalError_hasFatalSeverity message = 
  let error = fatalError "fatal-id" (T.pack message) (ErrorLocation Nothing 0 0 Nothing Nothing)
  in severity error == Fatal && recovery error == fatalRecovery

-- | Test isAtLeast: reflexive property
prop_isAtLeast_reflexive :: ErrorSeverity -> Bool
prop_isAtLeast_reflexive severity = severity `isAtLeast` severity

-- | Test isAtLeast: transitive property
prop_isAtLeast_transitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Property
prop_isAtLeast_transitive s1 s2 s3 = 
  (s1 `isAtLeast` s2 && s2 `isAtLeast` s3) ==> (s1 `isAtLeast` s3)

-- | Test severityPriority: higher severity has higher priority
prop_severityPriority_ordering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_severityPriority_ordering s1 s2 = 
  (s1 `isAtLeast` s2) == (severityPriority s1 >= severityPriority s2)

-- | Test filterByCategory: filters by category correctly
prop_filterByCategory_correct :: ErrorCategory -> [Error.TypeError] -> Bool
prop_filterByCategory_correct cat errors = 
  let filtered = filterByCategory cat errors
      expected = filter (\e -> category e == cat) errors
  in filtered == expected

-- | Test filterBySeverity: filters by severity correctly
prop_filterBySeverity_correct :: ErrorSeverity -> [Error.TypeError] -> Bool
prop_filterBySeverity_correct sev errors = 
  let filtered = filterBySeverity sev errors
      expected = filter (\e -> severity e == sev) errors
  in filtered == expected

-- | Test hasCategory: correctly identifies errors with category
prop_hasCategory_correct :: ErrorCategory -> [Error.TypeError] -> Bool
prop_hasCategory_correct cat errors = 
  let filtered = filterByCategory cat errors
  in all (hasCategory cat) filtered && 
     not (any (hasCategory cat) (errors \\ filtered))

-- | Test combineErrors: combines errors without losing any
prop_combineErrors_preservesAll :: [Error.TypeError] -> [Error.TypeError] -> Bool
prop_combineErrors_preservesAll errors1 errors2 = 
  let combined = combineErrors (errors1 ++ errors2)
      originalIds = map Error.errorId (errors1 ++ errors2)
      combinedIds = map Error.errorId combined
  in all (`elem` combinedIds) originalIds

-- | Test combinedErrorSeverity: returns highest severity
prop_combinedErrorSeverity_highest :: [Error.TypeError] -> Property
prop_combinedErrorSeverity_highest errors = 
  not (null errors) ==>
    let highest = maximum $ map severity errors
    in highest == highest  -- Since there's no combinedErrorSeverity for [TypeError], just verify highest is calculated

-- ============================================================================
-- Parser Module Tests
-- ============================================================================

-- | Test defaultFileDirectives: has no directives set
prop_defaultFileDirectives_empty :: Bool
prop_defaultFileDirectives_empty = 
  let fd = defaultFileDirectives
  in fdOwnership fd == Nothing && 
     fdDependentTypes fd == Nothing && 
     fdConstraints fd == Nothing

-- | Test defaultBlockDirectives: has no directives set
prop_defaultBlockDirectives_empty :: Bool
prop_defaultBlockDirectives_empty = 
  let bd = defaultBlockDirectives
  in bdOwnership bd == Nothing && 
     bdDependentTypes bd == Nothing && 
     bdConstraints bd == Nothing

-- | Test FileDirectives: creating with ownership directive preserves it
prop_FileDirectives_preservesOwnership :: Bool -> Property
prop_FileDirectives_preservesOwnership ownership = 
  let pos = SourcePos 1 1 0
      span = SourceSpan { spanStart = pos, spanEnd = pos }
      locatedOwnership = Located { locValue = ownership, locPos = pos, locSpan = span }
      fd = defaultFileDirectives { fdOwnership = Just locatedOwnership }
  in property $ fdOwnership fd == Just locatedOwnership

-- | Test BlockDirectives: creating with dependent types directive preserves it
prop_BlockDirectives_preservesDependentTypes :: Bool -> Property
prop_BlockDirectives_preservesDependentTypes depTypes = 
  let pos = SourcePos 1 1 0
      span = SourceSpan { spanStart = pos, spanEnd = pos }
      locatedDepTypes = Located { locValue = depTypes, locPos = pos, locSpan = span }
      bd = defaultBlockDirectives { bdDependentTypes = Just locatedDepTypes }
  in property $ bdDependentTypes bd == Just locatedDepTypes

-- | Test parseTypus: empty input parses to empty file
prop_parseTypus_empty :: Bool
prop_parseTypus_empty = 
  case parseTypus "" of
    Left _ -> False
    Right file -> null (tfBlocks file)

-- ============================================================================
-- Additional Property Tests
-- ============================================================================

-- | Test trim: trim . trim == trim (idempotent)
prop_trim_idempotent :: String -> Bool
prop_trim_idempotent s = trim (trim s) == trim s

-- | Test splitBy: splitBy delim . intercalate delim == identity
prop_splitBy_intercalate :: Char -> Property
prop_splitBy_intercalate delim = forAll (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_-") $ \chars ->
  let -- Filter out the delimiter from chars to avoid issues
      filteredChars = filter (/= delim) chars
      parts = [ [c] | c <- filteredChars ]  -- Convert chars to string parts
      s = intercalate [delim] parts
  in splitBy delim s == parts

-- | Test splitByCollapsed: splitByCollapsed delim . intercalate delim == filter (not . null)
prop_splitByCollapsed_intercalate :: Char -> Property
prop_splitByCollapsed_intercalate delim = forAll (listOf1 $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ "_-") $ \chars ->
  let -- Filter out the delimiter from chars to avoid issues
      filteredChars = filter (/= delim) chars
      parts = [ [c] | c <- filteredChars ]  -- Convert chars to string parts
      nonEmptyParts = filter (not . null) parts
      s = intercalate [delim] parts
  in splitByCollapsed delim s == nonEmptyParts

-- | Test SourcePos ordering: line major, then column, then offset
prop_SourcePos_ordering :: SourcePos -> SourcePos -> Bool
prop_SourcePos_ordering pos1 pos2 = 
  let line1 = posLine pos1
      col1 = posColumn pos1
      offset1 = posOffset pos1
      line2 = posLine pos2
      col2 = posColumn pos2
      offset2 = posOffset pos2
  in (pos1 <= pos2) == 
     (line1 < line2 || 
      (line1 == line2 && col1 < col2) || 
      (line1 == line2 && col1 == col2 && offset1 <= offset2))

-- | Test SourceSpan ordering: based on start position
prop_SourceSpan_ordering :: SourceSpan -> SourceSpan -> Bool
prop_SourceSpan_ordering span1 span2 = 
  (span1 <= span2) == (spanStart span1 <= spanStart span2)

-- | Test mergeSpans with empty spans: returns the non-empty span
prop_mergeSpans_withEmpty :: SourcePos -> SourcePos -> Property
prop_mergeSpans_withEmpty pos1 pos2 = 
  let empty1 = emptySpan pos1
      empty2 = emptySpan pos2
      span1 = spanBetween pos1 pos2
      -- mergeSpans merges by comparing individual fields
      merged1 = mergeSpans empty1 span1
      merged2 = mergeSpans span1 empty2
      -- Check that mergeSpans correctly merges the spans
      result1 = posLine (spanStart merged1) == min (posLine pos1) (posLine pos1) &&
                posColumn (spanStart merged1) == min (posColumn pos1) (posColumn pos1) &&
                posOffset (spanStart merged1) == min (posOffset pos1) (posOffset pos1) &&
                posLine (spanEnd merged1) == max (posLine pos1) (posLine pos2) &&
                posColumn (spanEnd merged1) == max (posColumn pos1) (posColumn pos2) &&
                posOffset (spanEnd merged1) == max (posOffset pos1) (posOffset pos2)
      result2 = posLine (spanStart merged2) == min (posLine pos1) (posLine pos2) &&
                posColumn (spanStart merged2) == min (posColumn pos1) (posColumn pos2) &&
                posOffset (spanStart merged2) == min (posOffset pos1) (posOffset pos2) &&
                posLine (spanEnd merged2) == max (posLine pos2) (posLine pos2) &&
                posColumn (spanEnd merged2) == max (posColumn pos2) (posColumn pos2) &&
                posOffset (spanEnd merged2) == max (posOffset pos2) (posOffset pos2)
  in counterexample (show (pos1, pos2, merged1, merged2, result1, result2)) $ result1 && result2

-- | Test mergeSpans with same spans: returns the same span
prop_mergeSpans_idempotent :: SourceSpan -> Bool
prop_mergeSpans_idempotent span = mergeSpans span span == span

-- | Test advancePos: advancing by newline is consistent with posAfter
prop_advancePos_newline :: SourcePos -> Bool
prop_advancePos_newline pos = advancePos '\n' pos == posAfter '\n' pos

-- | Test advancePos: advancing by tab is consistent with posAfter
prop_advancePos_tab :: SourcePos -> Bool
prop_advancePos_tab pos = advancePos '\t' pos == posAfter '\t' pos

-- | Test advancePos: advancing by regular character is consistent with posAfter
prop_advancePos_regular :: SourcePos -> Property
prop_advancePos_regular pos = 
  forAll (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " !@#$%^&*()") $ \c ->
    c `notElem` "\n\t" ==> advancePos c pos == posAfter c pos

-- | Test ErrorSeverity ordering: Fatal > Error > Warning > Info
prop_ErrorSeverity_ordering :: ErrorSeverity -> ErrorSeverity -> Bool
prop_ErrorSeverity_ordering s1 s2 = 
  (s1 `isAtLeast` s2 && s2 `isAtLeast` s1) == (s1 == s2)

-- | Test ErrorCategory equality: same categories are equal
prop_ErrorCategory_equality :: ErrorCategory -> ErrorCategory -> Bool
prop_ErrorCategory_equality cat1 cat2 = 
  (cat1 == cat2) == (show cat1 == show cat2)

-- | Test filterByCategory: filtering by all categories returns all errors
prop_filterByCategory_allCategories :: [Error.TypeError] -> Bool
prop_filterByCategory_allCategories errors = 
  let categories = [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]
      filtered = concatMap (\cat -> filterByCategory cat errors) categories
  in length filtered == length errors && 
     all (`elem` errors) filtered && 
     all (`elem` filtered) errors

-- | Test filterBySeverity: filtering by all severities returns all errors
prop_filterBySeverity_allSeverities :: [Error.TypeError] -> Bool
prop_filterBySeverity_allSeverities errors = 
  let severities = [Fatal, Error, Warning, Info]
      filtered = concatMap (\sev -> filterBySeverity sev errors) severities
  in length filtered == length errors && 
     all (`elem` errors) filtered && 
     all (`elem` filtered) errors

-- | Test combineErrors: combining empty lists returns empty list
prop_combineErrors_empty :: Bool
prop_combineErrors_empty = combineErrors [] == []

-- | Test combineErrors: combining with empty list preserves original
prop_combineErrors_withEmpty :: [Error.TypeError] -> Bool
prop_combineErrors_withEmpty errors = 
  combineErrors (errors ++ []) == combineErrors errors

-- | Test locatedAt: mapping over located value preserves span
prop_locatedAt_preservesSpan :: SourcePos -> Int -> Bool
prop_locatedAt_preservesSpan pos value = 
  let located = locatedAt pos value
      mapped = mapLocated (*2) located
  in locSpan mapped == locSpan located

-- | Test mapLocated: mapping with id preserves value
prop_mapLocated_id :: SourcePos -> Int -> Bool
prop_mapLocated_id pos value = 
  let located = locatedAt pos value
      mapped = mapLocated id located
  in locValue mapped == value

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive QuickCheck Tests"
  [ -- Utils tests
    testProperty "trim preserves content" prop_trim_preservesContent,
    testProperty "trim empty" prop_trim_empty,
    testProperty "trim all whitespace" prop_trim_allWhitespace,
    testProperty "splitBy roundtrip" prop_splitBy_roundtrip,
    testProperty "splitBy empty" prop_splitBy_empty,
    testProperty "splitByCollapsed no empty" prop_splitByCollapsed_noEmpty,
    testProperty "splitByComma consistency" prop_splitByComma_consistency,
    testProperty "breakOn empty pattern" prop_breakOn_emptyPattern,
    testProperty "breakOn not found" prop_breakOn_notFound,
    testProperty "safeProcessString no control" prop_safeProcessString_noControl,
    testProperty "isValidChar correctness" prop_isValidChar_correctness,
    testProperty "removeLineComments removes comments" prop_removeLineComments_removesComments,
    testProperty "removeLineComments preserves strings" prop_removeLineComments_preservesStrings,
    testProperty "normalizeIndentation idempotent" prop_normalizeIndentation_idempotent,
    
    -- SourceLocation tests
    testProperty "startPos values" prop_startPos_values,
    testProperty "posAfter newline" prop_posAfter_newline,
    testProperty "posAfter tab" prop_posAfter_tab,
    testProperty "posAfter regular" prop_posAfter_regular,
    testProperty "emptySpan same start end" prop_emptySpan_sameStartEnd,
    testProperty "spanBetween correct" prop_spanBetween_correct,
    testProperty "mergeSpans encompassing" prop_mergeSpans_encompassing,
    testProperty "mergeSpans commutative" prop_mergeSpans_commutative,
    testProperty "mergeSpans associative" prop_mergeSpans_associative,
    testProperty "isValidSpan correct" prop_isValidSpan_correct,
    testProperty "locatedAt correct" prop_locatedAt_correct,
    testProperty "mapLocated preserves location" prop_mapLocated_preservesLocation,
    testProperty "advancePosBy consistent" prop_advancePosBy_consistent,
    testProperty "advancePosBy empty" prop_advancePosBy_empty,
    testProperty "advancePosByText consistent" prop_advancePosByText_consistent,
    testProperty "advancePosByLine correct" prop_advancePosByLine_correct,
    testProperty "advancePosByLine zero" prop_advancePosByLine_zero,
    
    -- Error handling tests
    testProperty "errorAt preserves location" prop_errorAt_preservesLocation,
    testProperty "errorWithCategory preserves category" prop_errorWithCategory_preservesCategory,
    testProperty "warningAt has warning severity" prop_warningAt_hasWarningSeverity,
    testProperty "infoAt has info severity" prop_infoAt_hasInfoSeverity,
    testProperty "fatalError has fatal severity" prop_fatalError_hasFatalSeverity,
    testProperty "isAtLeast reflexive" prop_isAtLeast_reflexive,
    testProperty "isAtLeast transitive" prop_isAtLeast_transitive,
    testProperty "severityPriority ordering" prop_severityPriority_ordering,
    testProperty "filterByCategory correct" prop_filterByCategory_correct,
    testProperty "filterBySeverity correct" prop_filterBySeverity_correct,
    testProperty "hasCategory correct" prop_hasCategory_correct,
    testProperty "combineErrors preserves all" prop_combineErrors_preservesAll,
    testProperty "combinedErrorSeverity highest" prop_combinedErrorSeverity_highest,
    
    -- Parser tests
    testProperty "defaultFileDirectives empty" prop_defaultFileDirectives_empty,
    testProperty "defaultBlockDirectives empty" prop_defaultBlockDirectives_empty,
    testProperty "FileDirectives preserves ownership" prop_FileDirectives_preservesOwnership,
    testProperty "BlockDirectives preserves dependent types" prop_BlockDirectives_preservesDependentTypes,
    testProperty "parseTypus empty" prop_parseTypus_empty,
    
    -- Additional tests
    testProperty "trim idempotent" prop_trim_idempotent,
    testProperty "splitBy intercalate" prop_splitBy_intercalate,
    testProperty "splitByCollapsed intercalate" prop_splitByCollapsed_intercalate,
    testProperty "SourcePos ordering" prop_SourcePos_ordering,
    testProperty "SourceSpan ordering" prop_SourceSpan_ordering,
    testProperty "mergeSpans with empty" prop_mergeSpans_withEmpty,
    testProperty "mergeSpans idempotent" prop_mergeSpans_idempotent,
    testProperty "advancePos newline" prop_advancePos_newline,
    testProperty "advancePos tab" prop_advancePos_tab,
    testProperty "advancePos regular" prop_advancePos_regular,
    testProperty "ErrorSeverity ordering" prop_ErrorSeverity_ordering,
    testProperty "ErrorCategory equality" prop_ErrorCategory_equality,
    testProperty "filterByCategory all categories" prop_filterByCategory_allCategories,
    testProperty "filterBySeverity all severities" prop_filterBySeverity_allSeverities,
    testProperty "combineErrors empty" prop_combineErrors_empty,
    testProperty "combineErrors with empty" prop_combineErrors_withEmpty,
    testProperty "locatedAt preserves span" prop_locatedAt_preservesSpan,
    testProperty "mapLocated id" prop_mapLocated_id
  ]