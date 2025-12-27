{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CorePropertiesQuickCheckSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Utils
import SourceLocation
import Compiler.Errors.Core
import qualified Data.Text as T
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isSpace)

-- Custom generators for our types
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endLine <- choose (posLine start, posLine start + 100)
    endColumn <- choose (if endLine == posLine start then posColumn start else 1, 1000)
    endOffset <- choose (posOffset start, posOffset start + 1000)
    let end = SourcePos endLine endColumn endOffset
    return $ SourceSpan start end

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = do
    file <- arbitrary
    line <- choose (0, 1000)
    column <- choose (0, 1000)
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ ErrorLocation file line column endLine endColumn

-- Generator for non-empty strings that don't contain the delimiter
newtype SafeString = SafeString String deriving Show

instance Arbitrary SafeString where
  arbitrary = do
    chars <- listOf $ arbitrary `suchThat` (/= ',')
    return $ SafeString chars

spec :: Spec
spec = describe "Core Properties QuickCheck Tests" $ do
  
  describe "Utils string properties" $ do
    prop "trim is idempotent" $ \str ->
      trim (trim str) === trim str
    
    prop "trim removes leading and trailing whitespace" $ \str ->
      let trimmed = trim str
          hasLeadingSpace = not (null str) && isSpace (head str)
          hasTrailingSpace = not (null str) && isSpace (last str)
      in if hasLeadingSpace || hasTrailingSpace
         then length trimmed < length str
         else trimmed === str
    
    prop "splitBy and splitByCollapsed relationship" $ \delim (SafeString content) ->
      let normal = splitBy delim content
          collapsed = splitByCollapsed delim content
      in length collapsed <= length normal &&
         all (not . null) collapsed
    
    prop "splitBy preserves total content" $ \delim str ->
      let parts = splitBy delim str
          reconstructed = intercalate [delim] parts
      in if null str
         then parts == [""]
         else reconstructed === str
    
    prop "splitByComma is splitBy with comma" $ \str ->
      splitByComma str === splitBy ',' str
    
    prop "splitByCommaCollapsed is splitByCollapsed with comma" $ \str ->
      splitByCommaCollapsed str === splitByCollapsed ',' str
    
    prop "breakOn finds substring or returns original" $ \pat str ->
      let (before, after) = breakOn pat str
      in if null pat
         then before === "" && after === str
         else if pat `isInfixOf` str
              then before ++ pat ++ after === str
              else before === str && after === ""
    
    prop "breakOn with empty pattern returns empty prefix" $ \str ->
      let (before, after) = breakOn "" str
      in before === "" && after === str

  describe "SourceLocation mathematical properties" $ do
    prop "mergeSpans is commutative" $ \span1 span2 ->
      let merged1 = mergeSpans span1 span2
          merged2 = mergeSpans span2 span1
      in merged1 === merged2
    
    prop "mergeSpans is associative" $ \span1 span2 span3 ->
      let merged12 = mergeSpans span1 span2
          merged23 = mergeSpans span2 span3
          result1 = mergeSpans merged12 span3
          result2 = mergeSpans span1 merged23
      in result1 === result2
    
    prop "mergeSpans contains both original spans" $ \span1 span2 ->
      let merged = mergeSpans span1 span2
      in spanStart merged <= spanStart span1 &&
         spanEnd merged >= spanEnd span1 &&
         spanStart merged <= spanStart span2 &&
         spanEnd merged >= spanEnd span2
    
    prop "spanBetween is ordered" $ \pos1 pos2 ->
      let span = spanBetween pos1 pos2
      in (spanStart span === min pos1 pos2) && (spanEnd span === max pos1 pos2)
    
    prop "emptySpan has zero length" $ \pos ->
      let span = emptySpan pos
      in _spanLength span === 0
    
    prop "isValidSpan checks ordering" $ \pos ->
      let validSpan = spanBetween pos (pos { posOffset = posOffset pos + 10 })
          invalidSpan = spanBetween (pos { posOffset = posOffset pos + 10 }) pos
      in isValidSpan validSpan && not (isValidSpan invalidSpan)
    
    prop "advancePosBy is consistent with repeated advancePos" $ \chars startPos ->
      let result1 = advancePosBy chars startPos
          result2 = foldl (flip advancePos) startPos chars
      in result1 === result2
    
    prop "position distance is symmetric" $ \pos1 pos2 ->
      let dist1 = _posDistance pos1 pos2
          dist2 = _posDistance pos2 pos1
      in dist1 === dist2
    
    prop "position distance is non-negative" $ \pos1 pos2 ->
      let dist = _posDistance pos1 pos2
      in dist >= 0
    
    prop "line distance is symmetric" $ \pos1 pos2 ->
      let dist1 = _lineDistance pos1 pos2
          dist2 = _lineDistance pos2 pos1
      in dist1 === dist2
    
    prop "line distance is non-negative" $ \pos1 pos2 ->
      let dist = _lineDistance pos1 pos2
      in dist >= 0

  describe "Located value properties" $ do
    prop "mapLocated preserves location" $ \span value ->
      let located = locatedWithSpan span value
          mapped = mapLocated (+1) located
      in locSpan mapped === span && locPos mapped === spanStart span
    
    prop "mapLocated is functorial" $ \span value ->
      let located = locatedWithSpan span value
          mapped1 = mapLocated (*2) (mapLocated (+1) located)
          mapped2 = mapLocated ((+1) * 2) located
      in locValue mapped1 === locValue mapped2
    
    prop "locatedAt creates empty span" $ \pos value ->
      let located = locatedAt pos value
          span = locSpan located
      in spanStart span === pos && spanEnd span === pos

  describe "Error handling properties" $ do
    prop "severity comparison is transitive" $ \sev1 sev2 sev3 ->
      let comp12 = compareSeverity sev1 sev2
          comp23 = compareSeverity sev2 sev3
          comp13 = compareSeverity sev1 sev3
      in if comp12 == EQ && comp23 == EQ
         then comp13 == EQ
         else if comp12 == LT && comp23 == LT
              then comp13 == LT
              else if comp12 == GT && comp23 == GT
                   then comp13 == GT
                   else True  -- Other cases don't guarantee transitivity
    
    prop "isAtLeast is reflexive" $ \sev ->
      isAtLeast sev sev === True
    
    prop "isAtLeast is transitive" $ \sev1 sev2 sev3 ->
      if isAtLeast sev1 sev2 && isAtLeast sev2 sev3
      then isAtLeast sev1 sev3 === True
      else True  -- Don't require anything when premises don't hold
    
    prop "detailed severity priority is consistent with base severity" $ \base sub ->
      let detailed = DetailedSeverity base sub Nothing
          basePriority = severityPriority base
          detailedPriority = detailedSeverityPriority detailed
      in detailedPriority >= basePriority
    
    prop "custom recovery creates strategy with given values" $ \canRec shouldCont cost confidence ->
      let custom = customRecovery canRec shouldCont Nothing Nothing cost confidence
      in canRecover custom === canRec &&
         shouldContinue custom === shouldCont &&
         recoveryCost custom === cost &&
         recoveryConfidence custom === confidence

  describe "Error collection properties" $ do
    prop "getErrors only returns Error or Fatal severity" $ \errors ->
      let errorList = getErrors errors
      in all (\e -> severity e == Error || severity e == Fatal) errorList
    
    prop "getWarnings only returns Warning severity" $ \errors ->
      let warningList = getWarnings errors
      in all (\e -> severity e == Warning) warningList
    
    prop "getInfo only returns Info severity" $ \errors ->
      let infoList = getInfo errors
      in all (\e -> severity e == Info) infoList
    
    prop "getAllMessages preserves all errors" $ \errors ->
      getAllMessages errors === errors
    
    prop "hasErrors is true iff getErrors is non-empty" $ \errors ->
      hasErrors errors === not (null (getErrors errors))
    
    prop "hasWarnings is true iff getWarnings is non-empty" $ \errors ->
      hasWarnings errors === not (null (getWarnings errors))

  describe "Error recovery properties" $ do
    prop "sequence recovery combines costs additively" $ \rec1 rec2 ->
      let combined = _sequenceRecovery rec1 rec2
      in recoveryCost combined === recoveryCost rec1 + recoveryCost rec2
    
    prop "sequence recovery averages confidence" $ \rec1 rec2 ->
      let combined = _sequenceRecovery rec1 rec2
          expected = (recoveryConfidence rec1 + recoveryConfidence rec2) / 2
      in abs (recoveryConfidence combined - expected) < 0.001
    
    prop "sequence recovery requires both to be recoverable" $ \rec1 rec2 ->
      let combined = _sequenceRecovery rec1 rec2
      in canRecover combined === (canRecover rec1 && canRecover rec2)
    
    prop "sequence recovery requires both to continue" $ \rec1 rec2 ->
      let combined = _sequenceRecovery rec1 rec2
      in shouldContinue combined === (shouldContinue rec1 && shouldContinue rec2)

  describe "RecoveryContext properties" $ do
    prop "adding recovery attempt increments attempt count" $ \ctx strategy ->
      let updated = _addRecoveryAttempt strategy True ctx
      in recoveryAttempts updated === recoveryAttempts ctx + 1
    
    prop "success rate is between 0 and 1" $ \ctx ->
      let rate = _recoverySuccessRate ctx
      in rate >= 0.0 && rate <= 1.0
    
    prop "empty context has 0 success rate" $ 
      let ctx = _initialRecoveryContext 5
      in _recoverySuccessRate ctx === 0.0
    
    prop "successful only context has 1.0 success rate" $ \ctx ->
      let updated = _addRecoveryAttempt errorRecovery True ctx
      in _recoverySuccessRate updated === 1.0

  describe "CombinedError properties" $ do
    prop "filterCombinedErrorsBySeverity preserves ordering" $ \sev errors ->
      let filtered = filterCombinedErrorsBySeverity sev errors
      in all (\e -> combinedErrorSeverity e >= sev) filtered
    
    prop "combinedErrorSeverity is monotonic with filter" $ \sev1 sev2 errors ->
      if compareSeverity sev1 sev2 == LT
      then length (filterCombinedErrorsBySeverity sev2 errors) <= 
           length (filterCombinedErrorsBySeverity sev1 errors)
      else True