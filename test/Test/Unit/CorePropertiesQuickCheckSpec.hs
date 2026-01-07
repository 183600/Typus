module Test.Unit.CorePropertiesQuickCheckSpec where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..))
-- import Test.Hspec
import Utils
import SourceLocation
import Compiler.Errors.Core
import qualified Data.Text as T
import qualified Data.List as L
import Data.List 
              line <- choose (1, 1000)
    column <- choose (1, 1000)
    offset <- choose (0, 10000)
    return $ SourcePos line column offset

instance Arbitrary SourceSpan where
                                              arbitrary = do
              start <- arbitrary
    endLine <- choose (posLine start, posLine start + 100)
    endColumn <- choose (if                               endLine == posLine start then posColumn start else 1, 1000)
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
newtype                               SafeString = SafeString String deriving Show

instance Arbitrary SafeString where
                                              arbitrary = do
              chars <- listOf $ arbitrary `suchThat` (/= ',')
    return $ SafeString chars

tests :: TestTree
tests =   testGroup "Core Properties QuickCheck Tests"
  [ testGroup "Utils string properties"
    [             testProperty "trim is idempotent" $ \str ->
trim (trim str) === trim str
        
    ,             testProperty "trim removes leading and trailing whitespace" $ \str ->
        let trimmed = trim str
                                          hasLeadingSpace = not (null str) && isSpace (L.head str)
                                          hasTrailingSpace = not (null str) && isSpace (last str)
        in if hasLeadingSpace || hasTrailingSpace
           then property $ L.length trimmed < L.length str
           else                               trimmed === str
           
    ,             testProperty "splitBy and splitByCollapsed relationship" $ \delim (SafeString content) ->
        let normal = splitBy delim content
                                          collapsed = splitByCollapsed delim content
        in L.length collapsed <= L.length normal &&
           L.all (not . null) collapsed
    
    ,             testProperty "splitBy preserves total content" $ \delim str ->
        let parts = splitBy delim str
                                          reconstructed = intercalate [delim] parts
        in if null str
           then property $                               parts == [""]
           else                               reconstructed === str
    
    ,             testProperty "splitByComma is splitBy with comma" $ \str ->
        splitByComma                               str === splitBy ',' str
    
    ,             testProperty "splitByCommaCollapsed is splitByCollapsed with comma" $ \str ->
        splitByCommaCollapsed                               str === splitByCollapsed ',' str
    
    ,             testProperty "breakOn finds substring or returns original" $ \pat str ->
        let (before, after) = breakOn pat str
        in if null pat
           then                               before == "" &&                               after == str
           else if pat `L.isInfixOf` str
                then before ++ pat ++                               after === str
                else                               before == str &&                               after == ""
    
    ,             testProperty "breakOn with empty pattern returns empty prefix" $ \str ->
        let (before, after) = breakOn "" str
        in                               before == "" &&                               after == str
    ]
    
  , testGroup "SourceLocation mathematical properties"
    [             testProperty "mergeSpans is commutative" $ \span1 span2 ->
        let merged1 = mergeSpans span1 span2
                                          merged2 = mergeSpans span2 span1
        in                               merged1 === merged2
    
    ,             testProperty "mergeSpans is associative" $ \span1 span2 span3 ->
        let merged12 = mergeSpans span1 span2
                                          merged23 = mergeSpans span2 span3
                                          result1 = mergeSpans merged12 span3
                                          result2 = mergeSpans span1 merged23
        in                               result1 === result2
    
    ,             testProperty "mergeSpans contains both original spans" $ \span1 span2 ->
        let merged = mergeSpans span1 span2
        in spanStart merged <= spanStart span1 &&
           spanEnd merged >= spanEnd span1 &&
           spanStart merged <= spanStart span2 &&
           spanEnd merged >= spanEnd span2
    
    ,             testProperty "spanBetween is ordered" $ \pos1 pos2 ->
        let span = spanBetween pos1 pos2
        in (spanStart                               span == min pos1 pos2) && (spanEnd                               span == max pos2 pos2)
    
    --             testProperty "emptySpan has zero length" $ \pos ->
        --     let span = emptySpan pos
        --     in _spanLength                               span === 0
        -- Temporarily disabled - _spanLength not implemented
    
    --             testProperty "isValidSpan checks ordering" $ \pos ->
--     let validSpan = spanBetween pos (pos {                               posOffset = posOffset pos + 10 })
--                                       invalidSpan = spanBetween (pos {                               posOffset = posOffset pos + 10 }) pos
--     in isValidSpan validSpan && not (isValidSpan invalidSpan)
    
    ,             testProperty "advancePosBy is consistent with repeated advancePos" $ \chars startPos ->
        let result1 = advancePosBy chars startPos
                                          result2 = L.foldl (flip advancePos) startPos chars
        in                               result1 === result2
    
    --             testProperty "position distance is symmetric" $ \pos1 pos2 ->
        --     let dist1 = _posDistance pos1 pos2
        --                                       dist2 = _posDistance pos2 pos1
        --     in                               dist1 === dist2
    
    --             testProperty "position distance is non-negative" $ \pos1 pos2 ->
        --     let dist = _posDistance pos1 pos2
        --     in dist >= 0
        -- Temporarily disabled - _posDistance not implemented
    
    --             testProperty "line distance is symmetric" $ \pos1 pos2 ->
--     let dist1 = _lineDistance pos1 pos2
--                                       dist2 = _lineDistance pos2 pos1
--     in                               dist1 === dist2
    
--             testProperty "line distance is non-negative" $ \pos1 pos2 ->
--     let dist = _lineDistance pos1 pos2
--     in dist >= 0
    ]
    
  , testGroup "Located value properties"
    [             testProperty "mapLocated preserves location" $ \span value ->
        let located = locatedWithSpan span value
                                          mapped = mapLocated (+1) located
        in locSpan                               mapped == span && locPos                               mapped == spanStart span
    
    ,             testProperty "mapLocated is functorial" $ \span value ->
        let located = locatedWithSpan span value
                                          mapped1 = mapLocated (*2) (mapLocated (+1) located)
                                          mapped2 = mapLocated ((+1) * 2) located
        in locValue                               mapped1 === locValue mapped2
    
    ,             testProperty "locatedAt creates empty span" $ \pos value ->
        let located = locatedAt pos value
                                          span = locSpan located
        in spanStart                               span == pos && spanEnd                               span == pos
    ]
    
  , testGroup "Error handling properties"
    [             testProperty "severity comparison is transitive" $ \sev1 sev2 sev3 ->
        let comp12 = compareSeverity sev1 sev2
                                          comp23 = compareSeverity sev2 sev3
                                          comp13 = compareSeverity sev1 sev3
        in if                               comp12 == EQ &&                               comp23 == EQ
           then                               comp13 == EQ
           else if                               comp12 == LT &&                               comp23 == LT
                then                               comp13 == LT
                else if                               comp12 == GT &&                               comp23 == GT
                     then                               comp13 == GT
                     else True  -- Other cases don't guarantee transitivity
    
    ,             testProperty "isAtLeast is reflexive" $ \sev ->
        isAtLeast sev                               sev === True
    
    ,             testProperty "isAtLeast is transitive" $ \sev1 sev2 sev3 ->
        if isAtLeast sev1 sev2 && isAtLeast sev2 sev3
        then isAtLeast sev1                               sev3 === True
        else property True  -- Don't require anything when premises don't hold
    
    --             testProperty "detailed severity priority is consistent with base severity" $ \base sub ->
--     let detailed = DetailedSeverity base sub Nothing
--                                       basePriority = severityPriority base
--                                       detailedPriority = detailedSeverityPriority detailed
--     in detailedPriority >= basePriority
    
    --             testProperty "custom recovery creates strategy with given values" $ \canRec shouldCont cost confidence ->
--     let custom = customRecovery canRec shouldCont Nothing Nothing cost confidence
--     in canRecover                               custom === canRec &&
--        shouldContinue                               custom === shouldCont &&
--        recoveryCost                               custom === cost &&
--        recoveryConfidence                               custom === confidence
    ]
    
  , testGroup "Error collection properties"
    [             testProperty "getErrors only returns Error or Fatal severity" $ \errors ->
        let errorList = getErrors errors
        in L.all (\e -> severity                               e == Error || severity                               e == Fatal) errorList
    
    ,             testProperty "getWarnings only returns Warning severity" $ \errors ->
        let warningList = getWarnings errors
        in L.all (\e -> severity                               e == Warning) warningList
    
    ,             testProperty "getInfo only returns Info severity" $ \errors ->
        let infoList = getInfo errors
        in L.all (\e -> severity                               e == Info) infoList
    
    ,             testProperty "getAllMessages preserves all errors" $ \errors ->
        getAllMessages                               errors === errors
    
    ,             testProperty "hasErrors is true iff getErrors is non-empty" $ \errors ->
        hasErrors                               errors === not (L.null (getErrors errors)
    
    ,             testProperty "hasWarnings is true iff getWarnings is non-empty" $ \errors ->
        hasWarnings                               errors === not (L.null (getWarnings errors)
    ]
    
  , testGroup "Error recovery properties"
    [ --             testProperty "sequence recovery combines costs additively" $ \rec1 rec2 ->
--     let combined = _sequenceRecovery rec1 rec2
--     in recoveryCost                               combined === recoveryCost rec1 + recoveryCost rec2
    
--             testProperty "sequence recovery averages confidence" $ \rec1 rec2 ->
--     let combined = _sequenceRecovery rec1 rec2
--                                       expected = (recoveryConfidence rec1 + recoveryConfidence rec2) / 2
--     in abs (recoveryConfidence combined - expected) < 0.001
    
--             testProperty "sequence recovery requires both to be recoverable" $ \rec1 rec2 ->
--     let combined = _sequenceRecovery rec1 rec2
--     in canRecover                               combined === (canRecover rec1 && canRecover rec2)
    
--             testProperty "sequence recovery requires both to continue" $ \rec1 rec2 ->
--     let combined = _sequenceRecovery rec1 rec2
--     in shouldContinue                               combined === (shouldContinue rec1 && shouldContinue rec2)
    ]
    
  , -- testGroup "RecoveryContext properties"
    -- [             testProperty "adding recovery attempt increments attempt count" $ \ctx strategy ->
    --     let updated = _addRecoveryAttempt strategy True ctx
    --     in recoveryAttempts                               updated === recoveryAttempts ctx + 1
    
    -- ,             testProperty "success rate is between 0 and 1" $ \ctx ->
    --     let rate = _recoverySuccessRate ctx
    --     in rate >= 0.0 && rate <= 1.0
    
    -- ,             testProperty "empty context has 0 success rate" $ 
    --     let ctx = _initialRecoveryContext 5
    --     in _recoverySuccessRate                               ctx === 0.0
    
    -- ,             testProperty "successful only context has 1.0 success rate" $ \ctx ->
    --     let updated = _addRecoveryAttempt errorRecovery True ctx
    --     in _recoverySuccessRate                               updated === 1.0
    -- ]
      
    , testGroup "CombinedError properties"
      [             testProperty "filterCombinedErrorsBySeverity preserves ordering" $ \sev errors ->
          let filtered = filterCombinedErrorsBySeverity sev errors
          in L.all (\e -> combinedErrorSeverity e >= sev) filtered
    
      ,             testProperty "combinedErrorSeverity is monotonic with filter" $ \sev1 sev2 errors ->
          if compareSeverity sev1                               sev2 == LT
          then L.length (filterCombinedErrorsBySeverity sev2 errors) <= 
               L.length (filterCombinedErrorsBySeverity sev1 errors)
          else property True
    ]
  ]