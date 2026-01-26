{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.NewSourceLocationQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import SourceLocation
import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), TypeError(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..), CombinedError(..))
import qualified Compiler.Errors.Core as Error
import Control.DeepSeq (NFData, rnf)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Test.QuickCheck (Arbitrary(..), oneof, elements, resize, sized)

-- Arbitrary instances for QuickCheck
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary (Located String) where
  arbitrary = do
    str <- arbitrary
    span <- arbitrary
    return $ Located str (spanStart span) span

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [TypeChecking, Ownership, Parsing, Semantic, Runtime, Constraint, Inference, Integration, Unknown]

instance Arbitrary ErrorLocation where
  arbitrary = ErrorLocation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ErrorRecovery where
  arbitrary = ErrorRecovery <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ErrorContext where
  arbitrary = ErrorContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TypeError where
  arbitrary = sized $ \size -> do
    errorId <- arbitrary
    severity <- arbitrary
    category <- arbitrary
    message <- arbitrary
    location <- arbitrary
    context <- arbitrary
    recovery <- arbitrary
    suggestions <- arbitrary
    -- 限制递归深度，避免无限递归
    let relatedErrorsSize = max 0 (size `div` 3)
        errorChainSize = max 0 (size `div` 3)
    relatedErrors <- resize relatedErrorsSize $ listOf arbitrary
    errorChain <- resize errorChainSize $ listOf arbitrary
    timestamp <- arbitrary
    return $ TypeError errorId severity category message location context recovery suggestions relatedErrors errorChain timestamp

instance Arbitrary CombinedError where
  arbitrary = oneof [IntegrationError <$> arbitrary <*> arbitrary]

-- ============================================================================
-- SourceLocation Module QuickCheck Tests
-- ============================================================================

-- Test SourcePos properties
prop_sourcepos_start_pos_valid :: Property
prop_sourcepos_start_pos_valid = 
  property $ posLine startPos == 1 && posColumn startPos == 1 && posOffset startPos == 0

prop_sourcepos_pos_after_newline :: Positive Int -> Property
prop_sourcepos_pos_after_newline (Positive lineNum) = 
  let pos = SourcePos lineNum 1 0
      pos' = posAfter '\n' pos
  in property $ posLine pos' == lineNum + 1 && posColumn pos' == 1 && posOffset pos' == 1

prop_sourcepos_pos_after_tab :: Positive Int -> Property
prop_sourcepos_pos_after_tab (Positive col) = 
  let pos = SourcePos 1 col 0
      pos' = posAfter '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine pos' == 1 && posColumn pos' == expectedCol && posOffset pos' == 1

prop_sourcepos_pos_after_regular_char :: Positive Int -> Char -> Property
prop_sourcepos_pos_after_regular_char (Positive col) c = 
  let pos = SourcePos 1 col 0
      pos' = posAfter c pos
  in if c `elem` "\n\t"
     then property $ True  -- Skip special characters
     else property $ posLine pos' == 1 && posColumn pos' == col + 1 && posOffset pos' == 1

prop_sourcepos_pos_at :: Positive Int -> Positive Int -> Property
prop_sourcepos_pos_at (Positive line) (Positive col) = 
  let pos = posAt line col
  in property $ posLine pos == line && posColumn pos == col && posOffset pos == 0

prop_sourcepos_pos_at_line_col :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_pos_at_line_col (Positive line) (Positive col) (Positive offset) = 
  let pos = posAtLineCol line col offset
  in property $ posLine pos == line && posColumn pos == col && posOffset pos == offset

-- Test SourceSpan properties
prop_sourcespan_empty_span :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_empty_span (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      span = emptySpan pos
  in property $ spanStart span == pos && spanEnd span == pos

prop_sourcespan_span_from :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_span_from (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      span = spanFrom pos
  in property $ spanStart span == pos && spanEnd span == pos

prop_sourcespan_span_to :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_span_to (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      span = spanTo pos
  in property $ spanStart span == pos && spanEnd span == pos

prop_sourcespan_span_between :: Positive Int -> Positive Int -> Positive Int -> 
                                 Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_span_between (Positive line1) (Positive col1) (Positive offset1)
                             (Positive line2) (Positive col2) (Positive offset2) = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetween pos1 pos2
  in property $ spanStart span == pos1 && spanEnd span == pos2

prop_sourcespan_span_between_ordered :: Positive Int -> Positive Int -> Positive Int -> 
                                       Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_span_between_ordered (Positive line1) (Positive col1) (Positive offset1)
                                     (Positive line2) (Positive col2) (Positive offset2) = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
  in property $ comparePos start pos2 /= GT && comparePos pos1 end /= GT

prop_sourcespan_merge_spans :: Positive Int -> Positive Int -> Positive Int -> 
                              Positive Int -> Positive Int -> Positive Int ->
                              Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_merge_spans (Positive line1) (Positive col1) (Positive offset1)
                            (Positive line2) (Positive col2) (Positive offset2)
                            (Positive line3) (Positive col3) (Positive offset3) = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      pos3 = SourcePos line3 col3 offset3
      span1 = spanBetween pos1 pos2
      span2 = spanBetween pos2 pos3
      merged = mergeSpans span1 span2
      start = spanStart merged
      end = spanEnd merged
  in property $ comparePos start pos1 /= GT && comparePos pos2 end /= GT

prop_sourcespan_is_valid_span :: Positive Int -> Positive Int -> Positive Int -> 
                                Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_is_valid_span (Positive line1) (Positive col1) (Positive offset1)
                              (Positive line2) (Positive col2) (Positive offset2) = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetweenOrdered pos1 pos2
  in property $ isValidSpan span

prop_sourcespan_is_valid_block_span :: Positive Int -> Positive Int -> Positive Int -> 
                                      Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcespan_is_valid_block_span (Positive line1) (Positive col1) (Positive offset1)
                                    (Positive line2) (Positive col2) (Positive offset2) = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetweenOrdered pos1 pos2
  in property $ isValidBlockSpan span

-- Test Located properties
prop_located_at :: Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_located_at (Positive line) (Positive col) (Positive offset) value = 
  let pos = SourcePos line col offset
      located = locatedAt pos value
  in property $ locValue located == value && locPos located == pos && 
                spanStart (locSpan located) == pos && spanEnd (locSpan located) == pos

prop_located_with_span :: Positive Int -> Positive Int -> Positive Int -> 
                         Positive Int -> Positive Int -> Positive Int -> String -> Property
prop_located_with_span (Positive line1) (Positive col1) (Positive offset1)
                       (Positive line2) (Positive col2) (Positive offset2) value = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetweenOrdered pos1 pos2
      located = locatedWithSpan span value
  in property $ locValue located == value && locSpan located == span && 
                locPos located == spanStart span

prop_located_value :: String -> Property
prop_located_value value = 
  let pos = startPos
      located = locatedAt pos value
  in property $ locatedValue located == value

prop_located_span :: Positive Int -> Positive Int -> Positive Int -> Property
prop_located_span (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      span = emptySpan pos
      located = locatedWithSpan span "test"
  in property $ locatedSpan located == span

prop_located_pos :: Positive Int -> Positive Int -> Positive Int -> Property
prop_located_pos (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      span = emptySpan pos
      located = locatedWithSpan span "test"
  in property $ locatedPos located == pos

prop_map_located :: String -> String -> Property
prop_map_located value1 value2 = 
  let pos = startPos
      located1 = locatedAt pos value1
      located2 = mapLocated (const value2) located1
  in property $ locValue located2 == value2 && locPos located2 == pos

-- Test position advancement
prop_advance_pos_newline :: Positive Int -> Property
prop_advance_pos_newline (Positive lineNum) = 
  let pos = SourcePos lineNum 5 10
      pos' = advancePos '\n' pos
  in property $ posLine pos' == lineNum + 1 && posColumn pos' == 1 && posOffset pos' == 11

prop_advance_pos_tab :: Positive Int -> Property
prop_advance_pos_tab (Positive col) = 
  let pos = SourcePos 1 col 10
      pos' = advancePos '\t' pos
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in property $ posLine pos' == 1 && posColumn pos' == expectedCol && posOffset pos' == 11

prop_advance_pos_regular :: Positive Int -> Char -> Property
prop_advance_pos_regular (Positive col) c = 
  let pos = SourcePos 1 col 10
      pos' = advancePos c pos
  in if c `elem` "\n\t"
     then property $ True  -- Skip special characters
     else property $ posLine pos' == 1 && posColumn pos' == col + 1 && posOffset pos' == 11

prop_advance_pos_by :: String -> Positive Int -> Property
prop_advance_pos_by chars (Positive initialOffset) = 
  let pos = SourcePos 1 1 initialOffset
      pos' = advancePosBy chars pos
      expectedOffset = initialOffset + length chars
  in property $ posOffset pos' == expectedOffset

prop_advance_pos_by_text :: String -> Positive Int -> Property
prop_advance_pos_by_text str (Positive initialOffset) = 
  let pos = SourcePos 1 1 initialOffset
      text = T.pack str
      pos' = advancePosByText text pos
      expectedOffset = initialOffset + length str
  in property $ posOffset pos' == expectedOffset

prop_advance_pos_by_line :: Positive Int -> Positive Int -> Positive Int -> Property
prop_advance_pos_by_line (Positive line) (Positive col) (Positive numLines) = 
  let pos = SourcePos line col 10
      pos' = advancePosByLine numLines pos
  in property $ posLine pos' == line + numLines && posColumn pos' == 1

-- Test position comparison
prop_compare_pos_same_position :: Positive Int -> Positive Int -> Positive Int -> Property
prop_compare_pos_same_position (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
  in property $ comparePos pos pos == EQ

prop_compare_pos_line_priority :: Positive Int -> Positive Int -> Property
prop_compare_pos_line_priority (Positive line1) (Positive line2) = 
  let pos1 = SourcePos line1 1 0
      pos2 = SourcePos line2 1 0
  in if line1 < line2
     then property $ comparePos pos1 pos2 == LT
     else if line1 > line2
          then property $ comparePos pos1 pos2 == GT
          else property $ comparePos pos1 pos2 == EQ

prop_compare_pos_column_priority :: Positive Int -> Positive Int -> Positive Int -> Property
prop_compare_pos_column_priority (Positive line) (Positive col1) (Positive col2) = 
  let pos1 = SourcePos line col1 0
      pos2 = SourcePos line col2 0
  in if col1 < col2
     then property $ comparePos pos1 pos2 == LT
     else if col1 > col2
          then property $ comparePos pos1 pos2 == GT
          else property $ comparePos pos1 pos2 == EQ

prop_compare_pos_offset_priority :: Positive Int -> Positive Int -> Positive Int -> Positive Int -> Property
prop_compare_pos_offset_priority (Positive line) (Positive col) (Positive offset1) (Positive offset2) = 
  let pos1 = SourcePos line col offset1
      pos2 = SourcePos line col offset2
  in if offset1 < offset2
     then property $ comparePos pos1 pos2 == LT
     else if offset1 > offset2
          then property $ comparePos pos1 pos2 == GT
          else property $ comparePos pos1 pos2 == EQ

-- Test error location conversion
prop_to_error_location :: Positive Int -> Positive Int -> Positive Int -> Property
prop_to_error_location (Positive line) (Positive col) (Positive offset) = 
  let pos = SourcePos line col offset
      errLoc = toErrorLocation pos
  in property $ filePath errLoc == Nothing && 
                Error.line errLoc == line && 
                Error.column errLoc == col && 
                endLine errLoc == Nothing && 
                endColumn errLoc == Nothing

prop_to_error_location_with_span :: Positive Int -> Positive Int -> Positive Int -> 
                                  Positive Int -> Positive Int -> Positive Int -> Property
prop_to_error_location_with_span (Positive line1) (Positive col1) (Positive offset1)
                                (Positive line2) (Positive col2) (Positive offset2) = 
  let pos1 = SourcePos line1 col1 offset1
      pos2 = SourcePos line2 col2 offset2
      span = spanBetweenOrdered pos1 pos2
      start = spanStart span
      end = spanEnd span
      errLoc = toErrorLocationWithSpan span
  in property $ filePath errLoc == Nothing && 
                line errLoc == posLine start && 
                column errLoc == posColumn start && 
                endLine errLoc == Just (posLine end) && 
                endColumn errLoc == Just (posColumn end)

-- Test NFData instances
prop_sourcepos_nfdata :: SourcePos -> Property
prop_sourcepos_nfdata pos = property $ rnf pos === ()

prop_sourcespan_nfdata :: SourceSpan -> Property
prop_sourcespan_nfdata span = property $ rnf span === ()

prop_located_nfdata :: Located String -> Property
prop_located_nfdata located = property $ rnf located === ()

-- Unit tests for edge cases
test_source_location_edge_cases :: TestTree
test_source_location_edge_cases = testGroup "SourceLocation Edge Cases"
  [ testCase "startPos properties" $ do
      assertEqual "startPos line" 1 (posLine startPos)
      assertEqual "startPos column" 1 (posColumn startPos)
      assertEqual "startPos offset" 0 (posOffset startPos)
    
  , testCase "posAfter with special chars" $ do
      let posNL = posAfter '\n' startPos
      assertEqual "newline line" 2 (posLine posNL)
      assertEqual "newline column" 1 (posColumn posNL)
      assertEqual "newline offset" 1 (posOffset posNL)
      
      let posTab = posAfter '\t' startPos
      assertEqual "tab line" 1 (posLine posTab)
      assertEqual "tab column" 9 (posColumn posTab)
      assertEqual "tab offset" 1 (posOffset posTab)
    
  , testCase "spanBetween ordering" $ do
      let pos1 = SourcePos 1 5 0
          pos2 = SourcePos 2 3 10
          span = spanBetween pos1 pos2
      assertEqual "span start" pos1 (spanStart span)
      assertEqual "span end" pos2 (spanEnd span)
      
      let spanOrdered = spanBetweenOrdered pos1 pos2
      assertEqual "ordered span start" pos1 (spanStart spanOrdered)
      assertEqual "ordered span end" pos2 (spanEnd spanOrdered)
      
      let spanReversed = spanBetweenOrdered pos2 pos1
      assertEqual "reversed span start" pos1 (spanStart spanReversed)
      assertEqual "reversed span end" pos2 (spanEnd spanReversed)
    
  , testCase "mergeSpans coverage" $ do
      let pos1 = SourcePos 1 5 10
          pos2 = SourcePos 2 10 20
          pos3 = SourcePos 3 15 30
          span1 = spanBetween pos1 pos2
          span2 = spanBetween pos2 pos3
          merged = mergeSpans span1 span2
      assertEqual "merged start" pos1 (spanStart merged)
      assertEqual "merged end" pos3 (spanEnd merged)
    
  , testCase "isValidSpan" $ do
      let validSpan = spanBetween (SourcePos 1 1 0) (SourcePos 1 5 4)
      assertBool "valid span is valid" (isValidSpan validSpan)
      
      let invalidSpan = spanBetween (SourcePos 1 5 4) (SourcePos 1 1 0)
      assertBool "invalid span is not valid" (not $ isValidSpan invalidSpan)
    
  , testCase "located operations" $ do
      let pos = SourcePos 10 20 100
          value = "test value"
          located = locatedAt pos value
      assertEqual "located value" value (locValue located)
      assertEqual "located position" pos (locPos located)
      assertEqual "located span start" pos (spanStart $ locSpan located)
      assertEqual "located span end" pos (spanEnd $ locSpan located)
      
      let mapped = mapLocated (++ " modified") located
      assertEqual "mapped value" "test value modified" (locValue mapped)
      assertEqual "mapped position" pos (locPos mapped)
  ]

-- QuickCheck properties
test_source_location_properties :: TestTree
test_source_location_properties = testGroup "SourceLocation QuickCheck Properties"
  [ testProperty "SourcePos startPos valid" prop_sourcepos_start_pos_valid
  , testProperty "SourcePos posAfter newline" prop_sourcepos_pos_after_newline
  , testProperty "SourcePos posAfter tab" prop_sourcepos_pos_after_tab
  , testProperty "SourcePos posAfter regular char" prop_sourcepos_pos_after_regular_char
  , testProperty "SourcePos posAt" prop_sourcepos_pos_at
  , testProperty "SourcePos posAtLineCol" prop_sourcepos_pos_at_line_col
  , testProperty "SourceSpan emptySpan" prop_sourcespan_empty_span
  , testProperty "SourceSpan spanFrom" prop_sourcespan_span_from
  , testProperty "SourceSpan spanTo" prop_sourcespan_span_to
  , testProperty "SourceSpan spanBetween" prop_sourcespan_span_between
  , testProperty "SourceSpan spanBetweenOrdered" prop_sourcespan_span_between_ordered
  , testProperty "SourceSpan mergeSpans" prop_sourcespan_merge_spans
  , testProperty "SourceSpan isValidSpan" prop_sourcespan_is_valid_span
  , testProperty "SourceSpan isValidBlockSpan" prop_sourcespan_is_valid_block_span
  , testProperty "Located locatedAt" prop_located_at
  , testProperty "Located locatedWithSpan" prop_located_with_span
  , testProperty "Located locatedValue" prop_located_value
  , testProperty "Located locatedSpan" prop_located_span
  , testProperty "Located locatedPos" prop_located_pos
  , testProperty "Located mapLocated" prop_map_located
  , testProperty "advancePos newline" prop_advance_pos_newline
  , testProperty "advancePos tab" prop_advance_pos_tab
  , testProperty "advancePos regular" prop_advance_pos_regular
  , testProperty "advancePosBy" prop_advance_pos_by
  , testProperty "advancePosByText" prop_advance_pos_by_text
  , testProperty "advancePosByLine" prop_advance_pos_by_line
  , testProperty "comparePos same position" prop_compare_pos_same_position
  , testProperty "comparePos line priority" prop_compare_pos_line_priority
  , testProperty "comparePos column priority" prop_compare_pos_column_priority
  , testProperty "comparePos offset priority" prop_compare_pos_offset_priority
  , testProperty "toErrorLocation" prop_to_error_location
  , testProperty "toErrorLocationWithSpan" prop_to_error_location_with_span
  , testProperty "SourcePos NFData" prop_sourcepos_nfdata
  , testProperty "SourceSpan NFData" prop_sourcespan_nfdata
  , testProperty "Located NFData" prop_located_nfdata
  ]

-- Main test suite
sourceLocationTests :: TestTree
sourceLocationTests = testGroup "SourceLocation Module Tests"
  [ test_source_location_edge_cases
  , test_source_location_properties
  ]