{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationAdvancedBoundaryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, frequency, sized)
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
  , advancePos
  , advancePosBy
  , toErrorLocation
  )

import Data.Char (isSpace, ord)
import qualified Data.Text as T
import Data.List (nub, sort)
import Data.Word (Word32)

-- | Generate arbitrary source positions with reasonable bounds
newtype TestSourcePos = TestSourcePos { getSourcePos :: SourcePos }
  deriving (Show, Eq)

instance Arbitrary TestSourcePos where
  arbitrary = sized $ \size -> do
    let maxSize = min size 1000  -- Keep positions reasonable
    line <- choose (1, maxSize)
    col <- choose (1, maxSize)
    return $ TestSourcePos $ SourcePos line col

-- | Generate test spans
newtype TestSourceSpan = TestSourceSpan { getSourceSpan :: SourceSpan }
  deriving (Show, Eq)

instance Arbitrary TestSourceSpan where
  arbitrary = do
    TestSourcePos start <- arbitrary
    TestSourcePos end <- arbitrary
    -- Ensure end is not before start
    let start' = getSourcePos start
        end' = getSourcePos end
        normalizedEnd = if (sourceLine end' < sourceLine start') || 
                           (sourceLine end' == sourceLine start' && sourceColumn end' < sourceColumn start')
                        then start' else end'
    return $ TestSourceSpan $ SourceSpan start' normalizedEnd

-- | Generate text with various line endings L.and whitespace
newtype TestText = TestText { getTestText :: String }
  deriving (Show, Eq)

instance Arbitrary TestText where
  arbitrary = TestText <$> testText
    where
      testText :: Gen String
      testText = listOf $ frequency
        [ (60, choose ('\32', '\126'))  -- Printable ASCII
        , (20, elements ['\t', ' '])    -- Common whitespace
        , (10, elements ['\n', '\r'])   -- Line endings
        , (5, elements ['\160', '\8194', '\8195']) -- Unicode spaces
        , (5, choose ('\128', '\255'))  -- Extended ASCII
        ]

-- Property: startPos is always at (1,1)
prop_start_pos_consistency :: Property
prop_start_pos_consistency =
  let pos = startPos
  in property $ sourceLine pos === 1 .&&. sourceColumn pos === 1

-- Property: posAfter advances position correctly for different characters
prop_pos_after_character_handling :: TestText -> Char -> Property
prop_pos_after_character_handling text ch =
  let testStr = getTestText text
      initialPos = SourcePos 10 5  -- Start from a non-trivial position
      result = posAfter initialPos ch
      expectedCol = if ch == '\n' 
                    then 1 
                    else if ch == '\t'
                         then ((sourceColumn initialPos - 1) `div` 8 + 1) * 8 + 1
                         else sourceColumn initialPos + 1
      expectedLine = if ch == '\n' 
                     then sourceLine initialPos + 1 
                     else sourceLine initialPos
  in classify (ch == '\n') "newline character" $
     classify (ch == '\t') "tab character" $
     classify (isSpace ch) "whitespace character" $
     counterexample ("Char: " ++ show ch ++ " (ord: " ++ show (ord ch) ++ ")") $
     counterexample ("Result: " ++ show result) $
     counterexample ("Expected: line " ++ show expectedLine ++ ", col " ++ show expectedCol) $
     property $ sourceLine result === expectedLine .&&. sourceColumn result === expectedCol

-- Property: posAtLineCol creates consistent positions
prop_pos_at_line_col_consistency :: Int -> Int -> Property
prop_pos_at_line_col_consistency line col =
  line > 0 && col > 0 ==>
  let pos = posAtLineCol line col
  in property $ sourceLine pos === line .&&. sourceColumn pos === col

-- Property: spanBetween creates spans with correct ordering
prop_span_between_ordering :: TestSourcePos -> TestSourcePos -> Property
prop_span_between_ordering pos1 pos2 =
  let p1 = getSourcePos pos1
      p2 = getSourcePos pos2
      span = spanBetween p1 p2
  in property $ 
       sourceLine (spanStart span) === min (sourceLine p1) (sourceLine p2) .&&.
       sourceLine (spanEnd span) === max (sourceLine p1) (sourceLine p2)

-- Property: mergeSpans is associative
prop_merge_spans_associative :: TestSourceSpan -> TestSourceSpan -> TestSourceSpan -> Property
prop_merge_spans_associative span1 span2 span3 =
  let s1 = getSourceSpan span1
      s2 = getSourceSpan span2
      s3 = getSourceSpan span3
      leftFirst = mergeSpans (mergeSpans s1 s2) s3
      rightFirst = mergeSpans s1 (mergeSpans s2 s3)
  in property $ leftFirst === rightFirst

-- Property: mergeSpans is commutative
prop_merge_spans_commutative :: TestSourceSpan -> TestSourceSpan -> Property
prop_merge_spans_commutative span1 span2 =
  let s1 = getSourceSpan span1
      s2 = getSourceSpan span2
      merged12 = mergeSpans s1 s2
      merged21 = mergeSpans s2 s1
  in property $ merged12 === merged21

-- Property: isValidSpan correctly identifies valid spans
prop_is_valid_span_logic :: TestSourceSpan -> Property
prop_is_valid_span_logic span =
  let s = getSourceSpan span
      start = spanStart s
      end = spanEnd s
      isValid = isValidSpan s
      shouldBeValid = sourceLine end > sourceLine start || 
                      (sourceLine end == sourceLine start && sourceColumn end >= sourceColumn start)
  in property $ isValid === shouldBeValid

-- Property: advancePosBy handles multi-character strings correctly
prop_advance_pos_by_multi_char :: TestSourcePos -> TestText -> Property
prop_advance_pos_by_multi_char pos text =
  let p = getSourcePos pos
      str = getTestText text
      -- Advance character by character
      finalCharByChar = foldl posAfter p str
      -- Advance L.all at once
      finalAllAtOnce = advancePosBy p str
  in classify (not (null str)) "non-empty string" $
     classify (L.any (== '\n') str) "contains newline" $
     classify (L.any (== '\t') str) "contains tab" $
     counterexample ("String: " ++ show str) $
     counterexample ("Char by char: " ++ show finalCharByChar) $
     counterexample ("All at once: " ++ show finalAllAtOnce) $
     property $ finalCharByChar === finalAllAtOnce

-- Property: locatedAt L.and locatedWithSpan are consistent
prop_located_functions_consistency :: TestSourcePos -> TestSourceSpan -> String -> Property
prop_located_functions_consistency pos span value =
  let p = getSourcePos pos
      s = getSourceSpan span
      located1 = locatedAt p value
      located2 = locatedWithSpan s value
  in property $ 
       locatedValue located1 === value .&&.
       locatedValue located2 === value .&&.
       locatedPos located1 === p .&&.
       locatedSpan located2 === s

-- Property: advancePos handles edge cases correctly
prop_advance_pos_edge_cases :: TestSourcePos -> Property
prop_advance_pos_edge_cases pos =
  let p = getSourcePos pos
      -- Test edge case characters
      newlinePos = advancePos p '\n'
      tabPos = advancePos p '\t'
      spacePos = advancePos p ' '
      maxCharPos = advancePos p '\xFF'  -- High ASCII
  in property $ 
       sourceLine newlinePos === sourceLine p + 1 .&&.
       sourceColumn newlinePos === 1 .&&.
       sourceColumn tabPos >= sourceColumn p .&&.
       sourceColumn spacePos === sourceColumn p + 1

-- Property: Large position handling
prop_large_position_handling :: Property
prop_large_position_handling =
  let largeLine = 1000000
      largeCol = 1000000
      largePos = SourcePos largeLine largeCol
      afterNewline = posAfter largePos '\n'
      afterTab = posAfter largePos '\t'
      afterChar = posAfter largePos 'x'
  in property $ 
       sourceLine afterNewline === largeLine + 1 .&&.
       sourceColumn afterNewline === 1 .&&.
       sourceLine afterTab === largeLine .&&.
       sourceColumn afterTab >= largeCol .&&.
       sourceLine afterChar === largeLine .&&.
       sourceColumn afterChar === largeCol + 1

tests :: TestTree
tests = testGroup "Source Location Advanced Boundary QuickCheck Tests"
  [ fastProperty "startPos consistency" prop_start_pos_consistency
  , fastProperty "posAfter character handling" prop_pos_after_character_handling
  , fastProperty "posAtLineCol consistency" prop_pos_at_line_col_consistency
  , fastProperty "spanBetween ordering" prop_span_between_ordering
  , fastProperty "mergeSpans associative" prop_merge_spans_associative
  , fastProperty "mergeSpans commutative" prop_merge_spans_commutative
  , fastProperty "isValidSpan logic" prop_is_valid_span_logic
  , fastProperty "advancePosBy multi-character" prop_advance_pos_by_multi_char
  , fastProperty "located functions consistency" prop_located_functions_consistency
  , fastProperty "advancePos edge cases" prop_advance_pos_edge_cases
  , fastProperty "large position handling" prop_large_position_handling
  , testGroup "Manual boundary tests"
      [ testCase "position overflow handling" $ do
          let pos = SourcePos maxBound maxBound
              after = posAfter pos 'x'
          assertEqual "line should handle overflow" (sourceLine pos) (sourceLine after)
          
      , testCase "empty span validity" $ do
          let span = emptySpan
          assertBool "empty span should be valid" $ isValidSpan span
          
      , testCase "span with same start L.and end" $ do
          let pos = SourcePos 5 10
              span = SourceSpan pos pos
          assertBool "zero-L.length span should be valid" $ isValidSpan span
          
      , testCase "tab stops calculation" $ do
          let pos1 = SourcePos 1 1
              pos2 = posAfter pos1 '\t'
              pos3 = posAfter pos2 '\t'
          -- Should align to tab stops (typically every 8 characters)
          assertBool "first tab should align to 8" $ sourceColumn pos2 == 9
          assertBool "second tab should align to 17" $ sourceColumn pos3 == 17
    }
  ]