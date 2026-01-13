module Test.Unit.SourceLocationPositionQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation
  ( SourcePos(..), startPos, posAfter, posAt, posAtLineCol
  , advancePos, advancePosBy, advancePosByText
  )
import qualified Data.Text as T (pack)

-- | SourcePos 的 Arbitrary 实例
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- | 测试SourcePos的基本属性
prop_start_pos_properties :: Property
prop_start_pos_properties =
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- | 测试posAfter函数的属性
prop_pos_after_newline_increments_line :: Int -> Property
prop_pos_after_newline_increments_line line =
  line >= 0 ==> 
  let pos = SourcePos line 1 0
      newPos = posAfter '\n' pos
  in posLine newPos === line + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === 1

prop_pos_after_tab_aligns_to_8_columns :: Int -> Property
prop_pos_after_tab_aligns_to_8_columns column =
  column >= 1 && column <= 8 ==> 
  let pos = SourcePos 1 column 0
      expectedColumn = ((column - 1) `div` 8 + 1) * 8 + 1
      newPos = posAfter '\t' pos
  in posColumn newPos === expectedColumn .&&.
     posOffset newPos === 1

prop_pos_after_regular_char_increments_column :: Char -> Int -> Property
prop_pos_after_regular_char_increments_column c column =
  c /= '\n' && c /= '\t' && column >= 1 ==> 
  let pos = SourcePos 1 column 0
      newPos = posAfter c pos
  in posColumn newPos === column + 1 .&&.
     posOffset newPos === 1

-- | 测试posAt和posAtLineCol函数的属性
prop_pos_at_line_col_consistency :: Int -> Int -> Int -> Property
prop_pos_at_line_col_consistency line column offset =
  line >= 1 && column >= 1 && offset >= 0 ==> 
  let pos1 = posAt line column
      pos2 = posAtLineCol line column 0
  in pos1 === pos2

-- | 测试advancePos函数的属性
prop_advance_pos_empty_string :: SourcePos -> Property
prop_advance_pos_empty_string pos =
  advancePos ' ' pos === pos

prop_advance_pos_newline_behavior :: Int -> Int -> Property
prop_advance_pos_newline_behavior line column =
  line >= 1 && column >= 1 ==> 
  let pos = SourcePos line column 0
      newPos = advancePos '\n' pos
  in posLine newPos === line + 1 .&&.
     posColumn newPos === 1 .&&.
     posOffset newPos === 1

prop_advance_pos_multiple_chars :: String -> SourcePos -> Property
prop_advance_pos_multiple_chars s pos =
  let newPos = foldl (\p c -> advancePos c p) pos s
      expectedOffset = posOffset pos + length s
  in posOffset newPos === expectedOffset

-- | 测试advancePosBy函数的属性
prop_advance_pos_by_zero :: SourcePos -> Property
prop_advance_pos_by_zero pos =
  advancePosBy "0" pos === pos

prop_advance_pos_by_positive :: Int -> SourcePos -> Property
prop_advance_pos_by_positive n pos =
  n > 0 ==> 
  let newPos = advancePosBy (show n) pos
      expectedOffset = posOffset pos + n
  in posOffset newPos === expectedOffset

-- | 测试advancePosByText函数的属性
prop_advance_pos_by_text_empty :: SourcePos -> Property
prop_advance_pos_by_text_empty pos =
  advancePosByText (T.pack "") pos === pos

prop_advance_pos_by_text_consistency :: String -> SourcePos -> Property
prop_advance_pos_by_text_consistency s pos =
  advancePosByText (T.pack s) pos === foldl (\p c -> advancePos c p) pos s

tests :: TestTree
tests = testGroup "SourceLocation Position QuickCheck Tests"
  [ testProperty "start position properties" prop_start_pos_properties
  , testProperty "posAfter newline increments line" prop_pos_after_newline_increments_line
  , testProperty "posAfter tab aligns to 8 columns" prop_pos_after_tab_aligns_to_8_columns
  , testProperty "posAfter regular char increments column" prop_pos_after_regular_char_increments_column
  , testProperty "posAt and posAtLineCol consistency" prop_pos_at_line_col_consistency
  , testProperty "advancePos empty string" prop_advance_pos_empty_string
  , testProperty "advancePos newline behavior" prop_advance_pos_newline_behavior
  , testProperty "advancePos multiple chars" prop_advance_pos_multiple_chars
  , testProperty "advancePosBy zero" prop_advance_pos_by_zero
  , testProperty "advancePosBy positive" prop_advance_pos_by_positive
  , testProperty "advancePosByText empty" prop_advance_pos_by_text_empty
  , testProperty "advancePosByText consistency" prop_advance_pos_by_text_consistency
  ]