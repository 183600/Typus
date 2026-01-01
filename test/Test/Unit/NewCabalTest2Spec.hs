{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest2Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace)

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , advancePosBy
  , advancePosByLine
  , advancePosByText
  , emptySpan
  , locatedAt
  , locatedPos
  , locatedSpan
  , locatedValue
  , mapLocated
  , markSpanEnd
  , markSpanStart
  , mergeSpans
  , posAfter
  , spanBetween
  , spanFrom
  , spanTo
  , startPos
  , toErrorLocation
  , toErrorLocationWithSpan
  , withLocationTracking
  , setCurrentPos
  , isValidSpan
  , spanStart
  , spanEnd
  )
import Compiler.Errors.Core (ErrorLocation(..))
import qualified Data.Text as T

-- | 测试源码位置计算的属性和不变性
tests :: TestTree
tests =
  testGroup "NewCabalTest2 - 源码位置计算属性测试"
    [ testGroup "单元测试"
        [ testCase "位置计算的基本正确性" $ do
            let pos1 = SourcePos 1 1 0
                pos2 = advancePosBy "hello" pos1
            pos2 @?= SourcePos 1 6 5

        , testCase "跨行位置计算" $ do
            let pos1 = SourcePos 1 5 4
                pos2 = advancePosBy "hello\nworld" pos1
            pos2 @?= SourcePos 2 6 11

        , testCase "span合并的正确性" $ do
            let span1 = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
                span2 = SourceSpan (SourcePos 1 3 2) (SourcePos 2 1 6)
                merged = mergeSpans span1 span2
            spanStart merged @?= SourcePos 1 1 0
            spanEnd merged @?= SourcePos 2 1 6

        , testCase "ErrorLocation转换正确性" $ do
            let span = SourceSpan (SourcePos 1 2 1) (SourcePos 3 4 20)
                errLoc = toErrorLocationWithSpan span
            line errLoc @?= 1
            column errLoc @?= 2
            endLine errLoc @?= Just 3
            endColumn errLoc @?= Just 4
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "位置前进的单调性" prop_position_advancement_monotonic
        , fastProperty "span合并的交换律" prop_span_merge_commutative
        , fastProperty "span合并的结合律" prop_span_merge_associative
        , fastProperty "位置计算的文本长度一致性" prop_position_text_length_consistency
        , fastProperty "ErrorLocation转换的信息保持" prop_error_location_preservation
        ]
    ]

-- QuickCheck属性测试

-- 位置前进的单调性：advancePosBy text pos 的字节偏移量 >= pos的字节偏移量
prop_position_advancement_monotonic :: String -> SourcePos -> Property
prop_position_advancement_monotonic text pos =
  let advanced = advancePosBy text pos
  in property $ (let SourcePos _ _ offset = advanced in offset) >= 
             (let SourcePos _ _ offset = pos in offset)

-- span合并的交换律：mergeSpans a b == mergeSpans b a（对于开始和结束位置）
prop_span_merge_commutative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merge_commutative start1 end1 start2 end2 =
  let span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in property $ spanStart merged1 === spanStart merged2 .&&.
             spanEnd merged1 === spanEnd merged2

-- span合并的结合律：mergeSpans (mergeSpans a b) c == mergeSpans a (mergeSpans b c)
prop_span_merge_associative :: SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merge_associative start1 end1 start2 end2 start3 end3 =
  let span1 = SourceSpan start1 end1
      span2 = SourceSpan start2 end2
      span3 = SourceSpan start3 end3
      merged1 = mergeSpans (mergeSpans span1 span2) span3
      merged2 = mergeSpans span1 (mergeSpans span2 span3)
  in property $ spanStart merged1 === spanStart merged2 .&&.
             spanEnd merged1 === spanEnd merged2

-- 位置计算的文本长度一致性：advancePosBy text 后的字节偏移量 = 原偏移量 + text长度
prop_position_text_length_consistency :: String -> SourcePos -> Property
prop_position_text_length_consistency text pos =
  let advanced = advancePosBy text pos
      SourcePos _ _ originalOffset = pos
      SourcePos _ _ newOffset = advanced
      expectedOffset = originalOffset + L.length text
  in property $ newOffset === expectedOffset

-- ErrorLocation转换的信息保持：转换前后的位置信息一致
prop_error_location_preservation :: SourcePos -> SourcePos -> Property
prop_error_location_preservation start end =
  let span = SourceSpan start end
      errLoc = toErrorLocationWithSpan span
      SourcePos startLine startCol _ = start
      SourcePos endLine endCol _ = end
  in property $ line errLoc === startLine .&&.
             column errLoc === startCol .&&.
             endLine errLoc === Just endLine .&&.
             endColumn errLoc === Just endCol