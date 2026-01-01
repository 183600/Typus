{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationTrackingQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, resize, suchThat, vectorOf, arbitrary
  )

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

import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | 生成有效的行号
genLineNumber :: Gen Int
genLineNumber = choose (1, 1000)

-- | 生成有效的列号
genColumnNumber :: Gen Int
genColumnNumber = choose (1, 100)

-- | 生成有效的偏移量
genOffset :: Gen Int
genOffset = choose (0, 10000)

-- | 生成SourcePos
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- genLineNumber
  column <- genColumnNumber
  offset <- genOffset
  return $ SourcePos line column offset

-- | 生成有效的字符
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '] ++ ['\t'] ++ ['\n'] ++ punctuation
  where
    punctuation = "!@#$%^&*()_+-=[]{}|;':\",./<>?"

-- | 生成字符串
genString :: Gen String
genString = listOf genChar

-- | 生成文本
genText :: Gen Text
genText = T.pack <$> genString

-- | 生成SourceSpan
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  start <- genSourcePos
  endOffset <- choose (0, 100)
  let end = start { posOffset = posOffset start + endOffset }
  return $ SourceSpan start end

-- | 生成有效的SourceSpan（确保start <= end）
genValidSourceSpan :: Gen SourceSpan
genValidSourceSpan = do
  line1 <- genLineNumber
  line2 <- choose (line1, line1 + 10)  -- 确保end line >= start line
  col1 <- genColumnNumber
  col2 <- if line2 == line1 
           then choose (col1, col1 + 10)  -- 同一行时，end column >= start column
           else genColumnNumber
  let start = SourcePos line1 col1 0
      end = SourcePos line2 col2 (col2 + line2 * 10)
  return $ SourceSpan start end

-- | 生成Located值
genLocated :: Gen (Located String)
genLocated = do
  value <- genString
  pos <- genSourcePos
  span <- genValidSourceSpan
  return $ Located value pos span

-- 属性：startPos应该有正确的初始值
prop_startPos_values :: Property
prop_startPos_values =
  posLine startPos === 1 .&&.
  posColumn startPos === 1 .&&.
  posOffset startPos === 0

-- 属性：posAfter处理换行符应该增加行号并重置列号
prop_posAfter_newline :: Property
prop_posAfter_newline =
  forAll genSourcePos $ \pos ->
    let newPos = posAfter '\n' pos
    in posLine newPos === posLine pos + 1 .&&.
       posColumn newPos === 1 .&&.
       posOffset newPos === posOffset pos + 1

-- 属性：posAfter处理制表符应该对齐到下一个8的倍数列
prop_posAfter_tab :: Property
prop_posAfter_tab =
  forAll genSourcePos $ \pos ->
    let newPos = posAfter '\t' pos
        expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    in posColumn newPos === expectedCol .&&.
       posLine newPos === posLine pos .&&.
       posOffset newPos === posOffset pos + 1

-- 属性：posAfter处理普通字符应该增加列号
prop_posAfter_normal_char :: Property
prop_posAfter_normal_char =
  forAll genSourcePos $ \pos ->
  forAll (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' ']) $ \char ->
    let newPos = posAfter char pos
    in posColumn newPos === posColumn pos + 1 .&&.
       posLine newPos === posLine pos .&&.
       posOffset newPos === posOffset pos + 1

-- 属性：posAt应该创建指定行和列的位置
prop_posAt_creates_correct_position :: Property
prop_posAt_creates_correct_position =
  forAll genLineNumber $ \line ->
  forAll genColumnNumber $ \col ->
    let pos = posAt line col
    in posLine pos === line .&&.
       posColumn pos === col .&&.
       posOffset pos === 0

-- 属性：posAtLineCol应该创建指定行、列和偏移量的位置
prop_posAtLineCol_creates_correct_position :: Property
prop_posAtLineCol_creates_correct_position =
  forAll genLineNumber $ \line ->
  forAll genColumnNumber $ \col ->
  forAll genOffset $ \offset ->
    let pos = posAtLineCol line col offset
    in posLine pos === line .&&.
       posColumn pos === col .&&.
       posOffset pos === offset

-- 属性：emptySpan应该创建起点和终点相同的span
prop_emptySpan_same_start_end :: Property
prop_emptySpan_same_start_end =
  forAll genSourcePos $ \pos ->
    let span = emptySpan pos
    in spanStart span === pos .&&.
       spanEnd span === pos

-- 属性：spanFrom应该是emptySpan的别名
prop_spanFrom_alias :: Property
prop_spanFrom_alias =
  forAll genSourcePos $ \pos ->
    spanFrom pos === emptySpan pos

-- 属性：spanTo应该创建起点和终点相同的span
prop_spanTo_same_start_end :: Property
prop_spanTo_same_start_end =
  forAll genSourcePos $ \pos ->
    let span = spanTo pos
    in spanStart span === pos .&&.
       spanEnd span === pos

-- 属性：spanBetween应该创建正确的span
prop_spanBetween_correct :: Property
prop_spanBetween_correct =
  forAll genSourcePos $ \start ->
  forAll genSourcePos $ \end ->
    let span = spanBetween start end
    in spanStart span === start .&&.
       spanEnd span === end

-- 属性：mergeSpans应该选择最小的起点和最大的终点
prop_mergeSpans_correct :: Property
prop_mergeSpans_correct =
  forAll genValidSourceSpan $ \span1 ->
  forAll genValidSourceSpan $ \span2 ->
    let merged = mergeSpans span1 span2
    in spanStart merged === min (spanStart span1) (spanStart span2) .&&.
       spanEnd merged === max (spanEnd span1) (spanEnd span2)

-- 属性：isValidSpan应该正确检查span的有效性
prop_isValidSpan_correct :: Property
prop_isValidSpan_correct =
  forAll genSourceSpan $ \span ->
    let start = spanStart span
        end = spanEnd span
        isValid = start <= end
    in isValidSpan span === isValid

-- 属性：locatedAt应该创建具有指定位置的Located值
prop_locatedAt_correct :: Property
prop_locatedAt_correct =
  forAll genString $ \value ->
  forAll genSourcePos $ \pos ->
    let located = locatedAt value pos
    in locatedValue located === value .&&.
       locatedPos located === pos .&&.
       locatedSpan located === emptySpan pos

-- 属性：locatedWithSpan应该创建具有指定span的Located值
prop_locatedWithSpan_correct :: Property
prop_locatedWithSpan_correct =
  forAll genString $ \value ->
  forAll genSourcePos $ \pos ->
  forAll genValidSourceSpan $ \span ->
    let located = locatedWithSpan value span
    in locatedValue located === value .&&.
       locatedPos located === pos .&&.
       locatedSpan located === span

-- 属性：mapLocated应该正确映射Located值的值部分
prop_mapLocated_correct :: Property
prop_mapLocated_correct =
  forAll genLocated $ \located ->
    let doubled = mapLocated (L.map (*2)) located
        originalValue = locatedValue located
        mappedValue = locatedValue doubled
    in mappedValue === L.map (*2) originalValue

-- 属性：advancePos应该正确处理文本
prop_advancePos_correct :: Property
prop_advancePos_correct =
  forAll genSourcePos $ \startPos ->
  forAll genString $ \text ->
    let endPos = advancePos startPos text
        -- 简单验证：偏移量应该增加
        offsetIncreased = posOffset endPos >= posOffset startPos
    in offsetIncreased === True

-- 属性：advancePosBy应该正确处理指定数量的字符
prop_advancePosBy_correct :: Property
prop_advancePosBy_correct =
  forAll genSourcePos $ \startPos ->
  forAll genString $ \text ->
  forAll (choose (0, L.length text)) $ \n ->
    let endPos = advancePosBy startPos n text
        offsetIncreased = posOffset endPos >= posOffset startPos
    in offsetIncreased === True

-- 属性：advancePosByText应该正确处理Text
prop_advancePosByText_correct :: Property
prop_advancePosByText_correct =
  forAll genSourcePos $ \startPos ->
  forAll genText $ \text ->
    let endPos = advancePosByText startPos text
        offsetIncreased = posOffset endPos >= posOffset startPos
    in offsetIncreased === True

-- 属性：advancePosByLine应该正确处理行数
prop_advancePosByLine_correct :: Property
prop_advancePosByLine_correct =
  forAll genSourcePos $ \startPos ->
  forAll (choose (0, 10)) $ \lines ->
    let endPos = advancePosByLine startPos lines
        lineIncreased = posLine endPos >= posLine startPos
    in lineIncreased === True

-- 属性：toErrorLocation应该正确转换SourcePos
prop_toErrorLocation_correct :: Property
prop_toErrorLocation_correct =
  forAll genSourcePos $ \pos ->
    let errorLoc = toErrorLocation pos
    in errorLine errorLoc === posLine pos .&&.
       errorColumn errorLoc === posColumn pos

-- 属性：toErrorLocationWithSpan应该正确转换SourceSpan
prop_toErrorLocationWithSpan_correct :: Property
prop_toErrorLocationWithSpan_correct =
  forAll genValidSourceSpan $ \span ->
    let errorLoc = toErrorLocationWithSpan span
        start = spanStart span
    in errorLine errorLoc === posLine start .&&.
       errorColumn errorLoc === posColumn start

-- 属性：SourcePos的ord实例应该基于行优先，然后列
prop_sourcePos_ordering :: Property
prop_sourcePos_ordering =
  forAll genSourcePos $ \pos1 ->
  forAll genSourcePos $ \pos2 ->
    let line1 = posLine pos1
        line2 = posLine pos2
        col1 = posColumn pos1
        col2 = posColumn pos2
        expectedOrdering = compare line1 line2 <> compare col1 col2
        actualOrdering = compare pos1 pos2
    in actualOrdering === expectedOrdering

-- 属性：SourceSpan的ord实例应该基于起点优先，然后终点
prop_sourceSpan_ordering :: Property
prop_sourceSpan_ordering =
  forAll genValidSourceSpan $ \span1 ->
  forAll genValidSourceSpan $ \span2 ->
    let start1 = spanStart span1
        start2 = spanStart span2
        end1 = spanEnd span1
        end2 = spanEnd span2
        expectedOrdering = compare start1 start2 <> compare end1 end2
        actualOrdering = compare span1 span2
    in actualOrdering === expectedOrdering

tests :: TestTree
tests =
  testGroup "Source Location Tracking QuickCheck Tests"
    [ fastProperty "startPos values" prop_startPos_values
    , fastProperty "posAfter newline" prop_posAfter_newline
    , fastProperty "posAfter tab" prop_posAfter_tab
    , fastProperty "posAfter normal char" prop_posAfter_normal_char
    , fastProperty "posAt creates correct position" prop_posAt_creates_correct_position
    , fastProperty "posAtLineCol creates correct position" prop_posAtLineCol_creates_correct_position
    , fastProperty "emptySpan same start end" prop_emptySpan_same_start_end
    , fastProperty "spanFrom alias" prop_spanFrom_alias
    , fastProperty "spanTo same start end" prop_spanTo_same_start_end
    , fastProperty "spanBetween correct" prop_spanBetween_correct
    , fastProperty "mergeSpans correct" prop_mergeSpans_correct
    , fastProperty "isValidSpan correct" prop_isValidSpan_correct
    , fastProperty "locatedAt correct" prop_locatedAt_correct
    , fastProperty "locatedWithSpan correct" prop_locatedWithSpan_correct
    , fastProperty "mapLocated correct" prop_mapLocated_correct
    , fastProperty "advancePos correct" prop_advancePos_correct
    , fastProperty "advancePosBy correct" prop_advancePosBy_correct
    , fastProperty "advancePosByText correct" prop_advancePosByText_correct
    , fastProperty "advancePosByLine correct" prop_advancePosByLine_correct
    , fastProperty "toErrorLocation correct" prop_toErrorLocation_correct
    , fastProperty "toErrorLocationWithSpan correct" prop_toErrorLocationWithSpan_correct
    , fastProperty "SourcePos ordering" prop_sourcePos_ordering
    , fastProperty "SourceSpan ordering" prop_sourceSpan_ordering
    ]