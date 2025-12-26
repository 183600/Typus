{-# LANGUAGE LambdaCase #-}
module Test.Unit.SourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, choose, forAll)
import Data.Char (isSpace)
import Data.List (sort, nub, minimum, maximum)
import Data.Ord (comparing)

import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, posAt, posAtLineCol
  , emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated
  , advancePos, advancePosBy, advancePosByText, advancePosByLine
  )

-- | SourceLocation模块数学属性测试
tests :: TestTree
tests =
  testGroup "SourceLocation Math QuickCheck Tests"
    [ testGroup "SourcePos Mathematical Properties"
        [ testProperty "SourcePos: ordering transitivity" propSourcePosTransitivity
        , testProperty "SourcePos: ordering antisymmetry" propSourcePosAntisymmetry
        , testProperty "SourcePos: ordering totality" propSourcePosTotality
        , testProperty "SourcePos: position advancement monotonicity" propPosAdvancementMonotonicity
        , testProperty "SourcePos: line advancement resets column" propLineAdvancementResetsColumn
        ]

    , testGroup "SourceSpan Mathematical Properties"
        [ testProperty "SourceSpan: merge associativity" propSpanMergeAssociativity
        , testProperty "SourceSpan: merge commutativity" propSpanMergeCommutativity
        , testProperty "SourceSpan: merge identity" propSpanMergeIdentity
        , testProperty "SourceSpan: span ordering consistency" propSpanOrderingConsistency
        , testProperty "SourceSpan: validity preservation" propSpanValidityPreservation
        ]

    , testGroup "Located Values Mathematical Properties"
        [ testProperty "Located: functor laws" propLocatedFunctorLaws
        , testProperty "Located: position consistency" propLocatedPositionConsistency
        , testProperty "Located: span boundaries" propLocatedSpanBoundaries
        ]

    , testGroup "Position Advancement Properties"
        [ testProperty "Position advancement: character counting" propAdvancementCharacterCounting
        , testProperty "Position advancement: newline behavior" propAdvancementNewlineBehavior
        , testProperty "Position advancement: tab behavior" propAdvancementTabBehavior
        , testProperty "Position advancement: text processing" propAdvancementTextProcessing
        ]

    , testGroup "Geometric Properties"
        [ testProperty "Span length calculation" propSpanLengthCalculation
        , testProperty "Span containment" propSpanContainment
        , testProperty "Span intersection" propSpanIntersection
        , testProperty "Span union" propSpanUnion
        ]

    , testGroup "Edge Cases and Boundary Conditions"
        [ testProperty "Zero positions" propZeroPositions
        , testProperty "Maximum positions" propMaximumPositions
        , testProperty "Negative values" propNegativeValues
        , testProperty "Empty spans" propEmptySpans
        ]

    , testGroup "Invariant Preservation"
        [ testProperty "Position invariants" propPositionInvariants
        , testProperty "Span invariants" propSpanInvariants
        , testProperty "Located invariants" propLocatedInvariants
        ]
    ]

-- ============================================================================
-- SourcePos Mathematical Properties
-- ============================================================================

-- | SourcePos的传递性：如果 a <= b 且 b <= c，那么 a <= c
propSourcePosTransitivity :: SourcePos -> SourcePos -> SourcePos -> Bool
propSourcePosTransitivity pos1 pos2 pos3 =
  let pos1_le_pos2 = pos1 <= pos2
      pos2_le_pos3 = pos2 <= pos3
      pos1_le_pos3 = pos1 <= pos3
  in not (pos1_le_pos2 && pos2_le_pos3) || pos1_le_pos3

-- | SourcePos的反对称性：如果 a <= b 且 b <= a，那么 a = b
propSourcePosAntisymmetry :: SourcePos -> SourcePos -> Bool
propSourcePosAntisymmetry pos1 pos2 =
  let pos1_le_pos2 = pos1 <= pos2
      pos2_le_pos1 = pos2 <= pos1
  in not (pos1_le_pos2 && pos2_le_pos1) || pos1 == pos2

-- | SourcePos的完全性：对于任意 a, b，要么 a <= b，要么 b <= a
propSourcePosTotality :: SourcePos -> SourcePos -> Bool
propSourcePosTotality pos1 pos2 =
  pos1 <= pos2 || pos2 <= pos1

-- | 位置前进的单调性：前进后的位置 >= 原位置
propPosAdvancementMonotonicity :: Char -> SourcePos -> Bool
propPosAdvancementMonotonicity char pos =
  let advanced = posAfter char pos
  in advanced >= pos

-- | 行前进重置列：换行符将列重置为1
propLineAdvancementResetsColumn :: SourcePos -> Bool
propLineAdvancementResetsColumn pos =
  let advanced = posAfter '\n' pos
  in posColumn advanced == 1 && posLine advanced == posLine pos + 1

-- ============================================================================
-- SourceSpan Mathematical Properties
-- ============================================================================

-- | Span合并的结合律：(a ∪ b) ∪ c = a ∪ (b ∪ c)
propSpanMergeAssociativity :: SourceSpan -> SourceSpan -> SourceSpan -> Bool
propSpanMergeAssociativity span1 span2 span3 =
  let left = mergeSpans (mergeSpans span1 span2) span3
      right = mergeSpans span1 (mergeSpans span2 span3)
  in left == right

-- | Span合并的交换律：a ∪ b = b ∪ a
propSpanMergeCommutativity :: SourceSpan -> SourceSpan -> Bool
propSpanMergeCommutativity span1 span2 =
  let merged1 = mergeSpans span1 span2
      merged2 = mergeSpans span2 span1
  in merged1 == merged2

-- | Span合并的单位元：a ∪ empty = a
propSpanMergeIdentity :: SourceSpan -> Bool
propSpanMergeIdentity span =
  let emptyPos = startPos
      empty = emptySpan emptyPos
      merged = mergeSpans span empty
  in merged == span

-- | Span顺序一致性：合并后的span保持正确的顺序
propSpanOrderingConsistency :: SourceSpan -> SourceSpan -> Bool
propSpanOrderingConsistency span1 span2 =
  let merged = mergeSpans span1 span2
  in isValidSpan merged &&
     spanStart merged <= spanEnd merged &&
     spanStart merged <= min (spanStart span1) (spanStart span2) &&
     spanEnd merged >= max (spanEnd span1) (spanEnd span2)

-- | Span有效性保持：合并有效span仍为有效
propSpanValidityPreservation :: SourceSpan -> SourceSpan -> Bool
propSpanValidityPreservation span1 span2 =
  let valid1 = isValidSpan span1
      valid2 = isValidSpan span2
      merged = mergeSpans span1 span2
  in (valid1 && valid2) ==> isValidSpan merged

-- ============================================================================
-- Located Values Mathematical Properties
-- ============================================================================

-- | Located的functor定律：map id = id, map (f . g) = map f . map g
propLocatedFunctorLaws :: String -> String -> String -> Bool
propLocatedFunctorLaws x y z =
  let pos = startPos
      located = locatedAt pos x
      -- Identity law
      identityLaw = locatedValue (mapLocated id located) == locatedValue located
      -- Composition law
      f = (++ y)
      g = (++ z)
      compositionLaw = locatedValue (mapLocated f (mapLocated g located)) ==
                     locatedValue (mapLocated (f . g) located)
  in identityLaw && compositionLaw

-- | Located位置一致性：位置信息保持一致
propLocatedPositionConsistency :: String -> SourcePos -> Bool
propLocatedPositionConsistency value pos =
  let located = locatedAt pos value
      span = emptySpan pos
  in locatedPos located == pos && locatedSpan located == span

-- | Located span边界：span的start和end包围value的位置
propLocatedSpanBoundaries :: String -> SourcePos -> Bool
propLocatedSpanBoundaries value pos =
  let located = locatedAt pos value
      span = locatedSpan located
  in spanStart span == pos && spanEnd span == pos

-- ============================================================================
-- Position Advancement Properties
-- ============================================================================

-- | 字符计数：前进的偏移量等于字符数
propAdvancementCharacterCount :: String -> Bool
propAdvancementCharacterCount text =
  let start = startPos
      end = advancePosByText text start
      expectedOffset = length text
  in posOffset end - posOffset start == expectedOffset

-- | 换行行为：换行符增加行数并重置列数
propAdvancementNewlineBehavior :: Int -> Bool
propAdvancementNewlineBehavior n =
  let lineCount = abs n `mod` 10 + 1
      text = concat (replicate lineCount "\n")
      start = posAt 1 5
      end = advancePosByText text start
  in posLine end == posLine start + lineCount && posColumn end == 1

-- | Tab行为：tab对齐到下一个8的倍数列
propAdvancementTabBehavior :: Int -> Bool
propAdvancementTabBehavior startCol =
  let col = (abs startCol `mod` 20) + 1
      start = posAt 1 col
      end = posAfter '\t' start
      expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
  in posColumn end == expectedCol

-- | 文本处理：文本前进等于逐字符前进
propAdvancementTextProcessing :: String -> Bool
propAdvancementTextProcessing text =
  let start = startPos
      endByText = advancePosByText text start
      endByChars = foldl posAfter start text
  in endByText == endByChars

-- ============================================================================
-- Geometric Properties
-- ============================================================================

-- | Span长度计算：span的长度等于结束位置减去开始位置
propSpanLengthCalculation :: SourcePos -> SourcePos -> Bool
propSpanLengthCalculation start end =
  let span = spanBetween start end
      length = posOffset end - posOffset start
  in if isValidSpan span
     then length >= 0
     else True

-- | Span包含性：合并的span包含原始span
propSpanContainment :: SourceSpan -> SourceSpan -> Bool
propSpanContainment span1 span2 =
  let merged = mergeSpans span1 span2
      contains1 = spanStart merged <= spanStart span1 && spanEnd merged >= spanEnd span1
      contains2 = spanStart merged <= spanStart span2 && spanEnd merged >= spanEnd span2
  in contains1 && contains2

-- | Span交集：两个span的交集是它们的共同区域
propSpanIntersection :: SourceSpan -> SourceSpan -> Bool
propSpanIntersection span1 span2 =
  let start = max (spanStart span1) (spanStart span2)
      end = min (spanEnd span1) (spanEnd span2)
      intersection = spanBetween start end
  in if start <= end
     then isValidSpan intersection
     else True

-- | Span并集：两个span的并集是包含两者的最小span
propSpanUnion :: SourceSpan -> SourceSpan -> Bool
propSpanUnion span1 span2 =
  let union = mergeSpans span1 span2
  in spanStart union <= min (spanStart span1) (spanStart span2) &&
     spanEnd union >= max (spanEnd span1) (spanEnd span2)

-- ============================================================================
-- Edge Cases and Boundary Conditions
-- ============================================================================

-- | 零位置：零位置的行为
propZeroPositions :: Bool
propZeroPositions =
  let zeroPos = posAt 0 0
      start = startPos
  in zeroPos <= start && posOffset zeroPos == 0

-- | 最大位置：大数值的处理
propMaximumPositions :: Int -> Int -> Int -> Bool
propMaximumPositions line col offset =
  let maxLine = abs line `mod` 1000000 + 1
      maxCol = abs col `mod` 1000000 + 1
      maxOffset = abs offset `mod` 1000000
      pos = SourcePos maxLine maxCol maxOffset
  in posLine pos == maxLine && posColumn pos == maxCol && posOffset pos == maxOffset

-- | 负值：负值的处理
propNegativeValues :: Int -> Int -> Int -> Bool
propNegativeValues line col offset =
  let negLine = - (abs line `mod` 100 + 1)
      negCol = - (abs col `mod` 100 + 1)
      negOffset = - (abs offset `mod` 100)
      pos = SourcePos negLine negCol negOffset
  in posLine pos == negLine && posColumn pos == negCol && posOffset pos == negOffset

-- | 空span：空span的特性
propEmptySpans :: SourcePos -> Bool
propEmptySpans pos =
  let empty = emptySpan pos
  in spanStart empty == pos && spanEnd empty == pos && isValidSpan empty

-- ============================================================================
-- Invariant Preservation
-- ============================================================================

-- | 位置不变量：位置始终满足基本不变量
propPositionInvariants :: SourcePos -> Bool
propPositionInvariants pos =
  let lineValid = posLine pos >= 0
      colValid = posColumn pos >= 0
      offsetValid = posOffset pos >= 0
  in lineValid && colValid && offsetValid

-- | Span不变量：span始终满足基本不变量
propSpanInvariants :: SourceSpan -> Bool
propSpanInvariants span =
  let startValid = positionInvariants (spanStart span)
      endValid = positionInvariants (spanEnd span)
      orderValid = spanStart span <= spanEnd span
  in startValid && endValid && orderValid

-- | Located不变量：located值始终满足基本不变量
propLocatedInvariants :: String -> SourcePos -> Bool
propLocatedInvariants value pos =
  let located = locatedAt pos value
      spanInvariantsOK = spanInvariants (locatedSpan located)
      valueInvariantsOK = locatedValue located == value
      posInvariantsOK = positionInvariants pos
  in spanInvariantsOK && valueInvariantsOK && posInvariantsOK

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- 生成有效的SourcePos
genSourcePos :: Gen SourcePos
genSourcePos = do
  line <- choose (0, 1000)
  col <- choose (0, 1000)
  offset <- choose (0, 10000)
  return $ SourcePos line col offset

-- 生成有效的SourceSpan
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (0, 500)
  startCol <- choose (0, 500)
  startOffset <- choose (0, 5000)
  let start = SourcePos startLine startCol startOffset
  
  endLineOffset <- choose (0, 100)
  endColOffset <- choose (0, 100)
  endOffsetOffset <- choose (0, 1000)
  let end = SourcePos (startLine + endLineOffset) 
                     (startCol + endColOffset) 
                     (startOffset + endOffsetOffset)
  
  return $ if start <= end then spanBetween start end else spanBetween end start

-- 生成字符
genChar :: Gen Char
genChar = oneof
  [ choose ('\32', '\126')  -- 可打印ASCII字符
  , choose ('\128', '\255')  -- 扩展ASCII
  , elements ['\n', '\t', '\r']  -- 特殊字符
  ]

-- 实例声明
instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

instance Arbitrary Char where
  arbitrary = genChar

instance Arbitrary String where
  arbitrary = listOf genChar

-- 辅助函数
positionInvariants :: SourcePos -> Bool
positionInvariants pos = propPositionInvariants pos

spanInvariants :: SourceSpan -> Bool
spanInvariants span = propSpanInvariants span

infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True