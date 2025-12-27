{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.Unit.NewQuickCheckSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, choose, getSmall)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , startPos
  , posAfter
  , posAt
  , emptySpan
  , spanFrom
  , spanTo
  , mergeSpans
  , isValidSpan
  , spanStart
  , spanEnd
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , mapLocated
  )

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Char (isAlphaNum)

-- | 生成小的非负整数用于行列位置
smallNat :: Gen Int
smallNat = choose (0, 1000)

-- | 生成文件名
genFileName :: Gen String
genFileName = do
  base <- elements ["test", "example", "main", "module", "file"]
  ext <- elements [".typus", ".go", ".hs", ""]
  n <- choose (1, 99)
  return $ base ++ show n ++ ext

-- | 生成任意的 SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    file <- genFileName
    line <- smallNat
    column <- smallNat
    return $ SourcePos file (line + 1) (column + 1)  -- 确保行列从1开始

-- | 生成任意的 SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    -- 确保end >= start
    lineDiff <- choose (0, 10)
    colDiff <- choose (0, 20)
    let endLine = posLine start + lineDiff
        endCol = if lineDiff == 0 
                 then posColumn start + colDiff
                 else colDiff + 1
    let end = SourcePos (posFile start) endLine (max 1 endCol)
    return $ SourceSpan start end

tests :: TestTree
tests = testGroup "New QuickCheck SourceLocation Tests"
  [ sourcePosProperties
  , sourceSpanProperties
  , locatedValueProperties
  , mergeSpanProperties
  , positionMathProperties
  ]

-- | SourcePos 属性测试
sourcePosProperties :: TestTree
sourcePosProperties = testGroup "SourcePos Properties"
  [ testProperty "posAt creates position with given values" $ \file line col ->
      let pos = posAt file line col
      in posFile pos === file && posLine pos === line && posColumn pos === col
      
  , testProperty "startPos creates position at line 1, column 1" $ \file ->
      let pos = startPos file
      in posLine pos === 1 && posColumn pos === 1 && posFile pos === file
      
  , testProperty "posAfter newline increments line, resets column" $ \pos ->
      let newPos = posAfter pos '\n'
      in posLine newPos === posLine pos + 1 && posColumn newPos === 1
      
  , testProperty "posAfter non-newline increments column" $ \pos char ->
      let newPos = posAfter pos char
      in if char /= '\n'
         then posLine newPos === posLine pos && posColumn newPos === posColumn pos + 1
         else property True
         
  , testProperty "posAfter preserves file name" $ \pos char ->
      let newPos = posAfter pos char
      in posFile newPos === posFile pos
  ]

-- | SourceSpan 属性测试
sourceSpanProperties :: TestTree
sourceSpanProperties = testGroup "SourceSpan Properties"
  [ testProperty "spanFrom creates zero-length span" $ \pos ->
      let span = spanFrom pos
      in spanStart span === pos && spanEnd span === pos
      
  , testProperty "spanTo creates span from startPos" $ \pos ->
      let span = spanTo pos
          start = startPos (posFile pos)
      in spanStart span === start && spanEnd span === pos
      
  , testProperty "emptySpan has start and end at startPos of empty file" $ \() ->
      let empty = emptySpan
          start = spanStart empty
          end = spanEnd empty
      in posFile start === "" && posLine start === 1 && posColumn start === 1 &&
         start === end
         
  , testProperty "isValidSpan returns true for well-formed spans" $ \span ->
      let start = spanStart span
          end = spanEnd span
          valid = posLine start < posLine end ||
                  (posLine start == posLine end && posColumn start <= posColumn end)
      in isValidSpan span === valid
  ]

-- | Located 值属性测试
locatedValueProperties :: TestTree
locatedValueProperties = testGroup "Located Value Properties"
  [ testProperty "locatedAt creates located value at position" $ \pos value ->
      let located = locatedAt pos value
      in locatedValue located === value && spanStart (locatedSpan located) === pos &&
         spanEnd (locatedSpan located) === pos
         
  , testProperty "locatedWithSpan creates located value with span" $ \span value ->
      let located = locatedWithSpan span value
      in locatedValue located === value && locatedSpan located === span
         
  , testProperty "mapLocated preserves location" $ \span f ->
      let value = "test"
          located = locatedWithSpan span value
          newValue = value ++ " mapped"
          mapped = mapLocated (const newValue) located
      in locatedSpan mapped === locatedSpan located &&
         locatedValue mapped === newValue
  ]

-- | 合并范围的属性测试
mergeSpanProperties :: TestTree
mergeSpanProperties = testGroup "Merge Span Properties"
  [ testProperty "mergeSpans is commutative" $ \span1 span2 ->
      let merged1 = mergeSpans span1 span2
          merged2 = mergeSpans span2 span1
      in merged1 === merged2
      
  , testProperty "mergeSpans is associative" $ \span1 span2 span3 ->
      let merged1 = mergeSpans (mergeSpans span1 span2) span3
          merged2 = mergeSpans span1 (mergeSpans span2 span3)
      in merged1 === merged2
      
  , testProperty "mergeSpans contains both spans" $ \span1 span2 ->
      let merged = mergeSpans span1 span2
          start1 = spanStart span1
          end1 = spanEnd span1
          start2 = spanStart span2
          end2 = spanEnd span2
          mergedStart = spanStart merged
          mergedEnd = spanEnd merged
      in (mergedStart `isBeforeOrEqual` start1 && end1 `isBeforeOrEqual` mergedEnd &&
          mergedStart `isBeforeOrEqual` start2 && end2 `isBeforeOrEqual` mergedEnd)
          
  , testProperty "mergeSpans with empty span returns other" $ \span ->
      let empty = emptySpan
          merged1 = mergeSpans span empty
          merged2 = mergeSpans empty span
      in merged1 === span && merged2 === span
      
  , testProperty "mergeSpans with self returns self" $ \span ->
      let merged = mergeSpans span span
      in merged === span
  ]

-- | 位置数学属性测试
positionMathProperties :: TestTree
positionMathProperties = testGroup "Position Math Properties"
  [ testProperty "posAfter preserves monotonicity" $ \pos chars ->
      let positions = scanl posAfter pos chars
          isMonotonic = all (\(p1, p2) -> 
                               posLine p1 < posLine p2 ||
                               (posLine p1 == posLine p2 && posColumn p1 <= posColumn p2))
                           (zip positions (tail positions))
      in if null (tail positions)
         then property True
         else property isMonotonic
         
  , testProperty "multiple newlines accumulate correctly" $ \pos ->
      let pos1 = posAfter pos '\n'
          pos2 = posAfter pos1 '\n'
          pos3 = posAfter pos2 '\n'
      in posLine pos1 === posLine pos + 1 &&
         posLine pos2 === posLine pos + 2 &&
         posLine pos3 === posLine pos + 3 &&
         all (\p -> posColumn p == 1) [pos1, pos2, pos3]
         
  , testProperty "multiple characters advance column correctly" $ \pos ->
      let chars = "abcde"
          finalPos = foldl posAfter pos chars
      in posLine finalPos === posLine pos &&
         posColumn finalPos === posColumn pos + length chars
         
  , testProperty "mixed newlines and characters" $ \pos ->
      let chars = "ab\ncd\nef"
          finalPos = foldl posAfter pos chars
          expectedLine = posLine pos + 2  -- 2 newlines
          expectedCol = 2  -- "ef" after last newline
      in posLine finalPos === expectedLine && posColumn finalPos === expectedCol
  ]

-- | 辅助函数：检查位置顺序
isBeforeOrEqual :: SourcePos -> SourcePos -> Bool
isBeforeOrEqual pos1 pos2 =
  let line1 = posLine pos1
      line2 = posLine pos2
      col1 = posColumn pos1
      col2 = posColumn pos2
  in if line1 < line2
     then True
     else if line1 > line2
          then False
          else col1 <= col2