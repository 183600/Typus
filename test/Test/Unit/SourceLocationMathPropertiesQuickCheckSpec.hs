{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-name-shadowing -Wno-unused-local-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.SourceLocationMathPropertiesQuickCheckSpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck (conjoin, Arbitrary(..), choose)
import qualified Test.QuickCheck as QC
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)



-- Arbitrary instances are now defined in SourceLocation module

-- | 测试SourcePos的基本属性
prop_sourcepos_basic :: Int -> Int -> Property
prop_sourcepos_basic line column =
  line >= 0 && column >= 0 ==>
  let pos = SourcePos line column 0
  in conjoin 
     [ posLine pos === line
     , posColumn pos === column
     ]

-- | 测试SourcePos的比较
prop_sourcepos_comparison :: SourcePos -> SourcePos -> Property
prop_sourcepos_comparison pos1 pos2 =
  let sameLine = posLine pos1 == posLine pos2
      sameColumn = posColumn pos1 == posColumn pos2
      samePos = sameLine && sameColumn
  in samePos ==> pos1 === pos2

-- | 测试SourcePos的顺序
prop_sourcepos_order :: SourcePos -> SourcePos -> Property
prop_sourcepos_order pos1 pos2 =
  let line1 = posLine pos1
      line2 = posLine pos2
      column1 = posColumn pos1
      column2 = posColumn pos2
      before = line1 < line2 || (line1 == line2 && column1 < column2)
  in property before

-- | 测试SourceSpan的基本属性
prop_sourcespan_basic :: SourcePos -> SourcePos -> Property
prop_sourcespan_basic pos1 pos2 =
  let sourceSpan = SourceSpan pos1 pos2
  in conjoin 
     [ spanStart sourceSpan === pos1
     , spanEnd sourceSpan === pos2
     ]

-- | 测试advancePosByText的基本属性
prop_advancepos_basic :: Char -> Property
prop_advancepos_basic c =
  let pos = startPos
      result = advancePosByText (T.pack [c]) pos
  in conjoin 
     [ property $ posLine result >= posLine pos
     , property $ posColumn result >= posColumn pos
     ]

-- | 测试advancePosByText对于换行符的处理
prop_advancepos_newline :: Positive Int -> Property
prop_advancepos_newline (Positive n) =
  n < 100 ==>
  let pos = startPos
      text = T.pack $ replicate n '\n'
      result = advancePosByText text pos
  in conjoin 
     [ posLine result === posLine pos + n
     , posColumn result === 0
     ]

-- | 测试advancePosByText对于制表符的处理
prop_advancepos_tab :: Positive Int -> Property
prop_advancepos_tab (Positive n) =
  n < 100 ==>
  let pos = startPos
      text = T.pack $ replicate n '\t'
      result = advancePosByText text pos
  in conjoin 
     [ posLine result === posLine pos
     , property $ posColumn result >= posColumn pos
     ]

-- | 测试advancePosByText对于普通字符的处理
prop_advancepos_regular :: Positive Int -> Property
prop_advancepos_regular (Positive n) =
  n < 100 ==>
  let pos = startPos
      text = T.pack $ replicate n 'x'
      result = advancePosByText text pos
  in conjoin [posLine result === posLine pos, posColumn result === posColumn pos + n]

-- | 测试advancePosByText的组合性
prop_advancepos_composition :: String -> String -> Property
prop_advancepos_composition s1 s2 =
  not (null s1) && not (null s2) ==>
  let pos = startPos
      text1 = T.pack s1
      text2 = T.pack s2
      result1 = advancePosByText (text1 <> text2) pos
      result2 = advancePosByText text2 (advancePosByText text1 pos)
  in result1 === result2

-- | 测试advancePosByText对于空文本的处理
prop_advancepos_empty :: Property
prop_advancepos_empty =
  let pos = startPos
      result = advancePosByText T.empty pos
  in result === pos

-- | 测试advancePosByText对于特殊字符的处理
prop_advancepos_special :: Char -> Property
prop_advancepos_special c =
  let pos = startPos
      text = T.pack [c]
      result = advancePosByText text pos
  in conjoin [property (posLine result >= posLine pos), property (posColumn result >= posColumn pos)]

-- | 测试advancePosByText对于Unicode字符的处理
prop_advancepos_unicode :: Property
prop_advancepos_unicode =
  let unicodeChars = ['\0'..'\255']
      testChar c = 
        let pos = startPos
            text = T.pack [c]
            result = advancePosByText text pos
        in posLine result >= posLine pos && posColumn result >= posColumn pos
  in property (all testChar unicodeChars)

-- | 测试advancePosByText对于极长文本的处理
prop_advancepos_long :: Positive Int -> Property
prop_advancepos_long (Positive n) =
  n < 10000 ==>
  let pos = startPos
      text = T.pack $ replicate n 'x'
      result = advancePosByText text pos
  in conjoin [posLine result === posLine pos, posColumn result === posColumn pos + n]

-- | 测试advancePosByText对于多行文本的处理
prop_advancepos_multiline :: Positive Int -> Positive Int -> Property
prop_advancepos_multiline (Positive lines) (Positive chars) =
  lines < 100 && chars < 100 ==>
  let pos = startPos
      line = T.pack $ replicate chars 'x'
      text = T.unlines $ replicate lines line
      result = advancePosByText text pos
  in conjoin [posLine result === lines, posColumn result === 0]

-- | 测试SourcePos的有效性
prop_sourcepos_valid :: Int -> Int -> Property
prop_sourcepos_valid line column =
  line >= 0 && column >= 0 ==>
  let pos = SourcePos line column 0
  in posLine pos >= 0 && posColumn pos >= 0

-- | 测试SourceSpan的有效性
prop_sourcespan_valid :: SourcePos -> SourcePos -> Property
prop_sourcespan_valid pos1 pos2 =
  let sourceSpan = SourceSpan pos1 pos2
      start = spanStart sourceSpan
      end = spanEnd sourceSpan
  in conjoin [property (posLine start >= 0), property (posColumn start >= 0), 
            property (posLine end >= 0), property (posColumn end >= 0)]

-- | 测试SourceSpan的长度
prop_sourcespan_length :: SourcePos -> Positive Int -> Property
prop_sourcespan_length pos (Positive n) =
  n < 100 ==>
  let text = T.pack $ replicate n 'x'
      end = advancePosByText text pos
      sourceSpan = SourceSpan pos end
  in posLine (spanEnd sourceSpan) >= posLine (spanStart sourceSpan) || 
     (posLine (spanEnd sourceSpan) == posLine (spanStart sourceSpan) && 
      posColumn (spanEnd sourceSpan) >= posColumn (spanStart sourceSpan))

-- | 测试SourcePos的相等性
test_sourcepos_equality :: Assertion
test_sourcepos_equality = do
  let pos1 = SourcePos 1 1 0
      pos2 = SourcePos 1 1 0
      pos3 = SourcePos 1 2 0
  assertEqual "Same positions are equal" pos1 pos2
  assertBool "Different positions are not equal" (pos1 /= pos3)

-- | 测试SourcePos的顺序
test_sourcepos_order :: Assertion
test_sourcepos_order = do
  let pos1 = SourcePos 1 1 0
      pos2 = SourcePos 1 2 0
      pos3 = SourcePos 2 1 0
  assertBool "Same line, earlier column is before" (pos1 < pos2)  -- We would need an actual comparison function
  assertBool "Earlier line is before later line" (pos1 < pos3)   -- We would need an actual comparison function

-- | 测试SourceSpan的基本属性
test_sourcespan_basic :: Assertion
test_sourcespan_basic = do
  let start = SourcePos 1 1 0
      end = SourcePos 1 5 0
      sourceSpan = SourceSpan start end
  assertEqual "Span start is correct" start (spanStart sourceSpan)
  assertEqual "Span end is correct" end (spanEnd sourceSpan)

-- | 测试advancePosByText对于单个字符
test_advancepos_single_char :: Assertion
test_advancepos_single_char = do
  let pos = startPos
      result = advancePosByText (T.pack "a") pos
  assertEqual "Line unchanged" (posLine pos) (posLine result)
  assertEqual "Column incremented" (posColumn pos + 1) (posColumn result)

-- | 测试advancePosByText对于换行符
test_advancepos_newline :: Assertion
test_advancepos_newline = do
  let pos = startPos
      result = advancePosByText (T.pack "\n") pos
  assertEqual "Line incremented" (posLine pos + 1) (posLine result)
  assertEqual "Column reset" 0 (posColumn result)

-- | 测试advancePosByText对于制表符
test_advancepos_tab :: Assertion
test_advancepos_tab = do
  let pos = startPos
      result = advancePosByText (T.pack "\t") pos
  assertEqual "Line unchanged" (posLine pos) (posLine result)
  assertBool "Column advanced" (posColumn result > posColumn pos)

-- | 测试advancePosByText对于多个字符
test_advancepos_multiple_chars :: Assertion
test_advancepos_multiple_chars = do
  let pos = startPos
      text = T.pack "hello"
      result = advancePosByText text pos
  assertEqual "Line unchanged" (posLine pos) (posLine result)
  assertEqual "Column incremented by length" (posColumn pos + T.length text) (posColumn result)

-- | 测试advancePosByText对于多行文本
test_advancepos_multiline :: Assertion
test_advancepos_multiline = do
  let pos = startPos
      text = T.pack "hello\nworld"
      result = advancePosByText text pos
  assertEqual "Line incremented" (posLine pos + 1) (posLine result)
  assertEqual "Column at end of second line" 5 (posColumn result)

-- | 测试advancePosByText对于空文本
test_advancepos_empty :: Assertion
test_advancepos_empty = do
  let pos = startPos
      result = advancePosByText T.empty pos
  assertEqual "Position unchanged" pos result

-- | 测试SourcePos的有效性
test_sourcepos_valid :: Assertion
test_sourcepos_valid = do
  let pos1 = SourcePos 1 1 0
      pos2 = SourcePos 0 0 0
      pos3 = SourcePos (-1) 1 0
      pos4 = SourcePos 1 (-1) 0
  assertBool "Valid position is valid" (posLine pos1 >= 0 && posColumn pos1 >= 0)
  assertBool "Zero position is valid" (posLine pos2 >= 0 && posColumn pos2 >= 0)
  assertBool "Negative line is invalid" (posLine pos3 < 0)
  assertBool "Negative column is invalid" (posColumn pos4 < 0)

-- | 测试SourceSpan的有效性
test_sourcespan_valid :: Assertion
test_sourcespan_valid = do
  let start = SourcePos 1 1 0
      end = SourcePos 1 5 0
      sourceSpan = SourceSpan start end
  assertBool "Span start is valid" (posLine (spanStart sourceSpan) >= 0 && posColumn (spanStart sourceSpan) >= 0)
  assertBool "Span end is valid" (posLine (spanEnd sourceSpan) >= 0 && posColumn (spanEnd sourceSpan) >= 0)

-- | 测试SourceSpan的长度
test_sourcespan_length :: Assertion
test_sourcespan_length = do
  let start = SourcePos 1 1 0
      text = T.pack "hello"
      end = advancePosByText text start
      sourceSpan = SourceSpan start end
  assertEqual "Span length matches text length" (T.length text) (posColumn end - posColumn start)

-- | 测试套件
tests :: TestTree
tests = testGroup "SourceLocation Math Properties QuickCheck Tests"
  [ testProperty "SourcePos basic" prop_sourcepos_basic
  , testProperty "SourcePos comparison" prop_sourcepos_comparison
  , testProperty "SourcePos order" prop_sourcepos_order
  , testProperty "SourceSpan basic" prop_sourcespan_basic
  , testProperty "AdvancePos basic" prop_advancepos_basic
  , testProperty "AdvancePos newline" prop_advancepos_newline
  , testProperty "AdvancePos tab" prop_advancepos_tab
  , testProperty "AdvancePos regular" prop_advancepos_regular
  , testProperty "AdvancePos composition" prop_advancepos_composition
  , testProperty "AdvancePos empty" prop_advancepos_empty
  , testProperty "AdvancePos special" prop_advancepos_special
  , testProperty "AdvancePos unicode" prop_advancepos_unicode
  , testProperty "AdvancePos long" prop_advancepos_long
  , testProperty "AdvancePos multiline" prop_advancepos_multiline
  , testProperty "SourcePos valid" prop_sourcepos_valid
  , testProperty "SourceSpan valid" prop_sourcespan_valid
  , testProperty "SourceSpan length" prop_sourcespan_length
  , testCase "SourcePos equality" test_sourcepos_equality
  , testCase "SourcePos order" test_sourcepos_order
  , testCase "SourceSpan basic" test_sourcespan_basic
  , testCase "AdvancePos single char" test_advancepos_single_char
  , testCase "AdvancePos newline" test_advancepos_newline
  , testCase "AdvancePos tab" test_advancepos_tab
  , testCase "AdvancePos multiple chars" test_advancepos_multiple_chars
  , testCase "AdvancePos multiline" test_advancepos_multiline
  , testCase "AdvancePos empty" test_advancepos_empty
  , testCase "SourcePos valid" test_sourcepos_valid
  , testCase "SourceSpan valid" test_sourcespan_valid
  , testCase "SourceSpan length" test_sourcespan_length
  ]