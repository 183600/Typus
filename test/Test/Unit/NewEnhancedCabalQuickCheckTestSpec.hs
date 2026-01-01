{-# LANGUAGE CPP #-}

module Test.Unit.NewEnhancedCabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.QuickCheck ((===), Arbitrary(..), Gen, oneof, elements, listOf, choose, sized)
import qualified Test.QuickCheck as QC

import TestSupport.QuickCheck (fastProperty)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, emptySpan, spanFrom, mergeSpans, isValidSpan)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub)

-- | 10个新的QuickCheck测试用例，覆盖Typus项目的核心功能
tests :: TestTree
tests =
  testGroup "New Enhanced Cabal QuickCheck Tests"
    [ -- Utils模块测试
      testGroup "Utils Module Properties"
        [ fastProperty "trim: 双重应用等于单次应用" propTrimIdempotent
        , fastProperty "splitBy: 分割后重新连接等于原始字符串" propSplitByRoundtrip
        , fastProperty "splitByCollapsed: 不产生空字符串" propSplitByCollapsedNoEmpty
        , fastProperty "breakOn: 结果连接等于原始字符串" propBreakOnRoundtrip
        ]
    
    -- SourceLocation模块测试
    , testGroup "SourceLocation Module Properties"
        [ fastProperty "SourcePos: posAfter增加行号时列号重置为1" propPosAfterNewline
        , fastProperty "SourceSpan: mergeSpans包含两个原始span" propMergeSpansContains
        , fastProperty "SourceSpan: emptySpan总是无效" propEmptySpanInvalid
        ]
    
    -- 解析器相关测试
    , testGroup "Parser Related Properties"
        [ fastProperty "注释移除: 不改变字符串字面量内容" propCommentPreservesLiterals
        , fastProperty "缩进规范化: 保持相对缩进关系" propIndentationPreservesStructure
        ]
    
    -- 综合测试
    , testGroup "Integration Properties"
        [ fastProperty "字符串处理链: trim和normalizeIndentation可交换" propTrimIndentationCommutative
        , fastProperty "错误位置: 有效span的起始位置不大于结束位置" propValidSpanOrder
        ]
    ]

-- ============================================================================
-- Utils模块属性测试
-- ============================================================================

-- | trim函数是幂等的：trim(trim(x)) == trim(x)
propTrimIdempotent :: String -> Bool
propTrimIdempotent input =
  let once = trim input
      twice = trim once
  in once == twice

-- | splitBy分割后用分隔符重新连接等于原始字符串
propSplitByRoundtrip :: Char -> String -> Bool
propSplitByRoundtrip delim input =
  let parts = splitBy delim input
      rejoined = concatMap (\p -> if null p then "" else p ++ [delim]) (init parts) ++ 
                 if null parts then "" else last parts
  in rejoined == input

-- | splitByCollapsed从不产生空字符串
propSplitByCollapsedNoEmpty :: Char -> String -> Bool
propSplitByCollapsedNoEmpty delim input =
  let parts = splitByCollapsed delim input
  in L.all (not . null) parts

-- | breakOn的结果连接起来等于原始字符串
propBreakOnRoundtrip :: String -> String -> Bool
propBreakOnRoundtrip pattern input =
  let (prefix, suffix) = breakOn pattern input
  in if null pattern
     then prefix == "" && suffix == input
     else prefix ++ pattern ++ suffix == input

-- ============================================================================
-- SourceLocation模块属性测试
-- ============================================================================

-- | posAfter在遇到换行符时将列号重置为1
propPosAfterNewline :: SourcePos -> Bool
propPosAfterNewline pos =
  let posAfterNewline = posAfter pos '\n'
  in sourceColumn posAfterNewline == 1 && 
     sourceLine posAfterNewline == sourceLine pos + 1

-- | mergeSpans的结果包含两个原始span
propMergeSpansContains :: Int -> Int -> Int -> Int -> Bool
propMergeSpansContains start1 end1 start2 end2 =
  let pos1 = startPos { sourceLine = start1, sourceColumn = end1 }
      pos2 = startPos { sourceLine = start2, sourceColumn = end2 }
      span1 = spanFrom pos1 end1
      span2 = spanFrom pos2 end2
      merged = mergeSpans span1 span2
      mergedStart = spanStart merged
      mergedEnd = spanEnd merged
  in isValidSpan merged &&
     (sourceLine mergedStart <= sourceLine pos1 || sourceLine mergedStart <= sourceLine pos2) &&
     (sourceLine mergedEnd >= sourceLine pos1 || sourceLine mergedEnd >= sourceLine pos2)

-- | emptySpan总是无效的
propEmptySpanInvalid :: Bool
propEmptySpanInvalid = not (isValidSpan emptySpan)

-- ============================================================================
-- 解析器相关属性测试
-- ============================================================================

-- | 注释移除函数不改变字符串字面量内容
propCommentPreservesLiterals :: String -> Bool
propCommentPreservesLiterals input =
  let processed = removeLineComments input
      -- 简单检查：确保引号内的内容不被改变
      quotedContent inputStr = extractQuotedContent inputStr
      quotedProcessed = extractQuotedContent processed
  in quotedContent input == quotedProcessed
  where
    extractQuotedContent :: String -> [String]
    extractQuotedContent [] = []
    extractQuotedContent ('"':rest) = 
      let (content, _) = break (== '"') rest
      in content : extractQuotedContent (drop 1 rest)
    extractQuotedContent (_:rest) = extractQuotedContent rest

-- | 缩进规范化保持相对缩进关系
propIndentationPreservesStructure :: String -> Bool
propIndentationPreservesStructure input =
  let normalized = normalizeIndentation input
      originalLines = lines input
      normalizedLines = lines normalized
      -- 检查非空行的相对关系是否保持
      originalIndents = L.map (L.length . takeWhile isSpace) $ L.filter (not . L.all isSpace) originalLines
      normalizedIndents = L.map (L.length . takeWhile isSpace) $ L.filter (not . L.all isSpace) normalizedLines
  in L.length originalIndents == L.length normalizedIndents &&
     if not (null originalIndents) && not (null normalizedIndents)
     then L.all (>=0) (zipWith (-) (L.tail normalizedIndents) (init normalizedIndents)) ==
        L.all (>=0) (zipWith (-) (L.tail originalIndents) (init originalIndents))
     else True

-- ============================================================================
-- 综合属性测试
-- ============================================================================

-- | trim和normalizeIndentation操作是可交换的
propTrimIndentationCommutative :: String -> Bool
propTrimIndentationCommutative input =
  let trimThenIndent = normalizeIndentation (trim input)
      indentThenTrim = trim (normalizeIndentation input)
  in trimThenIndent == indentThenTrim

-- | 有效span的起始位置不大于结束位置
propValidSpanOrder :: Int -> Int -> Int -> Int -> Bool
propValidSpanOrder startLine startCol endLine endCol =
  let start = startPos { sourceLine = max 1 startLine, sourceColumn = max 1 startCol }
      endPos = startPos { sourceLine = max 1 endLine, sourceColumn = max 1 endCol }
      span = spanFrom start (sourceColumn endPos)
  in not (isValidSpan span) || 
     (sourceLine (spanStart span) < sourceLine (spanEnd span) ||
      (sourceLine (spanStart span) == sourceLine (spanEnd span) &&
       sourceColumn (spanStart span) <= sourceColumn (spanEnd span)))

-- ============================================================================
-- QuickCheck Arbitrary实例
-- ============================================================================

-- 为String生成更合理的测试数据
instance Arbitrary String where
  arbitrary = sized $ \n -> do
    len <- choose (0, n)
    listOf $ oneof [QC.arbitraryBoundedEnum, elements " \t\n\r//*/\"'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789,:;{}[]()"]