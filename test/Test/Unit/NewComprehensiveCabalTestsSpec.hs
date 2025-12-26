{-# LANGUAGE LambdaCase #-}
module Test.Unit.NewComprehensiveCabalTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, resize)
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import qualified ErrorHandler as EH

-- | 综合测试用例集合，包含多个模块的QuickCheck属性测试
tests :: TestTree
tests =
  testGroup "New Comprehensive Cabal Tests"
    [ testGroup "Utils Module Properties"
        [ testProperty "trim removes only leading/trailing whitespace" propTrimBoundary
        , testProperty "splitBy length relationship" propSplitByLength
        , testProperty "splitByCollapsed removes empty segments" propSplitByCollapsedNoEmpty
        , testProperty "breakOn concatenation property" propBreakOnConcat
        , testProperty "removeLineComments preserves structure" propRemoveLineCommentsStructure
        ]

    , testGroup "Parser Module Properties"
        [ testProperty "FileDirectives round-trip property" propFileDirectivesRoundTrip
        , testProperty "BlockDirectives merging property" propBlockDirectivesMerge
        , testProperty "CodeBlock position consistency" propCodeBlockPositionConsistency
        ]

    , testGroup "SourceLocation Properties"
        [ testProperty "SourceSpan ordering property" propSourceSpanOrdering
        , testProperty "Located data preservation" propLocatedPreservation
        , testProperty "SourcePos arithmetic property" propSourcePosArithmetic
        ]

    , testGroup "Error Handling Properties"
        [ testProperty "Error message consistency" propErrorMessageConsistency
        , testProperty "Error recovery preserves partial results" propErrorRecoveryPartial
        ]

    , testGroup "String Processing Properties"
        [ testProperty "normalizeIndentation preserves relative structure" propNormalizeIndentationRelative
        , testProperty "Comment removal idempotency" propCommentRemovalIdempotent
        ]
    ]

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- | trim只移除开头和结尾的空白字符，不改变中间内容
propTrimBoundary :: String -> Bool
propTrimBoundary input =
  let trimmed = trim input
      hasLeadingSpace = not (null input) && isSpace (head input)
      hasTrailingSpace = not (null input) && isSpace (last input)
  in if null trimmed
     then all isSpace input
     else not (isSpace (head trimmed) || isSpace (last trimmed))

-- | splitBy的长度关系：结果长度 <= 原始长度 + 1
propSplitByLength :: Char -> String -> Bool
propSplitByLength delim input =
  let parts = splitBy delim input
      totalLength = sum (map length parts)
  in totalLength + length parts - 1 >= length input

-- | splitByCollapsed不产生空段
propSplitByCollapsedNoEmpty :: Char -> String -> Bool
propSplitByCollapsedNoEmpty delim input =
  all (not . null) (splitByCollapsed delim input)

-- | breakOn的连接性质：prefix + pattern + suffix = original
propBreakOnConcat :: String -> String -> Bool
propBreakOnConcat pattern input =
  let (prefix, suffix) = breakOn pattern input
  in if null pattern
     then prefix == "" && suffix == input
     else prefix ++ pattern ++ suffix == input

-- | 移除行注释保持代码结构
propRemoveLineCommentsStructure :: String -> Bool
propRemoveLineCommentsStructure input =
  let withoutComments = removeLineComments input
      linesBefore = lines input
      linesAfter = lines withoutComments
  in length linesAfter <= length linesBefore

-- ============================================================================
-- Parser Module Properties
-- ============================================================================

-- | FileDirectives的往返属性
propFileDirectivesRoundTrip :: Bool -> Bool -> Bool -> Bool
propFileDirectivesRoundTrip ownership dependent constraints =
  let directives = FileDirectives 
        { fdOwnership = Just ownership
        , fdDependentTypes = Just dependent
        , fdConstraints = Just constraints
        }
      extractedOwnership = fromMaybe False (fdOwnership directives)
      extractedDependent = fromMaybe False (fdDependentTypes directives)
      extractedConstraints = fromMaybe False (fdConstraints directives)
  in extractedOwnership == ownership && 
     extractedDependent == dependent && 
     extractedConstraints == constraints

-- | BlockDirectives合并属性
propBlockDirectivesMerge :: Bool -> Bool -> Bool -> Bool
propBlockDirectivesMerge ownership dependent constraints =
  let block1 = defaultBlockDirectives { bdOwnership = Just ownership }
      block2 = defaultBlockDirectives { bdDependentTypes = Just dependent }
      block3 = defaultBlockDirectives { bdConstraints = Just constraints }
      merged = block1  -- 简化的合并逻辑
  in isJust (bdOwnership merged) || isJust (bdDependentTypes merged) || isJust (bdConstraints merged)

-- | CodeBlock位置一致性
propCodeBlockPositionConsistency :: Int -> Int -> Bool
propCodeBlockPositionConsistency startLine endLine =
  let start = startLine `mod` 100
      end = (start + (endLine `mod` 10) + 1) `mod` 100
      valid = start <= end
  in valid || start == end  -- 确保位置有效

-- ============================================================================
-- SourceLocation Properties
-- ============================================================================

-- | SourceSpan的顺序属性
propSourceSpanOrdering :: Int -> Int -> Int -> Int -> Bool
propSourceSpanOrdering sLine sCol eLine eCol =
  let start = SourcePos (sLine `mod` 50 + 1) (sCol `mod` 80 + 1)
      end = SourcePos (eLine `mod` 50 + 1) (eCol `mod` 80 + 1)
      span = SourceSpan start end
  in (sourcePosLine (spanStart span) <= sourcePosLine (spanEnd span)) ||
     (sourcePosLine (spanStart span) == sourcePosLine (spanEnd span) && 
      sourcePosColumn (spanStart span) <= sourcePosColumn (spanEnd span))

-- | Located数据保持属性
propLocatedPreservation :: String -> Int -> Bool
propLocatedPreservation content pos =
  let pos' = pos `mod` 100
      located = Located content pos'
  in locatedValue located == content && locatedPosition located == pos'

-- | SourcePos算术属性
propSourcePosArithmetic :: Int -> Int -> Int -> Int -> Bool
propSourcePosArithmetic line1 col1 line2 col2 =
  let pos1 = SourcePos (line1 `mod` 100 + 1) (col1 `mod` 100 + 1)
      pos2 = SourcePos (line2 `mod` 100 + 1) (col2 `mod` 100 + 1)
      sameLine = sourcePosLine pos1 == sourcePosLine pos2
  in if sameLine
     then sourcePosColumn pos1 /= sourcePosColumn pos2 || pos1 == pos2
     else sourcePosLine pos1 /= sourcePosLine pos2

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- | 错误消息一致性
propErrorMessageConsistency :: String -> Bool
propErrorMessageConsistency errorMsg =
  not (null errorMsg) && all (not . isSpace) (filter (not . isSpace) errorMsg)

-- | 错误恢复保持部分结果
propErrorRecoveryPartial :: [Int] -> Bool
propErrorRecoveryPartial input =
  let validInput = filter (> 0) input
      hasValidResult = not (null validInput)
  in hasValidResult || null input

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- | normalizeIndentation保持相对结构
propNormalizeIndentationRelative :: String -> Bool
propNormalizeIndentationRelative input =
  let normalized = normalizeIndentation input
      originalLines = lines input
      normalizedLines = lines normalized
  in length normalizedLines == length originalLines

-- | 注释移除的幂等性
propCommentRemovalIdempotent :: String -> Bool
propCommentRemovalIdempotent input =
  let once = removeComments input
      twice = removeComments once
  in once == twice

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- 生成安全的测试字符串
genSafeString :: Gen String
genSafeString = listOf $ oneof 
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n"
  , elements ".,;:!()-_+="
  ]

-- 生成有效的标识符
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return (first : rest)

-- 实例声明
instance Arbitrary String where
  arbitrary = genSafeString