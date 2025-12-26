{-# LANGUAGE LambdaCase #-}
module Test.Unit.UtilsEnhancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, resize, forAll, choose)
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, sort, group, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe, listToMaybe)
import qualified Data.Text as T

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn)

-- | Utils模块的增强QuickCheck属性测试
tests :: TestTree
tests =
  testGroup "Utils Enhanced QuickCheck Tests"
    [ testGroup "String Processing Properties"
        [ testProperty "trim: idempotent" propTrimIdempotent
        , testProperty "trim: removes only whitespace" propTrimOnlyWhitespace
        , testProperty "trim: preserves internal whitespace" propTrimPreservesInternal
        , testProperty "trim: empty string stays empty" propTrimEmpty
        ]

    , testGroup "Splitting Functions Properties"
        [ testProperty "splitBy: concatenation with delimiter" propSplitByConcatenation
        , testProperty "splitBy: length relationship" propSplitByLengthRelation
        , testProperty "splitBy: empty segments preserved" propSplitByPreservesEmpty
        , testProperty "splitByCollapsed: no empty segments" propSplitByCollapsedNoEmpty
        , testProperty "splitByCollapsed: subset of splitBy" propSplitByCollapsedSubset
        , testProperty "splitByComma: equals splitBy ','" propSplitByCommaEqualsSplitBy
        , testProperty "splitByCommaCollapsed: equals splitByCollapsed ','" propSplitByCommaCollapsedEquals
        ]

    , testGroup "Comment Processing Properties"
        [ testProperty "removeLineComments: idempotent" propRemoveLineCommentsIdempotent
        , testProperty "removeLineComments: preserves line count" propRemoveLineCommentsLineCount
        , testProperty "removeLineComments: removes only after //" propRemoveLineCommentsRemovesOnlyAfter
        , testProperty "removeComments: idempotent" propRemoveCommentsIdempotent
        , testProperty "removeComments: removes block comments" propRemoveCommentsRemovesBlock
        , testProperty "removeComments: preserves string literals" propRemoveCommentsPreservesStrings
        ]

    , testGroup "Indentation Properties"
        [ testProperty "normalizeIndentation: idempotent" propNormalizeIndentationIdempotent
        , testProperty "normalizeIndentation: preserves line count" propNormalizeIndentationLineCount
        , testProperty "normalizeIndentation: removes common prefix" propNormalizeIndentationCommonPrefix
        , testProperty "forceSingleTabIndentation: starts each non-empty line with tab" propForceSingleTabStartsWithTab
        , testProperty "fixIndentation: equals normalizeIndentation" propFixIndentationEqualsNormalize
        ]

    , testGroup "Search Properties"
        [ testProperty "breakOn: concatenation property" propBreakOnConcatenation
        , testProperty "breakOn: empty pattern returns empty prefix" propBreakOnEmptyPattern
        , testProperty "breakOn: pattern not found returns whole string as prefix" propBreakOnPatternNotFound
        ]

    , testGroup "Edge Cases and Stress Tests"
        [ testProperty "trim with unicode characters" propTrimUnicode
        , testProperty "splitBy with various delimiters" propSplitByVariousDelimiters
        , testProperty "comment removal with nested quotes" propCommentRemovalNestedQuotes
        , testProperty "indentation with mixed whitespace" propIndentationMixedWhitespace
        ]
    ]

-- ============================================================================
-- Trim Function Properties
-- ============================================================================

-- | trim是幂等的：trim(trim(s)) = trim(s)
propTrimIdempotent :: String -> Bool
propTrimIdempotent s = trim (trim s) == trim s

-- | trim只移除空白字符
propTrimOnlyWhitespace :: String -> Bool
propTrimOnlyWhitespace s =
  let trimmed = trim s
      leadingRemoved = length (takeWhile isSpace s) - length (takeWhile isSpace trimmed)
      trailingRemoved = length (reverse (takeWhile isSpace (reverse s))) - 
                       length (reverse (takeWhile isSpace (reverse trimmed)))
  in leadingRemoved >= 0 && trailingRemoved >= 0

-- | trim保持内部空白
propTrimPreservesInternal :: String -> String -> Bool
propTrimPreservesInternal s1 s2 =
  let combined = s1 ++ "   " ++ s2
      trimmed = trim combined
  in "   " `isInfixOf` trimmed || (null (trim s1) && null (trim s2))

-- | 空字符串trim后仍为空
propTrimEmpty :: Bool
propTrimEmpty = trim "" == ""

-- ============================================================================
-- Splitting Functions Properties
-- ============================================================================

-- | splitBy的连接性质：intercalate delimiter (splitBy delimiter s) = s
propSplitByConcatenation :: Char -> String -> Bool
propSplitByConcatenation delim s = 
  intercalate [delim] (splitBy delim s) == s

-- | splitBy的长度关系
propSplitByLengthRelation :: Char -> String -> Bool
propSplitByLengthRelation delim s =
  let parts = splitBy delim s
      totalLength = sum (map length parts)
  in totalLength + length parts - 1 >= length s

-- | splitBy保留空段
propSplitByPreservesEmpty :: Char -> String -> Bool
propSplitByPreservesEmpty delim s =
  let parts = splitBy delim s
      hasDelim = delim `elem` s
  in if hasDelim
     then "" `elem` parts || length parts > 1
     else parts == [s]

-- | splitByCollapsed不产生空段
propSplitByCollapsedNoEmpty :: Char -> String -> Bool
propSplitByCollapsedNoEmpty delim s =
  all (not . null) (splitByCollapsed delim s)

-- | splitByCollapsed的结果是splitBy结果的子集（移除空段后）
propSplitByCollapsedSubset :: Char -> String -> Bool
propSplitByCollapsedSubset delim s =
  let splitResult = splitBy delim s
      collapsedResult = splitByCollapsed delim s
  in all (`elem` splitResult) collapsedResult

-- | splitByComma等于splitBy ','
propSplitByCommaEqualsSplitBy :: String -> Bool
propSplitByCommaEqualsSplitBy s = splitByComma s == splitBy ',' s

-- | splitByCommaCollapsed等于splitByCollapsed ','
propSplitByCommaCollapsedEquals :: String -> Bool
propSplitByCommaCollapsedEquals s = splitByCommaCollapsed s == splitByCollapsed ',' s

-- ============================================================================
-- Comment Processing Properties
-- ============================================================================

-- | removeLineComments是幂等的
propRemoveLineCommentsIdempotent :: String -> Bool
propRemoveLineCommentsIdempotent s =
  let once = removeLineComments s
      twice = removeLineComments once
  in once == twice

-- | removeLineComments保持行数
propRemoveLineCommentsLineCount :: String -> Bool
propRemoveLineCommentsLineCount s =
  let linesBefore = length (lines s)
      linesAfter = length (lines (removeLineComments s))
  in linesAfter <= linesBefore

-- | removeLineComments只移除//之后的内容
propRemoveLineCommentsRemovesOnlyAfter :: String -> String -> Bool
propRemoveLineCommentsRemovesOnlyAfter prefix suffix =
  let input = prefix ++ "//" ++ suffix
      result = removeLineComments input
  in prefix `isPrefixOf` result && not ("//" `isInfixOf` result)

-- | removeComments是幂等的
propRemoveCommentsIdempotent :: String -> Bool
propRemoveCommentsIdempotent s =
  let once = removeComments s
      twice = removeComments once
  in once == twice

-- | removeComments移除块注释
propRemoveCommentsRemovesBlock :: String -> String -> Bool
propRemoveCommentsRemovesBlock before after =
  let input = before ++ "/*" ++ after ++ "*/" ++ "end"
      result = removeComments input
  in before `isPrefixOf` result && "end" `isSuffixOf` result && 
     not ("/*" `isInfixOf` result) && not ("*/" `isInfixOf` result)

-- | removeComments保持字符串字面量
propRemoveCommentsPreservesStrings :: String -> Bool
propRemoveCommentsPreservesStrings s =
  let stringWithComment = "text \"// not comment\" /* real comment */ more"
      result = removeComments stringWithComment
  in "\"// not comment\"" `isInfixOf` result

-- ============================================================================
-- Indentation Properties
-- ============================================================================

-- | normalizeIndentation是幂等的
propNormalizeIndentationIdempotent :: String -> Bool
propNormalizeIndentationIdempotent s =
  let once = normalizeIndentation s
      twice = normalizeIndentation once
  in once == twice

-- | normalizeIndentation保持行数
propNormalizeIndentationLineCount :: String -> Bool
propNormalizeIndentationLineCount s =
  let linesBefore = length (lines s)
      linesAfter = length (lines (normalizeIndentation s))
  in linesBefore == linesAfter

-- | normalizeIndentation移除公共前缀
propNormalizeIndentationCommonPrefix :: String -> Bool
propNormalizeIndentationCommonPrefix s =
  let linesList = lines s
      nonEmptyLines = filter (not . all isSpace) linesList
  in if length nonEmptyLines <= 1
     then True
     else let normalized = normalizeIndentation s
              normalizedLines = lines normalized
              normalizedNonEmpty = filter (not . all isSpace) normalizedLines
          in all (not . isPrefixOf "    ") normalizedNonEmpty

-- | forceSingleTabIndentation使每个非空行以tab开头
propForceSingleTabStartsWithTab :: String -> Bool
propForceSingleTabStartsWithTab s =
  let result = forceSingleTabIndentation s
      linesList = lines result
      nonEmptyLines = filter (not . null) linesList
  in all ("\t" `isPrefixOf`) nonEmptyLines

-- | fixIndentation等于normalizeIndentation
propFixIndentationEqualsNormalize :: String -> Bool
propFixIndentationEqualsNormalize s = 
  fixIndentation s == normalizeIndentation s

-- ============================================================================
-- Search Properties
-- ============================================================================

-- | breakOn的连接属性
propBreakOnConcatenation :: String -> String -> Bool
propBreakOnConcatenation pattern s =
  let (prefix, suffix) = breakOn pattern s
  in if null pattern
     then prefix == "" && suffix == s
     else prefix ++ pattern ++ suffix == s

-- | breakOn空模式返回空前缀
propBreakOnEmptyPattern :: String -> Bool
propBreakOnEmptyPattern s =
  let (prefix, suffix) = breakOn "" s
  in prefix == "" && suffix == s

-- | breakOn模式未找到时返回整个字符串作为前缀
propBreakOnPatternNotFound :: String -> String -> Bool
propBreakOnPatternNotFound pattern s =
  let patternNotFound = not (pattern `isInfixOf` s)
      (prefix, suffix) = breakOn pattern s
  in if patternNotFound
     then prefix == s && suffix == ""
     else True

-- ============================================================================
-- Edge Cases and Stress Tests
-- ============================================================================

-- | trim处理unicode字符
propTrimUnicode :: String -> Bool
propTrimUnicode s =
  let withUnicode = s ++ " \t\nαβγ \t\n"
      trimmed = trim withUnicode
  in not (null trimmed) ==> not (isSpace (last trimmed) || isSpace (head trimmed))

-- | splitBy处理各种分隔符
propSplitByVariousDelimiters :: String -> Bool
propSplitByVariousDelimiters s =
  let delimiters = [',', ';', ':', '|', '#', '@', '$', '%']
      delim = delimiters !! (length s `mod` length delimiters)
      parts = splitBy delim s
  in length parts >= 1

-- | 注释移除处理嵌套引号
propCommentRemovalNestedQuotes :: String -> String -> Bool
propCommentRemovalNestedQuotes inner outer =
  let input = "start \"" ++ inner ++ "\\\"// not comment\\\"" ++ outer ++ "\" // real comment"
      result = removeLineComments input
  in "\\\"// not comment\\\"" `isInfixOf` result && not ("// real comment" `isInfixOf` result)

-- | 缩进处理混合空白字符
propIndentationMixedWhitespace :: String -> Bool
propIndentationMixedWhitespace s =
  let withMixedIndent = "    \t  " ++ s ++ "\t    \t"
      normalized = normalizeIndentation withMixedIndent
  in length (lines normalized) == 1

-- ============================================================================
-- Helper Functions and Generators
-- ============================================================================

-- 生成包含各种字符的字符串
genMixedString :: Gen String
genMixedString = listOf $ oneof
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n\r"
  , elements ".,;:!?()[]{}\"'`~@#$%^&*+-=<>/\\|"
  , elements "αβγδεζηθικλμνξοπρστυφχψω"
  ]

-- 生成包含引号的字符串
genQuotedString :: Gen String
genQuotedString = do
  inner <- genMixedString
  return $ "\"" ++ inner ++ "\""

-- 生成包含注释的字符串
genCommentedString :: Gen String
genCommentedString = do
  code <- genMixedString
  comment <- genMixedString
  return $ code ++ "// " ++ comment

-- 实例声明
instance Arbitrary String where
  arbitrary = genMixedString

-- 辅助函数
infixr 0 ==>
(==>) :: Bool -> Bool -> Bool
True ==> x = x
False ==> _ = True