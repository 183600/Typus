{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, removeComments, normalizeIndentation, breakOn)
import Data.Char (isSpace, isLetter, isDigit)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T

-- | 新的Cabal QuickCheck测试模块，提供对Utils模块的全面属性测试
tests :: TestTree
tests =
  testGroup "New Cabal QuickCheck Tests"
    [ testGroup "String manipulation properties"
        [ testProperty "trim removes only leading/trailing spaces" prop_trimOnlyRemovesSpaces
        , testProperty "trim is idempotent" prop_trimIdempotent
        , testProperty "trim preserves internal spaces" prop_trimPreservesInternalSpaces
        , testProperty "trim of empty string is empty" prop_trimEmpty
        ]

    , testGroup "Splitting properties"
        [ testProperty "splitBy preserves order" prop_splitByPreservesOrder
        , testProperty "splitBy with delimiter not in string returns singleton" prop_splitByNoDelimiter
        , testProperty "splitByCollapsed removes empty segments" prop_splitByCollapsedRemovesEmpty
        , testProperty "splitByComma is splitBy with comma" prop_splitByCommaIsSplitByComma
        , testProperty "splitBy length is sum of 1 and delimiter count" prop_splitByLength
        ]

    , testGroup "Comment removal properties"
        [ testProperty "removeLineComments never increases string length" prop_removeLineCommentsNeverIncreases
        , testProperty "removeLineComments preserves line count" prop_removeLineCommentsPreservesLines
        , testProperty "removeComments removes all // patterns outside literals" prop_removeCommentsRemovesSlashSlash
        , testProperty "removeComments removes all /* */ patterns" prop_removeCommentsRemovesBlockComments
        ]

    , testGroup "Indentation properties"
        [ testProperty "normalizeIndentation preserves relative indentation" prop_normalizePreservesRelative
        , testProperty "normalizeIndentation never adds leading spaces to first non-empty line" prop_normalizeFirstLine
        , testProperty "breakOn either finds pattern or returns original" prop_breakOnBehavior
        ]

    , testGroup "Edge case tests"
        [ testCase "trim handles unicode whitespace correctly" $ do
            trim "\x2003\x2009hello\x00A0\x202F" @?= "hello"
        
        , testCase "splitBy handles empty delimiter gracefully" $ do
            splitBy 'a' "" @?= [""]
        
        , testCase "removeComments handles nested block comments correctly" $ do
            let input = "code /* outer /* inner */ still outer */ end"
                expected = "code  end"
            removeComments input @?= expected
        
        , testCase "normalizeIndentation handles mixed tabs and spaces" $ do
            let input = "\t    mixed\n\t    \tindentation"
                expected = "mixed\n\tindentation"
            normalizeIndentation input @?= expected
        ]
    ]

-- | trim只移除开头和结尾的空白字符
prop_trimOnlyRemovesSpaces :: String -> Bool
prop_trimOnlyRemovesSpaces s =
  let trimmed = trim s
      leadingRemoved = dropWhile isSpace s
      trailingRemoved = reverse (dropWhile isSpace (reverse leadingRemoved))
  in trimmed == trailingRemoved

-- | trim是幂等的
prop_trimIdempotent :: String -> Bool
prop_trimIdempotent s = trim (trim s) == trim s

-- | trim保留内部空格
prop_trimPreservesInternalSpaces :: String -> Bool
prop_trimPreservesInternalSpaces s =
  let trimmed = trim s
      internalSpaces = filter isSpace (take (length trimmed - 2) (drop 1 trimmed))
  in not (null trimmed) ==> (filter isSpace s) `elem` [[], internalSpaces]

-- | 空字符串trim后还是空字符串
prop_trimEmpty :: Bool
prop_trimEmpty = trim "" == ""

-- | splitBy保持顺序
prop_splitByPreservesOrder :: Char -> String -> Bool
prop_splitByPreservesOrder delim s =
  let parts = splitBy delim s
      reconstructed = intercalate [delim] parts
  in reconstructed == s

-- | 当分隔符不在字符串中时，splitBy返回单元素列表
prop_splitByNoDelimiter :: Char -> String -> Property
prop_splitByNoDelimiter delim s =
  delim `notElem` s ==> splitBy delim s == [s]

-- | splitByCollapsed移除空段
prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim s =
  let parts = splitByCollapsed delim s
  in all (not . null) parts

-- | splitByComma等同于使用逗号的splitBy
prop_splitByCommaIsSplitByComma :: String -> Bool
prop_splitByCommaIsSplitByComma s = splitByComma s == splitBy ',' s

-- | splitBy的长度等于1加上分隔符的数量
prop_splitByLength :: Char -> String -> Bool
prop_splitByLength delim s =
  let parts = splitBy delim s
      delimiterCount = length (filter (== delim) s)
  in length parts == delimiterCount + 1

-- | removeLineComments不会增加字符串长度
prop_removeLineCommentsNeverIncreases :: String -> Bool
prop_removeLineCommentsNeverIncreases s =
  let processed = removeLineComments s
  in length processed <= length s

-- | removeLineComments保持行数
prop_removeLineCommentsPreservesLines :: String -> Bool
prop_removeLineCommentsPreservesLines s =
  let originalLines = lines s
      processedLines = lines (removeLineComments s)
  in length originalLines == length processedLines

-- | removeComments移除所有不在字面量中的//模式
prop_removeCommentsRemovesSlashSlash :: String -> Property
prop_removeCommentsRemovesSlashSlash s =
  let processed = removeComments s
      hasSlashSlash = "//" `isInfixOf` processed
  in not hasSlashSlash

-- | removeComments移除所有块注释
prop_removeCommentsRemovesBlockComments :: String -> Property
prop_removeCommentsRemovesBlockComments s =
  let processed = removeComments s
      hasBlockComment = "/*" `isInfixOf` processed || "*/" `isInfixOf` processed
  in not hasBlockComment

-- | normalizeIndentation保持相对缩进
prop_normalizePreservesRelative :: String -> Property
prop_normalizePreservesRelative s =
  not (null (lines s)) && length (filter (not . all isSpace) (lines s)) > 1 ==>
  let originalLines = filter (not . all isSpace) (lines s)
      normalizedLines = filter (not . all isSpace) (lines (normalizeIndentation s))
      indentDifferences line1 line2 = 
        let indent1 = length (takeWhile isSpace line1)
            indent2 = length (takeWhile isSpace line2)
        in indent2 - indent1
      originalDiffs = zipWith indentDifferences originalLines (tail originalLines)
      normalizedDiffs = zipWith indentDifferences normalizedLines (tail normalizedLines)
  in originalDiffs == normalizedDiffs

-- | normalizeIndentation不会在第一个非空行添加前导空格
prop_normalizeFirstLine :: String -> Property
prop_normalizeFirstLine s =
  let nonEmptyLines = filter (not . null) (lines s)
  in not (null nonEmptyLines) ==>
  let firstLine = head nonEmptyLines
      normalizedFirstLine = head (filter (not . null) (lines (normalizeIndentation s)))
      originalLeading = length (takeWhile isSpace firstLine)
      normalizedLeading = length (takeWhile isSpace normalizedFirstLine)
  in normalizedLeading <= originalLeading

-- | breakOn要么找到模式，要么返回原始字符串
prop_breakOnBehavior :: String -> String -> Bool
prop_breakOnBehavior pattern s =
  let (prefix, suffix) = breakOn pattern s
  in if pattern `isInfixOf` s
     then prefix ++ pattern ++ suffix == s && pattern `isPrefixOf` (prefix ++ pattern ++ suffix)
     else prefix == s && null suffix

-- Helper function
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:ys) = xs : tails ys