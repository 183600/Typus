{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.Unit.NewQuickCheckUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, oneof, elements, listOf, getNonEmpty)

import Utils (trim, splitBy, splitByComma, splitByCollapsed, removeLineComments, removeComments, normalizeIndentation)
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

-- | 生成用于测试的任意字符
instance Arbitrary Char where
  arbitrary = oneof
    [ elements ['a'..'z']
    , elements ['A'..'Z']
    , elements ['0'..'9']
    , elements " \t\n\r,;:!@#$%^&*()[]{}<>-=_+|~`"
    , elements "中文测试。！，"  -- 添加一些中文字符
    , pure '\0'  -- null字符
    ]

-- | 生成用于测试的任意字符串
instance Arbitrary String where
  arbitrary = listOf arbitrary

tests :: TestTree
tests = testGroup "New QuickCheck Utils Tests"
  [ trimProperties
  , splitByProperties
  , commentRemovalProperties
  , indentationProperties
  ]

-- | trim 函数的属性测试
trimProperties :: TestTree
trimProperties = testGroup "Trim Properties"
  [ testProperty "trim removes L.all leading L.and trailing whitespace" $ \str ->
      let trimmed = trim str
          hasLeadingSpace = not (null trimmed) && isSpace (L.head trimmed)
          hasTrailingSpace = not (null trimmed) && isSpace (last trimmed)
      in not (hasLeadingSpace || hasTrailingSpace)
      
  , testProperty "trim is idempotent" $ \str ->
      let trimmedOnce = trim str
          trimmedTwice = trim trimmedOnce
      in trimmedOnce === trimmedTwice
      
  , testProperty "trim preserves non-whitespace content" $ \str ->
      let trimmed = trim str
          originalContent = L.filter (not . isSpace) str
          trimmedContent = L.filter (not . isSpace) trimmed
      in originalContent === trimmedContent
      
  , testProperty "trim empty string" $ \() ->
      let empty = "" :: String
          result = trim empty
      in result === empty
      
  , testProperty "trim L.all whitespace string" $ \whitespace ->
      let allWhitespace = L.all isSpace whitespace
          result = trim whitespace
      in if allWhitespace
         then result === ""
         else property True
  ]

-- | splitBy 函数的属性测试
splitByProperties :: TestTree
splitByProperties = testGroup "Split Properties"
  [ testProperty "splitBy preserves total L.length" $ \delim str ->
      let parts = splitBy delim str
          totalLength = L.sum (map L.length parts) + L.length parts - 1
          originalLength = L.length str
      in if null str
         then parts === [""]
         else totalLength === originalLength
         
  , testProperty "splitBy empty string returns single empty" $ \delim ->
      let result = splitBy delim ""
      in result === [""]
      
  , testProperty "splitBy with delimiter not in string returns single part" $ \str ->
      let delim = '\0'  -- unlikely to be in random string
          result = splitBy delim str
          in if '\0' `elem` str
             then property True
             else result === [str]
             
  , testProperty "splitByCollapsed removes empty parts" $ \delim str ->
      let normalParts = splitBy delim str
          collapsedParts = splitByCollapsed delim str
      in L.all (not . null) collapsedParts
      
  , testProperty "splitByComma equals splitBy with comma" $ \str ->
      let byComma = splitByComma str
          byChar = splitBy ',' str
      in byComma === byChar
      
  , testProperty "splitBy preserves order" $ \delim str ->
      let parts = splitBy delim str
          reconstructed = concatMap (\p -> p ++ [delim]) (init parts) ++ last parts
      in if null str
         then parts === [""]
         else if delim `elem` str
              then reconstructed === str
              else parts === [str]
  ]

-- | 注释移除函数的属性测试
commentRemovalProperties :: TestTree
commentRemovalProperties = testGroup "Comment Removal Properties"
  [ testProperty "removeLineComments removes L.all line comments" $ \str ->
      let result = removeLineComments str
          lines' = lines result
          hasLineComment = L.any ("//" `L.isPrefixOf`) lines'
      in not hasLineComment
      
  , testProperty "removeComments removes L.all block comments" $ \str ->
      let result = removeComments str
      in not ("/*" `L.isInfixOf` result && "*/" `L.isInfixOf` result)
      
  , testProperty "removeLineComments preserves string literals" $ \str ->
      let stringWithComment = "\"// not a comment\" // real comment"
          result = removeLineComments stringWithComment
      in "// not a comment" `L.isInfixOf` result
      
  , testProperty "removeComments preserves string literals" $ \str ->
      let stringWithComment = "\"/* not a comment */\" ++ " /* real comment */"
          result = removeComments stringWithComment
      in "/* not a comment */" `L.isInfixOf` result
      
  , testProperty "removeLineComments is idempotent" $ \str ->
      let once = removeLineComments str
          twice = removeLineComments once
      in once === twice
      
  , testProperty "removeComments is idempotent" $ \str ->
      let once = removeComments str
          twice = removeComments once
      in once === twice
      
  , testProperty "removeComments handles nested comments" $ \str ->
      let nested = "text /* outer /* inner */ still outer */ more text"
          result = removeComments nested
      in not ("/*" `L.isInfixOf` result)
  ]

-- | 缩进处理函数的属性测试
indentationProperties :: TestTree
indentationProperties = testGroup "Indentation Properties"
  [ testProperty "normalizeIndentation preserves line count" $ \str ->
      let originalLines = lines str
          resultLines = lines (normalizeIndentation str)
      in L.length originalLines === L.length resultLines
      
  , testProperty "normalizeIndentation preserves non-empty content" $ \str ->
      let originalLines = lines str
          resultLines = lines (normalizeIndentation str)
          originalContent = L.filter (not . null . trim) originalLines
          resultContent = L.filter (not . null . trim) resultLines
      in originalContent === resultContent
      
  , testProperty "normalizeIndentation is idempotent" $ \str ->
      let first = normalizeIndentation str
          second = normalizeIndentation first
      in first === second
      
  , testProperty "normalizeIndentation handles empty string" $ \() ->
      let empty = "" :: String
          result = normalizeIndentation empty
      in result === empty
      
  , testProperty "normalizeIndentation removes common leading whitespace" $ \str ->
      let lines' = lines str
          nonEmptyLines = L.filter (not . null . trim) lines'
      in if null nonEmptyLines
         then property True
         else let result = normalizeIndentation str
                  resultLines = lines result
                  resultNonEmpty = L.filter (not . null . trim) resultLines
                  hasCommonIndent = L.any (isPrefixOf "  ") resultNonEmpty
              in not hasCommonIndent
              
  , testProperty "normalizeIndentation preserves relative indentation" $ \str ->
      let lines' = lines str
          resultLines = lines (normalizeIndentation str)
          calculateIndents ls = L.map (L.length . takeWhile isSpace) ls
          originalIndents = calculateIndents $ L.filter (not . null . trim) lines'
          resultIndents = calculateIndents $ L.filter (not . null . trim) resultLines
      in if null originalIndents
         then property True
         else let minOriginal = L.minimum originalIndents
                  adjustedOriginal = L.map (subtract minOriginal) originalIndents
              in adjustedOriginal === resultIndents
  ]