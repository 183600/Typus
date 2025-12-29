{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest1Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import qualified Data.Text as T

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- | 测试字符串处理的边界情况和不变性
tests :: TestTree
tests =
  testGroup "NewCabalTest1 - 字符串处理边界测试"
    [ testGroup "单元测试"
        [ testCase "trim处理空白字符的边界情况" $ do
            trim "" @?= ""
            trim "   " @?= ""
            trim "\t\n\r" @?= ""
            trim "  hello  " @?= "hello"

        , testCase "splitBy处理空字符串" $ do
            splitBy ',' "" @?= [""]
            splitBy ',' "," @?= ["", ""]
            splitBy ',' ",," @?= ["", "", ""]

        , testCase "removeComments处理嵌套注释" $ do
            let input = "code /* outer /* inner */ still outer */ end"
                expected = "code  still outer  end"
            removeComments input @?= expected

        , testCase "normalizeIndentation处理混合缩进" $ do
            let input = "  line1\n\tline2\n    line3"
                result = normalizeIndentation input
                lines' = lines result
            length lines' @?= 3
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "trim的幂等性" prop_trim_idempotent
        , fastProperty "splitBy的长度不变性" prop_splitBy_length_invariant
        , fastProperty "removeComments保持代码内容" prop_removeComments_preserves_code
        , fastProperty "normalizeIndentation保持行数" prop_normalizeIndentation_preserves_lines
        , fastProperty "breakOn的分解正确性" prop_breakOn_decomposition_correct
        ]
    ]

-- QuickCheck属性测试

-- trim的幂等性：trim(trim(x)) == trim(x)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmedOnce = trim str
      trimmedTwice = trim trimmedOnce
  in property $ trimmedOnce === trimmedTwice

-- splitBy的长度不变性：splitBy d (xs ++ [d] ++ ys) 的长度 = splitBy d xs + splitBy d ys
prop_splitBy_length_invariant :: String -> String -> Char -> Property
prop_splitBy_length_invariant prefix suffix delim =
  let fullString = prefix ++ [delim] ++ suffix
      partsFull = splitBy delim fullString
      partsPrefix = splitBy delim prefix
      partsSuffix = splitBy delim suffix
  in property $ length partsFull === length partsPrefix + length partsSuffix

-- removeComments保持代码内容：移除注释后，原始代码内容仍然存在
prop_removeComments_preserves_code :: String -> String -> Property
prop_removeComments_preserves_code code comment =
  -- 避免字符串字面量中的注释标记
  not ('"' `elem` code) && not ('\'' `elem` code) &&
  not ('"' `elem` comment) && not ('\'' `elem` comment) &&
  not ("/*" `isInfixOf` code) && not ("*/" `isInfixOf` code) &&
  not ("//" `isInfixOf` code) ==>
  let withComments = code ++ " /* " ++ comment ++ " */ " ++ code ++ " // " ++ comment
      withoutComments = removeComments withComments
  in property $ code `isInfixOf` withoutComments .&&. 
     length (filter (== 'c') withoutComments) >= length (filter (== 'c') code)

-- normalizeIndentation保持行数：缩进规范化不改变行数
prop_normalizeIndentation_preserves_lines :: String -> Property
prop_normalizeIndentation_preserves_lines content =
  let normalized = normalizeIndentation content
      originalLines = length (lines content)
      normalizedLines = length (lines normalized)
  in property $ originalLines === normalizedLines

-- breakOn的分解正确性：breakOn p (xs ++ p ++ ys) == (xs ++ p, ys)
prop_breakOn_decomposition_correct :: String -> String -> String -> Property
prop_breakOn_decomposition_correct prefix pattern suffix =
  not (null pattern) ==>
  let haystack = prefix ++ pattern ++ suffix
      (before, after) = breakOn pattern haystack
  in property $ before === prefix ++ pattern .&&. after === suffix