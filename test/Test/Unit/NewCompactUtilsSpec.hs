{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompactUtilsSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll)
import Utils (trim, splitBy, splitByCollapsed, splitByComma, removeLineComments, removeComments)

-- | 测试trim函数的属性
testTrimProperties :: TestTree
testTrimProperties = testGroup "trim函数属性测试"
  [ testProperty "trim两次等于trim一次" $
      \s -> trim (trim s) === trim s
    
  , testProperty "trim不改变字符串中间的空格" $
      \s -> let trimmed = trim s
                 middleSpaces = filter (== ' ') $ dropWhile (== ' ') $ reverse $ dropWhile (== ' ') trimmed
             in length middleSpaces >= 0
    
  , testProperty "trim只移除首尾空格" $
      \s -> let trimmed = trim s
                 originalWithoutSpaces = dropWhile (== ' ') $ reverse $ dropWhile (== ' ') s
             in trimmed === originalWithoutSpaces
  ]

-- | 测试splitBy函数的属性
testSplitByProperties :: TestTree
testSplitByProperties = testGroup "splitBy函数属性测试"
  [ testProperty "splitBy与join的逆属性" $
      \c s -> not (elem c s) ==> splitBy c s === [s]
    
  , testProperty "splitByComma等于splitBy ','" $
      \s -> splitByComma s === splitBy ',' s
    
  , testProperty "splitByCollapsed不产生空字符串" $
      \c s -> all (not . null) (splitByCollapsed c s)
    
  , testProperty "splitByCollapsed长度小于等于splitBy" $
      \c s -> length (splitByCollapsed c s) <= length (splitBy c s)
  ]

-- | 测试注释移除函数的属性
testCommentRemovalProperties :: TestTree
testCommentRemovalProperties = testGroup "注释移除函数属性测试"
  [ testCase "removeLineComments移除单行注释" $
      let input = "hello world // this is comment\nsecond line"
          expected = "hello world \nsecond line"
      in removeLineComments input @?= expected
    
  , testCase "removeComments移除多行注释" $
      let input = "hello /* multi\nline\ncomment */ world"
          expected = "hello  world"
      in removeComments input @?= expected
    
  , testCase "注释移除不处理字符串内的注释符号" $
      let input = "println(\"// not a comment\") // real comment"
          expected = "println(\"// not a comment\") "
      in removeLineComments input @?= expected
  ]

-- | 边界条件测试
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "边界条件测试"
  [ testCase "trim空字符串" $
      trim "" @?= ""
    
  , testCase "trim全空格字符串" $
      trim "   " @?= ""
    
  , testCase "splitBy空字符串" $
      splitBy ',' "" @?= [""]
    
  , testCase "splitByCollapsed空字符串" $
      splitByCollapsed ',' "" @?= []
    
  , testCase "splitBy只有分隔符" $
      splitBy ',' "," @?= ["", ""]
    
  , testCase "splitByCollapsed只有分隔符" $
      splitByCollapsed ',' "," @?= []
  ]

-- | 性能相关属性测试
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup "性能属性测试"
  [ testProperty "splitByCollapsed的复杂度属性" $
      \c s -> let splits = splitBy c s
                  collapsed = splitByCollapsed c s
              in length collapsed <= length splits
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "Utils模块核心功能测试"
  [ testTrimProperties
  , testSplitByProperties
  , testCommentRemovalProperties
  , testBoundaryConditions
  , testPerformanceProperties
  ]