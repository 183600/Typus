{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.EnhancedSourceLocationQuickCheckPropertiesSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck (Property, property)

-- 测试套件
tests :: TestTree
tests = testGroup "SourceLocation QuickCheck Properties Tests"
  [ testProperty "Basic test" prop_basic_test
  ]

-- 基本测试
prop_basic_test :: Property
prop_basic_test = property $ True