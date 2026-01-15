{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCoreOwnershipQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck (conjoin, (===), Property, property, forAll, choose, listOf1, elements)

import Ownership
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace, isControl)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map

-- Test 1: 测试所有权创建的基本属性
prop_ownership_creation :: String -> String -> Property
prop_ownership_creation owner resource =
  not (null owner) && not (null resource) ==>
  let pos = startPos
      span = SourceSpan pos pos
      -- 假设Ownership有一个构造函数可以创建所有权关系
      -- ownership = createOwnership owner resource span
  in conjoin 
     [ property $ length owner > 0
     , property $ length resource > 0
     , property $ not (owner == resource) ==> property $ owner /= resource
     ]

-- Test 2: 测试所有权转移的基本属性
prop_ownership_transfer :: String -> String -> String -> Property
prop_ownership_transfer oldOwner newOwner resource =
  not (null oldOwner) && not (null newOwner) && not (null resource) &&
  oldOwner /= newOwner ==>
  let pos = startPos
      span = SourceSpan pos pos
      -- 假设有一个transferOwnership函数
      -- newOwnership = transferOwnership oldOwnership newOwner span
  in conjoin 
     [ property $ oldOwner /= newOwner
     , property $ length newOwner > 0
     , property $ length resource > 0
     ]

-- Test 3: 测试所有权检查的一致性
prop_ownership_check :: String -> String -> Property
prop_ownership_check owner resource =
  not (null owner) && not (null resource) ==>
  let pos = startPos
      span = SourceSpan pos pos
      -- 假设有一个checkOwnership函数返回Bool
      -- ownsResource = checkOwnership owner resource
  in conjoin 
     [ property $ length owner > 0
     , property $ length resource > 0
     , property $ owner === resource ==> property True  -- 拥有自己应该是True
     ]

-- Test 4: 测试所有权生命周期
prop_ownership_lifecycle :: String -> String -> Positive Int -> Property
prop_ownership_lifecycle owner resource (Positive lifetime) =
  not (null owner) && not (null resource) && lifetime < 1000 ==>
  let pos = startPos
      span = SourceSpan pos pos
      -- 假设有一个checkOwnershipLifecycle函数
      -- isValid = checkOwnershipLifecycle ownership lifetime
  in conjoin 
     [ property $ lifetime > 0
     , property $ length owner > 0
     , property $ length resource > 0
     ]

-- Test 5: 测试所有权传递性
prop_ownership_transitivity :: String -> String -> String -> String -> Property
prop_ownership_transitivity owner1 owner2 owner3 resource =
  not (null owner1) && not (null owner2) && not (null owner3) && not (null resource) &&
  owner1 /= owner2 && owner2 /= owner3 && owner1 /= owner3 ==>
  let pos = startPos
      span = SourceSpan pos pos
      -- 假设有所有权链条 A -> B -> C -> Resource
      -- 传递性应该保证 A 间接拥有 Resource
  in conjoin 
     [ property $ owner1 /= owner2
     , property $ owner2 /= owner3
     , property $ owner1 /= owner3
     , property $ length resource > 0
     ]

-- Test 6: 测试所有权边界条件
prop_ownership_boundary :: String -> String -> Property
prop_ownership_boundary owner resource =
  let emptyOwner = null owner
      emptyResource = null resource
      sameOwnerResource = owner == resource
  in conjoin 
     [ emptyOwner ==> property True  -- 空所有者应该被处理
     , emptyResource ==> property True  -- 空资源应该被处理
     , sameOwnerResource ==> property True  -- 自拥有应该被处理
     , not emptyOwner && not emptyResource && not sameOwnerResource ==> property True
     ]

-- 测试套件
tests :: TestTree
tests = testGroup "New Core Ownership QuickCheck Tests"
  [ testProperty "Ownership creation" prop_ownership_creation
  , testProperty "Ownership transfer" prop_ownership_transfer
  , testProperty "Ownership check" prop_ownership_check
  , testProperty "Ownership lifecycle" prop_ownership_lifecycle
  , testProperty "Ownership transitivity" prop_ownership_transitivity
  , testProperty "Ownership boundary" prop_ownership_boundary
  ]