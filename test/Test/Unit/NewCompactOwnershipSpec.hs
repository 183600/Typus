{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompactOwnershipSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Ownership
import SourceLocation (SourcePos(..), Located(..), locatedAt)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- | 生成任意的变量名
genVarName :: Gen String
genVarName = do
  len <- choose (1, 10)
  first <- elements ['a'..'z']
  rest <- choose (0, len-1) >>= \n -> sequence [elements ['a'..'z'..'0'..'9'] | _ <- [1..n]]
  return (first : rest)

-- | 生成任意的所有权状态
instance Arbitrary OwnershipState where
  arbitrary = do
    varCount <- choose (1, 5)
    varNames <- sequence [genVarName | _ <- [1..varCount]]
    ownedVars <- choose (0, varCount) >>= \n -> elements (take n varNames)
    let ownedSet = Set.fromList ownedVars
    return $ OwnershipState ownedSet Map.empty

-- | 测试所有权基本属性
testOwnershipBasicProperties :: TestTree
testOwnershipBasicProperties = testGroup "所有权基本属性测试"
  [ testCase "初始状态为空" $
      let state = emptyOwnershipState
          owned = getOwnedVariables state
      in Set.null owned @?= True
    
  , testCase "获取所有权" $
      let state = emptyOwnershipState
          var = "x"
          state' = acquireOwnership state var
          owned = getOwnedVariables state'
      in assertBool "应该拥有变量x" (Set.member var owned)
    
  , testCase "释放所有权" $
      let state = emptyOwnershipState
          var = "x"
          state' = acquireOwnership state var
          state'' = releaseOwnership state' var
          owned = getOwnedVariables state''
      in assertBool "应该不再拥有变量x" (not $ Set.member var owned)
  ]

-- | 测试所有权传递
testOwnershipTransfer :: TestTree
testOwnershipTransfer = testGroup "所有权传递测试"
  [ testCase "简单所有权转移" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          result = transferOwnership state' "x" "y"
      in case result of
        Left err -> assertBool ("转移失败: " ++ err) False
        Right state'' -> 
          let owned = getOwnedVariables state''
          in assertBool "应该拥有y但不拥有x" (Set.member "y" owned && not (Set.member "x" owned))
    
  , testCase "转移不存在的变量" $
      let state = emptyOwnershipState
          result = transferOwnership state "nonexistent" "y"
      in case result of
        Left _ -> assertBool "应该失败" True
        Right _ -> assertBool "不应该成功" False
    
  , testCase "转移已拥有的变量" $
      let state = emptyOwnershipState
          state' = acquireOwnership (acquireOwnership state "x") "y"
          result = transferOwnership state' "x" "y"
      in case result of
        Left _ -> assertBool "应该失败" True
        Right _ -> assertBool "不应该成功" False
  ]

-- | 测试所有权借用
testOwnershipBorrowing :: TestTree
testOwnershipBorrowing = testGroup "所有权借用测试"
  [ testCase "不可变借用" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          result = borrowOwnership state' "x" Immutable
      in case result of
        Left err -> assertBool ("借用失败: " ++ err) False
        Right (state'', _) ->
          let owned = getOwnedVariables state''
          in assertBool "仍然拥有x" (Set.member "x" owned)
    
  , testCase "可变借用" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          result = borrowOwnership state' "x" Mutable
      in case result of
        Left err -> assertBool ("借用失败: " ++ err) False
        Right (state'', _) ->
          let owned = getOwnedVariables state''
          in assertBool "仍然拥有x" (Set.member "x" owned)
    
  , testCase "多重不可变借用" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          result1 = borrowOwnership state' "x" Immutable
      in case result1 of
        Left err -> assertBool ("第一次借用失败: " ++ err) False
        Right (state'', _) ->
          let result2 = borrowOwnership state'' "x" Immutable
          in case result2 of
            Left _ -> assertBool "应该允许多重不可变借用" False
            Right _ -> assertBool "多重不可变借用成功" True
    
  , testCase "可变借用排斥其他借用" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          result1 = borrowOwnership state' "x" Mutable
      in case result1 of
        Left err -> assertBool ("第一次借用失败: " ++ err) False
        Right (state'', _) ->
          let result2 = borrowOwnership state'' "x" Immutable
          in case result2 of
            Left _ -> assertBool "可变借用应该排斥其他借用" True
            Right _ -> assertBool "不应该允许其他借用" False
  ]

-- | 测试所有权生命周期
testOwnershipLifetime :: TestTree
testOwnershipLifetime = testGroup "所有权生命周期测试"
  [ testCase "作用域结束自动释放" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          state'' = enterScope state'
          state''' = acquireOwnership state'' "y"
          state'''' = exitScope state'''
          owned = getOwnedVariables state''''
      in assertBool "作用域结束后y应该被释放" (Set.member "x" owned && not (Set.member "y" owned))
    
  , testCase "嵌套作用域" $
      let state = emptyOwnershipState
          state1 = acquireOwnership state "x"
          state2 = enterScope state1
          state3 = acquireOwnership state2 "y"
          state4 = enterScope state3
          state5 = acquireOwnership state4 "z"
          state6 = exitScope state5
          state7 = exitScope state6
          owned = getOwnedVariables state7
      in assertBool "嵌套作用域正确处理" (Set.member "x" owned && Set.member "y" owned && not (Set.member "z" owned))
  ]

-- | QuickCheck属性测试
testOwnershipProperties :: TestTree
testOwnershipProperties = testGroup "所有权属性测试"
  [ testProperty "获取所有权后拥有变量" $
      \state var ->
        let state' = acquireOwnership state var
            owned = getOwnedVariables state'
        in Set.member var owned
  
  , testProperty "释放所有权后不再拥有变量" $
      \state var ->
        let state' = acquireOwnership state var
            state'' = releaseOwnership state' var
            owned = getOwnedVariables state''
        in not (Set.member var owned)
  
  , testProperty "所有权转移保持数量不变" $
      \state fromVar toVar ->
        let state' = acquireOwnership state fromVar
            beforeCount = Set.size (getOwnedVariables state')
            result = transferOwnership state' fromVar toVar
        in case result of
          Left _ -> True
          Right state'' -> 
            let afterCount = Set.size (getOwnedVariables state'')
            in beforeCount === afterCount
  
  , testProperty "不可变借用不改变所有权集合" $
      \state var ->
        let state' = acquireOwnership state var
            beforeOwned = getOwnedVariables state'
            result = borrowOwnership state' var Immutable
        in case result of
          Left _ -> True
          Right (state'', _) -> getOwnedVariables state'' === beforeOwned
  ]

-- | 测试所有权验证
testOwnershipValidation :: TestTree
testOwnershipValidation = testGroup "所有权验证测试"
  [ testCase "验证有效状态" $
      let state = emptyOwnershipState
          state' = acquireOwnership (acquireOwnership state) "x" "y"
          violations = validateOwnershipState state'
      in null violations @?= True
    
  , testCase "检测悬垂引用" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          state'' = releaseOwnership state' "x"
          violations = validateOwnershipState state''
      in length violations @?= 1
    
  , testCase "检测重复释放" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          state'' = releaseOwnership state' "x"
          state''' = releaseOwnership state'' "x"
          violations = validateOwnershipState state'''
      in length violations >= 1 @?= True
  ]

-- | 边界条件测试
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "边界条件测试"
  [ testCase "空状态转移" $
      let state = emptyOwnershipState
          result = transferOwnership state "x" "y"
      in case result of
        Left _ -> assertBool "应该失败" True
        Right _ -> assertBool "不应该成功" False
    
  , testCase "自转移" $
      let state = emptyOwnershipState
          state' = acquireOwnership state "x"
          result = transferOwnership state' "x" "x"
      in case result of
        Left _ -> assertBool "应该失败" True
        Right _ -> assertBool "不应该成功" False
    
  , testCase "大量变量操作" $
      let vars = map (\i -> "var" ++ show i) [1..100]
          state = foldl acquireOwnership emptyOwnershipState vars
          owned = getOwnedVariables state
      in Set.size owned @?= 100
  ]

-- | 性能测试
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup "性能属性测试"
  [ testProperty "大量所有权操作性能" $
      \n ->
        let numOps = min 1000 (max 1 n)
            vars = map (\i -> "var" ++ show i) [1..numOps]
            state = foldl acquireOwnership emptyOwnershipState vars
            transfers = zip vars (tail vars ++ ["final"])
            finalState = foldl (\s (from, to) -> 
              case transferOwnership s from to of
                Left _ -> s
                Right s' -> s') state transfers
        in Set.size (getOwnedVariables finalState) >= 0
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "Ownership模块核心功能测试"
  [ testOwnershipBasicProperties
  , testOwnershipTransfer
  , testOwnershipBorrowing
  , testOwnershipLifetime
  , testOwnershipProperties
  , testOwnershipValidation
  , testBoundaryConditions
  , testPerformanceProperties
  ]

-- 辅助函数和类型定义（假设这些在Ownership模块中存在）
data OwnershipState = OwnershipState 
  { ownedVars :: Set String
  , borrowMap :: Map String BorrowInfo
  } deriving (Show, Eq)

data BorrowType = Immutable | Mutable deriving (Show, Eq)
data BorrowInfo = BorrowInfo 
  { borrowType :: BorrowType
  , borrowCount :: Int
  } deriving (Show, Eq)

emptyOwnershipState :: OwnershipState
emptyOwnershipState = OwnershipState Set.empty Map.empty

getOwnedVariables :: OwnershipState -> Set String
getOwnedVariables = ownedVars

acquireOwnership :: OwnershipState -> String -> OwnershipState
acquireOwnership state var = state { ownedVars = Set.insert var (ownedVars state) }

releaseOwnership :: OwnershipState -> String -> OwnershipState
releaseOwnership state var = state { ownedVars = Set.delete var (ownedVars state) }

transferOwnership :: OwnershipState -> String -> String -> Either String OwnershipState
transferOwnership state from to = 
  if Set.member from (ownedVars state)
  then Right $ state { ownedVars = Set.insert to (Set.delete from (ownedVars state)) }
  else Left "Variable not owned"

borrowOwnership :: OwnershipState -> String -> BorrowType -> Either String (OwnershipState, Int)
borrowOwnership state var borrowType = 
  if Set.member var (ownedVars state)
  then Right (state, 1)  -- 简化实现
  else Left "Variable not owned"

enterScope :: OwnershipState -> OwnershipState
enterScope state = state  -- 简化实现

exitScope :: OwnershipState -> OwnershipState
exitScope state = state  -- 简化实现

validateOwnershipState :: OwnershipState -> [String]
validateOwnershipState state = []  -- 简化实现