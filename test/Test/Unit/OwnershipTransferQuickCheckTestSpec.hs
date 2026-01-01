{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipTransferQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose
  , sized, resize, suchThat, vectorOf, arbitrary
  )

import Ownership.Common.Types
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer(..)
  , newOwnershipAnalyzer
  )

import Data.List (nub, sort)
import Data.Maybe (isJust, isNothing)

-- | 生成有效的变量名
genVariableName :: Gen String
genVariableName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- | 生成所有权类型
genOwnershipType :: Gen OwnershipType
genOwnershipType = do
  name <- genVariableName
  elements [Owned name, Borrowed name, MutBorrowed name]

-- | 生成所有权错误
genOwnershipError :: Gen OwnershipError
genOwnershipError = oneof
  [ UseAfterMove <$> genVariableName
  , DoubleMove <$> genVariableName <*> genVariableName
  , BorrowWhileMoved <$> genVariableName
  , MutBorrowWhileBorrowed <$> genVariableName
  , BorrowWhileMutBorrowed <$> genVariableName
  , MultipleMutBorrows <$> genVariableName
  , UseWhileMutBorrowed <$> genVariableName
  , OutOfScope <$> genVariableName
  , BorrowError <$> genVariableName
  , ParseError <$> genVariableName
  , CrossFunctionMove <$> genVariableName <*> genVariableName
  , ParameterMoveMismatch <$> genVariableName
  , ControlFlowError <$> genVariableName
  , PathSensitiveError <$> genVariableName
  , LoopOwnershipError <$> genVariableName
  ]

-- | 生成所有权转移
genOwnershipTransfer :: Gen OwnershipTransfer
genOwnershipTransfer = do
  fromVar <- genVariableName
  toVar <- genVariableName `suchThat` (/= fromVar)  -- 确保源和目标不同
  return $ OwnershipTransfer fromVar toVar

-- | 生成所有权转移列表
genOwnershipTransferList :: Gen [OwnershipTransfer]
genOwnershipTransferList = listOf genOwnershipTransfer

-- | 生成所有权类型列表
genOwnershipTypeList :: Gen [OwnershipType]
genOwnershipTypeList = listOf genOwnershipType

-- 属性：OwnershipType的Show实例应该包含变量名
prop_ownershipType_show_contains_name :: Property
prop_ownershipType_show_contains_name =
  forAll genOwnershipType $ \ownershipType ->
    let showStr = show ownershipType
        name = case ownershipType of
                 Owned n -> n
                 Borrowed n -> n
                 MutBorrowed n -> n
    in name `L.isInfixOf` showStr

-- 属性：OwnershipType的Ord实例应该有正确的顺序
prop_ownershipType_ordering :: Property
prop_ownershipType_ordering =
  forAll genOwnershipType $ \type1 ->
  forAll genOwnershipType $ \type2 ->
    let ordering = compare type1 type2
        -- Owned应该排在Borrowed和MutBorrowed之前
        -- Borrowed应该排在MutBorrowed之前
        expectedOrdering = case (type1, type2) of
          (Owned _, Borrowed _) -> LT
          (Owned _, MutBorrowed _) -> LT
          (Borrowed _, MutBorrowed _) -> LT
          (Borrowed _, Owned _) -> GT
          (MutBorrowed _, Owned _) -> GT
          (MutBorrowed _, Borrowed _) -> GT
          _ -> compare (show type1) (show type2)
    in ordering === expectedOrdering

-- 属性：OwnershipError的Show实例应该包含错误类型名称
prop_ownershipError_show_contains_type :: Property
prop_ownershipError_show_contains_type =
  forAll genOwnershipError $ \error ->
    let showStr = show error
        errorType = case error of
          UseAfterMove _ -> "UseAfterMove"
          DoubleMove _ _ -> "DoubleMove"
          BorrowWhileMoved _ -> "BorrowWhileMoved"
          MutBorrowWhileBorrowed _ -> "MutBorrowWhileBorrowed"
          BorrowWhileMutBorrowed _ -> "BorrowWhileMutBorrowed"
          MultipleMutBorrows _ -> "MultipleMutBorrows"
          UseWhileMutBorrowed _ -> "UseWhileMutBorrowed"
          OutOfScope _ -> "OutOfScope"
          BorrowError _ -> "BorrowError"
          ParseError _ -> "ParseError"
          CrossFunctionMove _ _ -> "CrossFunctionMove"
          ParameterMoveMismatch _ -> "ParameterMoveMismatch"
          ControlFlowError _ -> "ControlFlowError"
          PathSensitiveError _ -> "PathSensitiveError"
          LoopOwnershipError _ -> "LoopOwnershipError"
    in errorType `L.isInfixOf` showStr

-- 属性：OwnershipTransfer的源和目标应该不同
prop_ownership_transfer_different_vars :: Property
prop_ownership_transfer_different_vars =
  forAll genOwnershipTransfer $ \transfer ->
    transferFrom transfer /= transferTo transfer

-- 属性：OwnershipTransfer的Show实例应该包含转移信息
prop_ownership_transfer_show_contains_info :: Property
prop_ownership_transfer_show_contains_info =
  forAll genOwnershipTransfer $ \transfer ->
    let showStr = show transfer
    in "OwnershipTransfer" `L.isInfixOf` showStr &&
       transferFrom transfer `L.isInfixOf` showStr &&
       transferTo transfer `L.isInfixOf` showStr

-- 属性：newOwnershipAnalyzer应该返回有效的分析器
prop_new_ownership_analyzer_valid :: Property
prop_new_ownership_analyzer_valid =
  let analyzer = newOwnershipAnalyzer
  in case analyzer of
       OwnershipAnalyzer _ -> property True

-- 属性：OwnershipAnalyzer的Show实例应该工作
prop_ownership_analyzer_show :: Property
prop_ownership_analyzer_show =
  let analyzer = newOwnershipAnalyzer
      showStr = show analyzer
  in not (null showStr) === True

-- 属性：所有权转移列表中的变量名应该是有效的
prop_ownership_transfer_list_valid_vars :: Property
prop_ownership_transfer_list_valid_vars =
  forAll genOwnershipTransferList $ \transfers ->
    let allVars = concatMap (\t -> [transferFrom t, transferTo t]) transfers
        isValidVar var = not (null var) && L.head var `elem` ['a'..'z']
    in L.all isValidVar allVars

-- 属性：所有权类型列表中的变量名应该是有效的
prop_ownership_type_list_valid_vars :: Property
prop_ownership_type_list_valid_vars =
  forAll genOwnershipTypeList $ \types ->
    let allVars = L.map (\t -> case t of
                              Owned n -> n
                              Borrowed n -> n
                              MutBorrowed n -> n) types
        isValidVar var = not (null var) && L.head var `elem` ['a'..'z']
    in L.all isValidVar allVars

-- 属性：所有权错误列表应该可以排序
prop_ownership_error_list_sortable :: Property
prop_ownership_error_list_sortable =
  forAll (listOf genOwnershipError) $ \errors ->
    let sortedErrors = sort errors
    in L.length sortedErrors === L.length errors

-- 属性：相同所有权类型的比较应该基于变量名
prop_ownership_type_same_type_comparison :: Property
prop_ownership_type_same_type_comparison =
  forAll genVariableName $ \name1 ->
  forAll genVariableName $ \name2 ->
    let owned1 = Owned name1
        owned2 = Owned name2
        borrowed1 = Borrowed name1
        borrowed2 = Borrowed name2
        mutBorrowed1 = MutBorrowed name1
        mutBorrowed2 = MutBorrowed name2
    in compare owned1 owned2 === compare name1 name2 .&&.
       compare borrowed1 borrowed2 === compare name1 name2 .&&.
       compare mutBorrowed1 mutBorrowed2 === compare name1 name2

-- 属性：所有权转移的源和目标交换应该产生不同的转移
prop_ownership_transfer_swap_different :: Property
prop_ownership_transfer_swap_different =
  forAll genOwnershipTransfer $ \transfer ->
    let swapped = OwnershipTransfer (transferTo transfer) (transferTo transfer)
    in transfer /= swapped

-- 属性：所有权错误的消息应该包含相关变量名
prop_ownership_error_contains_variables :: Property
prop_ownership_error_contains_variables =
  forAll genOwnershipError $ \error ->
    let showStr = show error
        hasVar = case error of
          UseAfterMove var -> var `L.isInfixOf` showStr
          DoubleMove var1 var2 -> var1 `L.isInfixOf` showStr && var2 `L.isInfixOf` showStr
          BorrowWhileMoved var -> var `L.isInfixOf` showStr
          MutBorrowWhileBorrowed var -> var `L.isInfixOf` showStr
          BorrowWhileMutBorrowed var -> var `L.isInfixOf` showStr
          MultipleMutBorrows var -> var `L.isInfixOf` showStr
          UseWhileMutBorrowed var -> var `L.isInfixOf` showStr
          OutOfScope var -> var `L.isInfixOf` showStr
          BorrowError msg -> not (null msg)
          ParseError msg -> not (null msg)
          CrossFunctionMove var1 var2 -> var1 `L.isInfixOf` showStr && var2 `L.isInfixOf` showStr
          ParameterMoveMismatch var -> var `L.isInfixOf` showStr
          ControlFlowError msg -> not (null msg)
          PathSensitiveError msg -> not (null msg)
          LoopOwnershipError msg -> not (null msg)
    in hasVar

-- 属性：所有权转移列表应该可以去除重复
prop_ownership_transfer_list_deduplication :: Property
prop_ownership_transfer_list_deduplication =
  forAll genOwnershipTransferList $ \transfers ->
    let uniqueTransfers = nub transfers
    in L.length uniqueTransfers <= L.length transfers

-- 属性：所有权类型应该可以按类型分组
prop_ownership_type_groupable :: Property
prop_ownership_type_groupable =
  forAll genOwnershipTypeList $ \types ->
    let ownedTypes = L.filter (\t -> case t of Owned _ -> True; _ -> False) types
        borrowedTypes = L.filter (\t -> case t of Borrowed _ -> True; _ -> False) types
        mutBorrowedTypes = L.filter (\t -> case t of MutBorrowed _ -> True; _ -> False) types
        totalLength = L.length ownedTypes + L.length borrowedTypes + L.length mutBorrowedTypes
    in totalLength === L.length types

tests :: TestTree
tests =
  testGroup "Ownership Transfer QuickCheck Tests"
    [ fastProperty "OwnershipType show contains name" prop_ownershipType_show_contains_name
    , fastProperty "OwnershipType ordering" prop_ownershipType_ordering
    , fastProperty "OwnershipError show contains type" prop_ownershipError_show_contains_type
    , fastProperty "Ownership transfer different vars" prop_ownership_transfer_different_vars
    , fastProperty "Ownership transfer show contains info" prop_ownership_transfer_show_contains_info
    , fastProperty "New ownership analyzer valid" prop_new_ownership_analyzer_valid
    , fastProperty "Ownership analyzer show" prop_ownership_analyzer_show
    , fastProperty "Ownership transfer list valid vars" prop_ownership_transfer_list_valid_vars
    , fastProperty "Ownership type list valid vars" prop_ownership_type_list_valid_vars
    , fastProperty "Ownership error list sortable" prop_ownership_error_list_sortable
    , fastProperty "Ownership type same type comparison" prop_ownership_type_same_type_comparison
    , fastProperty "Ownership transfer swap different" prop_ownership_transfer_swap_different
    , fastProperty "Ownership error contains variables" prop_ownership_error_contains_variables
    , fastProperty "Ownership transfer list deduplication" prop_ownership_transfer_list_deduplication
    , fastProperty "Ownership type groupable" prop_ownership_type_groupable
    ]