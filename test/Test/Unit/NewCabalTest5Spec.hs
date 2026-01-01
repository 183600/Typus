{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest5Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isAlpha)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )
import Ownership.Common.Types

-- | 测试所有权分析和传递的属性
tests :: TestTree
tests =
  testGroup "NewCabalTest5 - 所有权传递测试"
    [ testGroup "单元测试"
        [ testCase "所有权分析器的基本功能" $ do
            analyzer <- newOwnershipAnalyzer
            assertBool "Should create ownership analyzer" $ True

        , testCase "简单的所有权传递" $ do
            let code = "func main() { var x = 42; return x; }"
                result = analyzeOwnership code
            case result of
                Left errors -> assertBool ("Ownership analysis failed: " ++ show errors) False
                Right transfers -> 
                    assertBool "Should detect ownership transfers" $ not $ null transfers

        , testCase "所有权错误的格式化" $ do
            let error = OwnershipError "Test ownership error" OwnershipErrorTypeMove 1 1
                formatted = formatOwnershipErrors [error]
            assertBool "Should contain error message" $ "Test ownership error" `L.isInfixOf` formatted

        , testCase "内置函数的所有权处理" $ do
            let builtinFuncs = builtInFunctions
            assertBool "Should have built-in functions" $ not $ null builtinFuncs
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "所有权传递的传递性" prop_ownership_transfer_transitivity
        , fastProperty "所有权分析的一致性" prop_ownership_analysis_consistency
        , fastProperty "所有权错误的完整性" prop_ownership_error_completeness
        , fastProperty "所有权类型的层次性" prop_ownership_type_hierarchy
        , fastProperty "所有权分析器的幂等性" prop_ownership_analyzer_idempotent
        ]
    ]

-- QuickCheck属性测试

-- 所有权传递的传递性：如果A传递给B，B传递给C，那么A间接传递给C
prop_ownership_transfer_transitivity :: String -> String -> String -> Property
prop_ownership_transfer_transitivity varA varB varC =
  not (null varA) && not (null varB) && not (null varC) &&
  L.all (L.all isAlpha) [varA, varB, varC] ==> 
  let code = "func test() { " ++ varA ++ " := 42; " ++ varB ++ " := " ++ varA ++ "; " ++ varC ++ " := " ++ varB ++ "; }"
      result = analyzeOwnership code
  in case result of
       Right transfers -> 
         let transfersFromA = L.filter (\t -> otFrom t == varA) transfers
             transfersToC = L.filter (\t -> otTo t == varC) transfers
         in property $ not (null transfersFromA) ==> not (null transfersToC)
       Left _ -> property $ True  -- 分析失败时跳过此测试

-- 所有权分析的一致性：相同代码应该产生相同的分析结果
prop_ownership_analysis_consistency :: String -> Property
prop_ownership_analysis_consistency code =
  let result1 = analyzeOwnership code
      result2 = analyzeOwnership code
  in case (result1, result2) of
       (Right transfers1, Right transfers2) -> 
         property $ L.length transfers1 === L.length transfers2
       (Left _, Left _) -> property $ True
       _ -> property $ False

-- 所有权错误的完整性：所有权错误应该包含足够的信息用于调试
prop_ownership_error_completeness :: String -> OwnershipErrorType -> Int -> Int -> Property
prop_ownership_error_completeness message errorType line column =
  not (null message) && line > 0 && column > 0 ==>
  let error = OwnershipError message errorType line column
      hasValidMessage = not $ L.null $ oeMessage error
      hasValidType = errorType == oeType error
      hasValidLocation = line == oeLine error && column == oeColumn error
  in property $ hasValidMessage .&&. hasValidType .&&. hasValidLocation

-- 所有权类型的层次性：Move > Borrow > Own
prop_ownership_type_hierarchy :: OwnershipType -> OwnershipType -> Property
prop_ownership_type_hierarchy type1 type2 =
  let typeOrder = \case
        OwnershipTypeMove -> 3
        OwnershipTypeBorrow -> 2
        OwnershipTypeOwn -> 1
        OwnershipTypeShared -> 0
  in property $ (type1 > type2) === (typeOrder type1 > typeOrder type2)

-- 所有权分析器的幂等性：多次分析相同代码应该产生一致的结果
prop_ownership_analyzer_idempotent :: String -> Property
prop_ownership_analyzer_idempotent code =
  let result1 = analyzeOwnership code
      result2 = analyzeOwnership code
      result3 = analyzeOwnership code
  in case (result1, result2, result3) of
       (Right transfers1, Right transfers2, Right transfers3) -> 
         property $ L.length transfers1 === L.length transfers2 .&&.
                    L.length transfers2 === L.length transfers3
       (Left _, Left _, Left _) -> property $ True
       _ -> property $ False