{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalTest8Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isAlpha, isDigit)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import SyntaxValidator
  ( validateSyntax
  , SyntaxError(..)
  , SyntaxWarning(..)
  , ValidationResult(..)
  , ValidationConfig(..)
  , defaultValidationConfig
  )
import SimpleSyntaxValidator
  ( simpleValidate
  , ValidationIssue(..)
  , ValidationSeverity(..)
  )

-- | 测试语法验证器的功能和属性
tests :: TestTree
tests =
  testGroup "NewCabalTest8 - 语法验证测试"
    [ testGroup "单元测试"
        [ testCase "基本语法验证" $ do
            let code = "func main() { return 42 }"
                result = validateSyntax code defaultValidationConfig
            case result of
                ValidationResult [] [] -> assertBool "Should validate successfully" True
                ValidationResult errors warnings -> 
                    assertBool ("Validation failed: " ++ show errors) False

        , testCase "语法错误检测" $ do
            let code = "func main( { return 42 }"  -- 缺少右括号
                result = validateSyntax code defaultValidationConfig
            case result of
                ValidationResult (error:_) warnings -> 
                    assertBool "Should detect syntax error" $ "parenthesis" `L.isInfixOf` (map toLower (show error))
                ValidationResult [] warnings -> 
                    assertBool "Should detect syntax error" False

        , testCase "语法警告检测" $ do
            let code = "func main() { var x = 42; return x; }"  -- 未使用的变量
                result = validateSyntax code defaultValidationConfig
            case result of
                ValidationResult errors (warning:_) -> 
                    assertBool "Should detect unused variable" $ "unused" `L.isInfixOf` (map toLower (show warning))
                ValidationResult errors [] -> 
                    assertBool "Should detect unused variable" False

        , testCase "简单语法验证器" $ do
            let code = "func test() { }"
                result = simpleValidate code
            assertBool "Should validate simple syntax" $ null result

        , testCase "复杂语法结构验证" $ do
            let code = unlines
                  [ "func complex(a int, b string) bool {"
                  , "  if a > 0 {"
                  , "    for i := 0; i < a; i++ {"
                  , "      fmt.Println(b)"
                  , "    }"
                  , "    return true"
                  , "  }"
                  , "  return false"
                  , "}"
                  ]
                result = validateSyntax code defaultValidationConfig
            case result of
                ValidationResult [] [] -> assertBool "Should validate complex syntax" True
                ValidationResult errors warnings -> 
                    assertBool ("Complex validation failed: " ++ show errors) False
        ]

    , testGroup "QuickCheck属性测试"
        [ fastProperty "语法验证的确定性" prop_syntax_validation_deterministic
        , fastProperty "语法错误的局部性" prop_syntax_error_locality
        , fastProperty "语法警告的一致性" prop_syntax_warning_consistency
        , fastProperty "语法验证的完整性" prop_syntax_validation_completeness
        , fastProperty "语法验证的健全性" prop_syntax_validation_soundness
        ]
    ]

-- QuickCheck属性测试

-- 语法验证的确定性：相同代码应该产生相同的验证结果
prop_syntax_validation_deterministic :: String -> Property
prop_syntax_validation_deterministic code =
  let result1 = validateSyntax code defaultValidationConfig
      result2 = validateSyntax code defaultValidationConfig
  in case (result1, result2) of
       (ValidationResult errors1 warnings1, ValidationResult errors2 warnings2) -> 
         property $ L.length errors1 === L.length errors2 .&&.
                    L.length warnings1 === L.length warnings2

-- 语法错误的局部性：语法错误应该指向具体的位置
prop_syntax_error_locality :: String -> Int -> Int -> Property
prop_syntax_error_locality code line column =
  line > 0 && column > 0 ==>
  let result = validateSyntax code defaultValidationConfig
  in case result of
       ValidationResult (error:_) warnings -> 
         let hasValidLocation = seLine error > 0 && seColumn error > 0
         in property $ hasValidLocation
       ValidationResult [] warnings -> 
         property $ True  -- 没有错误时跳过

-- 语法警告的一致性：相同的代码模式应该产生一致的警告
prop_syntax_warning_consistency :: String -> Property
prop_syntax_warning_consistency code =
  let result1 = validateSyntax code defaultValidationConfig
      result2 = validateSyntax code defaultValidationConfig
  in case (result1, result2) of
       (ValidationResult errors1 warnings1, ValidationResult errors2 warnings2) -> 
         let warningTypes1 = map swType warnings1
             warningTypes2 = map swType warnings2
         in property $ sort warningTypes1 === sort warningTypes2
       _ -> property $ True

-- 语法验证的完整性：有效的语法应该通过验证
prop_syntax_validation_completeness :: String -> Property
prop_syntax_validation_completeness code =
  let isValidSyntax = not (null code) && 
                      L.all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789(){};,: \n\t") code
      result = validateSyntax code defaultValidationConfig
  in isValidSyntax ==> 
     case result of
       ValidationResult errors warnings -> 
         property $ L.length errors === 0  -- 有效语法不应该有错误

-- 语法验证的健全性：无效的语法应该被检测出来
prop_syntax_validation_soundness :: String -> Property
prop_syntax_validation_soundness code =
  let hasObviousErrors = "func (" `L.isInfixOf` code || 
                         "return" `L.isInfixOf` code && not ("func" `L.isInfixOf` code) ||
                         "{{" `L.isInfixOf` code || "}}" `L.isInfixOf` code
      result = validateSyntax code defaultValidationConfig
  in hasObviousErrors ==>
     case result of
       ValidationResult (error:_) warnings -> 
         property $ True  -- 应该检测到错误
       ValidationResult [] warnings -> 
         property $ False  -- 明显的错误应该被检测到

-- 辅助函数
toLower :: String -> String
toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort (L.filter (< x) xs) ++ [x] ++ sort (L.filter (>= x) xs)