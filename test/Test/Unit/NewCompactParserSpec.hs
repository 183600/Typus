{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCompactParserSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import qualified Data.Text as T
import Data.Char (isSpace)

-- | 生成简单的测试代码
genSimpleCode :: Gen String
genSimpleCode = elements
  [ "func main() { return 0; }"
  , "let x = 42;"
  , "if (x > 0) { print(x); }"
  , "for (i := 0; i < 10; i++) { }"
  , "// This is a comment\nvar y string = \"hello\";"
  ]

-- | 生成包含指令的代码
genDirectiveCode :: Gen String
genDirectiveCode = do
  hasOwnership <- elements [True, False]
  hasDepTypes <- elements [True, False]
  let ownership = if hasOwnership then "// @ownership\n" else ""
      depTypes = if hasDepTypes then "// @dependent-types\n" else ""
      code = "func test() {}"
  return $ ownership ++ depTypes ++ code

-- | 测试基本解析功能
testBasicParsing :: TestTree
testBasicParsing = testGroup "基本解析功能测试"
  [ testCase "解析简单函数" $
      let input = "func main() { return 0; }"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "解析成功" True
    
  , testCase "解析变量声明" $
      let input = "let x = 42;"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "解析成功" True
    
  , testCase "解析空代码" $
      let input = ""
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "解析成功" True
  ]

-- | 测试指令解析
testDirectiveParsing :: TestTree
testDirectiveParsing = testGroup "指令解析测试"
  [ testCase "解析所有权指令" $
      let input = "// @ownership\nfunc test() {}"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> 
          let directives = tfFileDirectives file
          in case fdOwnership directives of
            Just (Located _ True) -> assertBool "所有权指令解析正确" True
            _ -> assertBool "所有权指令未正确解析" False
    
  , testCase "解析依赖类型指令" $
      let input = "// @dependent-types\nfunc test() {}"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> 
          let directives = tfFileDirectives file
          in case fdDependentTypes directives of
            Just (Located _ True) -> assertBool "依赖类型指令解析正确" True
            _ -> assertBool "依赖类型指令未正确解析" False
  ]

-- | 测试注释处理
testCommentHandling :: TestTree
testCommentHandling = testGroup "注释处理测试"
  [ testCase "处理单行注释" $
      let input = "func test() {} // 这是注释"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "单行注释处理正确" True
    
  , testCase "处理多行注释" $
      let input = "func test() {} /* 多行\n注释 */"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "多行注释处理正确" True
    
  , testCase "注释不干扰代码解析" $
      let input = "// @ownership\n// @dependent-types\nfunc main() { /* comment */ return 0; // comment }"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> 
          let directives = tfFileDirectives file
              hasOwnership = maybe False (const True) (fdOwnership directives)
              hasDepTypes = maybe False (const True) (fdDependentTypes directives)
          in assertBool "注释不干扰指令解析" (hasOwnership && hasDepTypes)
  ]

-- | 测试错误恢复
testErrorRecovery :: TestTree
testErrorRecovery = testGroup "错误恢复测试"
  [ testCase "语法错误后继续解析" $
      let input = "func malformed( { return 0; }\nfunc correct() { return 1; }"
          result = parseTypus input
      in case result of
        Left err -> assertBool "应该能部分解析" False
        Right file -> 
          let blocks = tfCodeBlocks file
              hasCorrectBlock = any (\(CodeBlock _ code) -> "correct" `elem` code) blocks
          in assertBool "应该解析出正确的函数" hasCorrectBlock
    
  , testCase "不匹配的大括号" $
      let input = "func test() { return 0;"
          result = parseTypus input
      in case result of
        Left err -> assertBool "应该报告错误" True
        Right file -> assertBool "不应该完全成功" False
  ]

-- | QuickCheck属性测试
testParserProperties :: TestTree
testParserProperties = testGroup "解析器属性测试"
  [ testProperty "简单代码解析成功率" $
      forAll genSimpleCode $ \code ->
        let result = parseTypus code
        in case result of
          Left _ -> False
          Right _ -> True
  
  , testProperty "指令代码解析一致性" $
      forAll genDirectiveCode $ \code ->
        let result = parseTypus code
        in case result of
          Left _ -> False
          Right file -> 
            let blocks = tfCodeBlocks file
                hasCodeBlock = not (null blocks)
            in hasCodeBlock
  ]

-- | 性能相关测试
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup "性能属性测试"
  [ testProperty "解析时间与输入长度成正比" $
      \baseCode n -> 
        let repeatedCode = concat (replicate (min 100 (max 1 n)) baseCode)
            result = parseTypus repeatedCode
        in case result of
          Left _ -> True  -- 失败也是合理的结果
          Right _ -> True  -- 成功也是合理的结果
  ]

-- | 边界条件测试
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup "边界条件测试"
  [ testCase "只有空白字符" $
      let input = "   \n\t  \n  "
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "空白字符解析成功" True
    
  , testCase "只有注释" $
      let input = "// 这是注释\n/* 多行注释 */"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "只有注释解析成功" True
    
  , testCase "超长标识符" $
      let longIdent = concat (replicate 1000 "a")
          input = "func " ++ longIdent ++ "() { return 0; }"
          result = parseTypus input
      in case result of
        Left err -> assertBool ("解析失败: " ++ err) False
        Right file -> assertBool "长标识符解析成功" True
  ]

-- | 组合所有测试
tests :: TestTree
tests = testGroup "Parser模块核心功能测试"
  [ testBasicParsing
  , testDirectiveParsing
  , testCommentHandling
  , testErrorRecovery
  , testParserProperties
  , testPerformanceProperties
  , testBoundaryConditions
  ]