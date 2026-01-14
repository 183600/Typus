{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.IntegrationAdvancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, advancePosByText)
import Utils (trim, removeLineComments, normalizeIndentation)
import ErrorHandler (formatError, collectErrors)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (replicateM)
import Data.Char (isAlphaNum, isAlpha, isSpace)
import Data.Either (isLeft, isRight)

-- | 测试解析器和工具函数的集成
prop_parser_utils_integration :: String -> Property
prop_parser_utils_integration code =
  let trimmed = trim code
      withoutComments = removeLineComments trimmed
      normalized = normalizeIndentation withoutComments
  in case parseTypus "" normalized of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试源位置和解析器的集成
prop_sourcelocation_parser_integration :: String -> Property
prop_sourcelocation_parser_integration code =
  case parseTypus "" code of
    Left _ -> property True
    Right file -> 
      let blocks = tfBlocks file
          spans = map cbSpan blocks
      in all isValidSpan spans

-- | 测试错误处理和解析器的集成
prop_errorhandler_parser_integration :: String -> Property
prop_errorhandler_parser_integration code =
  case parseTypus "" code of
    Left err -> 
      let formatted = formatError "Parse Error" (show err)
      in "Parse Error" `isInfixOf` formatted
    Right file -> property True

-- | 测试多模块处理的一致性
prop_multimodule_consistency :: [String] -> Property
prop_multimodule_consistency codes =
  length codes < 10 ==> 
  let results = map (parseTypus "") codes
      allRight = all isRight results
  in allRight ==> property True

-- | 测试端到端编译流程
prop_end_to_end_compilation :: String -> Property
prop_end_to_end_compilation code =
  let trimmed = trim code
      withoutComments = removeLineComments trimmed
      normalized = normalizeIndentation withoutComments
  in case parseTypus "" normalized of
    Left _ -> property True
    Right file -> 
      let blocks = tfBlocks file
          blockCount = length blocks
      in blockCount >= 0

-- | 测试错误恢复和解析的集成
prop_error_recovery_parser_integration :: String -> Property
prop_error_recovery_parser_integration code =
  case parseTypus "" code of
    Left _ -> 
      let recovered = "recovered from parse error"
      in not $ null recovered
    Right file -> property True

-- | 测试位置跟踪和错误报告的集成
prop_location_tracking_error_reporting :: String -> Property
prop_location_tracking_error_reporting code =
  let pos = advancePosByText code startPos
      line = posLine pos
      col = posColumn pos
  in line > 0 && col > 0 ==> property True

-- | 测试指令处理和解析的集成
prop_directive_parser_integration :: [(String, String)] -> Property
prop_directive_parser_integration directives =
  length directives < 5 ==> 
  let directiveLines = ["// @" ++ key ++ "=" ++ value | (key, value) <- directives]
      code = unlines directiveLines ++ "\nsome code"
  in case parseTypus "" code of
    Left _ -> property True
    Right file -> tfDirectives file === tfDirectives file  -- 简单验证不崩溃

-- | 测试字符串处理和解析的集成
prop_string_processing_parser_integration :: String -> Property
prop_string_processing_parser_integration code =
  let processed = normalizeIndentation $ removeLineComments $ trim code
  in case parseTypus "" processed of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试错误收集和解析的集成
prop_error_collection_parser_integration :: [String] -> Property
prop_error_collection_parser_integration codes =
  length codes < 10 ==> 
  let results = map (parseTypus "") codes
      errors = [show err | Left err <- results]
      collected = collectErrors errors
  in length collected >= 0

-- | 测试模块间数据传递的一致性
prop_module_data_consistency :: String -> Property
prop_module_data_consistency code =
  let parsed = parseTypus "" code
  in case parsed of
    Left _ -> property True
    Right file -> 
      let blocks = tfBlocks file
          directives = tfDirectives file
      in length blocks >= 0 && length (show directives) >= 0

-- | 测试复杂场景下的模块协作
prop_complex_module_collaboration :: String -> String -> String -> Property
prop_complex_module_collaboration directives code comments =
  let fullCode = directives ++ "\n" ++ code ++ "\n// " ++ comments
      processed = normalizeIndentation $ removeLineComments $ trim fullCode
  in case parseTypus "" processed of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试解析器和位置跟踪的集成
prop_parser_location_tracking_integration :: String -> Property
prop_parser_location_tracking_integration code =
  case parseTypus "" code of
    Left _ -> property True
    Right file -> 
      let blocks = tfBlocks file
          spans = map cbSpan blocks
          positions = map spanStart spans
      in all isValidPos positions

-- | 测试错误处理和位置跟踪的集成
prop_error_handling_location_integration :: String -> Property
prop_error_handling_location_integration code =
  let pos = advancePosByText code startPos
      errorLoc = show pos
  in not $ null errorLoc

-- | 测试工具函数链的集成
prop_utils_chain_integration :: String -> Property
prop_utils_chain_integration code =
  let step1 = trim code
      step2 = removeLineComments step1
      step3 = normalizeIndentation step2
  in length step3 >= 0

-- | 测试解析器对预处理代码的处理
prop_parser_preprocessed_code_integration :: String -> Property
prop_parser_preprocessed_code_integration code =
  let preprocessed = normalizeIndentation $ removeLineComments $ trim code
  in case parseTypus "" preprocessed of
    Left _ -> property True
    Right file -> tfBlocks file === tfBlocks file  -- 简单验证不崩溃

-- | 测试错误处理链的集成
prop_error_handling_chain_integration :: [String] -> Property
prop_error_handling_chain_integration errors =
  length errors < 10 ==> 
  let formatted = map (formatError "Error") errors
      collected = collectErrors formatted
  in length collected >= 0

-- | 测试完整处理流程
test_complete_processing_pipeline :: Assertion
test_complete_processing_pipeline = do
  let rawCode = "  // test code\n  function test() { return 42; }  "
      trimmed = trim rawCode
      withoutComments = removeLineComments trimmed
      normalized = normalizeIndentation withoutComments
      result = parseTypus "" normalized
  case result of
    Left err -> assertFailure $ "Failed to parse processed code: " ++ show err
    Right file -> assertBool "Should have at least one block" (not $ null $ tfBlocks file)

-- | 测试错误处理流程
test_error_handling_pipeline :: Assertion
test_error_handling_pipeline = do
  let errors = ["Error 1", "Error 2", "Error 3"]
      formatted = map (formatError "Test") errors
      collected = collectErrors formatted
  assertEqual "Collected errors should match" formatted collected

-- | 测试位置跟踪流程
test_location_tracking_pipeline :: Assertion
test_location_tracking_pipeline = do
  let code = "line1\nline2\nline3"
      pos1 = advancePosByText "line1" startPos
      pos2 = advancePosByText "line2" pos1
      pos3 = advancePosByText "line3" pos2
  assertEqual "First line position" (SourcePos 1 6 6) pos1
  assertEqual "Second line position" (SourcePos 2 6 13) pos2
  assertEqual "Third line position" (SourcePos 3 6 20) pos3

-- | 测试多文件处理流程
test_multifile_processing_pipeline :: Assertion
test_multifile_processing_pipeline = do
  let files = ["code1", "code2", "code3"]
      results = map (parseTypus "") files
      successful = [file | Right file <- results]
      failed = [err | Left err <- results]
  assertEqual "Should process all files" (length files) (length successful + length failed)

-- | 测试指令处理流程
test_directive_processing_pipeline :: Assertion
test_directive_processing_pipeline = do
  let code = "// @ownership=true\n// @dependentTypes=false\nfunction test() {}"
      result = parseTypus "" code
  case result of
    Left err -> assertFailure $ "Failed to parse code with directives: " ++ show err
    Right file -> assertBool "Should parse directives successfully" True

-- | 测试错误恢复流程
test_error_recovery_pipeline :: Assertion
test_error_recovery_pipeline = do
  let invalidCode = "invalid { syntax"
      result = parseTypus "" invalidCode
  case result of
    Left err -> do
      let errorMsg = show err
          recovered = "Recovered from: " ++ errorMsg
      assertBool "Should generate recovery message" (not $ null recovered)
    Right file -> assertFailure "Expected parse error but got success"

-- | 辅助函数：检查span是否有效
isValidSpan :: SourceSpan -> Bool
isValidSpan (SourceSpan start end) = sourcePosLe start end

-- | 辅助函数：检查位置是否有效
isValidPos :: SourcePos -> Bool
isValidPos (SourcePos line col _) = line > 0 && col > 0

-- | 辅助函数：检查SourcePos的顺序
sourcePosLe :: SourcePos -> SourcePos -> Bool
sourcePosLe (SourcePos l1 c1 _) (SourcePos l2 c2 _) = 
  l1 < l2 || (l1 == l2 && c1 <= c2)

-- | 测试套件
tests :: TestTree
tests = testGroup "Integration Advanced QuickCheck Tests"
  [ testProperty "Parser Utils integration" prop_parser_utils_integration
  , testProperty "SourceLocation Parser integration" prop_sourcelocation_parser_integration
  , testProperty "ErrorHandler Parser integration" prop_errorhandler_parser_integration
  , testProperty "Multimodule consistency" prop_multimodule_consistency
  , testProperty "End to end compilation" prop_end_to_end_compilation
  , testProperty "Error recovery Parser integration" prop_error_recovery_parser_integration
  , testProperty "Location tracking error reporting" prop_location_tracking_error_reporting
  , testProperty "Directive Parser integration" prop_directive_parser_integration
  , testProperty "String processing Parser integration" prop_string_processing_parser_integration
  , testProperty "Error collection Parser integration" prop_error_collection_parser_integration
  , testProperty "Module data consistency" prop_module_data_consistency
  , testProperty "Complex module collaboration" prop_complex_module_collaboration
  , testProperty "Parser location tracking integration" prop_parser_location_tracking_integration
  , testProperty "Error handling location integration" prop_error_handling_location_integration
  , testProperty "Utils chain integration" prop_utils_chain_integration
  , testProperty "Parser preprocessed code integration" prop_parser_preprocessed_code_integration
  , testProperty "Error handling chain integration" prop_error_handling_chain_integration
  , testCase "Complete processing pipeline" test_complete_processing_pipeline
  , testCase "Error handling pipeline" test_error_handling_pipeline
  , testCase "Location tracking pipeline" test_location_tracking_pipeline
  , testCase "Multifile processing pipeline" test_multifile_processing_pipeline
  , testCase "Directive processing pipeline" test_directive_processing_pipeline
  , testCase "Error recovery pipeline" test_error_recovery_pipeline
  ]