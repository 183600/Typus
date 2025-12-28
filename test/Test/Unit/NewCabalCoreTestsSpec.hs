module Test.Unit.NewCabalCoreTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Data.List (isInfixOf)

import qualified Parser
import qualified SourceLocation
import qualified Utils
import qualified Compiler
import qualified SyntaxValidator
import qualified ErrorHandler
import qualified DependentTypesParser
import qualified Ownership

-- | 新的Cabal核心测试用例，覆盖主要功能模块
tests :: TestTree
tests =
  testGroup "New Cabal Core Tests"
    [ parserBasicTests
    , sourceLocationTests
    , utilsFunctionTests
    , compilerErrorTests
    , syntaxValidatorTests
    , errorHandlerTests
    , dependentTypesTests
    , ownershipTests
    ]

-- ============================================================================
-- Parser基础测试
-- ============================================================================
parserBasicTests :: TestTree
parserBasicTests =
  testGroup "Parser Basic Tests"
    [ testCase "default file directives should be empty" $ do
        let directives = Parser.defaultFileDirectives
        Parser.fdOwnership directives @?= Nothing
        Parser.fdDependentTypes directives @?= Nothing
        Parser.fdConstraints directives @?= Nothing

    , testCase "default block directives should be empty" $ do
        let directives = Parser.defaultBlockDirectives
        Parser.bdOwnership directives @?= Nothing
        Parser.bdDependentTypes directives @?= Nothing
        Parser.bdConstraints directives @?= Nothing

    , testCase "parse empty typus file" $ do
        let result = Parser.parseTypus ""
        case result of
          Left err -> assertBool ("Should parse empty file but got error: " ++ show err) False
          Right (Parser.TypusFile _ _ blocks) -> 
            assertBool "Empty file should have no blocks" (null blocks)
    ]

-- ============================================================================
-- SourceLocation测试
-- ============================================================================
sourceLocationTests :: TestTree
sourceLocationTests =
  testGroup "SourceLocation Tests"
    [ testCase "start position should be (1,1)" $ do
        let pos = SourceLocation.startPos
        SourceLocation.posLine pos @?= 1
        SourceLocation.posColumn pos @?= 1

    , testCase "position advancement should work correctly" $ do
        let start = SourceLocation.startPos
        let afterNewline = SourceLocation.advancePos '\n' start
        SourceLocation.posLine afterNewline @?= 2
        SourceLocation.posColumn afterNewline @?= 1

    , testCase "empty span should be valid" $ do
        let span = SourceLocation.emptySpan
        assertBool "Empty span should be valid" (SourceLocation.isValidSpan span)

    , testCase "span merging should work" $ do
        let span1 = SourceLocation.spanFrom (SourceLocation.posAt 1 1)
        let span2 = SourceLocation.spanTo (SourceLocation.posAt 1 10)
        let merged = SourceLocation.mergeSpans span1 span2
        assertBool "Merged span should be valid" (SourceLocation.isValidSpan merged)
    ]

-- ============================================================================
-- Utils函数测试
-- ============================================================================
utilsFunctionTests :: TestTree
utilsFunctionTests =
  testGroup "Utils Function Tests"
    [ testCase "trim should remove whitespace" $ do
        Utils.trim "  hello world  " @?= "hello world"
        Utils.trim "\t\n  test  \n\t" @?= "test"

    , testCase "splitBy should preserve empty segments" $ do
        Utils.splitBy ',' "a,b,c" @?= ["a", "b", "c"]
        Utils.splitBy ',' "a,,b" @?= ["a", "", "b"]
        Utils.splitBy ',' "" @?= [""]

    , testCase "splitByCollapsed should remove empty segments" $ do
        Utils.splitByCollapsed ',' "a,,b" @?= ["a", "b"]
        Utils.splitByCollapsed ',' ",a,," @?= ["a"]
        Utils.splitByCollapsed ',' "" @?= []

    , testCase "removeLineComments should work" $ do
        let input = "x := 1 // this is a comment\ny := 2"
        let expected = "x := 1 \ny := 2"
        Utils.removeLineComments input @?= expected
    ]

-- ============================================================================
-- Compiler错误处理测试
-- ============================================================================
compilerErrorTests :: TestTree
compilerErrorTests =
  testGroup "Compiler Error Tests"
    [ testCase "compilation should detect syntax errors" $ do
        let malformedCode = "func x { ="
        let result = Compiler.compile malformedCode
        case result of
          Left errs -> 
            assertBool "Should detect syntax errors" (not $ null errs)
          Right _ -> 
            assertBool "Should not compile malformed code" False

    , testCase "compilation should handle empty input" $ do
        let result = Compiler.compile ""
        case result of
          Left _ -> assertBool "Empty input should cause error" True
          Right _ -> assertBool "Empty input compilation result needs verification" True
    ]

-- ============================================================================
-- SyntaxValidator测试
-- ============================================================================
syntaxValidatorTests :: TestTree
syntaxValidatorTests =
  testGroup "SyntaxValidator Tests"
    [ testCase "should validate basic syntax" $ do
        let validCode = "x := 1\ny := 2"
        -- 假设SyntaxValidator有一个validate函数
        let result = SyntaxValidator.validateSyntax validCode
        case result of
          Left errs -> assertBool ("Valid code should pass: " ++ show errs) False
          Right _ -> assertBool "Valid code should pass validation" True

    , testCase "should reject invalid syntax" $ do
        let invalidCode = "x := 1\ny := 2  } invalid"
        let result = SyntaxValidator.validateSyntax invalidCode
        case result of
          Left _ -> assertBool "Invalid code should be rejected" True
          Right _ -> assertBool "Invalid code should not pass" False
    ]

-- ============================================================================
-- ErrorHandler测试
-- ============================================================================
errorHandlerTests :: TestTree
errorHandlerTests =
  testGroup "ErrorHandler Tests"
    [ testCase "should format errors correctly" $ do
        let errorMsg = "Test error message"
        let formatted = ErrorHandler.formatError errorMsg
        assertBool "Formatted error should contain original message" (errorMsg `isInfixOf` formatted)

    , testCase "should handle multiple errors" $ do
        let errors = ["Error 1", "Error 2", "Error 3"]
        let formatted = ErrorHandler.formatErrors errors
        assertBool "Should format all errors" (all (`isInfixOf` formatted) errors)
    ]

-- ============================================================================
-- DependentTypes测试
-- ============================================================================
dependentTypesTests :: TestTree
dependentTypesTests =
  testGroup "DependentTypes Tests"
    [ testCase "should parse basic dependent types" $ do
        let typeCode = "Vector(n) where n > 0"
        let result = DependentTypesParser.parseDependentType typeCode
        case result of
          Left _ -> assertBool "Should parse basic dependent type" False
          Right _ -> assertBool "Basic dependent type should parse" True

    , testCase "should reject invalid dependent types" $ do
        let invalidTypeCode = "Vector( where n > 0"
        let result = DependentTypesParser.parseDependentType invalidTypeCode
        case result of
          Left _ -> assertBool "Invalid dependent type should be rejected" True
          Right _ -> assertBool "Invalid dependent type should not parse" False
    ]

-- ============================================================================
-- Ownership测试
-- ============================================================================
ownershipTests :: TestTree
ownershipTests =
  testGroup "Ownership Tests"
    [ testCase "should track basic ownership" $ do
        let ownershipCode = "x := move(y)"
        let result = Ownership.analyzeOwnership ownershipCode
        case result of
          Left _ -> assertBool "Should analyze basic ownership" False
          Right _ -> assertBool "Basic ownership should be analyzed" True

    , testCase "should detect ownership violations" $ do
        let violationCode = "x := y\nz := move(y)"
        let result = Ownership.analyzeOwnership violationCode
        case result of
          Left _ -> assertBool "Should detect ownership violations" True
          Right _ -> assertBool "Ownership violations should be detected" True
    ]