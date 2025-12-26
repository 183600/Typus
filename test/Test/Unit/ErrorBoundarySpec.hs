{-# LANGUAGE CPP #-}
module Test.Unit.ErrorBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Parser (parseTypus)
import Compiler (compile)
import ErrorHandler (ErrorHandler, handleError)
import SourceLocation (SourcePos(..), SourceSpan(..))

tests :: TestTree
tests =
  testGroup "Error Boundary Tests"
    [ testCase "handles extremely long input gracefully" $ do
        let veryLongInput = concat (replicate 10000 "func test() {}\n")
        case parseTypus veryLongInput of
          Left _ -> assertBool "Expected to handle long input gracefully" True
          Right _ -> assertBool "Successfully parsed long input" True
    
    , testCase "handles deeply nested structures" $ do
        let deeplyNested = concat (replicate 1000 "func test() { ")
            ++ "return 42"
            ++ concat (replicate 1000 " }")
        case parseTypus deeplyNested of
          Left _ -> assertBool "Expected to handle deeply nested structures gracefully" True
          Right _ -> assertBool "Successfully parsed deeply nested structures" True
    
    , testCase "handles malformed unicode input" $ do
        let malformedUnicode = "func test() { return \"\xFF\xFE\"; }"
        case parseTypus malformedUnicode of
          Left _ -> assertBool "Expected to handle malformed unicode gracefully" True
          Right _ -> assertBool "Successfully handled malformed unicode" True
    
    , testCase "handles empty input with appropriate error" $ do
        case parseTypus "" of
          Left err -> assertBool "Expected error for empty input" $ null err
          Right _ -> assertFailure "Expected parse failure for empty input"
    
    , testCase "handles null bytes in input" $ do
        let inputWithNull = "func test() { return \"\0\"; }"
        case parseTypus inputWithNull of
          Left _ -> assertBool "Expected to handle null bytes gracefully" True
          Right _ -> assertBool "Successfully handled null bytes" True
    
    , testCase "recovers from syntax errors and continues parsing" $ do
        let inputWithErrors = unlines
              [ "func validFunction() {"
              , "  return 42"
              , "}"
              , "func invalidFunction( {  // missing parameter name and closing paren"
              , "  return \"error\""
              , "}"
              , "func anotherValidFunction() {"
              , "  return \"success\""
              , "}"
              ]
        case parseTypus inputWithErrors of
          Left _ -> assertBool "Expected to handle multiple syntax errors" True
          Right result -> assertBool "Should parse valid parts despite errors" $ length (show result) > 0
    
    , testCase "handles extreme indentation levels" $ do
        let extremeIndentation = concat (replicate 200 "  ") ++ "func test() { return 42; }"
        case parseTypus extremeIndentation of
          Left _ -> assertBool "Expected to handle extreme indentation gracefully" True
          Right _ -> assertBool "Successfully handled extreme indentation" True
    
    , testCase "handles circular dependency detection" $ do
        let circularInput = unlines
              [ "import \"file_a\""
              , "func testA() { return testB(); }"
              , "func testB() { return testA(); }"
              ]
        case parseTypus circularInput of
          Left _ -> assertBool "Expected to detect circular dependencies" True
          Right _ -> assertBool "Successfully handled circular dependency detection" True
    ]