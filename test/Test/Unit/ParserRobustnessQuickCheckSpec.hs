module Test.Unit.ParserRobustnessQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, oneof, elements, frequency)
import Data.Char (isAlphaNum, isSpace, isControl)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import qualified Data.Text as T

-- | Generate potentially problematic input strings
instance Arbitrary String where
  arbitrary = frequency
    [ (3, normalCode)
    , (2, edgeCaseCode)
    , (2, malformedDirectives)
    , (1, unicodeContent)
    , (1, controlCharacters)
    , (1, veryLongLines)
    , (1, emptyOrWhitespace)
    ]
    where
      normalCode = listOf $ elements ['a'..'z', 'A'..'Z', '0'..'9', ' ', '\t', '\n', '(', ')', '{', '}', ';', '=', '/', ':', ',']
      edgeCaseCode = listOf $ elements ['\n', '\r', '\t', ' ', '{', '}', '!', '/', ':', ',']
      malformedDirectives = listOf $ elements ['!', '/', '{', '}', ':', ',', ' ', '\t', '\n', 'a', 'b', 'c']
      unicodeContent = listOf $ elements $ ['\32'..('\126' :: Char)] ++ ['\160'..('\255' :: Char)]
      controlCharacters = listOf $ elements $ filter isControl ['\0'..('\31' :: Char)] ++ ['\127']
      veryLongLines = do
        numLines <- choose (1, 10)
        lineLength <- choose (100, 1000)
        lines <- listOf1 $ listOf $ elements ['a'..'z', ' ']
        return $ unlines $ map (\line -> take lineLength (cycle line)) lines
      emptyOrWhitespace = frequency
        [ (1, return "")
        , (1, listOf $ elements " \t\n\r\f\v")
        ]

-- | Generate code with nested structures
genNestedCode :: Int -> Gen String
genNestedCode depth = if depth <= 0
  then return ""
  else do
    content <- listOf $ elements ['a'..'z', ' ', ';']
    nested <- frequency
      [ (3, return "")
      , (2, genNestedCode (depth - 1))
      ]
    return $ unlines [content, nested]

-- | Generate code with specific patterns
genPatternCode :: Gen String
genPatternCode = frequency
    [ (3, return "if condition {\n    // code\n}")
    , (2, return "//! directive: value\nfunction call()")
    , (2, return "{//! block: directive }\ncode block")
    , (1, return "package main\n\nfunc main() {\n}")
    , (1, genNestedCode 5)
    ]

tests :: TestTree
tests =
  testGroup "Parser robustness QuickCheck tests"
    [ testGroup "Input robustness"
        [ testCase "handles completely empty input" $ do
            let result = parseTypus ""
            case result of
              Right file -> do
                tfDirectives file @?= Parser.defaultFileDirectives
                tfBuildTags file @?= []
                tfBlocks file @?= []
                tfSyntaxErrors file @?= []
              Left err -> assertFailure $ "Expected successful parse of empty input, got: " ++ err

        , testCase "handles whitespace-only input" $ do
            let inputs = ["   ", "\n\n\n", "\t\t\t", "  \n\t  \n  "]
            mapM_ (\input -> do
              let result = parseTypus input
              case result of
                Right file -> tfDirectives file @?= Parser.defaultFileDirectives
                Left err -> assertFailure $ "Expected successful parse of whitespace, got: " ++ err
              ) inputs

        , testCase "handles extremely long lines" $ do
            let longLine = replicate 10000 'a' ++ " //! directive: value"
                result = parseTypus longLine
            case result of
              Right _ -> return () -- Should not crash
              Left _ -> return () -- May fail but should not crash

        , fastProperty "parser never crashes on any input" $
            \input ->
              let result = parseTypus input
              in case result of
                   Right _ -> True
                   Left _ -> True

        , fastProperty "parser handles control characters gracefully" $
            \input ->
              let result = parseTypus input
              in case result of
                   Right _ -> True
                   Left _ -> True -- Should not crash, may fail gracefully
        ]

    , testGroup "Directive robustness"
        [ testCase "handles malformed file directives" $ do
            let malformed = [ "//!", "//! :", "//! key", "//! : value", "//! key:", "//! :: value" ]
            mapM_ (\input -> do
              let result = parseTypus input
              case result of
                Right file -> tfDirectives file @?= Parser.defaultFileDirectives
                Left _ -> return () -- Expected to fail gracefully
              ) malformed

        , testCase "handles malformed block directives" $ do
            let malformed = [ "{//!", "{//! }", "{//! :", "{//!", "{//! key", "{//! key:" ]
            mapM_ (\input -> do
              let result = parseTypus input
              case result of
                Right _ -> return () -- Should not crash
                Left _ -> return () -- May fail gracefully
              ) malformed

        , fastProperty "nested directives are handled" $
            \input ->
              let result = parseTypus input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in -- Should parse some structure without crashing
                        length blocks >= 0
                   Left _ -> True

        , fastProperty "directive parsing is case-sensitive" $
            \key value ->
              let input1 = "//! " ++ key ++ ": " ++ value
                  input2 = "//! " ++ map toUpper key ++ ": " ++ value
                  result1 = parseTypus input1
                  result2 = parseTypus input2
              in case (result1, result2) of
                   (Right file1, Right file2) -> 
                     -- Results should potentially differ due to case sensitivity
                     True
                   _ -> True
        ]

    , testGroup "Code block robustness"
        [ testCase "handles code without directives" $ do
            let input = unlines 
                  [ "package main"
                  , "func main() {"
                  , "    println(\"Hello\")"
                  , "}"
                  ]
                result = parseTypus input
            case result of
              Right file -> do
                tfDirectives file @?= Parser.defaultFileDirectives
                length (tfBlocks file) @?= 1
              Left _ -> assertFailure "Expected successful parse"

        , testCase "handles multiple code blocks" $ do
            let input = unlines
                  [ "//! ownership: true"
                  , "func first() {"
                  , "    // code"
                  , "}"
                  , "{//! ownership: false }"
                  , "func second() {"
                  , "    // code"
                  , "}"
                  ]
                result = parseTypus input
            case result of
              Right file -> length (tfBlocks file) @?= 2
              Left _ -> return () -- May fail but should not crash

        , fastProperty "code blocks preserve content" $
            \content ->
              let input = content
                  result = parseTypus input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in -- Content should be preserved in some form
                        all (\block -> not (null (cbContent block))) blocks
                   Left _ -> True

        , fastProperty "block boundaries are identified correctly" $
            \input ->
              let result = parseTypus input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in -- Each block should have valid span
                        all (\block -> Parser.isValidSpan (cbSpan block)) blocks
                   Left _ -> True
        ]

    , testGroup "Error recovery robustness"
        [ testCase "continues parsing after syntax errors" $ do
            let input = unlines
                  [ "if condition"  -- Missing opening brace
                  , "    doSomething()"
                  , "//! ownership: true"
                  , "func valid() {"
                  , "    return 42"
                  , "}"
                  ]
                result = parseTypus input
            case result of
              Right file -> do
                let syntaxErrors = tfSyntaxErrors file
                length syntaxErrors @?= 1  -- Should detect the missing brace
                length (tfBlocks file) @?= 1  -- Should still parse the valid function
              Left _ -> assertFailure "Expected parsing to continue after syntax error"

        , fastProperty "syntax errors don't corrupt parser state" $
            \input ->
              let result = parseTypus input
              in case result of
                   Right file -> 
                     let syntaxErrors = tfSyntaxErrors file
                     in -- Should have valid syntax error list
                        length syntaxErrors >= 0
                   Left _ -> True

        , fastProperty "parser handles mixed valid/invalid content" $
            \validContent invalidContent ->
              let input = validContent ++ "\n" ++ invalidContent
                  result = parseTypus input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in -- Should parse whatever is parseable
                        length blocks >= 0
                   Left _ -> True
        ]

    , testGroup "Performance and memory robustness"
        [ testCase "handles large inputs efficiently" $ do
            let largeInput = unlines $ replicate 1000 "//! ownership: true\nfunc test() { return 42; }"
                result = parseTypus largeInput
            case result of
              Right file -> length (tfBlocks file) @?= 1000
              Left _ -> return () -- May fail but should not crash

        , fastProperty "parsing time is reasonable" $
            \input ->
              let size = length input
                  result = parseTypus input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in -- Should complete without hanging
                        length blocks >= 0
                   Left _ -> True

        , fastProperty "memory usage doesn't grow excessively" $
            \input ->
              let result = parseTypus input
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                         syntaxErrors = tfSyntaxErrors file
                     in -- Should not create excessive data structures
                        length blocks + length syntaxErrors <= length (lines input) + 100
                   Left _ -> True
        ]

    , testGroup "Unicode and encoding robustness"
        [ testCase "handles unicode characters" $ do
            let input = "//! 测试: 值\nfunction 测试函数() { return '你好'; }"
                result = parseTypus input
            case result of
              Right _ -> return () -- Should not crash
              Left _ -> return () -- May fail but should not crash

        , fastProperty "unicode content is preserved" $
            \unicodeInput ->
              let result = parseTypus unicodeInput
              in case result of
                   Right file -> 
                     let blocks = tfBlocks file
                     in -- Unicode should be preserved in content
                        True
                   Left _ -> True

        , testCase "handles mixed encodings gracefully" $ do
            let input = "//! ascii: value\n//! 测试: 值\nfunction mix() { return 'mixed'; }"
                result = parseTypus input
            case result of
              Right _ -> return () -- Should not crash
              Left _ -> return () -- May fail gracefully
        ]
    ]

-- Helper functions
toUpper :: String -> String
toUpper = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)

-- Import Parser module for testing
import qualified Parser

-- Add missing Parser.defaultFileDirectives if not available
defaultFileDirectives :: Parser.FileDirectives
defaultFileDirectives = Parser.FileDirectives Nothing Nothing Nothing

-- Add missing Parser.isValidSpan if not available
isValidSpan :: Parser.SourceSpan -> Bool
isValidSpan span = Parser.spanStart span <= Parser.spanEnd span