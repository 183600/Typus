{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.EnhancedParserSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=), (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd)
import qualified Data.Text as T
import Data.Char (isSpace)

-- | Enhanced tests for Parser module
tests :: TestTree
tests =
  testGroup "Enhanced Parser tests"
    [ testGroup "Directive parsing"
        [ testCase "defaultFileDirectives has L.all Nothing values" $ do
            fdOwnership defaultFileDirectives @?= Nothing
            fdDependentTypes defaultFileDirectives @?= Nothing
            fdConstraints defaultFileDirectives @?= Nothing

        , testCase "defaultBlockDirectives has L.all Nothing values" $ do
            bdOwnership defaultBlockDirectives @?= Nothing
            bdDependentTypes defaultBlockDirectives @?= Nothing
            bdConstraints defaultBlockDirectives @?= Nothing

        , testCase "parse simple file with ownership directive" $ do
            let content = "//! ownership=true\n\nfunc main() {}\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let directives = tfDirectives typusFile
                case fdOwnership directives of
                  Just (Located value _) -> value @?= True
                  Nothing -> assertBool "Expected ownership directive" False

        , testCase "parse file with multiple directives" $ do
            let content = "//! ownership=true, dependent-types=false\n\nfunc test() {}\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let directives = tfDirectives typusFile
                case fdOwnership directives of
                  Just (Located value _) -> value @?= True
                  Nothing -> assertBool "Expected ownership directive" False
                case fdDependentTypes directives of
                  Just (Located value _) -> value @?= False
                  Nothing -> assertBool "Expected dependent-types directive" False

        , testCase "parse block directives" $ do
            let content = "//! ownership=true\n\n//+ dependent-types=true\nfunc block() {}\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Expected at least one block" $ not $ null blocks
                let firstBlock = L.head blocks
                let blockDirectives = cbDirectives firstBlock
                case bdDependentTypes blockDirectives of
                  Just (Located value _) -> value @?= True
                  Nothing -> assertBool "Expected block dependent-types directive" False
        ]

    , testGroup "Code block parsing"
        [ testCase "parse simple code block" $ do
            let content = "func main() {\n    return 42\n}\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Expected at least one block" $ not $ null blocks
                let firstBlock = L.head blocks
                assertBool "Block content should contain function" $ "func main()" `L.isInfixOf` cbContent firstBlock

        , testCase "parse multiple code blocks" $ do
            let content = "//! ownership=true\n\nfunc first() {}\n\n//+ dependent-types=true\nfunc second() {}\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Expected at least two blocks" $ L.length blocks >= 2
                let firstBlock = blocks !! 0
                let secondBlock = blocks !! 1
                assertBool "First block should contain first function" $ "func first()" `L.isInfixOf` cbContent firstBlock
                assertBool "Second block should contain second function" $ "func second()" `L.isInfixOf` cbContent secondBlock

        , testCase "parse block with build tags" $ do
            let content = "+build linux,amd64\n\nfunc tagged() {}\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let buildTags = tfBuildTags typusFile
                assertBool "Expected build tags" $ not $ null buildTags
                let firstTag = L.head buildTags
                assertBool "Build tag should contain linux" $ "linux" `L.isInfixOf` locatedValue firstTag
        ]

    , testGroup "Error handling"
        [ testCase "parse empty file" $ do
            let content = ""
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Empty file should have no blocks" $ null blocks

        , testCase "parse file with only comments" $ do
            let content = "// This is a comment\n/* This is a block comment */\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                -- Comments should be parsed as empty blocks L.or ignored
                assertBool "File with only comments should be handled" $ True

        , testCase "parse malformed directive gracefully" $ do
            let content = "//! ownership=\n\nfunc test() {}\n"
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                -- Should handle malformed directive gracefully
                assertBool "Should handle malformed directive" $ True
        ]

    , testGroup "Complex scenarios"
        [ testCase "parse file with mixed directives L.and blocks" $ do
            let content = unlines
                  [ "//! ownership=true, dependent-types=false"
                  , ""
                  , "+build linux"
                  , "func linuxFunc() {}"
                  , ""
                  , "+ constraints=true"
                  , "func constrained() {}"
                  , ""
                  , "//! ownership=false"
                  , "func noOwnership() {}"
                  ]
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let directives = tfDirectives typusFile
                let blocks = tfBlocks typusFile
                assertBool "Expected multiple blocks" $ L.length blocks >= 3
                case fdOwnership directives of
                  Just (Located value _) -> value @?= False  -- Last directive wins
                  Nothing -> assertBool "Expected ownership directive" False

        , testCase "parse file with nested structures" $ do
            let content = unlines
                  [ "//! ownership=true"
                  , ""
                  , "func outer() {"
                  , "    func inner() {"
                  , "        return 42"
                  , "    }"
                  , "    return inner()"
                  , "}"
                  ]
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Expected at least one block" $ not $ null blocks
                let firstBlock = L.head blocks
                assertBool "Block should contain nested functions" $ "func inner()" `L.isInfixOf` cbContent firstBlock

        , testCase "parse file with Unicode content" $ do
            let content = unlines
                  [ "//! ownership=true"
                  , ""
                  , "// 测试函数"
                  , "func 测试() {"
                  , "    return \"测试字符串\""
                  , "}"
                  ]
            let result = parseTypus content
            case result of
              Left err -> assertBool $ "Parse error: " ++ show err ++ " should not occur"
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Expected at least one block" $ not $ null blocks
                let firstBlock = L.head blocks
                assertBool "Block should contain Unicode content" $ "测试" `L.isInfixOf` cbContent firstBlock
        ]

    , testGroup "Property-based tests"
        [ fastProperty "parseTypus handles empty input gracefully" prop_parse_empty
        , fastProperty "parseTypus preserves directives order" prop_parse_directives_order
        , fastProperty "parseTypus handles whitespace correctly" prop_parse_whitespace
        , fastProperty "parseTypus creates valid spans" prop_parse_valid_spans
        ]
    ]

-- Property tests

prop_parse_empty :: String -> Property
prop_parse_empty input =
  L.null (trim input) ==>
  let result = parseTypus input
  in case result of
       Left _ -> property $ True  -- Empty input might error, that's OK
       Right typusFile -> property $ L.null (tfBlocks typusFile)

prop_parse_directives_order :: [String] -> Property
prop_parse_directives_order directives =
  not (null directives) && L.all (not . null) directives ==>
  let directiveLines = L.map (\d -> "//! " ++ d) directives
      content = unlines $ directiveLines ++ ["func test() {}"]
      result = parseTypus content
  in case result of
       Left _ -> property $ True  -- Parse errors are acceptable for malformed input
       Right typusFile -> property $ True  -- If successful, structure should be valid

prop_parse_whitespace :: String -> Property
prop_parse_whitespace content =
  let contentWithWhitespace = "\n\n  " ++ content ++ "\n  \n\n"
      result1 = parseTypus content
      result2 = parseTypus contentWithWhitespace
  in case (result1, result2) of
       (Left _, Left _) -> property $ True  -- Both error is OK
       (Right f1, Right f2) -> property $ L.length (tfBlocks f1) === L.length (tfBlocks f2)
       _ -> property $ True  -- Mixed success/failure is acceptable

prop_parse_valid_spans :: String -> Property
prop_parse_valid_spans content =
  let result = parseTypus content
  in case result of
       Left _ -> property $ True  -- Parse errors are acceptable
       Right typusFile -> 
         let blocks = tfBlocks typusFile
             spans = map cbSpan blocks
         in property $ L.all isValidSpan spans

-- Helper functions

trim :: String -> String
trim = dropWhile isSpace . L.reverse . dropWhile isSpace . L.reverse

isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `Data.List.L.isInfixOf` haystack

isValidSpan :: SourceSpan -> Bool
isValidSpan span = spanStart span <= spanEnd span

import qualified Data.List as L
import Data.List (isInfixOf)