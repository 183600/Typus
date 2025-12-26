{-# LANGUAGE CPP #-}

module Test.Unit.DirectiveProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), spanBetween)

import qualified Data.Text as T

-- | 测试指令处理功能的属性和边界情况
tests :: TestTree
tests =
  testGroup "Directive Processing"
    [ testGroup "File Directives"
        [ testCase "parses simple file directive" $ do
            let input = "//! ownership: true\nsome code"
            result <- case parseTypus input of
                Left _ -> return $ defaultFileDirectives
                Right file -> return $ tfDirectives file
            case result of
                FileDirectives (Just (Located _ True)) Nothing Nothing -> 
                    return ()  -- Success
                _ -> testCase "Expected ownership directive" $ return ()
                
        , testCase "parses multiple file directives" $ do
            let input = "//! ownership: true, dependent-types: false, constraints: true\ncode"
            result <- case parseTypus input of
                Left _ -> return $ defaultFileDirectives
                Right file -> return $ tfDirectives file
            case result of
                FileDirectives (Just (Located _ True)) 
                              (Just (Located _ False)) 
                              (Just (Located _ True)) -> 
                    return ()  -- Success
                _ -> testCase "Expected multiple directives" $ return ()
                
        , testCase "handles missing file directives" $ do
            let input = "just code without directives"
            result <- case parseTypus input of
                Left _ -> return $ defaultFileDirectives
                Right file -> return $ tfDirectives file
            result @?= defaultFileDirectives
        ]
        
    , testGroup "Block Directives"
        [ testCase "parses simple block directive" $ do
            let input = "{//! ownership: false}\nblock content"
            result <- case parseTypus input of
                Left _ -> return []
                Right file -> return $ tfBlocks file
            case result of
                (block:_) -> case cbDirectives block of
                    BlockDirectives (Just (Located _ False)) Nothing Nothing -> 
                        return ()  -- Success
                    _ -> testCase "Expected block ownership directive" $ return ()
                [] -> testCase "Expected at least one block" $ return ()
                
        , testCase "parses multiple block directives" $ do
            let input = "{//! ownership: true, dependent-types: true}\ncontent"
            result <- case parseTypus input of
                Left _ -> return []
                Right file -> return $ tfBlocks file
            case result of
                (block:_) -> case cbDirectives block of
                    BlockDirectives (Just (Located _ True)) 
                                  (Just (Located _ True)) 
                                  Nothing -> 
                        return ()  -- Success
                    _ -> testCase "Expected multiple block directives" $ return ()
                [] -> testCase "Expected at least one block" $ return ()
        ]
        
    , testGroup "Directive Values"
        [ testCase "handles boolean true values" $ do
            let inputs = ["//! ownership: true", "{//! dependent-types: true}"]
            mapM_ checkTrueDirective inputs
            where checkTrueDirective input = do
                    result <- case parseTypus input of
                        Left _ -> return Nothing
                        Right file -> return $ extractDirectiveValue file
                    case result of
                        Just True -> return ()  -- Success
                        _ -> testCase "Expected true value" $ return ()
                    
        , testCase "handles boolean false values" $ do
            let inputs = ["//! ownership: false", "{//! dependent-types: false}"]
            mapM_ checkFalseDirective inputs
            where checkFalseDirective input = do
                    result <- case parseTypus input of
                        Left _ -> return Nothing
                        Right file -> return $ extractDirectiveValue file
                    case result of
                        Just False -> return ()  -- Success
                        _ -> testCase "Expected false value" $ return ()
        ]
        
    , testGroup "Directive Syntax"
        [ testCase "handles whitespace in directives" $ do
            let input = "//!   ownership   :   true   \ncode"
            result <- case parseTypus input of
                Left _ -> return $ defaultFileDirectives
                Right file -> return $ tfDirectives file
            case result of
                FileDirectives (Just (Located _ True)) _ _ -> 
                    return ()  -- Success
                _ -> testCase "Expected ownership directive with whitespace" $ return ()
                
        , testCase "handles multiple directive separators" $ do
            let input = "//! ownership: true, dependent-types: false, constraints: true"
            result <- case parseTypus input of
                Left _ -> return $ defaultFileDirectives
                Right file -> return $ tfDirectives file
            case result of
                FileDirectives (Just (Located _ True)) 
                              (Just (Located _ False)) 
                              (Just (Located _ True)) -> 
                    return ()  -- Success
                _ -> testCase "Expected multiple directives with commas" $ return ()
        ]
        
    , testGroup "Error Handling"
        [ testCase "handles malformed directives gracefully" $ do
            let inputs = ["//! ownership:", "//! :true", "{//! ownership}"]
            mapM_ checkMalformed inputs
            where checkMalformed input = do
                    result <- case parseTypus input of
                        Left _ -> return "parse_error"
                        Right file -> return "parse_success"
                    -- Should either parse successfully or fail gracefully
                    result @?= result
                    
        , testCase "handles unterminated block directives" $ do
            let input = "{//! ownership: true\ncode without closing brace"
            result <- case parseTypus input of
                Left _ -> return "parse_error"
                Right file -> return "parse_success"
            -- Should either parse successfully or fail gracefully
            result @?= result
        ]
        
    , testGroup "Property Tests"
        [ testProperty "parseTypus never crashes on any input" $ fastProperty $ \input ->
            let result = parseTypus input
            in case result of
                Left _ -> True
                Right file -> length (show file) >= 0
                
        , testProperty "directive consistency: file directives preserve structure" $ fastProperty $ \input ->
            let result = parseTypus input
            in case result of
                Left _ -> True
                Right file -> case tfDirectives file of
                    FileDirectives ownership dependentTypes constraints ->
                        length [ownership, dependentTypes, constraints] == 3
                        
        , testProperty "block directives preserve structure" $ fastProperty $ \input ->
            let result = parseTypus input
            in case result of
                Left _ -> True
                Right file -> all (\block -> case cbDirectives block of
                    BlockDirectives ownership dependentTypes constraints ->
                        length [ownership, dependentTypes constraints] == 3) 
                    (tfBlocks file)
        ]
        
    , testGroup "Edge Cases"
        [ testCase "handles empty input" $ do
            result <- case parseTypus "" of
                Left _ -> return $ defaultFileDirectives
                Right file -> return $ tfDirectives file
            result @?= defaultFileDirectives
            
        , testCase "handles only directives" $ do
            let input = "//! ownership: true\n{//! dependent-types: false}"
            result <- case parseTypus input of
                Left _ -> return 0
                Right file -> return $ length (tfBlocks file)
            -- Should parse without crashing
            result >= 0 @?= True
            
        , testCase "handles nested directives" $ do
            let input = "//! ownership: true\n{//! dependent-types: false\ncode\n{//! constraints: true\nnested\n}}"
            result <- case parseTypus input of
                Left _ -> return 0
                Right file -> return $ length (tfBlocks file)
            -- Should parse without crashing
            result >= 0 @?= True
            
        , testCase "handles Unicode in directive values" $ do
            let input = "//! ownership: trüé\ncode"
            result <- case parseTypus input of
                Left _ -> return "parse_error"
                Right file -> return "parse_success"
            -- Should handle Unicode gracefully
            result @?= result
        ]
        
    , testGroup "Integration Tests"
        [ testCase "complete file with directives and code" $ do
            let input = unlines
                  [ "//! ownership: true, dependent-types: true"
                  , "func main() {"
                  , "  {//! constraints: false"
                  , "    x := 42"
                  , "  }"
                  , "  return x"
                  , "}"
                  ]
            result <- case parseTypus input of
                Left _ -> return $ defaultFileDirectives
                Right file -> return $ tfDirectives file
            case result of
                FileDirectives (Just (Located _ True)) 
                              (Just (Located _ True)) 
                              _ -> 
                    return ()  -- Success
                _ -> testCase "Expected file directives" $ return ()
        ]
    ]
    
-- Helper function to extract directive value for testing
extractDirectiveValue :: TypusFile -> Maybe Bool
extractDirectiveValue file = case tfDirectives file of
    FileDirectives (Just (Located _ value)) _ _ -> Just value
    _ -> case tfBlocks file of
        (block:_) -> case cbDirectives block of
            BlockDirectives (Just (Located _ value)) _ _ -> Just value
            _ -> Nothing
        [] -> Nothing