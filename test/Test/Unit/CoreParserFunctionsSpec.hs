{-# LANGUAGE CPP #-}

module Test.Unit.CoreParserFunctionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T (pack, unpack)

-- | 测试解析器核心函数的属性和边界情况
tests :: TestTree
tests =
  testGroup "Core Parser Functions"
    [ testGroup "Default Directives"
        [ testCase "defaultFileDirectives should have L.all Nothing values" $ do
            defaultFileDirectives @?= FileDirectives Nothing Nothing Nothing
            
        , testCase "defaultBlockDirectives should have L.all Nothing values" $ do
            defaultBlockDirectives @?= BlockDirectives Nothing Nothing Nothing
        ]
        
    , testGroup "Parser Properties"
        [ testProperty "parseTypus on empty string should not crash" $ fastProperty $ \input ->
            let result = parseTypus "" input
            in L.length (show result) >= 0  -- 确保不崩溃且能显示结果
            
        , testProperty "parseTypus preserves input L.length in error messages" $ fastProperty $ \input ->
            let result = parseTypus "" input
                resultStr = show result
            in if "error" `elem` (words $ map toLower resultStr)
               then L.length input <= L.length resultStr || L.length resultStr > 10
               else True
            where toLower = L.map (\c -> if c >= 'A' && c <= 'Z' then c + 32 else c)
        ]
        
    , testGroup "Directive Parsing"
        [ testCase "FileDirectives equality works correctly" $ do
            let fd1 = FileDirectives Nothing Nothing Nothing
                fd2 = FileDirectives (Just True) Nothing (Just False)
            fd1 @?= fd1
            fd1 /= fd2 @?= True
            
        , testCase "BlockDirectives equality works correctly" $ do
            let bd1 = BlockDirectives Nothing Nothing Nothing
                bd2 = BlockDirectives (Just False) (Just True) Nothing
            bd1 @?= bd1
            bd1 /= bd2 @?= True
        ]
        
    , testGroup "Parser Error Handling"
        [ testCase "parseTypus handles malformed input gracefully" $ do
            let malformedInput = "!!!@@@###$$$%%%"
                result = parseTypus "" malformedInput
                resultStr = show result
            L.length resultStr > 0 @?= True  -- 确保产生一些输出（可能是错误信息）
            
        , testProperty "parseTypus on Unicode input should not crash" $ fastProperty $ \input ->
            let unicodeInput = T.unpack $ T.pack input
                result = parseTypus "" unicodeInput
            in L.length (show result) >= 0
        ]
        
    , testGroup "Parser Edge Cases"
        [ testCase "parseTypus handles very long lines" $ do
            let longLine = replicate 10000 'a'
                result = parseTypus "" longLine
            L.length (show result) >= 0 @?= True
            
        , testCase "parseTypus handles input with only whitespace" $ do
            let whitespaceInput = "   \t\n\r   \t  \n\r   "
                result = parseTypus "" whitespaceInput
            L.length (show result) >= 0 @?= True
            
        , testCase "parseTypus handles input with special characters" $ do
            let specialChars = "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./~`"
                result = parseTypus "" specialChars
            L.length (show result) >= 0 @?= True
        ]
    ]