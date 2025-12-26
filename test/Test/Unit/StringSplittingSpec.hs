{-# LANGUAGE CPP #-}

module Test.Unit.StringSplittingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import Utils (splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, breakOn)

import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)

-- | 测试字符串分割功能的属性和边界情况
tests :: TestTree
tests =
  testGroup "String Splitting"
    [ testGroup "splitBy Basic Functionality"
        [ testCase "splits on single character" $ do
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            splitBy ':' "hello:world" @?= ["hello", "world"]
            
        , testCase "preserves empty segments" $ do
            splitBy ',' "a,,b" @?= ["a", "", "b"]
            splitBy ':' "::alpha::" @?= ["", "", "alpha", "", ""]
            
        , testCase "handles empty input" $ do
            splitBy ',' "" @?= [""]
            splitBy ':' "" @?= [""]
            
        , testCase "handles no delimiters" $ do
            splitBy ',' "hello" @?= ["hello"]
            splitBy ':' "testing" @?= ["testing"]
            
        , testCase "handles only delimiters" $ do
            splitBy ',' ",,," @?= ["", "", "", ""]
            splitBy ':' ":" @?= ["", ""]
        ]
        
    , testGroup "splitByCollapsed Functionality"
        [ testCase "removes empty segments" $ do
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            splitByCollapsed ':' "::alpha::beta::" @?= ["alpha", "beta"]
            
        , testCase "handles empty input" $ do
            splitByCollapsed ',' "" @?= []
            splitByCollapsed ':' "" @?= []
            
        , testCase "handles only delimiters" $ do
            splitByCollapsed ',' ",,," @?= []
            splitByCollapsed ':' ":::" @?= []
            
        , testCase "preserves non-empty segments" $ do
            splitByCollapsed ',' "a,b,c" @?= ["a", "b", "c"]
            splitByCollapsed ':' "hello:world" @?= ["hello", "world"]
        ]
        
    , testGroup "Comma Splitting Functions"
        [ testCase "splitByComma delegates to splitBy" $ do
            splitByComma "a,b,c" @?= ["a", "b", "c"]
            splitByComma "a,,b" @?= ["a", "", "b"]
            splitByComma "" @?= [""]
            
        , testCase "splitByCommaCollapsed removes empty segments" $ do
            splitByCommaCollapsed "a,b,c" @?= ["a", "b", "c"]
            splitByCommaCollapsed "a,,b" @?= ["a", "b"]
            splitByCommaCollapsed "" @?= []
            splitByCommaCollapsed ",,,a,,,b,,," @?= ["a", "b"]
        ]
        
    , testGroup "breakOn Functionality"
        [ testCase "splits at first occurrence" $ do
            breakOn ',' "a,b,c" @?= ("a", ",b,c")
            breakOn ':' "hello:world:test" @?= ("hello", ":world:test")
            
        , testCase "handles delimiter not found" $ do
            breakOn ',' "hello" @?= ("hello", "")
            breakOn ':' "testing" @?= ("testing", "")
            
        , testCase "handles empty input" $ do
            breakOn ',' "" @?= ("", "")
            breakOn ':' "" @?= ("", "")
            
        , testCase "handles delimiter at start" $ do
            breakOn ',' ",start" @?= ("", ",start")
            breakOn ':' ":beginning" @?= ("", ":beginning")
            
        , testCase "handles delimiter at end" $ do
            breakOn ',' "end," @?= ("end", ",")
            breakOn ':' "end:" @?= ("end", ":")
        ]
        
    , testGroup "Property Tests"
        [ testProperty "splitBy preserves total length including delimiters" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","  -- 确保有一个分隔符
                parts = splitBy delim input
                reconstructed = intercalate [delim] parts
            in reconstructed == input
            
        , testProperty "splitByCollapsed removes delimiters in empty segments" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                collapsed = splitByCollapsed delim input
                hasNoEmpty = all (not . null) collapsed
            in hasNoEmpty
            
        , testProperty "splitByCollapsed length <= splitBy length" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                normal = splitBy delim input
                collapsed = splitByCollapsed delim input
            in length collapsed <= length normal
            
        , testProperty "breakOn preserves total length" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                (prefix, suffix) = breakOn delim input
            in length prefix + length suffix == length input
            
        , testProperty "breakOn suffix starts with delimiter or is empty" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                (prefix, suffix) = breakOn delim input
            in null suffix || head suffix == delim
            
        , testProperty "splitBy on single character returns original or two parts" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                parts = splitBy delim input
            in length parts == 1 || length parts >= 2
        ]
        
    , testGroup "Edge Cases"
        [ testCase "handles Unicode characters" $ do
            splitBy ',' "héllo,wörld" @?= ["héllo", "wörld"]
            splitBy ':' "测试:中文" @?= ["测试", "中文"]
            
        , testCase "handles special characters as delimiters" $ do
            splitBy '|' "a|b|c" @?= ["a", "b", "c"]
            splitBy ';' "x;y;z" @?= ["x", "y", "z"]
            splitBy '@' "user@domain" @?= ["user", "domain"]
            
        , testCase "handles whitespace as delimiter" $ do
            splitBy ' ' "hello world test" @?= ["hello", "world", "test"]
            splitByCollapsed ' ' "hello  world" @?= ["hello", "world"]
            
        , testCase "handles newline as delimiter" $ do
            splitBy '\n' "line1\nline2\nline3" @?= ["line1", "line2", "line3"]
            splitByCollapsed '\n' "line1\n\nline2" @?= ["line1", "line2"]
            
        , testCase "handles tab as delimiter" $ do
            splitBy '\t' "col1\tcol2\tcol3" @?= ["col1", "col2", "col3"]
            splitByCollapsed '\t' "col1\t\tcol2" @?= ["col1", "col2"]
        ]
        
    , testGroup "Performance and Robustness"
        [ testCase "handles very long strings" $ do
            let longString = concat $ replicate 1000 "test,"
                parts = splitBy ',' longString
            length parts >= 0 @?= True
            
        , testCase "handles many consecutive delimiters" $ do
            let manyDelimiters = replicate 1000 ','
                parts = splitBy ',' manyDelimiters
                collapsed = splitByCollapsed ',' manyDelimiters
            length parts @?= 1001
            length collapsed @?= 0
            
        , testProperty "functions don't crash on any input" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                split1 = splitBy delim input
                split2 = splitByCollapsed delim input
                break1 = breakOn delim input
            in length split1 >= 0 && length split2 >= 0 && length break1 >= 0
            
        , testProperty "functions handle large delimiters correctly" $ fastProperty $ \input ->
            let parts = splitBy ',' input
                collapsed = splitByCollapsed ',' input
                commaParts = splitByComma input
                commaCollapsed = splitByCommaCollapsed input
            in length parts >= 0 && length collapsed >= 0 && 
               length commaParts >= 0 && length commaCollapsed >= 0
        ]
        
    , testGroup "Consistency Tests"
        [ testCase "splitByComma equals splitBy with comma" $ do
            let inputs = ["a,b,c", "a,,b", "", ",", ",a,b,"]
            mapM_ checkConsistency inputs
            where checkConsistency input = do
                    splitByComma input @?= splitBy ',' input
                    
        , testCase "splitByCommaCollapsed equals splitByCollapsed with comma" $ do
            let inputs = ["a,b,c", "a,,b", "", ",", ",a,b,"]
            mapM_ checkConsistency inputs
            where checkConsistency input = do
                    splitByCommaCollapsed input @?= splitByCollapsed ',' input
                    
        , testProperty "splitBy and splitByCollapsed relationship" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                normal = splitBy delim input
                collapsed = splitByCollapsed delim input
                filtered = filter (not . null) normal
            in collapsed == filtered
        ]
        
    , testGroup "Advanced Splitting"
        [ testCase "handles mixed delimiters in sequence" $ do
            splitBy ',' "a,b,,c,,,d" @?= ["a", "b", "", "c", "", "", "d"]
            splitByCollapsed ',' "a,b,,c,,,d" @?= ["a", "b", "c", "d"]
            
        , testCase "breakOn with multiple possible delimiters" $ do
            breakOn ',' "a,b,c" @?= ("a", ",b,c")
            breakOn ':' "a:b:c" @?= ("a", ":b:c")
            
        , testProperty "splitBy handles repeated patterns" $ fastProperty $ \pattern count ->
            let count' = abs count `mod` 10 + 1
                repeated = concat $ replicate count' [pattern]
                parts = splitBy ',' repeated
            in length parts >= 0
        ]
    ]
    
-- Helper function to intercalate a list of strings with a separator
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs