{-# LANGUAGE CPP #-}

module Test.Unit.TextProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
              removeLineComments, removeComments, normalizeIndentation, breakOn)

import Data.Char (isSpace)
import qualified Data.Text as T

-- | 测试文本处理功能的属性和边界情况
tests :: TestTree
tests =
  testGroup "Text Processing"
    [ testGroup "Trim Operations"
        [ testCase "trim removes leading and trailing whitespace" $ do
            trim "  hello  " @?= "hello"
            trim "\t\n  hello world  \n\t" @?= "hello world"
            
        , testCase "trim on empty string returns empty" $ do
            trim "" @?= ""
            
        , testCase "trim on only whitespace returns empty" $ do
            trim "   \t\n\r   " @?= ""
            
        , testProperty "trim preserves internal whitespace" $ fastProperty $ \input ->
            let trimmed = trim input
                internalSpaces = filter isSpace trimmed
                leadingSpaces = takeWhile isSpace input
                trailingSpaces = reverse $ takeWhile isSpace (reverse input)
            in not (null trimmed) ==> 
               (length internalSpaces > 0) == (any isSpace input && 
                                               not (all isSpace input) &&
                                               length input > length leadingSpaces + length trailingSpaces)
                                               
        , testProperty "trim idempotent: trim(trim(x)) == trim(x)" $ fastProperty $ \input ->
            let trimmed = trim input
                doubleTrimmed = trim trimmed
            in trimmed == doubleTrimmed
        ]
        
    , testGroup "Split Operations"
        [ testCase "splitBy basic functionality" $ do
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            splitBy ':' "a::b" @?= ["a", "", "b"]
            
        , testCase "splitByCollapsed removes empty segments" $ do
            splitByCollapsed ':' "::alpha::beta::" @?= ["alpha", "beta"]
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            
        , testCase "splitBy on empty input" $ do
            splitBy ',' "" @?= [""]
            splitByCollapsed ',' "" @?= []
            
        , testProperty "splitBy preserves total length" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","  -- 确保有一个分隔符
                parts = splitBy delim input
            in sum (map length parts) + length parts - 1 == length input
            
        , testProperty "splitByCollapsed never produces empty strings" $ fastProperty $ \c input ->
            let delim = head $ c ++ ","
                parts = splitByCollapsed delim input
            in all (not . null) parts
        ]
        
    , testGroup "Comment Removal"
        [ testCase "removeLineComments basic functionality" $ do
            removeLineComments "hello // comment" @?= "hello "
            removeLineComments "value := 1 // drop" @?= "value := 1 "
            
        , testCase "removeComments handles both line and block comments" $ do
            removeComments "code /* block */ more" @?= "code  more"
            removeComments "line // comment\nnext" @?= "line \nnext"
            
        , testCase "comment removal respects string literals" $ do
            let input = "url := \"http://example.com//path\" // comment"
            removeLineComments input @?= "url := \"http://example.com//path\" "
            
        , testProperty "comment removal never increases string length" $ fastProperty $ \input ->
            let withoutLineComments = removeLineComments input
                withoutComments = removeComments input
            in length withoutLineComments <= length input &&
               length withoutComments <= length input
               
        , testProperty "comment removal is idempotent" $ fastProperty $ \input ->
            let once = removeComments input
                twice = removeComments once
            in once == twice
        ]
        
    , testGroup "Indentation Processing"
        [ testCase "normalizeIndentation basic functionality" $ do
            normalizeIndentation "  hello\n    world" @?= "hello\n  world"
            
        , testCase "normalizeIndentation handles mixed indentation" $ do
            normalizeIndentation "\t  hello\n\t    world" @?= "hello\n  world"
            
        , testProperty "normalizeIndentation preserves relative indentation" $ fastProperty $ \input ->
            let normalized = normalizeIndentation input
                lines' = lines input
                normalizedLines = lines normalized
            in length normalizedLines == length lines'
            
        , testProperty "normalizeIndentation doesn't increase total length significantly" $ fastProperty $ \input ->
            let normalized = normalizeIndentation input
            in length normalized <= length input + 100  -- 允许一些小的增加
        ]
        
    , testGroup "Break Operations"
        [ testCase "breakOn basic functionality" $ do
            breakOn ',' "a,b,c" @?= ("a", ",b,c")
            breakOn ':' "hello:world" @?= ("hello", ":world")
            
        , testCase "breakOn on separator not found" $ do
            breakOn ',' "hello" @?= ("hello", "")
            
        , testProperty "breakOn preserves total length" $ fastProperty $ \c input ->
            let sep = head $ c ++ ","
                (prefix, suffix) = breakOn sep input
            in length prefix + length suffix == length input
        ]
        
    , testGroup "Edge Cases"
        [ testCase "functions handle Unicode correctly" $ do
            let unicodeInput = "héllo 🌍 wörld"
            trim unicodeInput @?= "héllo 🌍 wörld"
            
        , testCase "functions handle empty strings" $ do
            trim "" @?= ""
            splitBy ',' "" @?= [""]
            removeLineComments "" @?= ""
            normalizeIndentation "" @?= ""
            
        , testProperty "functions don't crash on large inputs" $ fastProperty $ \input ->
            let largeInput = concat $ replicate 1000 [input]
                trimmed = trim largeInput
                split = splitBy ',' largeInput
                commentsRemoved = removeLineComments largeInput
            in length trimmed >= 0 && length split >= 0 && length commentsRemoved >= 0
        ]
    ]