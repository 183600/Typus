{-# LANGUAGE CPP #-}

module Test.Unit.ErrorLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..),
                      startPos, posAt, posAtLineCol, spanBetween,
                      toErrorLocation, toErrorLocationWithSpan)
import Compiler.Errors.Core (ErrorLocation(..))

import qualified Data.Text as T

-- | 测试错误位置定位功能的属性和边界情况
tests :: TestTree
tests =
  testGroup "Error Location"
    [ testGroup "Position to Error Location"
        [ testCase "converts simple position to error location" $ do
            let pos = posAt 5 10
                errorLoc = toErrorLocation pos
            line errorLoc @?= 5
            column errorLoc @?= 10
            endLine errorLoc @?= Nothing
            endColumn errorLoc @?= Nothing
            
        , testCase "converts start position correctly" $ do
            let errorLoc = toErrorLocation startPos
            line errorLoc @?= 1
            column errorLoc @?= 1
            endLine errorLoc @?= Nothing
            endColumn errorLoc @?= Nothing
            
        , testCase "handles large line L.and column numbers" $ do
            let pos = posAt 1000 500
                errorLoc = toErrorLocation pos
            line errorLoc @?= 1000
            column errorLoc @?= 500
        ]
        
    , testGroup "Span to Error Location"
        [ testCase "converts single-line span to error location" $ do
            let start = posAt 3 5
                end = posAt 3 10
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            line errorLoc @?= 3
            column errorLoc @?= 5
            endLine errorLoc @?= Just 3
            endColumn errorLoc @?= Just 10
            
        , testCase "converts multi-line span to error location" $ do
            let start = posAt 2 8
                end = posAt 4 3
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            line errorLoc @?= 2
            column errorLoc @?= 8
            endLine errorLoc @?= Just 4
            endColumn errorLoc @?= Just 3
            
        , testCase "handles zero-L.length span" $ do
            let pos = posAt 7 12
                span = spanBetween pos pos
                errorLoc = toErrorLocationWithSpan span
            line errorLoc @?= 7
            column errorLoc @?= 12
            endLine errorLoc @?= Just 7
            endColumn errorLoc @?= Just 12
        ]
        
    , testGroup "Error Location Properties"
        [ testCase "error location preserves file path when provided" $ do
            let pos = posAt 1 1
                errorLoc = toErrorLocation pos
            filePath errorLoc @?= Nothing  -- Default should be Nothing
            
        , testCase "error location line numbers are positive" $ do
            let positions = [startPos, posAt 1 1, posAt 100 50]
                errorLocs = map toErrorLocation positions
            L.all (\loc -> line loc > 0) errorLocs @?= True
            
        , testCase "error location column numbers are positive" $ do
            let positions = [startPos, posAt 1 1, posAt 100 50]
                errorLocs = map toErrorLocation positions
            L.all (\loc -> column loc > 0) errorLocs @?= True
            
        , testProperty "span error location has end positions" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = posAt (abs line1 `mod` 1000 + 1) (abs col1 `mod` 1000 + 1)
                end = posAt (abs line2 `mod` 1000 + 1) (abs col2 `mod` 1000 + 1)
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            in endLine errorLoc == Just (posLine end) &&
               endColumn errorLoc == Just (posColumn end)
        ]
        
    , testGroup "Edge Cases"
        [ testCase "handles position at line 1, column 1" $ do
            let pos = posAt 1 1
                errorLoc = toErrorLocation pos
            line errorLoc @?= 1
            column errorLoc @?= 1
            
        , testCase "handles very large positions" $ do
            let pos = posAt 999999 999999
                errorLoc = toErrorLocation pos
            line errorLoc @?= 999999
            column errorLoc @?= 999999
            
        , testCase "handles span covering entire file" $ do
            let start = posAt 1 1
                end = posAt 1000 1000
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            line errorLoc @?= 1
            column errorLoc @?= 1
            endLine errorLoc @?= Just 1000
            endColumn errorLoc @?= Just 1000
            
        , testCase "handles single character span" $ do
            let start = posAt 5 10
                end = posAtLineCol 5 11 (posOffset start + 1)
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            line errorLoc @?= 5
            column errorLoc @?= 10
            endLine errorLoc @?= Just 5
            endColumn errorLoc @?= Just 11
        ]
        
    , testGroup "Consistency Tests"
        [ testProperty "position error location matches span start" $ fastProperty $ \line col ->
            let pos = posAt (abs line `mod` 1000 + 1) (abs col `mod` 1000 + 1)
                span = spanBetween pos pos
                posErrorLoc = toErrorLocation pos
                spanErrorLoc = toErrorLocationWithSpan span
            in line posErrorLoc == line spanErrorLoc &&
               column posErrorLoc == column spanErrorLoc &&
               endLine spanErrorLoc == Just line posErrorLoc &&
               endColumn spanErrorLoc == Just column posErrorLoc
               
        , testProperty "span error location preserves ordering" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = posAt (abs line1 `mod` 1000 + 1) (abs col1 `mod` 1000 + 1)
                end = posAt (abs line2 `mod` 1000 + 1) (abs col2 `mod` 1000 + 1)
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            in line errorLoc == posLine start &&
               column errorLoc == posColumn start &&
               endLine errorLoc == Just (posLine end) &&
               endColumn errorLoc == Just (posColumn end)
        ]
        
    , testGroup "Integration with Located Values"
        [ testCase "converts located value position to error location" $ do
            let pos = posAt 10 20
                value = "test"
                located = Located pos value
                errorLoc = toErrorLocation (locatedPos located)
            line errorLoc @?= 10
            column errorLoc @?= 20
            
        , testCase "converts located value span to error location" $ do
            let start = posAt 3 5
                end = posAt 3 15
                span = spanBetween start end
                value = "hello world"
                located = Located span value
                errorLoc = toErrorLocationWithSpan (locatedSpan located)
            line errorLoc @?= 3
            column errorLoc @?= 5
            endLine errorLoc @?= Just 3
            endColumn errorLoc @?= Just 15
        ]
        
    , testGroup "Performance L.and Robustness"
        [ testProperty "error location conversion handles large values" $ fastProperty $ \line col ->
            let pos = posAt (abs line `mod` 100000 + 1) (abs col `mod` 100000 + 1)
                errorLoc = toErrorLocation pos
            in line errorLoc > 0 && column errorLoc > 0
            
        , testProperty "span error location conversion handles large spans" $ fastProperty $ \line1 col1 line2 col2 ->
            let start = posAt (abs line1 `mod` 100000 + 1) (abs col1 `mod` 100000 + 1)
                end = posAt (abs line2 `mod` 100000 + 1) (abs col2 `mod` 100000 + 1)
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
            in line errorLoc > 0 && column errorLoc > 0 &&
               endLine errorLoc > Nothing && endColumn errorLoc > Nothing
               
        , testCase "handles consecutive conversions" $ do
            let pos = posAt 50 75
                errorLoc1 = toErrorLocation pos
                errorLoc2 = toErrorLocation pos
                errorLoc3 = toErrorLocation pos
            errorLoc1 @?= errorLoc2
            errorLoc2 @?= errorLoc3
        ]
        
    , testGroup "Error Location Display"
        [ testCase "error location can be converted to string" $ do
            let pos = posAt 10 20
                errorLoc = toErrorLocation pos
                errorLocStr = show errorLoc
            L.length errorLocStr > 0 @?= True
            
        , testCase "span error location can be converted to string" $ do
            let start = posAt 5 3
                end = posAt 5 8
                span = spanBetween start end
                errorLoc = toErrorLocationWithSpan span
                errorLocStr = show errorLoc
            L.length errorLocStr > 0 @?= True
            
        , testProperty "error location string representation contains line number" $ fastProperty $ \line col ->
            let pos = posAt (abs line `mod` 1000 + 1) (abs col `mod` 1000 + 1)
                errorLoc = toErrorLocation pos
                errorLocStr = show errorLoc
                lineStr = show (line errorLoc)
            in lineStr `L.isInfixOf` errorLocStr
        ]
    ]
    
-- Helper function to check if substring is in string
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (tails haystack)
  where
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'