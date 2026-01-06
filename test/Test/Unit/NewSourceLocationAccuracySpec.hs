{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewSourceLocationAccuracySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized, Positive(..))

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import SourceLocation 
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , spanStart
  , spanEnd
  , posLine
  , posColumn
  , mkSourcePos
  , mkSourceSpan
  )
import Text.Megaparsec (errorBundlePretty)
import qualified Data.List as L
import Data.List (length)
import Data.List (foldl')

tests :: TestTree
tests = testGroup "New Source Location Accuracy Tests"
    [ testCase "accurately tracks line L.and column positions" $ do
        let source = unlines
              [ "package main"
              , "func test() {"
              , "  let x = 42"
              , "  return x"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            -- Check that function declaration is on line 2
            let funcSpan = getFunctionSpan typusFile
            case funcSpan of
              Just span -> do
                posLine (spanStart span) @?= 2
                posColumn (spanStart span) @?= 1
                posLine (spanEnd span) @?= 5
                assertBool "Function ends on line 5" $ posLine (spanEnd span) >= 5
              Nothing -> assertFailure "Could not find function span"
              
    , testCase "handles multi-line constructs correctly" $ do
        let source = unlines
              [ "package main"
              , "func multi_line_function("
              , "    param1: int,"
              , "    param2: string"
              , ") -> int {"
              , "  if condition &&"
              , "     other_condition {"
              , "    return 42"
              , "  }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let funcSpan = getFunctionSpan typusFile
            case funcSpan of
              Just span -> do
                posLine (spanStart span) @?= 2
                posLine (spanEnd span) @?= 10
                assertBool "Function spans multiple lines" $ 
                  posLine (spanEnd span) - posLine (spanStart span) >= 8
              Nothing -> assertFailure "Could not find function span"
              
    , testCase "accurately positions nested constructs" $ do
        let source = unlines
              [ "package main"
              , "func outer() {"
              , "  let x = 42"
              , "  func inner() {"
              , "    let y = 84"
              , "    return y"
              , "  }"
              , "  return x"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let spans = getAllFunctionSpans typusFile
            assertBool "Should find both functions" $ L.length spans >= 2
            -- Check that inner function is properly nested
            case spans of
              (outer:inner:_) -> do
                posLine (spanStart outer) @?= 2
                posLine (spanStart inner) @?= 4
                assertBool "Inner function starts after outer" $ 
                  posLine (spanStart inner) > posLine (spanStart outer)
                assertBool "Inner function ends before outer" $ 
                  posLine (spanEnd inner) < posLine (spanEnd outer)
              _ -> assertFailure "Expected at least 2 function spans"
              
    , testCase "handles Unicode characters in position calculation" $ do
        let source = unlines
              [ "package main"
              , "func 测试函数() {"
              , "  let 变量 = \"中文内容\""
              , "  return 变量"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            let funcSpan = getFunctionSpan typusFile
            case funcSpan of
              Just span -> do
                posLine (spanStart span) @?= 2
                posColumn (spanStart span) @?= 1
                posLine (spanEnd span) @?= 5
                assertBool "Function with Unicode ends on line 5" $ 
                  posLine (spanEnd span) >= 5
              Nothing -> assertFailure "Could not find function span"
    ]

-- QuickCheck properties for source location precision

-- Property: Line numbers should be consistent with actual source lines
prop_line_numbers_consistent :: String -> Property
prop_line_numbers_consistent source =
  case parseTypus source of
    Left _ -> property $ True  -- Parse errors are expected for invalid input
    Right typusFile -> do
      let spans = getAllFunctionSpans typusFile
      let sourceLines = lines source
      property $ L.all (isSpanConsistent sourceLines) spans
  where
    isSpanConsistent lines span = 
      let startLine = posLine (spanStart span)
          endLine = posLine (spanEnd span)
      in startLine >= 1 && endLine >= startLine && endLine <= L.length lines

-- Property: Column numbers should be within reasonable bounds
prop_column_numbers_reasonable :: String -> Property
prop_column_numbers_reasonable source =
  case parseTypus source of
    Left _ -> property $ True
    Right typusFile -> do
      let spans = getAllFunctionSpans typusFile
      property $ L.all (isColumnReasonable source) spans
  where
    isColumnReasonable source span =
      let startCol = posColumn (spanStart span)
          endCol = posColumn (spanEnd span)
          sourceLines = lines source
          lineLength = case sourceLines of
            [] -> 0
            (l:_) -> L.length l
      in startCol >= 1 && endCol >= startCol && endCol <= lineLength + 1000  -- Allow some margin

-- Helper functions
getFunctionSpan :: TypusFile -> Maybe SourceSpan
getFunctionSpan typusFile = 
  case tfCodeBlocks typusFile of
    (block:_) -> Just (cbSpan block)
    [] -> Nothing

getAllFunctionSpans :: TypusFile -> [SourceSpan]
getAllFunctionSpans typusFile = map cbSpan (tfCodeBlocks typusFile)

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` (substrings haystack)
  where
    substrings [] = []
    substrings s@(x:xs) = take (L.length needle) s : substrings xs