{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Unit.IndentationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, oneof, elements, forAll)
import Utils (normalizeIndentation, forceSingleTabIndentation, fixIndentation)
import Data.Char (isSpace)

tests :: TestTree
tests = testGroup "Indentation Tests"
  [ testGroup "normalizeIndentation basic functionality"
    [ testCase "removes common prefix indentation" $
        normalizeIndentation "    foo\n      bar" @?= "foo\n  bar"
    , testCase "handles single line" $
        normalizeIndentation "    single line" @?= "single line"
    , testCase "handles no indentation" $
        normalizeIndentation "foo\nbar" @?= "foo\nbar"
    , testCase "preserves empty lines" $
        normalizeIndentation "  foo\n\n  bar" @?= "foo\n\nbar"
    , testCase "handles only empty lines" $
        normalizeIndentation "\n\n\n" @?= "\n\n\n"
    ]
  , testGroup "normalizeIndentation mixed indentation"
    [ testCase "handles mixed spaces and tabs" $
        normalizeIndentation "\t  foo\n\t  \tbar" @?= "foo\n\tbar"
    , testCase "handles tabs only" $
        normalizeIndentation "\tfoo\n\t\tbar" @?= "foo\n\tbar"
    , testCase "handles spaces only" $
        normalizeIndentation "  foo\n    bar" @?= "foo\n  bar"
    , testCase "handles inconsistent mixed indentation" $
        normalizeIndentation "  \tfoo\n\t  bar" @?= "foo\nbar"
    ]
  , testGroup "normalizeIndentation language-specific styles"
    [ testCase "handles Python-style indentation" $
        normalizeIndentation "def func():\n    if True:\n        print(\"hello\")\n    else:\n        print(\"world\")" @?= 
        "def func():\nif True:\n    print(\"hello\")\nelse:\n    print(\"world\")"
    , testCase "handles Haskell-style indentation" $
        normalizeIndentation "  where\n    x = 1\n    y = 2\n  in x + y" @?= 
        "where\nx = 1\ny = 2\nin x + y"
    ]
  , testGroup "normalizeIndentation edge cases"
    [ testCase "handles lines with only whitespace" $
        normalizeIndentation "  foo\n    \n  bar" @?= "foo\n  \nbar"
    , testCase "handles leading empty lines" $
        normalizeIndentation "\n  \n  foo\n  bar" @?= "\n  \nfoo\nbar"
    , testCase "handles trailing empty lines" $
        normalizeIndentation "  foo\n  bar\n  \n" @?= "foo\nbar\n  \n"
    ]
  , testGroup "forceSingleTabIndentation functionality"
    [ testCase "handles single-line strings" $
        forceSingleTabIndentation "hello world" @?= "\thello world"
    , testCase "handles multi-line strings" $
        forceSingleTabIndentation "hello\nworld" @?= "\thello\n\tworld"
    , testCase "trims and tabs each non-empty line" $
        forceSingleTabIndentation "  foo\n    bar\n  baz" @?= "\tfoo\n\tbar\n\tbaz"
    , testCase "preserves empty lines" $
        forceSingleTabIndentation "foo\n\nbar" @?= "\tfoo\n\n\tbar"
    ]
  , testGroup "fixIndentation functionality"
    [ testCase "fixIndentation equals normalizeIndentation" $
        let content = "    foo\n      bar\n  baz"
        in fixIndentation content @?= normalizeIndentation content
    , testCase "fixIndentation handles complex case" $
        fixIndentation "  def func():\n    return 42\n  \n" @?= "def func():\n  return 42\n  \n"
    ]
  , testGroup "Properties"
    [ testProperty "normalizeIndentation preserves line count" $
        \s -> not (null s) ==> length (lines (normalizeIndentation s)) == length (lines s)
    , testProperty "normalizeIndentation never adds leading spaces to first non-empty line" $
        \s -> let normalized = normalizeIndentation s
                  nonEmptyLines = filter (not . all isSpace) $ lines normalized
              in not (null nonEmptyLines) ==> 
                 let firstLine = head nonEmptyLines
                 in null firstLine || not (isSpace (head firstLine))
    , testProperty "normalizeIndentation preserves relative indentation" $
        \s -> let original = lines s
                  normalized = lines (normalizeIndentation s)
                  getIndent l = length $ takeWhile isSpace l
                  originalIndents = map getIndent $ filter (not . null) original
                  normalizedIndents = map getIndent $ filter (not . null) normalized
              in length originalIndents == length normalizedIndents
    , testProperty "forceSingleTabIndentation adds tab to non-empty lines" $
        \s -> let result = forceSingleTabIndentation s
                  lines' = lines result
                  nonEmptyLines = filter (not . all isSpace) lines'
              in all ((== '\t') . head) nonEmptyLines
    , testProperty "normalizeIndentation is idempotent" $
        \s -> normalizeIndentation (normalizeIndentation s) == normalizeIndentation s
    ]
  , testGroup "Complex test cases"
    [ testCase "handles deeply nested indentation" $
        let content = "        level1\n            level2\n                level3\n                    level4"
        in normalizeIndentation content @?= "level1\n    level2\n        level3\n            level4"
    , testCase "handles inconsistent indentation levels" $
        let content = "  level1\n    level2\n  level3\n      level4"
        in normalizeIndentation content @?= "level1\n  level2\nlevel1\n    level4"
    , testCase "handles very long lines with indentation" $
        let longLine = "    " ++ replicate 200 'x'
            content = longLine ++ "\n    short"
        in normalizeIndentation content @?= replicate 200 'x' ++ "\nshort"
    ]
  , testGroup "Line ending handling"
    [ testCase "handles Windows line endings" $
        let content = "  foo\r\n  bar\r\n    baz"
        in normalizeIndentation content @?= "foo\r\nbar\r\n  baz"
    , testCase "handles mixed line endings" $
        let content = "  foo\n  bar\r\n  baz\r\n"
        in normalizeIndentation content @?= "foo\nbar\r\nbaz\r\n"
    ]
  ]

-- Helper functions
hasConsistentRelativeIndentation :: [String] -> [String] -> Bool
hasConsistentRelativeIndentation original normalized = 
  let getIndent l = length $ takeWhile isSpace l
      originalIndents = map getIndent $ filter (not . null) original
      normalizedIndents = map getIndent $ filter (not . null) normalized
      differences = zipWith (-) (tail normalizedIndents) (tail originalIndents)
  in all (== head differences) (tail differences)

-- Generators for specific test cases
genIndentedLine :: Gen String
genIndentedLine = do
  indent <- choose (0, 8)
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' '
  return $ replicate indent ' ' ++ content

genIndentedText :: Gen String
genIndentedText = do
  numLines <- choose (1, 10)
  lines' <- listOf genIndentedLine
  return $ unlines (take numLines lines')

genMixedIndentation :: Gen String
genMixedIndentation = do
  numLines <- choose (1, 5)
  lines' <- listOf $ do
    spaces <- choose (0, 4)
    tabs <- choose (0, 2)
    content <- listOf $ elements $ ['a'..'z'] ++ ' '
    return $ replicate spaces ' ' ++ replicate tabs '\t' ++ content
  return $ unlines (take numLines lines')

-- Note: Arbitrary instance for String is provided by QuickCheck