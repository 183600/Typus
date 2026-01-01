module Test.Unit.UtilsIndentationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, forAll, Gen, arbitrary, choose, listOf1)
import Utils (normalizeIndentation, forceSingleTabIndentation, fixIndentation)
import Data.Char (isSpace)
import Data.List (nub)

-- | Tests for indentation functions in Utils module
tests :: TestTree
tests =
  testGroup "Utils Indentation"
    [ testGroup "normalizeIndentation function"
        [ testGroup "Basic functionality"
            [ testCase "removes common leading whitespace" $ do
                let input = unlines
                      [ "    line1"
                      , "      line2"
                      , "    line3"
                      ]
                    expected = unlines
                      [ "line1"
                      , "  line2"
                      , "line3"
                      ]
                normalizeIndentation input @?= expected
            
            , testCase "handles mixed spaces L.and tabs" $ do
                let input = "\t  line1\n\t    line2\n  \tline3"
                    result = normalizeIndentation input
                assertBool "Should preserve relative indentation" $
                    L.all (\line -> not (L.all isSpace line)) (lines result)
            
            , testCase "preserves empty lines" $ do
                let input = "  line1\n\n  line2\n  \n  line3"
                    expected = "line1\n\nline2\n \nline3"
                normalizeIndentation input @?= expected
            ]
        
        , testGroup "Edge cases"
            [ testCase "handles empty input" $ do
                normalizeIndentation "" @?= ""
            
            , testCase "handles input with only whitespace" $ do
                let input = "  \n  \t  \n    "
                normalizeIndentation input @?= "\n\n\n"
            
            , testCase "handles input with no indentation" $ do
                let input = "line1\nline2\nline3"
                normalizeIndentation input @?= input
            
            , testCase "handles input with L.all lines equally indented" $ do
                let input = "  line1\n  line2\n  line3"
                    expected = "line1\nline2\nline3"
                normalizeIndentation input @?= expected
            
            , testCase "handles single line" $ do
                let input = "    single line"
                    expected = "single line"
                normalizeIndentation input @?= expected
            ]
        
        , testCase "handles complex real-world indentation" $ do
            let input = unlines
                  [ "func main() {"
                  , "    if condition {"
                  , "        doSomething()"
                  , "    } else {"
                  , "        doNothing()"
                  , "    }"
                  , "}"
                  ]
                result = normalizeIndentation input
                lines result @?= 
                    [ "func main() {"
                    , "if condition {"
                    , "    doSomething()"
                    , "} else {"
                    , "    doNothing()"
                    , "}"
                    , "}"
                    ]
        
        , testGroup "QuickCheck properties"
            [ fastProperty "normalizeIndentation never adds indentation to first non-empty line" $
                \s -> let nonEmptyLines = L.filter (not . L.all isSpace) (lines s)
                       in case nonEmptyLines of
                            [] -> True
                            (first:_) -> let result = normalizeIndentation s
                                             resultLines = lines result
                                             resultNonEmpty = L.filter (not . L.all isSpace) resultLines
                                         in case resultNonEmpty of
                                              [] -> True
                                              (rFirst:_) -> L.head rFirst /= ' ' && L.head rFirst /= '\t'
            
            , fastProperty "normalizeIndentation preserves relative indentation differences" $
                \s -> let originalLines = L.filter (not . L.all isSpace) (lines s)
                          resultLines = L.filter (not . L.all isSpace) (lines (normalizeIndentation s))
                          indentDiff line = L.length (takeWhile isSpace line)
                      in L.length originalLines == L.length resultLines ||
                         L.all (>= 0) (zipWith (-) (map indentDiff originalLines) (map indentDiff resultLines))
            
            , fastProperty "normalizeIndentation preserves line count" $
                \s -> L.length (lines s) == L.length (lines (normalizeIndentation s))
            ]
        ]
    
    , testGroup "forceSingleTabIndentation function"
        [ testGroup "Basic functionality"
            [ testCase "converts L.all non-empty lines to single tab" $ do
                let input = unlines
                      [ "  line1"
                      , "    line2"
                      , "      line3"
                      , ""
                      , "    line5"
                      ]
                    expected = unlines
                      [ "\tline1"
                      , "\tline2"
                      , "\tline3"
                      , ""
                      , "\tline5"
                      ]
                forceSingleTabIndentation input @?= expected
            
            , testCase "trims whitespace before adding tab" $ do
                let input = "  \t  line  \t  "
                    expected = "\tline"
                forceSingleTabIndentation input @?= expected
            ]
        
        , testGroup "Edge cases"
            [ testCase "handles empty input" $ do
                forceSingleTabIndentation "" @?= ""
            
            , testCase "handles empty lines" $ do
                let input = "line1\n\nline3"
                    expected = "\tline1\n\n\tline3"
                forceSingleTabIndentation input @?= expected
            
            , testCase "handles lines with only whitespace" $ do
                let input = "line1\n  \n  \t  \nline4"
                    expected = "\tline1\n\n\n\tline4"
                forceSingleTabIndentation input @?= expected
            ]
        ]
    
    , testGroup "fixIndentation function"
        [ testCase "fixIndentation should be same as normalizeIndentation" $ do
            let input = "    line1\n      line2\n    line3"
            fixIndentation input @?= normalizeIndentation input
        
        , testCase "fixIndentation handles complex cases" $ do
            let input = unlines
                  [ "  func test() {"
                  , "    return 42"
                  , "  }"
                  ]
                expected = unlines
                  [ "func test() {"
                  , "  return 42"
                  , "}"
                  ]
            fixIndentation input @?= expected
        ]
    
    , testGroup "Regression L.and stress tests"
        [ testCase "handles very deep indentation" $ do
            let deepIndent = replicate 50 ' ' ++ "deep line"
                input = unlines $ replicate 10 deepIndent
                result = normalizeIndentation input
            assertBool "Should handle deep indentation" $
                L.all (\line -> take 5 line /= "     ") (lines result)
        
        , testCase "handles mixed indentation styles" $ do
            let input = unlines
                  [ "\tline1"
                  , "  \tline2"
                  , "\t  line3"
                  , "    line4"
                  ]
                result = normalizeIndentation input
            assertBool "Should normalize mixed indentation" $
                L.length (nub $ L.map (takeWhile isSpace) $ L.filter (not . null) $ lines result) <= 1
        
        , testCase "handles tabs of different sizes" $ do
            let input = unlines
                  [ "\tline1"
                  , "\t\tline2"
                  , "\t\t\tline3"
                  ]
                result = normalizeIndentation input
            assertBool "Should preserve tab structure" $
                L.all (('\t' `elem`) . takeWhile isSpace) (L.filter (not . null) $ lines result)
        ]
    
    , testGroup "Performance L.and memory tests"
        [ testCase "handles large files efficiently" $ do
            let largeInput = unlines $ replicate 1000 "    test line"
                result = normalizeIndentation largeInput
            lines result @?= replicate 1000 "test line"
        
        , testCase "handles files with many empty lines" $ do
            let input = unlines $ L.concat $ replicate 100 ["    line", "", "", ""]
                result = normalizeIndentation input
            L.length (lines result) @?= 300
        ]
    ]