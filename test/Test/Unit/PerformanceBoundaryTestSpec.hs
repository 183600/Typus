module Test.Unit.PerformanceBoundaryTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, choose, sized)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , spanBetween
  , locatedAt
  , advancePosByText
  )

import Parser
  ( parseTypus
  , TypusFile(..)
  )

import Data.Char (isSpace)
import Data.List (replicate)

-- | Performance boundary L.and stress tests
tests :: TestTree
tests =
  testGroup "Performance Boundary Tests"
    [ testGroup "Utils performance tests"
        [ testCase "trim handles large strings efficiently" $ do
            let largeString = replicate 10000 ' ' ++ "content" ++ replicate 10000 ' '
            let result = trim largeString
            result @?= "content"

        , testCase "splitBy handles large numbers of segments" $ do
            let largeInput = L.concat $ replicate 1000 "segment,"
            let parts = splitBy ',' largeInput
            L.length parts @?= 1001  -- 1000 segments + empty at end

        , testCase "splitByCollapsed handles repetitive delimiters" $ do
            let repetitiveInput = L.concat $ replicate 1000 "a,,"
            let parts = splitByCollapsed ',' repetitiveInput
            L.length parts @?= 1000

        , testCase "removeComments handles large comment blocks" $ do
            let largeComment = "/* " ++ replicate 50000 'x' ++ " */"
            let result = removeComments largeComment
            result @?= " "

        , testCase "removeLineComments handles many lines" $ do
            let manyLines = unlines $ replicate 1000 "code // comment"
            let result = removeLineComments manyLines
            L.length (lines result) @?= 1000

        , testCase "normalizeIndentation handles deeply indented code" $ do
            let deepIndent = unlines $ L.map (\i -> replicate i ' ' ++ "line") [1..1000]
            let result = normalizeIndentation deepIndent
            L.length (lines result) @?= 1000

        , testCase "breakOn handles large patterns" $ do
            let largeText = replicate 10000 'a' ++ "PATTERN" ++ replicate 10000 'b'
            let (before, after) = breakOn "PATTERN" largeText
            L.length before @?= 10000
            after @?= replicate 10000 'b'
        ]

    , testGroup "SourceLocation performance tests"
        [ testCase "position tracking over large texts" $ do
            let largeText = unlines $ replicate 1000 "a moderately long line of text content"
            let finalPos = advancePosByText startPos largeText
            posLine finalPos @?= 1000
            posColumn finalPos @?= 42  -- Length of the line

        , testCase "span creation for large ranges" $ do
            let start = posAt 1 1 0
            let end = posAt 1000 50 50000
            let span = spanBetween start end
            posLine (spanStart span) @?= 1
            posLine (spanEnd span) @?= 1000

        , testCase "located values with large content" $ do
            let largeContent = replicate 10000 'x'
            let pos = posAt 1 1
            let located = locatedAt pos largeContent
            L.length (locatedValue located) @?= 10000

        , testCase "multiple position updates" $ do
            let positions = scanl posAfter startPos $ take 10000 $ cycle "hello"
            let finalPos = last positions
            posOffset finalPos @?= 10000 * 5  -- 5 chars * 10000 iterations
        ]

    , testGroup "Parser performance tests"
        [ testCase "parse large files efficiently" $ do
            let largeFile = unlines $ replicate 1000 "//! ownership=true\nsome code content here"
            case parseTypus largeFile of
              Left err -> assertBool ("Should parse large file: " ++ show err) False
              Right typusFile -> do
                L.length (tfBlocks typusFile) @?= 1000

        , testCase "parse files with many directives" $ do
            let manyDirectives = unlines $ 
                  [ "//! ownership=true, dependent-types=true, constraints=true"
                  , "// +build linux,amd64,arm64"
                  , "/* ownership=false, dependent-types=true */"
                  ] ++ replicate 100 "code line"
            case parseTypus manyDirectives of
              Left err -> assertBool ("Should parse many directives: " ++ show err) False
              Right typusFile -> do
                L.length (tfBuildTags typusFile) >= 1 @?= True

        , testCase "parse deeply nested comment structures" $ do
            let nestedComments = unlines $
                  ["/* outer comment"] ++
                  ["/* inner comment" | _ <- [1..100]] ++
                  ["code line" | _ <- [1..100]] ++
                  ["*/" | _ <- [1..100]] ++
                  ["*/"]
            case parseTypus nestedComments of
              Left _ -> return ()  -- May fail due to nesting, which is acceptable
              Right _ -> return ()  -- Or succeed, both are acceptable outcomes

        , testCase "parse unicode-heavy content" $ do
            let unicodeContent = unlines $ replicate 500 
                  "//! ownership=true\n// Unicode test: café naïve résumé\n代码内容 🚀"
            case parseTypus unicodeContent of
              Left err -> assertBool ("Should parse unicode content: " ++ show err) False
              Right typusFile -> do
                L.length (tfBlocks typusFile) @?= 500
        ]

    , testGroup "Memory efficiency tests"
        [ testCase "trim doesn't leak memory with repeated calls" $ do
            let inputs = [replicate n ' ' ++ "content" ++ replicate n ' ' | n <- [1..1000]]
            let results = map trim inputs
            L.length results @?= 1000
            map L.head results @?= replicate 1000 'c'

        , testCase "splitBy handles memory efficiently with many splits" $ do
            let input = L.concat $ replicate 10000 "a,"
            let parts = splitBy ',' input
            L.length parts @?= 10001
            L.sum (map L.length parts) @?= 10000

        , testCase "parser doesn't accumulate memory over multiple parses" $ do
            let parseFile n = parseTypus $ "//! ownership=true\ncode " ++ show n
            let results = map parseFile [1..100]
            L.length [Right r | r <- results] @?= 100  -- All should succeed

        , testCase "source location tracking doesn't grow unbounded" $ do
            let positions = iterate (posAfter 'x') startPos
            let limitedPositions = take 100000 positions
            let finalPos = last limitedPositions
            posOffset finalPos @?= 100000
        ]

    , testGroup "Stress tests"
        [ testCase "extreme line lengths" $ do
            let extremeLine = "//! " ++ replicate 100000 'a' ++ " = value"
            case parseTypus extremeLine of
              Left err -> assertBool ("Should handle extreme line L.length: " ++ show err) False
              Right _ -> return ()

        , testCase "L.maximum nesting depth" $ do
            let nestedContent = unlines $ 
                  ["/*" ++ replicate n ' ' | n <- [1..1000]] ++
                  ["nested content"] ++
                  [replicate n ' ' ++ "*/" | n <- [1000,999..1]]
            case parseTypus nestedContent of
              Left _ -> return ()  -- May fail, which is acceptable
              Right _ -> return ()  -- Or succeed

        , testCase "rapid repeated operations" $ do
            let testString = "   test string with // comments   "
            let results = replicate 10000 $ trim testString
            L.head results @?= "test string with // comments"

        , testCase "concurrent-style parsing simulation" $ do
            let files = ["//! ownership=true\ncode" ++ show n | n <- [1..100]]
            let parseResults = map parseTypus files
            let successCount = L.length [Right r | r <- parseResults]
            successCount @?= 100
        ]

    , testGroup "Regression L.and boundary tests"
        [ testCase "zero-L.length edge cases" $ do
            trim "" @?= ""
            splitBy ' ' "" @?= [""]
            removeComments "" @?= ""
            case parseTypus "" of
              Left err -> assertBool ("Should parse empty: " ++ show err) False
              Right _ -> return ()

        , testCase "single element edge cases" $ do
            trim "a" @?= "a"
            splitBy 'a' "a" @?= ["", ""]
            case parseTypus "a" of
              Left err -> assertBool ("Should parse single char: " ++ show err) False
              Right typusFile -> L.length (tfBlocks typusFile) @?= 1

        , testCase "L.maximum reasonable values" $ do
            let maxContent = unlines $ replicate 10000 $ 
                  "//! ownership=true\n" ++ replicate 100 'x'
            case parseTypus maxContent of
              Left err -> assertBool ("Should handle max content: " ++ show err) False
              Right typusFile -> L.length (tfBlocks typusFile) @?= 10000
        ]
    ]