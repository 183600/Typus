module Test.Unit.ValueAnalysisSpec (tests) where

import Compiler.GoAst (GoModule, parseGoModule)
import Compiler.ValueAnalysis
import Data.List (sort)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
    testGroup "ValueAnalysis"
        [ testCase "handles comments, strings, and multi-line references" $ do
            goModule <- parseModule sampleSource
            let infos = analyzeValueSemantics goModule
                kindOf name = [viKind info | info <- infos, viName info == name]
                sortedCopies = sort (extractValueCopyVars goModule)

            kindOf "value" @?= [ValueCopy]
            kindOf "text" @?= [ValueCopy]
            kindOf "numbers" @?= [Reference]
            kindOf "first" @?= [ValueCopy]
            kindOf "second" @?= [Reference]
            kindOf "groupedValue" @?= [ValueCopy]
            kindOf "groupedRef" @?= [Reference]
            kindOf "inner" @?= [Reference]
            kindOf "lookup" @?= [Reference]
            kindOf "fake" @?= []

            sortedCopies @?= ["first", "groupedValue", "text", "value"]
        ]

parseModule :: String -> IO GoModule
parseModule source =
    case parseGoModule (lines source) of
        Left err -> assertFailure ("Failed to parse Go module: " ++ err)
        Right goMod -> pure goMod

sampleSource :: String
sampleSource = unlines
    [ "package main"
    , ""
    , "var ("
    , "    numbersList = []int{1, 2, 3}"
    , "    lookup = map[string]int{"
    , "        \"a\": 1,"
    , "    }"
    , "    groupedValue, groupedRef = 5, make([]byte, 0)"
    , ")"
    , ""
    , "func example(items []string) {"
    , "    // fake := make([]int, 0)"
    , "    value := 42"
    , "    text := \"make(inside)\""
    , "    numbers := make([]int,"
    , "        0)"
    , "    first, second := 1, make([]int, 1)"
    , "    for _, entry := range items {"
    , "        inner := make([]string, 0)"
    , "    }"
    , "}"
    ]
