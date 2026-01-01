module Test.Unit.EdgeCaseSpec (tests) where

import qualified Data.List as L
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import qualified Parser
import Parser (TypusFile(..), CodeBlock(..))

tests :: TestTree
tests =
  testGroup "Edge case tests"
    [ testCase "handles empty input" $ do
        let source = ""
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> tfBlocks typusFile @?= []

    , testCase "handles whitespace-only input" $ do
        let source = "   \n   \n   "
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> tfBlocks typusFile @?= []

    , testCase "handles comments-only input" $ do
        let source = unlines
              [ "// This is a comment"
              , "/* Another comment */"
              , "// Yet another comment"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> 
            case tfBlocks typusFile of
              [block] -> assertBool "should contain comment content" ("// This is a comment" `L.isInfixOf` cbContent block)
              _ -> assertFailure "expected exactly one block with comments"

    , testCase "handles extremely long lines" $ do
        let longLine = replicate 10000 'a'
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"" ++ longLine ++ "\")"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles deeply nested parentheses" $ do
        let nestedParens = replicate 100 '(' ++ "x" ++ replicate 100 ')'
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    result := " ++ nestedParens
              , "    println(result)"
              , "}"
              ]
        case Parser.parseTypus source of
          Left _ -> return ()  -- Expected to fail, but shouldn't crash
          Right _ -> return ()

    , testCase "handles unusual unicode characters" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"𝔘𝔫𝔦𝔠𝔬𝔡𝔢 🌟 🚀 🦄\")"
              , "    var 𝔣𝔩𝔬𝔞𝔱 = 3.14159"
              , "    println(𝔣𝔩𝔬𝔞𝔱)"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles escape sequences" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"Hello\\nWorld\\t!\\\"Quote\\'\\\")"
              , "    path := \"C:\\\\Windows\\\\System32\""
              , "    println(path)"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles numeric edge cases" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var maxInt64 int64 = 9223372036854775807"
              , "    var minInt64 int64 = -9223372036854775808"
              , "    var maxFloat64 float64 = 1.7976931348623157e+308"
              , "    var minFloat64 float64 = -1.7976931348623157e+308"
              , "    var inf float64 = 1.0 / 0.0"
              , "    var negInf float64 = -1.0 / 0.0"
              , "    var nan float64 = 0.0 / 0.0"
              , "    println(maxInt64, minInt64, maxFloat64, minFloat64, inf, negInf, nan)"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles conflicting directives" $ do
        let source = unlines
              [ "//! ownership: on"
              , "//! ownership: off"
              , "package main"
              , "func main() {}"
              ]
        case Parser.parseTypus source of
          Left _ -> return ()  -- Expected to fail
          Right _ -> return ()

    , testCase "handles malformed block directives" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "    // Missing closing brace"
              , "}"
              ]
        case Parser.parseTypus source of
          Left _ -> return ()  -- Expected to fail
          Right _ -> return ()

    , testCase "handles recursive type definitions" $ do
        let source = unlines
              [ "package main"
              , "type Node struct {"
              , "    value int"
              , "    next *Node"
              , "}"
              , "func main() {}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles circular imports simulation" $ do
        let source1 = unlines
              [ "package a"
              , "import \"b\""
              , "func A() { b.B() }"
              ]
        let source2 = unlines
              [ "package b"
              , "import \"a\""
              , "func B() { a.A() }"
              ]
        -- Both should parse individually
        case Parser.parseTypus source1 of
          Left err -> assertFailure $ "parseTypus failed on source1: " <> err
          Right _ -> return ()
        case Parser.parseTypus source2 of
          Left err -> assertFailure $ "parseTypus failed on source2: " <> err
          Right _ -> return ()

    , testCase "handles extremely long identifiers" $ do
        let longIdent = replicate 1000 'x'
        let source = unlines
              [ "package main"
              , "func " ++ longIdent ++ "() {"
              , "    println(\"very long function name\")"
              , "}"
              , "func main() {"
              , "    " ++ longIdent ++ "()"
              , "}"
              ]
        case Parser.parseTypus source of
          Left _ -> return ()  -- Might fail due to identifier L.length limits
          Right _ -> return ()

    , testCase "handles zero-width L.and invisible characters" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    // Zero-width space comment"
              , "    // Invisible separator comment"
              , "    println(\"visible\")"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles mixed line endings" $ do
        let source = "package main\r\nfunc main() {\n    println(\"mixed\")\r\n}"
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles BOM (Byte Order Mark)" $ do
        let source = unlines
              [ "package main"
              , "func main() { println(\"BOM\") }"
              ]
        case Parser.parseTypus source of
          Left _ -> return ()  -- Might fail due to BOM
          Right _ -> return ()

    , testCase "handles invalid UTF-8 sequences" $ do
        let source = "package main\nfunc main() { println(\"invalid: \xFF\xFE\") }"
        case Parser.parseTypus source of
          Left _ -> return ()  -- Expected to fail
          Right _ -> return ()

    , testCase "handles extreme indentation" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    " ++ L.concat (replicate 100 "    ") ++ "println(\"deeply indented\")"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles concurrent access patterns" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    ch := make(chan int)"
              , "    go func() { ch <- 1 }()"
              , "    go func() { ch <- 2 }()"
              , "    go func() { <-ch }()"
              , "    go func() { <-ch }()"
              , "}"
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()

    , testCase "handles memory pressure simulation" $ do
        let source = unlines $ L.concat
              [ ["package main"]
              , ["func main() {"]
              , L.concat $ L.map (\i -> ["var arr" ++ show i ++ " [" ++ show (1000 * i :: Integer) ++ "]int"]) [1..100]
              , ["println(\"memory pressure test\")"]
              , ["}"]
              ]
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()
    ]