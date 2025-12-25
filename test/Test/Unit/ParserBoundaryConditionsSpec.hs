{-# LANGUAGE CPP #-}

module Test.Unit.ParserBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isPrefixOf, isInfixOf)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanEnd
  , spanStart
  )

tests :: TestTree
tests = testGroup "Parser Boundary Conditions"
  [ emptyInputTests
  , malformedDirectiveTests
  , extremeInputTests
  , encodingTests
  , nestedStructureTests
  , errorRecoveryTests
  ]

emptyInputTests :: TestTree
emptyInputTests = testGroup "Empty Input Tests"
  [ testCase "handles completely empty input" $ do
      let source = ""
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on empty input: " <> err
        Right typusFile -> do
          tfDirectives typusFile @?= defaultFileDirectives
          tfBuildTags typusFile @?= []
          tfBlocks typusFile @?= []

  , testCase "handles whitespace-only input" $ do
      let source = "   \n\t\n   \n"
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on whitespace-only input: " <> err
        Right typusFile -> do
          tfDirectives typusFile @?= defaultFileDirectives
          tfBuildTags typusFile @?= []
          tfBlocks typusFile @?= []

  , testCase "handles comment-only input" $ do
      let source = unlines
            [ "// This is a comment"
            , "// Another comment"
            , ""
            , "/* Block comment */"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on comment-only input: " <> err
        Right typusFile -> do
          tfDirectives typusFile @?= defaultFileDirectives
          tfBuildTags typusFile @?= []
          tfBlocks typusFile @?= []
  ]

malformedDirectiveTests :: TestTree
malformedDirectiveTests = testGroup "Malformed Directive Tests"
  [ testCase "handles incomplete file directive" $ do
      let source = "//! ownership"
      case parseTypus source of
        Left err -> assertBool ("should fail on incomplete directive: " <> err) $
          "Invalid file directive format" `isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail on incomplete directive"

  , testCase "handles invalid directive values" $ do
      let source = unlines
            [ "//! ownership: maybe"
            , "package main"
            ]
      case parseTypus source of
        Left err -> assertBool ("should fail on invalid value: " <> err) $
          "Invalid boolean value" `isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail on invalid directive value"

  , testCase "handles unknown directive keys" $ do
      let source = unlines
            [ "//! unknown_feature: on"
            , "package main"
            ]
      case parseTypus source of
        Left err -> assertBool ("should fail on unknown directive: " <> err) $
          "Unknown file directive" `isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail on unknown directive"

  , testCase "handles malformed block directive without opening brace" $ do
      let source = unlines
            [ "package main"
            , "func main() {"
            , "    //! ownership: on"
            , "    println(\"test\")"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " <> err
        Right typusFile -> do
          -- Should parse as regular code, not as a directive
          assertBool "should parse as regular code" $ not $ null $ tfBlocks typusFile

  , testCase "handles unclosed block directive" $ do
      let source = unlines
            [ "package main"
            , "func main() {"
            , "    {//! ownership: on"
            , "        println(\"test\")"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertBool ("should fail on unclosed directive: " <> err) $
          "Unclosed directive block" `isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail on unclosed directive block"
  ]

extremeInputTests :: TestTree
extremeInputTests = testGroup "Extreme Input Tests"
  [ testCase "handles very long lines" $ do
      let longString = replicate 10000 'a'
          source = unlines
            [ "//! ownership: on"
            , "package main"
            , "func main() {"
            , "    x := \"" ++ longString ++ "\""
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on long lines: " <> err
        Right typusFile -> do
          assertBool "should parse long lines" $ not $ null $ tfBlocks typusFile

  , testCase "handles deeply nested structures" $ do
      let nestedBraces = replicate 100 '{'
          closingBraces = replicate 100 '}'
          source = unlines
            [ "package main"
            , "func main() {"
            , nestedBraces
            , "    println(\"deeply nested\")"
            , closingBraces
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on deeply nested structures: " <> err
        Right typusFile -> do
          assertBool "should parse deeply nested structures" $ not $ null $ tfBlocks typusFile

  , testCase "handles many small blocks" $ do
      let manyBlocks = unlines $ concat $ replicate 50
            [ "{//! ownership: on"
            , "println(\"test\")"
            , "}"
            ]
          source = unlines
            [ "package main"
            , "func main() {"
            , manyBlocks
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on many blocks: " <> err
        Right typusFile -> do
          let blocks = tfBlocks typusFile
              ownershipBlocks = filter (maybe False locatedValue . bdOwnership . cbDirectives) blocks
          assertBool "should find many ownership blocks" $ length ownershipBlocks >= 50
  ]

encodingTests :: TestTree
encodingTests = testGroup "Encoding Tests"
  [ testCase "handles Unicode characters in strings" $ do
      let source = unlines
            [ "//! ownership: on"
            , "package main"
            , "func main() {"
            , "    println(\"Hello, 世界 🌍\")"
            , "    s := \"Café Münchner Kindl\""
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on Unicode: " <> err
        Right typusFile -> do
          assertBool "should parse Unicode characters" $ not $ null $ tfBlocks typusFile

  , testCase "handles various newline formats" $ do
      let sourceWithCR = "//! ownership: on\rpackage main\rfunc main() {}\r"
          sourceWithCRLF = "//! ownership: on\r\npackage main\r\nfunc main() {}\r\n"
          sourceWithLF = "//! ownership: on\npackage main\nfunc main() {}\n"
      
      -- Test each newline format
      mapM_ (\(desc, src) -> testCase desc $ do
        case parseTypus src of
          Left err -> assertFailure $ "parseTypus failed on " ++ desc ++ ": " ++ err
          Right typusFile -> do
            assertBool ("should parse " ++ desc) $ not $ null $ tfBlocks typusFile
        ) [ ("CR newlines", sourceWithCR)
           , ("CRLF newlines", sourceWithCRLF)
           , ("LF newlines", sourceWithLF)
           ]

  , testCase "handles tabs and spaces mixed" $ do
      let source = unlines
            [ "//! ownership: on"
            , "package main"
            , "func main() {"
            , "\tprintln(\"tab\")"
            , "    println(\"spaces\")"
            , "\t    println(\"mixed\")"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on mixed indentation: " <> err
        Right typusFile -> do
          assertBool "should parse mixed indentation" $ not $ null $ tfBlocks typusFile
  ]

nestedStructureTests :: TestTree
nestedStructureTests = testGroup "Nested Structure Tests"
  [ testCase "handles nested directives" $ do
      let source = unlines
            [ "package main"
            , "func main() {"
            , "    {//! ownership: on"
            , "        outer := true"
            , "        {//! dependent_types: on"
            , "            inner := 42"
            , "        }"
            , "    }"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on nested directives: " <> err
        Right typusFile -> do
          let blocks = tfBlocks typusFile
              ownershipBlocks = filter (maybe False locatedValue . bdOwnership . cbDirectives) blocks
              dependentTypeBlocks = filter (maybe False locatedValue . bdDependentTypes . cbDirectives) blocks
          assertBool "should find ownership block" $ length ownershipBlocks >= 1
          assertBool "should find dependent types block" $ length dependentTypeBlocks >= 1

  , testCase "handles directives in complex control structures" $ do
      let source = unlines
            [ "package main"
            , "func main() {"
            , "    if true {"
            , "        {//! ownership: on"
            , "            if false {"
            , "                {//! dependent_types: on"
            , "                    x := 42"
            , "                }"
            , "            }"
            , "        }"
            , "    }"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed on complex control structures: " <> err
        Right typusFile -> do
          assertBool "should parse directives in control structures" $ not $ null $ tfBlocks typusFile
  ]

errorRecoveryTests :: TestTree
errorRecoveryTests = testGroup "Error Recovery Tests"
  [ testCase "recovers from syntax errors in blocks" $ do
      let source = unlines
            [ "package main"
            , "func main() {"
            , "    {//! ownership: on"
            , "        valid := true"
            , "        if true {  // Missing closing brace"
            , "            x := 42"
            , "        }"
            , "        recovered := true"
            , "    }"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " <> err
        Right typusFile -> do
          -- Should still parse despite syntax error
          let syntaxErrors = tfSyntaxErrors typusFile
          assertBool "should detect syntax errors" $ not $ null syntaxErrors
          assertBool "should still parse blocks" $ not $ null $ tfBlocks typusFile

  , testCase "handles malformed Go syntax but valid directives" $ do
      let source = unlines
            [ "//! ownership: on"
            , "//! dependent_types: on"
            , "package main"
            , "func main( {  // Missing closing parenthesis"
            , "    {//! constraints: on"
            , "        malformed syntax here"
            , "    }"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " <> err
        Right typusFile -> do
          let directives = tfDirectives typusFile
          case fdOwnership directives of
            Nothing -> assertFailure "expected ownership directive"
            Just loc -> locatedValue loc @?= True
          case fdDependentTypes directives of
            Nothing -> assertFailure "expected dependent types directive"
            Just loc -> locatedValue loc @?= True
          assertBool "should detect syntax errors" $ not $ null $ tfSyntaxErrors typusFile

  , testCase "continues parsing after directive errors" $ do
      let source = unlines
            [ "//! invalid_directive: on"
            , "package main"
            , "func main() {"
            , "    {//! ownership: on"
            , "        println(\"should parse this\")"
            , "    }"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " <> err
        Right typusFile -> do
          -- Should parse valid parts despite invalid directive
          let blocks = tfBlocks typusFile
              ownershipBlocks = filter (maybe False locatedValue . bdOwnership . cbDirectives) blocks
          assertBool "should find valid ownership block" $ length ownershipBlocks >= 1
  ]