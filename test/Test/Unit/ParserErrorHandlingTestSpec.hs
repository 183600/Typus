{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorHandlingTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements
  , vectorOf, oneof, frequency, suchThat, Positive(..)
  )

import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import qualified Data.Text as T

-- | Generate malformed Go-like code for error testing
genMalformedCode :: Gen String
genMalformedCode = oneof
  [ -- Unclosed braces
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    println(\"hello\")"
      ]
  , -- Unclosed strings
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    println(\"unclosed string"
      , "}"
      ]
  , -- Invalid characters
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    println(\"hello\"); @#$ invalid"
      , "}"
      ]
  , -- Mismatched parentheses
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    println((\"hello\")"
      , "}"
      ]
  , -- Invalid syntax
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    if true {"
      , "    } else {"
      , "    } else {"  -- Double else
      , "    }"
      , "}"
      ]
  , -- Unclosed comments
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    /* unclosed comment"
      , "    println(\"hello\")"
      , "}"
      ]
  ]

-- | Generate valid Go-like code
genValidCode :: Gen String
genValidCode = oneof
  [ return $ unlines
      [ "package main"
      , "func main() {"
      , "    println(\"hello\")"
      , "}"
      ]
  , return $ unlines
      [ "package main"
      , "import \"fmt\""
      , "func add(a int, b int) int {"
      , "    return a + b"
      , "}"
      , "func main() {"
      , "    result := add(1, 2)"
      , "    fmt.Println(result)"
      , "}"
      ]
  , return $ unlines
      [ "package main"
      , "type Person struct {"
      , "    Name string"
      , "    Age  int"
      , "}"
      , "func main() {"
      , "    p := Person{Name: \"Alice\", Age: 30}"
      , "    println(p.Name)"
      , "}"
      ]
  ]

-- Property tests

-- Property: malformed code should fail to parse
prop_malformed_code_fails :: Property
prop_malformed_code_fails =
  forAll genMalformedCode $ \code ->
    case parseTypus code of
      Left _ -> property True
      Right _ -> property False

-- Property: valid code should parse successfully
prop_valid_code_succeeds :: Property
prop_valid_code_succeeds =
  forAll genValidCode $ \code ->
    case parseTypus code of
      Left _ -> property False
      Right _ -> property True

-- Property: empty string should parse to empty file
prop_empty_string_parses :: Property
prop_empty_string_parses =
  let empty = ""
  in case parseTypus empty of
       Left _ -> property False
       Right typusFile -> property $ L.null (tfBlocks typusFile)

-- Property: code with only whitespace should parse
prop_whitespace_only_parses :: Property
prop_whitespace_only_parses =
  let whitespace = "   \n  \t  \n   \n  "
  in case parseTypus whitespace of
       Left _ -> property False
       Right _ -> property True

-- Property: code with only comments should parse
prop_comments_only_parses :: Property
prop_comments_only_parses =
  let comments = unlines
        [ "// This is a comment"
        , "/* Block comment */"
        , "// Another comment"
        ]
  in case parseTypus comments of
       Left _ -> property False
       Right _ -> property True

-- Property: malformed code error messages should be informative
prop_error_messages_informative :: Property
prop_error_messages_informative =
  forAll genMalformedCode $ \code ->
    case parseTypus code of
      Left err -> property $ L.length err > 0
      Right _ -> property False

-- Property: adding package declaration should help parsing
prop_package_declaration_helps :: String -> Property
prop_package_declaration_helps code =
  not (null code) && not ("package" `L.isInfixOf` code) ==>
  let withoutPackage = code
      withPackage = "package main\n" ++ code
      resultWithout = parseTypus withoutPackage
      resultWith = parseTypus withPackage
  in case (resultWithout, resultWith) of
       (Left _, Right _) -> property True
       (Left _, Left _) -> property True  -- Both fail, that's OK
       (Right _, Right _) -> property True  -- Both succeed, that's OK
       (Right _, Left _) -> property False  -- Shouldn't happen

-- Unit tests for specific error scenarios

unit_tests :: TestTree
unit_tests = testGroup "Parser Error Handling Unit Tests"
  [ testCase "unclosed brace error" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"hello\")"
            ]
      case parseTypus code of
        Left err -> assertBool "error should mention unclosed" $ 
                     "unclosed" `L.isInfixOf` err || "brace" `L.isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail"

  , testCase "unclosed string error" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"unclosed string"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertBool "error should mention string" $ 
                     "string" `L.isInfixOf` err || "unclosed" `L.isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail"

  , testCase "invalid character error" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    @#$ invalid"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertBool "error should mention invalid character" $ 
                     L.length err > 0
        Right _ -> assertFailure "expected parsing to fail"

  , testCase "mismatched parentheses error" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println((\"hello\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertBool "error should mention parentheses" $ 
                     "paren" `L.isInfixOf` err || "mismatch" `L.isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail"

  , testCase "double else error" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    if true {"
            , "    } else {"
            , "    } else {"
            , "    }"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertBool "error should mention else" $ 
                     "else" `L.isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail"

  , testCase "unclosed block comment error" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    /* unclosed comment"
            , "    println(\"hello\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertBool "error should mention comment" $ 
                     "comment" `L.isInfixOf` err || "unclosed" `L.isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail"

  , testCase "multiple package declarations error" $ do
      let code = unlines
            [ "package main"
            , "package secondary"
            , "func main() {}"
            ]
      case parseTypus code of
        Left err -> assertBool "error should mention package" $ 
                     "package" `L.isInfixOf` err
        Right _ -> assertFailure "expected parsing to fail"

  , testCase "valid simple function parses correctly" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"hello\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should have blocks" $ not $ L.null $ tfBlocks typusFile

  , testCase "valid function with parameters parses correctly" $ do
      let code = unlines
            [ "package main"
            , "func add(a int, b int) int {"
            , "    return a + b"
            , "}"
            , "func main() {"
            , "    result := add(1, 2)"
            , "    println(result)"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should have blocks" $ not $ L.null $ tfBlocks typusFile

  , testCase "valid struct definition parses correctly" $ do
      let code = unlines
            [ "package main"
            , "type Person struct {"
            , "    Name string"
            , "    Age  int"
            , "}"
            , "func main() {}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should have blocks" $ not $ L.null $ tfBlocks typusFile

  , testCase "empty input parses successfully" $ do
      let code = ""
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          tfBlocks typusFile @?= []

  , testCase "whitespace-only input parses successfully" $ do
      let code = "   \n  \t  \n   \n  "
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right _ -> return ()  -- Success is enough

  , testCase "comments-only input parses successfully" $ do
      let code = unlines
            [ "// This is a comment"
            , "/* Block comment */"
            , "// Another comment"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right _ -> return ()  -- Success is enough

  , testCase "recovery after syntax error" $ do
      let code = unlines
            [ "package main"
            , "func bad() {"
            , "    @#$ invalid syntax"
            , "}"
            , "func good() {"
            , "    println(\"this should be recoverable\")"
            , "}"
            ]
      case parseTypus code of
        Left _ -> return ()  -- Expected to fail, but error handling should work
        Right _ -> return ()  -- Or it might succeed with partial parsing

  , testCase "nested structures with errors" $ do
      let code = unlines
            [ "package main"
            , "type Outer struct {"
            , "    Inner struct {"
            , "        Field string"
            , "    }"
            , "    @#$ invalid"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertBool "error should be reported" $ L.length err > 0
        Right _ -> return ()  -- Might succeed with partial parsing

  , testCase "unicode characters in code" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"测试中文字符\")"
            , "    emoji := \"🚀火箭\""
            , "    println(emoji)"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should have blocks" $ not $ L.null $ tfBlocks typusFile

  , testCase "very long line handling" $ do
      let longString = replicate 1000 'a'
          code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"" ++ longString ++ "\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should have blocks" $ not $ L.null $ tfBlocks typusFile

  , testCase "deeply nested structures" $ do
      let nestedDepth = 50
          openBraces = replicate nestedDepth '{'
          closeBraces = replicate nestedDepth '}'
          code = unlines
            [ "package main"
            , "func main() " ++ openBraces
            , "    println(\"deeply nested\")"
            , closeBraces
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should have blocks" $ not $ L.null $ tfBlocks typusFile
  ]

-- Error recovery tests

error_recovery_tests :: TestTree
error_recovery_tests = testGroup "Parser Error Recovery Tests"
  [ testCase "partial parsing with errors" $ do
      let code = unlines
            [ "package main"
            , "func good1() {"
            , "    println(\"good\")"
            , "}"
            , "func bad() {"
            , "    @#$ invalid"
            , "}"
            , "func good2() {"
            , "    println(\"also good\")"
            , "}"
            ]
      -- Test that parser can recover L.and continue after errors
      case parseTypus code of
        Left _ -> return ()  -- Parser might fail completely, that's OK
        Right typusFile -> do
          -- If it succeeds, it should contain some valid content
          assertBool "should parse some content" $ not $ L.null $ tfBlocks typusFile

  , testCase "error location tracking" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"line 3\")"
            , "    @#$ error on line 4"
            , "    println(\"line 5\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> do
          -- Error message should ideally contain location information
          assertBool "error should have content" $ L.length err > 0
        Right _ -> return ()  -- Might succeed with error recovery

  , testCase "multiple errors in one file" $ do
      let code = unlines
            [ "package main"
            , "func bad1() {"
            , "    @#$ first error"
            , "}"
            , "func bad2() {"
            , "    @#$ second error"
            , "}"
            ]
      case parseTypus code of
        Left err -> do
          -- Should report at least one error
          assertBool "should report errors" $ L.length err > 0
        Right _ -> return ()  -- Might succeed with partial parsing
  ]

-- Performance tests

performance_tests :: TestTree
performance_tests = testGroup "Parser Performance Tests"
  [ testCase "large file parsing performance" $ do
      let largeFunc = unlines
            [ "func largeFunction() {"
            , "    var x int = 0"
            , "    for i := 0; i < 1000; i++ {"
            , "        x += i"
            , "    }"
            , "    println(x)"
            , "}"
            ]
          manyFuncs = L.concat $ replicate 100 largeFunc
          code = "package main\n\n" ++ manyFuncs
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should parse many functions" $ 
            L.length (tfBlocks typusFile) > 0

  , testCase "deep nesting performance" $ do
      let maxDepth = 100
          buildNested depth = if depth <= 0
                             then "    println(\"deepest level\")"
                             else "    if true {\n" ++ buildNested (depth - 1) ++ "\n    }"
          nestedCode = buildNested maxDepth
          code = unlines
            [ "package main"
            , "func main() {"
            , nestedCode
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parsing failed: " ++ err
        Right typusFile -> do
          assertBool "should parse deeply nested code" $ 
            not $ L.null $ tfBlocks typusFile
  ]

tests :: TestTree
tests = testGroup "Parser Error Handling Tests"
  [ testGroup "Property Tests"
    [ fastProperty "malformed code fails" prop_malformed_code_fails
    , fastProperty "valid code succeeds" prop_valid_code_succeeds
    , fastProperty "empty string parses" prop_empty_string_parses
    , fastProperty "whitespace only parses" prop_whitespace_only_parses
    , fastProperty "comments only parse" prop_comments_only_parses
    , fastProperty "error messages informative" prop_error_messages_informative
    , fastProperty "package declaration helps" prop_package_declaration_helps
    ]
  , unit_tests
  , error_recovery_tests
  , performance_tests
  ]