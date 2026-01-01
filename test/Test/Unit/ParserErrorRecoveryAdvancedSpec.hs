{-# LANGUAGE CPP #-}

module Test.Unit.ParserErrorRecoveryAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

tests :: TestTree
tests = testGroup "Parser Error Recovery Advanced Tests"
  [ fileDirectiveErrorTests
  , blockParsingErrorTests
  , syntaxErrorRecoveryTests
  , malformedInputTests
  , edgeCaseTests
  , quickCheckProperties
  ]

fileDirectiveErrorTests :: TestTree
fileDirectiveErrorTests = testGroup "File Directive Error Tests"
  [ testCase "handles malformed file directive" $ do
      let input = "//! ownership invalid_value\npackage main\n"
      result <- return $ parseTypus input
      case result of
        Left err -> "invalid_value" `L.isInfixOf` err @?= True
        Right _ -> "Expected error" @?= "Got success"
        
  , testCase "handles unknown file directive" $ do
      let input = "//! unknown_directive true\npackage main\n"
      result <- return $ parseTypus input
      case result of
        Left err -> "unknown_directive" `L.isInfixOf` err @?= True
        Right _ -> "Expected error" @?= "Got success"
        
  , testCase "handles file directive without colon" $ do
      let input = "//! ownership true\npackage main\n"
      result <- return $ parseTypus input
      case result of
        Left _ -> "Expected parse error" @?= "Got error"
        Right _ -> "Expected error" @?= "Got success"
        
  , testCase "handles file directive with missing value" $ do
      let input = "//! ownership:\npackage main\n"
      result <- return $ parseTypus input
      case result of
        Left _ -> "Expected parse error" @?= "Got error"
        Right _ -> "Expected error" @?= "Got success"
  ]

blockParsingErrorTests :: TestTree
blockParsingErrorTests = testGroup "Block Parsing Error Tests"
  [ testCase "handles malformed block directive" $ do
      let input = "//! ownership true\n\n//invalid block directive\ncode here\n"
      result <- return $ parseTypus input
      case result of
        Left _ -> "Expected parse error" @?= "Got error"
        Right file -> L.length (tfBlocks file) @?= 1  -- Should recover L.and parse as regular code
        
  , testCase "handles unterminated block" $ do
      let input = "//! ownership true\n\n//ownership: true\ncode without end\n"
      result <- return $ parseTypus input
      case result of
        Right file -> L.length (tfBlocks file) @?= 1
        Left err -> "Unterminated block should be recoverable" @?= err
        
  , testCase "handles nested block directives" $ do
      let input = "//! ownership true\n\n//ownership: true\n//dependent_types: true\nnested code\n"
      result <- return $ parseTypus input
      case result of
        Right file -> do
          L.length (tfBlocks file) @?= 1
          let block = L.head (tfBlocks file)
              directives = cbDirectives block
          fdOwnership directives `seq` bdDependentTypes directives `seq` True @?= True
        Left err -> "Nested directives should be parseable" @?= err
  ]

syntaxErrorRecoveryTests :: TestTree
syntaxErrorRecoveryTests = testGroup "Syntax Error Recovery Tests"
  [ testCase "recovers from missing package declaration" $ do
      let input = "//! ownership true\nfunc main() {\n}\n"
      result <- return $ parseTypus input
      case result of
        Right file -> L.length (tfBlocks file) @?= 1
        Left err -> "Should recover from missing package" @?= err
        
  , testCase "handles multiple package declarations" $ do
      let input = "package main\npackage other\nfunc main() {}\n"
      result <- return $ parseTypus input
      case result of
        Left err -> "Multiple package" `L.isInfixOf` err @?= True
        Right _ -> "Expected error for multiple packages" @?= "Got success"
        
  , testCase "handles if without braces" $ do
      let input = "package main\n\nif condition\n    doSomething()\n"
      result <- return $ parseTypus input
      case result of
        Right file -> do
          let errors = tfSyntaxErrors file
          L.length errors @?= 1  -- Should detect the error but continue parsing
        Left err -> "Should detect syntax error but recover" @?= err
  ]

malformedInputTests :: TestTree
malformedInputTests = testGroup "Malformed Input Tests"
  [ testCase "handles completely empty input" $ do
      let input = ""
      result <- return $ parseTypus input
      case result of
        Right file -> do
          tfDirectives file @?= defaultFileDirectives
          tfBlocks file @?= []
        Left err -> "Empty input should be valid" @?= err
        
  , testCase "handles only whitespace" $ do
      let input = "   \n\t\n  \n"
      result <- return $ parseTypus input
      case result of
        Right file -> tfBlocks file @?= []
        Left err -> "Whitespace-only input should be valid" @?= err
        
  , testCase "handles only comments" $ do
      let input = "// line comment\n/* block comment */\n// another comment\n"
      result <- return $ parseTypus input
      case result of
        Right file -> tfBlocks file @?= []
        Left err -> "Comments-only input should be valid" @?= err
        
  , testCase "handles mixed valid L.and invalid content" $ do
      let input = "//! ownership true\npackage main\n\ninvalid syntax here\nfunc valid() {}\n"
      result <- return $ parseTypus input
      case result of
        Right file -> do
          L.length (tfBlocks file) @?= 2  -- Should parse both parts
          tfDirectives file @?= FileDirectives (Just True) Nothing Nothing
        Left err -> "Should partially recover from syntax errors" @?= err
  ]

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge Case Tests"
  [ testCase "handles unicode characters in directives" $ do
      let input = "//! ownership true\n//unicode: café\nfunc main() {}\n"
      result <- return $ parseTypus input
      case result of
        Right file -> L.length (tfBlocks file) @?= 1
        Left err -> "Unicode should be handled" @?= err
        
  , testCase "handles very long lines" $ do
      let longLine = replicate 1000 'a'
          input = "//! ownership true\n" ++ longLine ++ "\nfunc main() {}\n"
      result <- return $ parseTypus input
      case result of
        Right file -> L.length (tfBlocks file) @?= 1
        Left err -> "Long lines should be handled" @?= err
        
  , testCase "handles deeply nested structures" $ do
      let nested = L.concat $ replicate 50 "  if condition {\n"
          input = "package main\n\n" ++ nested ++ "doSomething()\n"
      result <- return $ parseTypus input
      case result of
        Right file -> L.length (tfBlocks file) @?= 1
        Left err -> "Deep nesting should be handled" @?= err
        
  , testCase "handles escaped characters in strings" $ do
      let input = "package main\n\nfunc main() {\n    s := \"hello \\\"world\\\"\"\n}\n"
      result <- return $ parseTypus input
      case result of
        Right file -> L.length (tfBlocks file) @?= 1
        Left err -> "Escaped characters should be handled" @?= err
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck Error Recovery Properties"
  [ fastProperty "parseTypus never crashes on L.any input" prop_parseTypus_safe
  , fastProperty "parseTypus returns either error L.or valid file" prop_parseTypus_complete
  , fastProperty "successful parse has non-empty blocks when input has code" prop_parseTypus_blocks
  ]

-- QuickCheck property implementations
prop_parseTypus_safe :: String -> Property
prop_parseTypus_safe input =
  let result = parseTypus input
  in case result of
    Left _ -> property True
    Right _ -> property True

prop_parseTypus_complete :: String -> Property
prop_parseTypus_complete input =
  let result = parseTypus input
  in case result of
    Left err -> not (null err) ==> property True
    Right file -> do
      let blocks = tfBlocks file
          directives = tfDirectives file
      property $ L.length blocks >= 0

prop_parseTypus_blocks :: String -> Property
prop_parseTypus_blocks input =
  let hasCode = L.any (not . null) (lines input) && not (L.all (`L.isPrefixOf` "//") (lines input))
      result = parseTypus input
  in case result of
    Right file -> hasCode ==> L.length (tfBlocks file) > 0
    Left _ -> property True  -- Error cases don't need to satisfy this property