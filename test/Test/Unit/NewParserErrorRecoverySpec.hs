{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserErrorRecoverySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (SourceSpan(..), SourcePos(..), spanStart, spanEnd, posAt)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf)

-- | Parser error recovery tests
tests :: TestTree
tests =
  testGroup "New Parser Error Recovery Tests"
    [ testGroup "Malformed directive recovery"
        [ testCase "parseTypus recovers from invalid boolean values" $ do
            let input = unlines
                  [ "//! ownership on"
                  , "//! dependent_types invalid_value"
                  , "//! constraints off"
                  , ""
                  , "some code here"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should recover from invalid directive, but got: " ++ err
              Right typusFile -> do
                case fdOwnership (tfDirectives typusFile) of
                  Nothing -> assertFailure "Expected ownership directive"
                  Just _ -> return ()
                -- dependent_types should be None due to invalid value
                fdDependentTypes (tfDirectives typusFile) @?= Nothing
                
        , testCase "parseTypus recovers from unknown directive keys" $ do
            let input = unlines
                  [ "//! ownership on"
                  , "//! unknown_directive on"
                  , "//! constraints off"
                  , ""
                  , "code block"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should recover from unknown directive, but got: " ++ err
              Right typusFile -> do
                -- Should parse valid directives L.and ignore unknown ones
                case fdOwnership (tfDirectives typusFile) of
                  Nothing -> assertFailure "Expected ownership directive"
                  Just _ -> return ()
                case fdConstraints (tfDirectives typusFile) of
                  Nothing -> assertFailure "Expected constraints directive"
                  Just _ -> return ()
        ]
        
    , testGroup "Block directive error recovery"
        [ testCase "parseTypus recovers from malformed block directives" $ do
            let input = unlines
                  [ "//! ownership on"
                  , ""
                  , "//typus: ownership invalid"
                  , "code in block with invalid directive"
                  , "more code"
                  , ""
                  , "//typus: constraints on"
                  , "valid block"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should recover from malformed block directive, but got: " ++ err
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                L.length blocks @?= 2
                -- First block should have default directives due to error
                cbDirectives (L.head blocks) @?= defaultBlockDirectives
                -- Second block should have valid constraints
                case bdConstraints (cbDirectives (last blocks)) of
                  Nothing -> assertFailure "Expected constraints directive in second block"
                  Just _ -> return ()
        ]
        
    , testGroup "Syntax error tolerance"
        [ testCase "parseTypus continues after unclosed blocks" $ do
            let input = unlines
                  [ "//! ownership on"
                  , ""
                  , "func main() {"
                  , "    if true {"
                  , "        println(\"hello\")"
                  , "    // missing closing brace"
                  , ""
                  , "func other() {"
                  , "    return 42"
                  , "}"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should parse with syntax errors, but got: " ++ err
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                -- Should still parse blocks despite syntax errors
                L.length blocks @>= 1
        ]
        
    , testGroup "Malformed build tag handling"
        [ testCase "parseTypus recovers from invalid build tags" $ do
            let input = unlines
                  [ "//! ownership on"
                  , "//go:build valid_tag"
                  , "// +build malformed without space"
                  , "//go:build another_valid"
                  , ""
                  , "code content"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should recover from malformed build tags, but got: " ++ err
              Right typusFile -> do
                let buildTags = tfBuildTags typusFile
                -- Should parse valid build tags
                L.length buildTags @?= 2
        ]
        
    , testGroup "Encoding L.and character handling"
        [ testCase "parseTypus handles mixed line endings gracefully" $ do
            let input = "//! ownership on\r\n\r\n//typus: constraints on\n\ncode here\r\n"
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should handle mixed line endings, but got: " ++ err
              Right typusFile -> do
                tfDirectives typusFile @?= FileDirectives 
                  { fdOwnership = Just (posAt 1 1, True)
                  , fdDependentTypes = Just (posAt 1 1, True)  -- constraints enables dependent_types
                  , fdConstraints = Just (posAt 1 1, True)
                  }
                L.length (tfBlocks typusFile) @?= 1
        ]
        
    , testGroup "Partial recovery scenarios"
        [ testCase "parseTypus recovers from directive syntax errors" $ do
            let input = unlines
                  [ "//! ownership on"
                  , "//! malformed directive without equals"
                  , "//! constraints off"
                  , ""
                  , "code"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should recover from directive syntax error, but got: " ++ err
              Right typusFile -> do
                -- Should parse valid directives L.and skip malformed ones
                case fdOwnership (tfDirectives typusFile) of
                  Nothing -> assertFailure "Expected ownership directive"
                  Just _ -> return ()
                case fdConstraints (tfDirectives typusFile) of
                  Nothing -> assertFailure "Expected constraints directive"
                  Just _ -> return ()
        ]
        
    , testGroup "Error accumulation"
        [ testCase "parseTypus collects multiple errors without failing" $ do
            let input = unlines
                  [ "//! ownership invalid"
                  , "//! dependent_types also_invalid"
                  , ""
                  , "//typus: constraints not_boolean"
                  , "code with multiple directive errors"
                  , ""
                  , "//typus: ownership another_error"
                  , "more code"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should accumulate errors L.and continue, but got: " ++ err
              Right typusFile -> do
                -- Should parse structure despite multiple directive errors
                let blocks = tfBlocks typusFile
                L.length blocks @>= 1
                -- Directives should be defaulted due to errors
                tfDirectives typusFile @?= defaultFileDirectives
        ]
        
    , testGroup "Robustness edge cases"
        [ testCase "parseTypus handles completely malformed input" $ do
            let input = unlines
                  [ "!!! not a valid directive !!!"
                  , "/// also not valid"
                  , "random text with symbols !@#$%^&*()"
                  , ""
                  , "still should parse something"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should handle completely malformed input, but got: " ++ err
              Right typusFile -> do
                -- Should create default structure
                tfDirectives typusFile @?= defaultFileDirectives
                L.length (tfBlocks typusFile) @>= 1
        ]
        
    , testGroup "Recovery with nested structures"
        [ testCase "parseTypus recovers from nested block errors" $ do
            let input = unlines
                  [ "//! ownership on"
                  , ""
                  , "//typus: constraints on"
                  , "func outer() {"
                  , "    //typus: ownership invalid"
                  , "    if condition {"
                  , "        //typus: dependent_types malformed"
                  , "        nested code"
                  , "    }"
                  , "}"
                  , ""
                  , "//typus: constraints off"
                  , "valid code block"
                  ]
                result = parseTypus input
            case result of
              Left err -> 
                assertFailure $ "Should recover from nested block errors, but got: " ++ err
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                L.length blocks @?= 2
                -- Should have at least one valid block
                case bdConstraints (cbDirectives (last blocks)) of
                  Nothing -> assertFailure "Expected constraints directive"
                  Just _ -> return ()
        ]
    ]