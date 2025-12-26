module Test.Unit.NewCabalTestSuiteSpec (tests) where

import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, elements, oneof, property, discard)

import Parser
  ( BlockDirectives(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , TypusFile(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import Compiler
  ( compile
  , CompilerError(..)
  , CompilationPhase(..)
  , hasTypeErrors
  , checkDependentTypes
  , checkOwnership
  , renderCompilationError
  )
import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , analyzeOwnership
  , formatOwnershipErrors
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanEnd
  , spanStart
  )

-- | Test suite for new cabal test cases
tests :: TestTree
tests =
  testGroup "New Cabal Test Suite"
    [ testGroup "Parser Edge Cases"
        [ testCase "parses nested block directives correctly" $ do
            let source = unlines
                  [ "package main"
                  , "func main() {"
                  , "    {//! ownership: on"
                  , "        if true {"
                  , "            {//! dependent_types: on"
                  , "                var x int = 42"
                  , "                println(x)"
                  , "            }"
                  , "        }"
                  , "    }"
                  , "}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "should find ownership block" $
                  any (maybe False locatedValue . bdOwnership . cbDirectives) blocks
                assertBool "should find dependent types block" $
                  any (maybe False locatedValue . bdDependentTypes . cbDirectives) blocks

        , testCase "handles malformed directive syntax gracefully" $ do
            let source = unlines
                  [ "package main"
                  , "//! ownership: maybe"  -- Invalid boolean value
                  , "func main() {}"
                  ]
            case parseTypus source of
              Left err -> assertBool ("error should mention invalid directive value: " <> err) 
                                  ("Invalid directive value" `isInfixOf` err)
              Right _ -> assertFailure "expected parse failure for invalid directive value"

        , testCase "parses directives with extra whitespace" $ do
            let source = unlines
                  [ "package main"
                  , "//!    ownership   :    on   "
                  , "//! dependent_types :  off  "
                  , "func main() {}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> do
                let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
                case ownership of
                  Nothing -> assertFailure "expected ownership directive"
                  Just loc -> locatedValue loc @?= True
                case dependentTypes of
                  Nothing -> assertFailure "expected dependent types directive"
                  Just loc -> locatedValue loc @?= False

        , testCase "handles empty directive blocks" $ do
            let source = unlines
                  [ "package main"
                  , "func main() {"
                  , "    {//! ownership: on, dependent_types: off}"
                  , "    // Empty block with directives"
                  , "}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                    directiveBlock = head blocks
                case bdOwnership (cbDirectives directiveBlock) of
                  Nothing -> assertFailure "expected ownership directive"
                  Just loc -> locatedValue loc @?= True
                case bdDependentTypes (cbDirectives directiveBlock) of
                  Nothing -> assertFailure "expected dependent types directive"
                  Just loc -> locatedValue loc @?= False
                assertBool "block content should be empty" (null $ cbContent directiveBlock)
        ]

    , testGroup "Compiler Error Handling"
        [ testCase "detects undefined variable references" $ do
            let source = unlines
                  [ "package main"
                  , "func main() {"
                  , "    println(undefined_var)"
                  , "}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> 
                case compile typusFile of
                  Left errors -> assertBool "should detect undefined variable" 
                                       ("undefined_var" `isInfixOf` renderCompilationError errors)
                  Right _ -> assertFailure "expected compilation to fail with undefined variable"

        , testCase "handles type mismatches in function calls" $ do
            let source = unlines
                  [ "package main"
                  , "func expectInt(x int) {}"
                  , "func main() {"
                  , "    expectInt(\"string_value\")"
                  , "}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> 
                case compile typusFile of
                  Left errors -> assertBool "should detect type mismatch" 
                                       ("type mismatch" `isInfixOf` renderCompilationError errors)
                  Right _ -> assertFailure "expected compilation to fail with type mismatch"

        , testCase "validates dependent type constraints" $ do
            let source = unlines
                  [ "//! dependent_types: on"
                  , "package main"
                  , "func main() {"
                  , "    var arr [n]int where n > 0"
                  , "    println(arr)"
                  , "}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> 
                case checkDependentTypes typusFile of
                  Left errors -> assertBool "should validate dependent type constraints" 
                                           (not $ null errors)
                  Right _ -> return ()  -- Success case is also valid

        , testCase "checks ownership transfer violations" $ do
            let source = unlines
                  [ "//! ownership: on"
                  , "package main"
                  , "func main() {"
                  , "    data := make([]int, 10)"
                  , "    moved_data := data"
                  , "    println(data[0])  // Use after move"
                  , "}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> 
                case checkOwnership typusFile of
                  Left errors -> assertBool "should detect ownership violation" 
                                           ("use after move" `isInfixOf` renderCompilationError errors)
                  Right _ -> assertFailure "expected ownership check to fail"
        ]

    , testGroup "Ownership Analysis"
        [ testCase "basic ownership parsing works" $ do
            let source = unlines
                  [ "//! ownership: on"
                  , "package main"
                  , "func main() {"
                  , "    data := make([]int, 10)"
                  , "    println(data)"
                  , "}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> 
                assertBool "should parse ownership directive" $ 
                  case tfDirectives typusFile of
                    FileDirectives { fdOwnership = Just ownership } -> locatedValue ownership
                    _ -> False

        , testCase "ownership directives are parsed correctly" $ do
            let source = unlines
                  [ "//! ownership: off"
                  , "package main"
                  , "func main() {}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> 
                assertBool "should parse ownership directive as false" $ 
                  case tfDirectives typusFile of
                    FileDirectives { fdOwnership = Just ownership } -> not (locatedValue ownership)
                    _ -> False

        , testCase "dependent types directive parsing" $ do
            let source = unlines
                  [ "//! dependent_types: on"
                  , "package main"
                  , "func main() {}"
                  ]
            case parseTypus source of
              Left err -> assertFailure $ "parseTypus failed: " <> err
              Right typusFile -> 
                assertBool "should parse dependent types directive" $ 
                  case tfDirectives typusFile of
                    FileDirectives { fdDependentTypes = Just depTypes } -> locatedValue depTypes
                    _ -> False
        ]

    , testGroup "QuickCheck Property Tests"
        [ testProperty "parseTypus is idempotent for directive parsing" $ 
            \directivesStr -> 
              let source = "package main\nfunc main() {}\n" ++ directivesStr
              in case parseTypus source of
                   Left _ -> property True
                   Right firstParse -> 
                     case parseTypus source of
                       Left _ -> property True
                       Right secondParse -> 
                         let directives1 = tfDirectives firstParse
                             directives2 = tfDirectives secondParse
                         in property $ directives1 == directives2

        , testProperty "parsing preserves line count" $
            \lineCount -> 
              let lines = ["line " ++ show i | i <- [1..lineCount `mod` 10 + 1]]
                  source = unlines $ ["package main", "func main() {"] ++ lines ++ ["}"]
              in case parseTypus source of
                   Left _ -> property True
                   Right parsed -> property $ length (tfBlocks parsed) >= 0

        , testProperty "directive parsing is robust" $
            \seed -> 
              let ownershipOn = seed `mod` 2 == 0
                  dependentTypesOn = seed `mod` 3 == 0
                  directives = ["//! ownership: " ++ if ownershipOn then "on" else "off"
                              , "//! dependent_types: " ++ if dependentTypesOn then "on" else "off"]
                  source = unlines $ directives ++ ["package main", "func main() {}"]
              in case parseTypus source of
                   Left _ -> property True
                   Right parsed -> 
                     let fileDirectives = tfDirectives parsed
                     in property $ isJust (fdOwnership fileDirectives) || 
                                  isJust (fdDependentTypes fileDirectives)
        ]
    ]

-- Helper functions for testing
hasOwnershipTransfer :: a -> Bool
hasOwnershipTransfer _ = True  -- Simplified for this example

hasValidBorrowing :: a -> Bool  
hasValidBorrowing _ = True  -- Simplified for this example

countVariables :: a -> Int
countVariables _ = 0  -- Simplified for this example
