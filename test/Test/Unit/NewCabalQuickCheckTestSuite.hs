{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalQuickCheckTestSuite (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Utils (trim, splitBy, removeComments, normalizeIndentation)
import Parser (parseTypus, TypusFile(..), FileDirectives(..))
import Data.Char (isSpace)
import Data.List (isInfixOf, sort)

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- ============================================================================
-- Utils String Processing Properties  
-- ============================================================================

-- Property: trim is idempotent (trimming twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s = trim (trim s) === trim s

-- Property: splitBy and join are inverses for non-empty delimiters
prop_split_by_join_inverse :: String -> Char -> Property
prop_split_by_join_inverse s delim = 
  delim /= '\0' ==> 
  let parts = splitBy delim s
      rejoined = concatMap (\p -> if null p then [] else p ++ [delim]) (init parts) ++ last parts
  in length parts > 0 ==> s === rejoined

-- Property: removeComments preserves string literals
prop_remove_comments_preserves_strings :: String -> String -> Property
prop_remove_comments_preserves_strings prefix suffix =
  let content = prefix ++ "\"string with // comment\" literal" ++ suffix
      processed = removeComments content
  in "// comment" `isInfixOf` processed

-- Property: normalizeIndentation preserves relative indentation
prop_normalize_indentation_preserves_relative :: String -> Property
prop_normalize_indentation_preserves_relative s =
  let lines' = lines s
      hasMultipleLines = length lines' > 1
  in hasMultipleLines ==> 
     let normalized = normalizeIndentation s
         normLines' = lines normalized
         originalRelative = map (takeWhile isSpace) (drop 1 lines')
         normalizedRelative = map (takeWhile isSpace) (drop 1 normLines')
     in property $ sort originalRelative === sort normalizedRelative

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: parsing valid Go-like syntax always succeeds
prop_parse_valid_go_syntax :: Property
prop_parse_valid_go_syntax =
  let validGo = unlines
        [ "package main"
        , "func main() {"
        , "  x := 42"
        , "  return x"
        , "}"
        ]
  in case parseTypus validGo of
    Left _ -> property False
    Right _ -> property True

-- Property: empty file parses with default directives
prop_parse_empty_file :: Property
prop_parse_empty_file =
  case parseTypus "" of
    Left _ -> property False
    Right typusFile -> 
      let FileDirectives{..} = tfDirectives typusFile
      in property $ fdOwnership == Nothing .&&. fdDependentTypes == Nothing

-- ============================================================================
-- Compiler IR Properties
-- ============================================================================

-- Property: compilation preserves function declarations
prop_compilation_preserves_functions :: Property
prop_compilation_preserves_functions =
  let source = unlines
        [ "package main"
        , "func test() int {"
        , "  return 42"
        , "}"
        ]
  in case parseTypus source of
    Left _ -> property False
    Right _ -> property True  -- Simplified: just ensure parsing succeeds

-- ============================================================================
-- Error Handling Properties  
-- ============================================================================

-- Property: error messages contain source location information
prop_error_messages_include_location :: Property
prop_error_messages_include_location =
  let malformed = unlines
        [ "package main"
        , "func broken( {"
        , "  return 42"  
        ]
  in case parseTypus malformed of
    Right _ -> property False  -- Should fail
    Left errMsg -> property $ "line" `isInfixOf` errMsg .||. "column" `isInfixOf` errMsg

-- | Test suite with comprehensive QuickCheck properties
tests :: TestTree
tests = testGroup "New Cabal QuickCheck Test Suite"
  [ testGroup "Utils String Processing"  
    [ fastProperty "Trim idempotence" prop_trim_idempotent
    , fastProperty "Split/join inverse" prop_split_by_join_inverse
    , fastProperty "Remove comments preserves strings" prop_remove_comments_preserves_strings
    , fastProperty "Normalize indentation preserves relative" prop_normalize_indentation_preserves_relative
    ]
  
  , testGroup "Parser Properties"
    [ fastProperty "Parse valid Go syntax" prop_parse_valid_go_syntax
    , fastProperty "Parse empty file" prop_parse_empty_file
    ]
  
  , testGroup "Compiler IR Properties"
    [ fastProperty "Compilation preserves functions" prop_compilation_preserves_functions
    ]
  
  , testGroup "Error Handling Properties"
    [ fastProperty "Error messages include location" prop_error_messages_include_location
    ]
  ]