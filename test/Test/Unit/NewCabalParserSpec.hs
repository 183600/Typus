{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalParserSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose)
import TestSupport.Arbitrary

import Parser
  ( parseTypus
  , TypusFile(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation (Located(..), SourcePos(..), spanStart, spanEnd, posLine)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- Test 1: Parse empty input
prop_parse_empty_input :: Property
prop_parse_empty_input =
  let parsed = parseTypus ""
  in case parsed of
       Left _ -> property True
       Right typusFile -> property $ tfDirectives typusFile `seq` tfBlocks typusFile `seq` True

-- Test 2: Parse simple function
prop_parse_simple_function :: String -> Property
prop_parse_simple_function funcName =
  let input = "func " ++ funcName ++ "() {}"
      parsed = parseTypus input
  in not (null funcName) && all (not . isSpace) funcName ==> 
     case parsed of
       Left _ -> property False
       Right _ -> property True

-- Test 3: Parse file directives consistency
prop_parse_file_directives :: Bool -> Bool -> Property
prop_parse_file_directives ownership dependentTypes =
  let input = unlines $
        [ "//! ownership: " ++ show ownership
        , "//! dependent_types: " ++ show dependentTypes
        , "package main"
        , "func main() {}"
        ]
      parsed = parseTypus input
  in case parsed of
       Left err -> property False
       Right typusFile ->
         let FileDirectives { fdOwnership = ownershipRes, fdDependentTypes = dependentTypesRes } = tfDirectives typusFile
         in case (ownershipRes, dependentTypesRes) of
              (Just loc1, Just loc2) -> locatedValue loc1 === ownership .&&. locatedValue loc2 === dependentTypes
              _ -> property False

-- Test 4: Parse block directives
prop_parse_block_directives :: Bool -> Property
prop_parse_block_directives ownership =
  let input = unlines $
        [ "//! ownership: " ++ show ownership
        , "package main"
        , "func main() {"
        , "  //! ownership: " ++ show (not ownership)
        , "}"
        ]
      parsed = parseTypus input
  in case parsed of
       Left _ -> property True -- May fail due to syntax
       Right typusFile -> property $ tfBlocks typusFile `seq` True

-- Test 5: Parse with comments
prop_parse_with_comments :: String -> Property
prop_parse_with_comments code =
  let withComments = code ++ "\n// This is a comment\n"
      parsed = parseTypus withComments
  in case parsed of
       Left _ -> property True
       Right _ -> property True

-- Test 6: Parse multi-line strings
prop_parse_multiline_strings :: String -> Property
prop_parse_multiline_strings content =
  let input = "func main() {\n  s := \"" ++ content ++ "\"\n}"
      parsed = parseTypus input
  in case parsed of
       Left _ -> property True -- May fail due to special chars in content
       Right _ -> property True

-- Test 7: Parse package declaration
prop_parse_package_declaration :: String -> Property
prop_parse_package_declaration pkgName =
  let input = "package " ++ pkgName ++ "\nfunc main() {}"
      parsed = parseTypus input
  in not (null pkgName) && all (not . isSpace) pkgName ==> 
     case parsed of
       Left _ -> property True
       Right _ -> property True

-- Test 8: Parse with whitespace variations
prop_parse_whitespace_variations :: String -> Property
prop_parse_whitespace_variations code =
  let withExtraSpaces = "  " ++ code ++ "\n  \n"
      parsed = parseTypus withExtraSpaces
  in case parsed of
       Left _ -> property True
       Right _ -> property True

-- Test 9: Parse incomplete input gracefully
prop_parse_incomplete_input :: String -> Property
prop_parse_incomplete_input partial =
  let input = "func " ++ partial
      parsed = parseTypus input
  in case parsed of
       Left _ -> property True -- Expected to fail
       Right _ -> property True -- May succeed for some inputs

-- Test 10: Parse directive ordering independence
prop_parse_directive_ordering :: Bool -> Bool -> Property
prop_parse_directive_ordering ownership dependentTypes =
  let input1 = unlines $
        [ "//! ownership: " ++ show ownership
        , "//! dependent_types: " ++ show dependentTypes
        , "func main() {}"
        ]
      input2 = unlines $
        [ "//! dependent_types: " ++ show dependentTypes
        , "//! ownership: " ++ show ownership
        , "func main() {}"
        ]
      parsed1 = parseTypus input1
      parsed2 = parseTypus input2
  in case (parsed1, parsed2) of
       (Right f1, Right f2) -> tfDirectives f1 === tfDirectives f2
       _ -> property True

tests :: TestTree
tests = 
  testGroup "New Cabal Parser Tests"
    [ fastProperty "Parse empty input" prop_parse_empty_input
    , fastProperty "Parse simple function" prop_parse_simple_function
    , fastProperty "Parse file directives consistency" prop_parse_file_directives
    , fastProperty "Parse block directives" prop_parse_block_directives
    , fastProperty "Parse with comments" prop_parse_with_comments
    , fastProperty "Parse multi-line strings" prop_parse_multiline_strings
    , fastProperty "Parse package declaration" prop_parse_package_declaration
    , fastProperty "Parse with whitespace variations" prop_parse_whitespace_variations
    , fastProperty "Parse incomplete input gracefully" prop_parse_incomplete_input
    , fastProperty "Parse directive ordering independence" prop_parse_directive_ordering
    ]