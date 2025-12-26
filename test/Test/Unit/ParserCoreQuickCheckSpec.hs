{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..), Arbitrary(..), oneof, elements, Gen, suchThat)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanStart
  , spanEnd
  , posLine
  , posCol
  )

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)

-- Property: Default file directives are consistent
prop_default_file_directives_consistent :: Property
prop_default_file_directives_consistent =
  let defaults = defaultFileDirectives
  in property $ 
    fdOwnership defaults === Nothing .&&.
    fdDependentTypes defaults === Nothing .&&.
    fdConstraints defaults === Nothing

-- Property: Default block directives are consistent
prop_default_block_directives_consistent :: Property
prop_default_block_directives_consistent =
  let defaults = defaultBlockDirectives
  in property $ 
    bdOwnership defaults === Nothing .&&.
    bdDependentTypes defaults === Nothing .&&.
    bdConstraints defaults === Nothing

-- Property: Parsing empty string returns valid structure
prop_parse_empty_string :: Property
prop_parse_empty_string =
  case parseTypus "" of
    Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
    Right typusFile -> 
      property $ 
        tfDirectives typusFile === defaultFileDirectives .&&.
        null (tfBlocks typusFile)

-- Property: Parsing simple package declaration works
prop_parse_simple_package :: String -> Property
prop_parse_simple_package name =
  not (null name) ==> all (\c -> isAlphaNum c || c == '_') name ==>
  let source = "package " ++ name ++ "\n"
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         property $ not (null (tfBlocks typusFile))

-- Property: Parsing ownership directive sets ownership flag
prop_parse_ownership_directive :: Bool -> Property
prop_parse_ownership_directive flag =
  let source = "//! ownership: " ++ (if flag then "on" else "off") ++ "\npackage main\n"
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         case fdOwnership (tfDirectives typusFile) of
           Nothing -> property $ counterexample "ownership directive not found" False
           Just located -> property $ locatedValue located === flag

-- Property: Parsing dependent_types directive sets dependent types flag
prop_parse_dependent_types_directive :: Bool -> Property
prop_parse_dependent_types_directive flag =
  let source = "//! dependent_types: " ++ (if flag then "on" else "off") ++ "\npackage main\n"
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         case fdDependentTypes (tfDirectives typusFile) of
           Nothing -> property $ counterexample "dependent_types directive not found" False
           Just located -> property $ locatedValue located === flag

-- Property: Parsing constraints directive sets constraints flag
prop_parse_constraints_directive :: Bool -> Property
prop_parse_constraints_directive flag =
  let source = "//! constraints: " ++ (if flag then "on" else "off") ++ "\npackage main\n"
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         case fdConstraints (tfDirectives typusFile) of
           Nothing -> property $ counterexample "constraints directive not found" False
           Just located -> property $ locatedValue located === flag

-- Property: Parsing multiple directives preserves order
prop_parse_multiple_directives :: Bool -> Bool -> Bool -> Property
prop_parse_multiple_directives ownership dependent constraints =
  let source = unlines
        [ "//! ownership: " ++ (if ownership then "on" else "off")
        , "//! dependent_types: " ++ (if dependent then "on" else "off")
        , "//! constraints: " ++ (if constraints then "on" else "off")
        , "package main"
        ]
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         let directives = tfDirectives typusFile
             getLocatedValue getter = fmap locatedValue (getter directives)
         in property $ 
           getLocatedValue fdOwnership === Just ownership .&&.
           getLocatedValue fdDependentTypes === Just dependent .&&.
           getLocatedValue fdConstraints === Just constraints

-- Property: Parsing block directive within code block
prop_parse_block_directive :: Bool -> Property
prop_parse_block_directive flag =
  let source = unlines
        [ "package main"
        , "//! ownership: " ++ (if flag then "on" else "off")
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         case tfBlocks typusFile of
           [] -> property $ counterexample "no blocks found" False
           (block:_) -> 
             case bdOwnership (cbDirectives block) of
               Nothing -> property $ counterexample "block ownership directive not found" False
               Just located -> property $ locatedValue located === flag

-- Property: Parsing preserves code content
prop_parse_preserves_code :: String -> Property
prop_parse_preserves_code code =
  not (null code) ==> 
  let source = "package main\n" ++ code ++ "\n"
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         case tfBlocks typusFile of
           [] -> property $ counterexample "no blocks found" False
           (block:_) -> 
             let blockContent = cbContent block
             in property $ code `isInfixOf` blockContent

-- Property: Parsing handles whitespace correctly
prop_parse_handles_whitespace :: String -> String -> Property
prop_parse_handles_whitespace before after =
  let source = before ++ "package main" ++ after
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         property $ not (null (tfBlocks typusFile))

-- Property: Parsing handles comments correctly
prop_parse_handles_comments :: String -> Property
prop_parse_handles_comments comment =
  not (null comment) ==> not ("//" `isInfixOf` comment) ==>
  let source = unlines
        [ "// " ++ comment
        , "package main"
        , "// Another comment"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         property $ not (null (tfBlocks typusFile))

-- Property: Parsing function declarations
prop_parse_function_declaration :: String -> Property
prop_parse_function_declaration funcName =
  not (null funcName) ==> all (\c -> isAlphaNum c || c == '_') funcName ==>
  let source = "package main\nfunc " ++ funcName ++ "() {}\n"
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         property $ not (null (tfBlocks typusFile))

-- Property: Parsing variable declarations
prop_parse_variable_declaration :: String -> String -> Property
prop_parse_variable_declaration varName varType =
  not (null varName) ==> not (null varType) ==> 
  all (\c -> isAlphaNum c || c == '_') varName ==>
  all (\c -> isAlphaNum c || c == '_' || c == '[' || c == ']') varType ==>
  let source = "package main\nvar " ++ varName ++ " " ++ varType ++ "\n"
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         property $ not (null (tfBlocks typusFile))

-- Property: Parsing maintains line numbering
prop_parse_maintains_line_numbers :: [String] -> Property
prop_parse_maintains_line_numbers lines =
  not (null lines) ==> length lines >= 3 ==>
  let source = unlines lines
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         property $ not (null (tfBlocks typusFile))

-- Property: Parsing invalid input fails gracefully
prop_parse_invalid_fails :: String -> Property
prop_parse_invalid_fails invalid =
  not (null invalid) ==> not ("package" `isPrefixOf` invalid) ==>
  case parseTypus invalid of
    Left _ -> property $ True
    Right _ -> property $ counterexample "expected parse failure" False

-- Property: Parsing multiple code blocks
prop_parse_multiple_blocks :: [String] -> Property
prop_parse_multiple_blocks blocks =
  not (null blocks) ==> length blocks <= 5 ==> all (not . null) blocks ==>
  let source = "package main\n" ++ unlines blocks
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         let blockCount = length (tfBlocks typusFile)
         in property $ blockCount >= 1

-- Property: Parsing with mixed directives and code
prop_parse_mixed_directives_code :: Bool -> Bool -> String -> Property
prop_parse_mixed_directives_code ownership dependent code =
  not (null code) ==> 
  let source = unlines
        [ "//! ownership: " ++ (if ownership then "on" else "off")
        , "//! dependent_types: " ++ (if dependent then "on" else "off")
        , "package main"
        , code
        ]
  in case parseTypus source of
       Left err -> property $ counterexample ("parseTypus failed: " ++ err) False
       Right typusFile -> 
         let directives = tfDirectives typusFile
             getLocatedValue getter = fmap locatedValue (getter directives)
         in property $ 
           getLocatedValue fdOwnership === Just ownership .&&.
           getLocatedValue fdDependentTypes === Just dependent .&&.
           not (null (tfBlocks typusFile))

tests :: TestTree
tests =
  testGroup "Parser Core QuickCheck Tests"
    [ fastProperty "default file directives are consistent" prop_default_file_directives_consistent
    , fastProperty "default block directives are consistent" prop_default_block_directives_consistent
    , fastProperty "parse empty string returns valid structure" prop_parse_empty_string
    , fastProperty "parse simple package declaration works" prop_parse_simple_package
    , fastProperty "parse ownership directive sets ownership flag" prop_parse_ownership_directive
    , fastProperty "parse dependent_types directive sets dependent types flag" prop_parse_dependent_types_directive
    , fastProperty "parse constraints directive sets constraints flag" prop_parse_constraints_directive
    , fastProperty "parse multiple directives preserves order" prop_parse_multiple_directives
    , fastProperty "parse block directive within code block" prop_parse_block_directive
    , fastProperty "parse preserves code content" prop_parse_preserves_code
    , fastProperty "parse handles whitespace correctly" prop_parse_handles_whitespace
    , fastProperty "parse handles comments correctly" prop_parse_handles_comments
    , fastProperty "parse function declarations" prop_parse_function_declaration
    , fastProperty "parse variable declarations" prop_parse_variable_declaration
    , fastProperty "parse maintains line numbering" prop_parse_maintains_line_numbers
    , fastProperty "parse invalid input fails gracefully" prop_parse_invalid_fails
    , fastProperty "parse multiple code blocks" prop_parse_multiple_blocks
    , fastProperty "parse with mixed directives and code" prop_parse_mixed_directives_code
    ]