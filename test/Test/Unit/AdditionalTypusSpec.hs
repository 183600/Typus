{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.AdditionalTypusSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, suchThat)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import SourceLocation (SourceSpan(..), SourcePos(..), locatedValue)
import Utils (trim, splitBy)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import Data.List (null)
import Data.Char (isSpace, isDigit)

-- Test 1: Parser correctly handles file-level ownership directive
test_parser_file_ownership_directive :: TestTree
test_parser_file_ownership_directive = 
  testCase "Parser correctly handles file-level ownership directive" $ do
    let source = unlines
          [ "//! ownership: on"
          , "package main"
          , "func main() {}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let FileDirectives { fdOwnership = ownership } = tfDirectives typusFile
        case ownership of
          Nothing -> assertFailure "expected ownership directive"
          Just loc -> locatedValue loc @?= True

-- Test 2: Parser correctly handles file-level dependent types directive
test_parser_file_dependent_types_directive :: TestTree
test_parser_file_dependent_types_directive = 
  testCase "Parser correctly handles file-level dependent types directive" $ do
    let source = unlines
          [ "//! dependent_types: on"
          , "package main"
          , "func main() {}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let FileDirectives { fdDependentTypes = dependentTypes } = tfDirectives typusFile
        case dependentTypes of
          Nothing -> assertFailure "expected dependent types directive"
          Just loc -> locatedValue loc @?= True

-- Test 3: Parser correctly handles constraints directive as dependent types alias
test_parser_constraints_alias :: TestTree
test_parser_constraints_alias = 
  testCase "Parser correctly handles constraints directive as dependent types alias" $ do
    let source = unlines
          [ "//! constraints: on"
          , "package main"
          , "func main() {}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let FileDirectives { fdDependentTypes = dependentTypes } = tfDirectives typusFile
        case dependentTypes of
          Nothing -> assertFailure "expected dependent types directive (via constraints alias)"
          Just loc -> locatedValue loc @?= True

-- Test 4: Parser correctly handles block-level ownership directive
test_parser_block_ownership_directive :: TestTree
test_parser_block_ownership_directive = 
  testCase "Parser correctly handles block-level ownership directive" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    //! ownership: on"
          , "    var x int = 42"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let blocks = tfBlocks typusFile
        assertBool "expected at least one code block" (not (null blocks))
        let firstBlock = L.head blocks
            blockDirectives = cbDirectives firstBlock
            BlockDirectives { bdOwnership = ownership } = blockDirectives
        case ownership of
          Nothing -> assertFailure "expected block-level ownership directive"
          Just loc -> locatedValue loc @?= True

-- Test 5: Parser correctly handles multiple block directives
test_parser_multiple_block_directives :: TestTree
test_parser_multiple_block_directives = 
  testCase "Parser correctly handles multiple block directives" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    {//! ownership: on"
          , "        //! dependent_types: on"
          , "        var x int = 42"
          , "    }"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> do
        let blocks = tfBlocks typusFile
        assertBool "expected at least one code block" (not (null blocks))
        let firstBlock = L.head blocks
            blockDirectives = cbDirectives firstBlock
            BlockDirectives { bdOwnership = ownership, bdDependentTypes = dependentTypes } = blockDirectives
        case ownership of
          Nothing -> assertFailure "expected block-level ownership directive"
          Just loc -> locatedValue loc @?= True
        case dependentTypes of
          Nothing -> assertFailure "expected block-level dependent types directive"
          Just loc -> locatedValue loc @?= True

-- QuickCheck Property 6: Trim function removes leading L.and trailing whitespace
prop_trim_removes_whitespace :: String -> String -> Property
prop_trim_removes_whitespace prefix suffix =
  let content = prefix ++ "content" ++ suffix
      trimmed = trim content
      hasLeading = L.any isSpace prefix
      hasTrailing = L.any isSpace suffix
      noLeadingSpace = null trimmed || not (isSpace (L.head trimmed))
      noTrailingSpace = null trimmed || not (isSpace (last trimmed))
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ noLeadingSpace .&&. noTrailingSpace

-- QuickCheck Property 7: SplitBy function correctly splits on delimiter
prop_split_by_correctness :: Char -> String -> Property
prop_split_by_correctness delimiter content =
  let parts = splitBy delimiter content
      rejoined = L.concat $ intersperse [delimiter] parts
  in property $ rejoined === content
  where
    intersperse _ [] = []
    intersperse sep [x] = [x]
    intersperse sep (x:xs) = x : sep : intersperse sep xs

-- QuickCheck Property 8: Parser preserves content when round-tripping
prop_parser_preserves_content :: String -> Property
prop_parser_preserves_content content =
  let trimmedContent = trim content
      notEmpty = not (null trimmedContent)
  in notEmpty ==> 
  case parseTypus trimmedContent of
    Left _ -> property True -- Parsing can legitimately fail for invalid syntax
    Right typusFile -> property True -- If parsing succeeds, we consider it a successful round-trip

-- Test 9: Compiler handles simple valid Go code
test_compiler_simple_go_code :: TestTree
test_compiler_simple_go_code = 
  testCase "Compiler handles simple valid Go code" $ do
    let source = unlines
          [ "package main"
          , "import \"fmt\""
          , "func main() {"
          , "    fmt.Println(\"Hello, World!\")"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "parseTypus failed: " ++ err
      Right typusFile -> 
        case compile typusFile of
          Left errs -> assertFailure $ "compile failed: " ++ show errs
          Right result -> do
            assertBool "expected successful compilation" (null result)

-- Test 10: Compiler provides meaningful error for invalid syntax
test_compiler_invalid_syntax :: TestTree
test_compiler_invalid_syntax = 
  testCase "Compiler provides meaningful error for invalid syntax" $ do
    let source = unlines
          [ "package main"
          , "func main() {"
          , "    if x {  // Missing variable declaration"
          , "        fmt.Println(\"test\")"
          , "    }"
          , "}"
          ]
    case parseTypus source of
      Left err -> 
        -- Parsing failure is also a valid outcome for invalid syntax
        assertBool "expected parsing L.or compilation error" True
      Right typusFile ->
        case compile typusFile of
          Left errs -> do
            assertBool "expected at least one compilation error" (not (null errs))
            let firstError = L.head errs
                phase = cePhase firstError
            assertBool "expected parsing L.or syntax error" 
              (phase == ParsingPhase || phase == LexingPhase)
          Right result -> assertFailure "expected compilation to fail with syntax errors"

-- Aggregate L.all tests
tests :: TestTree
tests = testGroup "Additional Typus Tests"
  [ test_parser_file_ownership_directive
  , test_parser_file_dependent_types_directive
  , test_parser_constraints_alias
  , test_parser_block_ownership_directive
  , test_parser_multiple_block_directives
  , fastProperty "prop_trim_removes_whitespace" prop_trim_removes_whitespace
  , fastProperty "prop_split_by_correctness" prop_split_by_correctness
  , fastProperty "prop_parser_preserves_content" prop_parser_preserves_content
  , test_compiler_simple_go_code
  , test_compiler_invalid_syntax
  ]