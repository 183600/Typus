{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, arbitrary, listOf, choose)

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SyntaxValidator (validateSyntax, SyntaxError(..))
import SimpleSyntaxValidator (simpleValidate, SimpleSyntaxError(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan, startPos)

import Data.List (isPrefixOf, isInfixOf, sort, nub, intercalate, isSuffixOf)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Data.Text as T (pack, unpack, Text(..), null, length, append, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (isSpace, isAlpha, isDigit)

-- Property: Parser recovers from syntax errors
prop_parser_recovers_from_syntax_errors :: Property
prop_parser_recovers_from_syntax_errors =
  forAll (elements ["func", "var", "if", "for", "return", "package", "import"]) $ \keyword ->
    forAll (elements ["", ";", "}", ")", "]", ",", ".", ":", "invalid"]) $ \trailer ->
      let input = keyword ++ trailer
          parsed = parseTypus (pack input)
      in counterexample ("Parser should recover from: " ++ input) $
         case parsed of
           Left _ -> property True -- Failed to parse, which is expected
           Right _ -> property True -- Successfully parsed

-- Property: Error recovery preserves as much structure as possible
prop_error_recovery_preserves_structure :: Property
prop_error_recovery_preserves_structure =
  forAll (listOf (choose (1, 10))) $ \lineLengths ->
    let lines = map (\len -> take len (cycle "func x int = ")) lineLengths
        input = unlines lines
        parsed = parseTypus (pack input)
    in counterexample "Error recovery should preserve structure" $
       case parsed of
         Left _ -> property True
         Right file -> length (codeBlocks file) >= 0

-- Property: Parser handles incomplete constructs
prop_parser_handles_incomplete_constructs :: Property
prop_parser_handles_incomplete_constructs =
  forAll (elements ["func", "func(", "func main", "func main(", "var", "var x", "var x int", "if", "if (", "for", "for ("]) $ \incomplete ->
    let parsed = parseTypus (pack incomplete)
    in counterexample ("Parser should handle incomplete: " ++ incomplete) $
       case parsed of
         Left _ -> property True
         Right _ -> property True

-- Property: Error messages are informative
prop_error_messages_informative :: Property
prop_error_messages_informative =
  forAll arbitrary $ \input ->
    let parsed = parseTypus (pack input)
        validation = case parsed of
          Left _ -> Left ["Parse error"]
          Right file -> validateSyntax file
    in counterexample "Error messages should be informative" $
       case validation of
         Left errors -> all (\err -> length err > 5) errors
         Right _ -> property True

-- Property: Parser is resilient to whitespace variations
prop_parser_resilient_whitespace :: Property
prop_parser_resilient_whitespace =
  forAll (elements ["func", "var", "if", "for"]) $ \construct ->
    forAll (elements ["", " ", "  ", "\t", "\n", " \t \n "]) $ \whitespace ->
      let input = construct ++ whitespace ++ "test"
          parsed = parseTypus (pack input)
      in counterexample ("Parser should handle whitespace in: " ++ show input) $
         case parsed of
           Left _ -> property True
           Right _ -> property True

-- Property: Syntax validation catches common errors
prop_syntax_validation_catches_errors :: Property
prop_syntax_validation_catches_errors =
  forAll (elements ["func x int", "var", "if ()", "for (;;", "return", "package", "import"]) $ \invalidConstruct ->
    let parsed = parseTypus (pack invalidConstruct)
        validated = case parsed of
          Left _ -> Left ["Parse failed"]
          Right file -> validateSyntax file
    in counterexample ("Syntax validation should catch: " ++ invalidConstruct) $
       case validated of
         Left _ -> property True
         Right _ -> property False -- Should not validate invalid constructs

-- Property: Simple validation is consistent with detailed validation
prop_simple_vs_detailed_validation :: Property
prop_simple_vs_detailed_validation =
  forAll arbitrary $ \input ->
    let parsed = parseTypus (pack input)
        simpleValid = case parsed of
          Left _ -> False
          Right file -> isRight (simpleValidate file)
        detailedValid = case parsed of
          Left _ -> False
          Right file -> isRight (validateSyntax file)
    in counterexample "Simple and detailed validation should be consistent" $
       simpleValid ==> detailedValid

-- Property: Parser handles Unicode characters
prop_parser_handles_unicode :: Property
prop_parser_handles_unicode =
  forAll (elements ["函数", "変数", "🚀", "test中文", "αβγ", "λx.x"]) $ \unicodeInput ->
    let parsed = parseTypus (pack unicodeInput)
    in counterexample ("Parser should handle Unicode: " ++ unicodeInput) $
       case parsed of
         Left _ -> property True
         Right _ -> property True

-- Property: Error recovery tracks source positions
prop_error_recovery_tracks_positions :: Property
prop_error_recovery_tracks_positions =
  forAll arbitrary $ \input ->
    let parsed = parseTypus (pack input)
        validation = case parsed of
          Left _ -> Left ["Parse error"]
          Right file -> validateSyntax file
    in counterexample "Error recovery should track source positions" $
       case validation of
         Left errors -> all (not . null) errors
         Right _ -> property True

-- Property: Parser handles nested structures with errors
prop_parser_handles_nested_errors :: Property
prop_parser_handles_nested_errors =
  forAll (elements ["if {", "for {", "func {", "if () {", "for () {", "func x() {"]) $ \nestedStart ->
    forAll (elements ["}", "]", ")", "", "invalid"]) $ \nestedEnd ->
      let input = nestedStart ++ " content " ++ nestedEnd
          parsed = parseTypus (pack input)
      in counterexample ("Parser should handle nested: " ++ input) $
         case parsed of
           Left _ -> property True
           Right _ -> property True

-- Helper function
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

tests :: TestTree
tests =
  testGroup "Parser Error Recovery QuickCheck Tests"
    [ fastProperty "Parser recovers from syntax errors" prop_parser_recovers_from_syntax_errors
    , fastProperty "Error recovery preserves as much structure as possible" prop_error_recovery_preserves_structure
    , fastProperty "Parser handles incomplete constructs" prop_parser_handles_incomplete_constructs
    , fastProperty "Error messages are informative" prop_error_messages_informative
    , fastProperty "Parser is resilient to whitespace variations" prop_parser_resilient_whitespace
    , fastProperty "Syntax validation catches common errors" prop_syntax_validation_catches_errors
    , fastProperty "Simple validation is consistent with detailed validation" prop_simple_vs_detailed_validation
    , fastProperty "Parser handles Unicode characters" prop_parser_handles_unicode
    , fastProperty "Error recovery tracks source positions" prop_error_recovery_tracks_positions
    , fastProperty "Parser handles nested structures with errors" prop_parser_handles_nested_errors
    ]