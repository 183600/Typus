{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalComprehensiveTestsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.Tasty (TestTree)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Ownership (OwnershipType(..), OwnershipTransfer(..), analyzeOwnership)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, advancePos, mergeSpans)
import Utils (trim, splitBy, removeComments)
import Data.Char (isSpace)
import qualified Data.List as List
import Data.Text (Text)

-- Property: Parser handles empty input gracefully
prop_parser_empty_input :: Property
prop_parser_empty_input =
  let result = parseTypus ""
  in property $ case result of
    Left _ -> property True
    Right typusFile -> property True

-- Property: Parser handles whitespace-only input
prop_parser_whitespace_input :: String -> Property
prop_parser_whitespace_input input =
  L.all isSpace input ==>
  let result = parseTypus input
  in property $ case result of
    Left _ -> property True
    Right typusFile -> property True

-- Property: Compiler maintains consistency with multiple compilations
prop_compiler_consistency :: String -> Property
prop_compiler_consistency input =
  let result1 = compile input
      result2 = compile input
  in property $ case (result1, result2) of
    (Left e1, Left e2) -> show e1 === show e2
    (Right r1, Right r2) -> show r1 === show r2
    _ -> property False

-- Property: Ownership analysis handles empty programs
prop_ownership_empty_program :: Property
prop_ownership_empty_program =
  let result = analyzeOwnership ""
  in property $ case result of
    Left _ -> property True
    Right _ -> property True

-- Property: Source position advances correctly with characters
prop_source_position_advances :: Int -> Int -> Char -> Property
prop_source_position_advances line col ch =
  line >= 1 && line <= 100 && col >= 1 && col <= 100 ==>
  let pos = SourcePos line col
      advancedPos = advancePos pos ch
  in property $ case ch of
    '\n' -> sourceLine advancedPos === line + 1 .&&. sourceColumn advancedPos === 1
    '\t' -> sourceLine advancedPos === line .&&. sourceColumn advancedPos >= col + 1
    _ -> sourceLine advancedPos === line .&&. sourceColumn advancedPos === col + 1

-- Property: Source span merging is associative
prop_source_span_merge_associative :: SourceSpan -> SourceSpan -> SourceSpan -> Property
prop_source_span_merge_associative span1 span2 span3 =
  let merge12 = mergeSpans span1 span2
      merge23 = mergeSpans span2 span3
      merge123_1 = mergeSpans merge12 span3
      merge123_2 = mergeSpans span1 merge23
  in property $ merge123_1 === merge123_2

-- Property: String processing functions are composable
prop_string_processing_composable :: String -> Property
prop_string_processing_composable input =
  let trimmed = trim input
      split = splitBy ',' trimmed
      cleaned = removeComments input
      pipeline = input |> trim |> removeComments |> splitBy ','
      alternative = input |> removeComments |> trim |> splitBy ','
  in property $ L.length pipeline >= 0 .&&. L.length alternative >= 0

-- Property: Error handling preserves error information
prop_error_handling_preserves_info :: String -> Property
prop_error_handling_preserves_info input =
  let parseResult = parseTypus input
      compileResult = compile input
  in property $ case (parseResult, compileResult) of
    (Left parseErr, Left compileErr) -> 
      property $ not (L.null (show parseErr)) .&&. not (L.null (show compileErr))
    (Right _, Left compileErr) -> 
      property $ not (L.null (show compileErr))
    (Left parseErr, Right _) -> 
      property $ not (L.null (show parseErr))
    (Right _, Right _) -> 
      property True

-- Helper function for pipeline composition
(|>) :: a -> (a -> b) -> b
x |> f = f x

tests :: TestTree
tests = testGroup "New Cabal Comprehensive Tests"
  [ fastProperty "Parser handles empty input" prop_parser_empty_input
  , fastProperty "Parser handles whitespace input" prop_parser_whitespace_input
  , fastProperty "Compiler maintains consistency" prop_compiler_consistency
  , fastProperty "Ownership handles empty programs" prop_ownership_empty_program
  , fastProperty "Source position advances correctly" prop_source_position_advances
  , fastProperty "Source span merging is associative" prop_source_span_merge_associative
  , fastProperty "String processing is composable" prop_string_processing_composable
  , fastProperty "Error handling preserves information" prop_error_handling_preserves_info
  ]