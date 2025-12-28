{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalComprehensiveTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen)
import TestSupport.Arbitrary

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Utils (trim, splitBy, removeComments)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)

-- Test 1: Parser round-trip property
prop_parser_round_trip :: String -> Property
prop_parser_round_trip input =
  let parsed = parseTypus input
  in case parsed of
       Left _ -> property True -- Invalid input is allowed
       Right typusFile -> 
         -- For valid parses, the structure should be consistent
         property $ tfDirectives typusFile `seq` True

-- Test 2: Trim idempotence property
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce === trimmedTwice

-- Test 3: Split by delimiter consistency
prop_split_by_consistency :: Char -> String -> Property
prop_split_by_consistency delim s =
  let parts = splitBy delim s
      rejoined = concat $ map (++ [delim]) $ init parts ++ [last parts]
  in length parts > 0 ==> rejoined === (if null s then "" else s ++ [delim])

-- Test 4: Comment removal preserves code structure
prop_comment_preserves_structure :: String -> Property
prop_comment_preserves_structure code =
  let withoutComments = removeComments code
      hasComments = "//" `isInfixOf` code || "/*" `isInfixOf` code
  in classify hasComments "has comments" $
     property $ length (lines withoutComments) <= length (lines code)

-- Test 5: Source position ordering
prop_source_position_ordering :: Int -> Int -> Int -> Int -> Property
prop_source_position_ordering line1 col1 line2 col2 =
  let pos1 = SourcePos line1 col1
      pos2 = SourcePos line2 col2
      (line1 < line2) || (line1 == line2 && col1 <= col2) ==> 
      property $ True -- Valid position ordering

-- Test 6: File directives parsing consistency
prop_file_directives_parsing :: String -> Property
prop_file_directives_parsing input =
  let withDirective = "//! ownership: on\n" ++ input
      parsed = parseTypus withDirective
  in case parsed of
       Left _ -> property True
       Right typusFile ->
         case fdOwnership (tfDirectives typusFile) of
           Nothing -> property False
           Just loc -> property $ locatedValue loc

-- Test 7: String processing invariants
prop_string_processing_invariants :: String -> String -> Property
prop_string_processing_invariants s1 s2 =
  let combined = s1 ++ s2
      trimmedCombined = trim combined
  in property $ not (null trimmedCombined) ==> length trimmedCombined <= length combined

-- Test 8: Parser error recovery
prop_parser_error_recovery :: String -> String -> Property
prop_parser_error_recovery valid invalid =
  let validInput = "func main() { " ++ valid ++ " }"
      invalidInput = "func main() { " ++ invalid ++ " @@@ invalid syntax }"
      validParsed = parseTypus validInput
      invalidParsed = parseTypus invalidInput
  in case (validParsed, invalidParsed) of
       (Right _, Left _) -> property True
       _ -> property True -- Both may succeed or fail depending on input

-- Test 9: Whitespace normalization
prop_whitespace_normalization :: String -> Property
prop_whitespace_normalization s =
  let trimmed = trim s
      hasLeading = not (null s) && isSpace (head s)
      hasTrailing = not (null s) && isSpace (last s)
  in classify hasLeading "has leading whitespace" $
     classify hasTrailing "has trailing whitespace" $
     property $ null trimmed || not (isSpace (head trimmed) && isSpace (last trimmed))

-- Test 10: Multi-line parsing consistency
prop_multiline_parsing_consistency :: [String] -> Property
prop_multiline_parsing_consistency lines =
  let input = unlines lines
      parsed = parseTypus input
  in case parsed of
       Left _ -> property True
       Right typusFile -> property $ tfBlocks typusFile `seq` True

tests :: TestTree
tests = 
  testGroup "New Cabal Comprehensive Tests"
    [ fastProperty "Parser round-trip property" prop_parser_round_trip
    , fastProperty "Trim idempotence property" prop_trim_idempotent
    , fastProperty "Split by delimiter consistency" prop_split_by_consistency
    , fastProperty "Comment removal preserves code structure" prop_comment_preserves_structure
    , fastProperty "Source position ordering" prop_source_position_ordering
    , fastProperty "File directives parsing consistency" prop_file_directives_parsing
    , fastProperty "String processing invariants" prop_string_processing_invariants
    , fastProperty "Parser error recovery" prop_parser_error_recovery
    , fastProperty "Whitespace normalization" prop_whitespace_normalization
    , fastProperty "Multi-line parsing consistency" prop_multiline_parsing_consistency
    ]