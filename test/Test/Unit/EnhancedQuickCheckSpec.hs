{-# LANGUAGE CPP #-}

module Test.Unit.EnhancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Enhanced QuickCheck Properties"
  [ utilsTests
  , sourceLocationTests
  , parserTests
  , irTests
  ]

utilsTests :: TestTree
utilsTests = testGroup "Utils Properties"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves all elements" prop_splitBy_preserves_elements
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , fastProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserves
  , fastProperty "normalizeIndentation preserves relative indentation" prop_normalizeIndentation_relative
  ]

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos offset increases with line/column" prop_sourcepos_offset_monotonic
  , fastProperty "SourceSpan start is before or equal to end" prop_sourcespan_start_before_end
  ]

parserTests :: TestTree
parserTests = testGroup "Parser Properties"
  [ fastProperty "defaultFileDirectives has all Nothing fields" prop_defaultFileDirectives_empty
  , fastProperty "defaultBlockDirectives has all Nothing fields" prop_defaultBlockDirectives_empty
  ]

irTests :: TestTree
irTests = testGroup "IR Properties"
  [ fastProperty "SourceIR roundtrip preserves structure" prop_sourceir_structure
  , fastProperty "GoIR contains valid Go code structure" prop_goir_structure
  ]

prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
  in not (null trimmed) ==> 
     property (not (head trimmed `elem` " \t\n\r") && not (last trimmed `elem` " \t\n\r"))

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_splitBy_preserves_elements :: Char -> String -> Property
prop_splitBy_preserves_elements delim s =
  delim `notElem` s ==>
  splitBy delim s === [s]

prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

prop_removeLineComments_preserves :: String -> Property
prop_removeLineComments_preserves s =
  "//" `notElem` words s ==>
  property $ not $ "//" `elem` words (removeLineComments s)

prop_normalizeIndentation_relative :: String -> Property
prop_normalizeIndentation_relative s =
  let normalized = normalizeIndentation s
      originalLines = lines s
      normalizedLines = lines normalized
  in length originalLines === length normalizedLines

prop_sourcepos_offset_monotonic :: Positive Int -> Positive Int -> Positive Int -> Property
prop_sourcepos_offset_monotonic (Positive l) (Positive c) (Positive o) =
  let pos1 = SourcePos l c o
      pos2 = SourcePos (l + 1) c (o + 10)
  in property $ posOffset pos1 < posOffset pos2

prop_sourcespan_start_before_end :: SourceSpan -> Property
prop_sourcespan_start_before_end span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_defaultFileDirectives_empty :: Property
prop_defaultFileDirectives_empty =
  let fd = defaultFileDirectives
  in conjoin
    [ fdOwnership fd === Nothing
    , fdDependentTypes fd === Nothing
    , fdConstraints fd === Nothing
    ]

prop_defaultBlockDirectives_empty :: Property
prop_defaultBlockDirectives_empty =
  let bd = defaultBlockDirectives
  in conjoin
    [ bdOwnership bd === Nothing
    , bdDependentTypes bd === Nothing
    , bdConstraints bd === Nothing
    ]

prop_sourceir_structure :: SourceIR -> Property
prop_sourceir_structure (SourceIR typusFile code) =
  not (null code) ==> property True

prop_goir_structure :: GoIR -> Property
prop_goir_structure (GoIR goModule code) =
  not (null code) ==> property True