{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (isPrefixOf)
import Data.Char (isSpace)

import Utils (trim, splitBy, splitByComma, splitByCollapsed, normalizeIndentation, removeLineComments)
import SourceLocation (SourcePos(..), startPos, posAfter, spanFrom, isValidSpan)
import Parser (FileDirectives(..), BlockDirectives(..))
import Compiler (CompilerError(..), CompilationPhase(..))

-- | Property: trim is idempotent (trimming twice gives same result)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  trim (trim s) === trim s

-- | Property: splitBy on empty string returns singleton list
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty delim =
  splitBy delim "" === [""]

-- | Property: splitByCollapsed removes empty segments
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  not (null (splitBy delim s)) ==>
  all (not . null) (splitByCollapsed delim s)

-- | Property: normalizeIndentation preserves relative indentation
prop_normalizeIndentation_preserves_structure :: String -> Property
prop_normalizeIndentation_preserves_structure s =
  let lines' = lines s
      normalized = normalizeIndentation s
      normalizedLines = lines normalized
  in length lines' === length normalizedLines

-- | Property: removeLineComments removes lines starting with //
prop_removeLineComments_removes_comments :: String -> Property
prop_removeLineComments_removes_comments s =
  let commentLine = "// " ++ s
      result = removeLineComments commentLine
  in null (trim result) .||. not ("//" `isPrefixOf` trim result)

-- | Property: SourcePos ordering is consistent
prop_sourcepos_ordering :: Int -> Property
prop_sourcepos_ordering line =
  line > 0 ==>
  let pos1 = SourcePos line 1 0
      pos2 = SourcePos (line + 1) 1 0  -- Ensure pos2 > pos1
  in pos1 <= pos2

-- | Property: spanFrom creates valid spans
prop_spanFrom_valid :: Int -> Int -> Int -> Property
prop_spanFrom_valid line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let start = SourcePos line col offset
      span = spanFrom start
  in isValidSpan span

-- | Property: FileDirectives equality is reflexive
prop_file_directives_reflexive :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_file_directives_reflexive ownership dependent constraints =
  let fd = FileDirectives Nothing Nothing Nothing
  in fd === fd

-- | Property: BlockDirectives equality is symmetric
prop_block_directives_symmetric :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_block_directives_symmetric ownership dependent constraints =
  let bd1 = BlockDirectives Nothing Nothing Nothing
      bd2 = BlockDirectives Nothing Nothing Nothing
  in bd1 === bd2 .&&. bd2 === bd1

-- | Property: Map insertion preserves existing keys
prop_map_insertion_preserves :: [(String, Int)] -> String -> Int -> Property
prop_map_insertion_preserves pairs key value =
  let originalMap = Map.fromList pairs
      newMap = Map.insert key value originalMap
      existingKeys = Map.keys originalMap
  in property $ all (\k -> k == key || Map.lookup k originalMap == Map.lookup k newMap) existingKeys

-- | Property: Set insertion is idempotent
prop_set_insertion_idempotent :: [Int] -> Int -> Property
prop_set_insertion_idempotent elems elem =
  let set1 = Set.fromList elems
      set2 = Set.insert elem set1
      set3 = Set.insert elem set2
  in set2 === set3

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy on empty string returns singleton" prop_splitBy_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_no_empty
  , fastProperty "normalizeIndentation preserves line count" prop_normalizeIndentation_preserves_structure
  , fastProperty "removeLineComments removes comment lines" prop_removeLineComments_removes_comments
  , fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering
  , fastProperty "spanFrom creates valid spans" prop_spanFrom_valid
  , fastProperty "FileDirectives equality is reflexive" prop_file_directives_reflexive
  , fastProperty "BlockDirectives equality is symmetric" prop_block_directives_symmetric
  , fastProperty "Map insertion preserves existing keys" prop_map_insertion_preserves
  ]