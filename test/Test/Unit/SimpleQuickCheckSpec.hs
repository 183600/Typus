{-# LANGUAGE CPP #-}

module Test.Unit.SimpleQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set

import Parser (FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Utils (trim, splitBy, removeLineComments, splitByComma)
import TestSupport.Arbitrary ()

prop_trim_removes_spaces :: Property
prop_trim_removes_spaces =
  forAll (listOf (elements " \t\n\r")) $ \spaces ->
    trim spaces == ""

prop_splitBy_empty_string :: Char -> Property
prop_splitBy_empty_string delim =
  splitBy delim "" === [""]

prop_splitByComma_single_element :: String -> Property
prop_splitByComma_single_element s =
  not (',' `elem` s) ==>
  splitByComma s === [s]

prop_map_empty_lookup :: String -> Property
prop_map_empty_lookup key =
  Map.lookup key (Map.empty :: Map.Map String Int) === Nothing

prop_set_empty_not_member :: Int -> Property
prop_set_empty_not_member value =
  property (not (Set.member value (Set.empty :: Set.Set Int)))

prop_sourcepos_components :: Int -> Int -> Int -> Property
prop_sourcepos_components line col offset =
  line > 0 && col > 0 && offset >= 0 ==>
  let pos = SourcePos line col offset
  in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset

prop_sourcespan_components :: SourceSpan -> Property
prop_sourcespan_components srcSpan =
  let start = spanStart srcSpan
      end = spanEnd srcSpan
  in property $ posOffset start <= posOffset end

prop_file_directives_equality :: Property
prop_file_directives_equality =
  let fd1 = FileDirectives Nothing Nothing Nothing
      fd2 = FileDirectives Nothing Nothing Nothing
  in fd1 === fd2

prop_block_directives_equality :: Property
prop_block_directives_equality =
  let bd1 = BlockDirectives Nothing Nothing Nothing
      bd2 = BlockDirectives Nothing Nothing Nothing
  in bd1 === bd2

prop_removeLineComments_empty :: Property
prop_removeLineComments_empty =
  removeLineComments "" === ""

tests :: TestTree
tests = testGroup "Simple QuickCheck Tests"
  [ fastProperty "trim removes L.all whitespace" prop_trim_removes_spaces
  , fastProperty "splitBy on empty string returns singleton list" prop_splitBy_empty_string
  , fastProperty "splitByComma on string without comma returns singleton" prop_splitByComma_single_element
  , fastProperty "Map lookup on empty map returns Nothing" prop_map_empty_lookup
  , fastProperty "Set member on empty set returns False" prop_set_empty_not_member
  , fastProperty "SourcePos components are preserved" prop_sourcepos_components
  , fastProperty "SourceSpan start offset <= end offset" prop_sourcespan_components
  , fastProperty "FileDirectives with L.all Nothing are equal" prop_file_directives_equality
  , fastProperty "BlockDirectives with L.all Nothing are equal" prop_block_directives_equality
  , fastProperty "removeLineComments on empty string returns empty" prop_removeLineComments_empty
  ]
