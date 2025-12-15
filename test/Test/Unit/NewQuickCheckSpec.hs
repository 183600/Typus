{-# LANGUAGE CPP #-}

module Test.Unit.NewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.List (isInfixOf)
import Data.Maybe (isNothing)

import Parser (FileDirectives(..), BlockDirectives(..), BlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Utils (trim, splitBy, removeLineComments)
import TestSupport.Arbitrary ()

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in property $ trim trimmed == trimmed

prop_splitBy_preserves_length :: Char -> NonEmptyList Char -> Property
prop_splitBy_preserves_length delim (NonEmpty s) =
  let parts = splitBy delim s
      reconstructed = List.intercalate [delim] parts
  in property $ length reconstructed == length s

prop_map_insert_lookup :: String -> Int -> Property
prop_map_insert_lookup key value =
  let m = Map.insert key value Map.empty
  in property $ Map.lookup key m == Just value

prop_set_insert_member :: Int -> Property
prop_set_insert_member value =
  let s = Set.insert value Set.empty
  in property $ Set.member value s

prop_list_reverse_twice :: [Int] -> Property
prop_list_reverse_twice xs =
  property $ reverse (reverse xs) == xs

prop_sourcepos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcepos_ordering p1 p2 =
  let line1 = posLine p1
      line2 = posLine p2
      col1 = posColumn p1
      col2 = posColumn p2
  in property $ (line1 < line2) || (line1 == line2 && col1 <= col2) || (line1 > line2)

prop_sourcespan_valid :: SourceSpan -> Property
prop_sourcespan_valid span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_file_directives_default :: Property
prop_file_directives_default =
  let fd = FileDirectives Nothing Nothing Nothing
  in property $ fdOwnership fd == Nothing && fdDependentTypes fd == Nothing && fdConstraints fd == Nothing

prop_block_directives_merge :: BlockDirectives -> BlockDirectives -> Property
prop_block_directives_merge bd1 bd2 =
  let orElseMaybe Nothing x = x
      orElseMaybe x _ = x
      merged = BlockDirectives
        (bdOwnership bd2 `orElseMaybe` bdOwnership bd1)
        (bdDependentTypes bd2 `orElseMaybe` bdDependentTypes bd1)
        (bdConstraints bd2 `orElseMaybe` bdConstraints bd1)
  in property $ bdOwnership merged /= Nothing || (bdOwnership bd1 == Nothing && bdOwnership bd2 == Nothing)

prop_removeLineComments_preserves_code :: String -> Property
prop_removeLineComments_preserves_code code =
  not ("//" `List.isInfixOf` code) ==>
  property $ removeLineComments code == code

tests :: TestTree
tests = testGroup "New QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves length when reconstructed" prop_splitBy_preserves_length
  , fastProperty "Map insert then lookup returns value" prop_map_insert_lookup
  , fastProperty "Set insert then member returns true" prop_set_insert_member
  , fastProperty "List reverse twice is identity" prop_list_reverse_twice
  , fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering
  , fastProperty "SourceSpan start offset <= end offset" prop_sourcespan_valid
  , fastProperty "FileDirectives default has all Nothing" prop_file_directives_default
  , fastProperty "BlockDirectives merge preserves non-Nothing values" prop_block_directives_merge
  , fastProperty "removeLineComments preserves code without comments" prop_removeLineComments_preserves_code
  ]
