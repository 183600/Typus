{-# LANGUAGE CPP #-}

module Test.Unit.AdditionalPropertyTestsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub)

import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset)
import Utils (trim, splitBy, removeLineComments)
import Compiler.IR (SourceIR(..), SemanticIR(..), buildSourceIR)
import Compiler.TypeChecker (Type(..), TypeEnv(..), buildTypeEnv)
import TestSupport.Arbitrary ()

prop_list_sort_idempotent :: [Int] -> Property
prop_list_sort_idempotent xs =
  sort (sort xs) === sort xs

prop_list_nub_idempotent :: [Int] -> Property
prop_list_nub_idempotent xs =
  nub (nub xs) === nub xs

prop_map_insert_lookup :: String -> Int -> Map.Map String Int -> Property
prop_map_insert_lookup key value m =
  Map.lookup key (Map.insert key value m) === Just value

prop_set_insert_member :: Int -> Set.Set Int -> Property
prop_set_insert_member x s =
  Set.member x (Set.insert x s) === True

prop_string_reverse_involutive :: String -> Property
prop_string_reverse_involutive s =
  reverse (reverse s) === s

prop_list_length_append :: [Int] -> [Int] -> Property
prop_list_length_append xs ys =
  length (xs ++ ys) === length xs + length ys

prop_sourcepos_ordering :: Property
prop_sourcepos_ordering =
  forAll genValidSourcePos $ \pos1 ->
  forAll genValidSourcePos $ \pos2 ->
    (posOffset pos1 < posOffset pos2) ==> (pos1 < pos2)
  where
    genValidSourcePos = do
      line <- choose (1, 1000)
      col <- choose (1, 1000)
      offset <- choose (0, 10000)
      return $ SourcePos line col offset

prop_trim_preserves_non_whitespace :: String -> Property
prop_trim_preserves_non_whitespace s =
  let trimmed = trim s
      nonWs = filter (not . (`elem` " \t\n\r")) s
  in all (`elem` trimmed) nonWs === True

prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s =
  delim `notElem` s ==>
  concat (splitBy delim s) === s

tests :: TestTree
tests = testGroup "Additional Property Tests"
  [ fastProperty "list sort is idempotent" prop_list_sort_idempotent
  , fastProperty "list nub is idempotent" prop_list_nub_idempotent
  , fastProperty "map insert then lookup returns value" prop_map_insert_lookup
  , fastProperty "set insert makes element member" prop_set_insert_member
  , fastProperty "string reverse is involutive" prop_string_reverse_involutive
  , fastProperty "list length is additive over append" prop_list_length_append
  , fastProperty "sourcepos offset ordering" prop_sourcepos_ordering
  , fastProperty "trim preserves non-whitespace characters" prop_trim_preserves_non_whitespace
  , fastProperty "splitBy preserves content when delimiter absent" prop_splitBy_preserves_content
  ]
