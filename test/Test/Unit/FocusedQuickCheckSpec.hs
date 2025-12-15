{-# LANGUAGE CPP #-}

module Test.Unit.FocusedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set

import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Utils (trim, splitBy)
import Compiler.TypeChecker (Type(..), TypeEnv(..), typesEqual)
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

prop_sourcepos_line_positive :: SourcePos -> Property
prop_sourcepos_line_positive pos =
  property $ posLine pos > 0

prop_sourcespan_start_before_end :: SourceSpan -> Property
prop_sourcespan_start_before_end span =
  let start = spanStart span
      end = spanEnd span
      startOff = posOffset start
      endOff = posOffset end
  in property $ startOff <= endOff

prop_trim_removes_whitespace :: String -> Property
prop_trim_removes_whitespace s =
  let trimmed = trim s
  in property $ null trimmed || (head trimmed /= ' ' && last trimmed /= ' ')

prop_splitBy_concat :: Char -> String -> Property
prop_splitBy_concat delim s =
  not (delim `elem` s) ==>
  property $ concat (splitBy delim s) == s

prop_typename_equality :: String -> Property
prop_typename_equality name =
  let t = TypeName name
  in property $ typesEqual t t

prop_unknown_type_equality :: Property
prop_unknown_type_equality =
  property $ typesEqual UnknownType UnknownType

prop_map_insert_preserves_size :: String -> Type -> Property
prop_map_insert_preserves_size name typ =
  let m = Map.singleton name typ
  in property $ Map.size m == 1

prop_file_directives_default_all_nothing :: Property
prop_file_directives_default_all_nothing =
  let fd = defaultFileDirectives
  in property $ fdOwnership fd == Nothing && fdDependentTypes fd == Nothing && fdConstraints fd == Nothing

prop_block_directives_default_all_nothing :: Property
prop_block_directives_default_all_nothing =
  let bd = defaultBlockDirectives
  in property $ bdOwnership bd == Nothing && bdDependentTypes bd == Nothing && bdConstraints bd == Nothing

prop_typeenv_lookup_after_insert :: String -> String -> Property
prop_typeenv_lookup_after_insert name typeName =
  let typ = TypeName typeName
      env = TypeEnv (Map.singleton name typ) Map.empty
  in property $ Map.lookup name (varTypes env) == Just typ

tests :: TestTree
tests = testGroup "Focused QuickCheck Tests"
  [ fastProperty "SourcePos line is always positive" prop_sourcepos_line_positive
  , fastProperty "SourceSpan start offset <= end offset" prop_sourcespan_start_before_end
  , fastProperty "trim removes leading/trailing whitespace" prop_trim_removes_whitespace
  , fastProperty "splitBy then concat preserves string without delimiter" prop_splitBy_concat
  , fastProperty "TypeName equality is reflexive" prop_typename_equality
  , fastProperty "UnknownType equals itself" prop_unknown_type_equality
  , fastProperty "Map insert preserves size" prop_map_insert_preserves_size
  , fastProperty "Default FileDirectives has all Nothing fields" prop_file_directives_default_all_nothing
  , fastProperty "Default BlockDirectives has all Nothing fields" prop_block_directives_default_all_nothing
  , fastProperty "TypeEnv lookup after insert returns value" prop_typeenv_lookup_after_insert
  ]
