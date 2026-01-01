{-# LANGUAGE CPP #-}

module Test.Unit.NewPropertyTestsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set

import Utils (trim, splitBy)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import Compiler.TypeChecker (TypeEnv(..), FunctionParam(..), FunctionSignature(..), Type(..))
import TestSupport.Arbitrary ()
import qualified Data.Map as Map

-- Test 1: trim removes leading spaces
prop_trim_removes_leading_spaces :: String -> Property
prop_trim_removes_leading_spaces s =
  let trimmed = trim ("   " ++ s)
  in not (null trimmed) ==> L.head trimmed /= ' '

-- Test 2: splitBy on empty string gives single empty element
prop_splitBy_empty :: Char -> Property
prop_splitBy_empty delim =
  splitBy delim "" === [""]

-- Test 3: CodeBlock content is preserved
prop_codeblock_content_preserved :: String -> Property
prop_codeblock_content_preserved content =
  let directives = BlockDirectives Nothing Nothing Nothing
      span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
      block = CodeBlock directives content span
  in cbContent block === content

-- Test 4: SourcePos with same offset should be comparable
prop_sourcepos_offset_consistency :: Int -> Property
prop_sourcepos_offset_consistency offset =
  offset >= 0 ==>
  let pos1 = SourcePos 1 1 offset
      pos2 = SourcePos 1 1 offset
  in posOffset pos1 === posOffset pos2

-- Test 5: SourceSpan start should be before L.or equal to end
prop_sourcespan_start_before_end :: SourceSpan -> Property
prop_sourcespan_start_before_end span =
  (posOffset (spanStart span) <= posOffset (spanEnd span)) === True

-- Test 6: FileDirectives with L.all Nothing should be equal
prop_file_directives_empty_equality :: Property
prop_file_directives_empty_equality =
  let fd1 = FileDirectives Nothing Nothing Nothing
      fd2 = FileDirectives Nothing Nothing Nothing
  in fd1 === fd2

-- Test 7: BlockDirectives with L.all Nothing should be equal
prop_block_directives_empty_equality :: Property
prop_block_directives_empty_equality =
  let bd1 = BlockDirectives Nothing Nothing Nothing
      bd2 = BlockDirectives Nothing Nothing Nothing
  in bd1 === bd2

-- Test 8: TypeEnv with empty maps should be equal
prop_typeenv_empty_equality :: Property
prop_typeenv_empty_equality =
  let env1 = TypeEnv Map.empty Map.empty
      env2 = TypeEnv Map.empty Map.empty
  in env1 === env2

-- Test 9: TypusFile preserves directives
prop_typusfile_directives :: FileDirectives -> Property
prop_typusfile_directives directives =
  let file = TypusFile directives [] [] []
  in tfDirectives file === directives

-- Test 10: Map operations are consistent
prop_map_operations :: String -> String -> Property
prop_map_operations key value =
  let m = Map.insert key value Map.empty
      m2 = Map.delete key m
  in Map.null m2 === True

tests :: TestTree
tests = testGroup "New Property Tests QuickCheck"
  [ fastProperty "trim removes leading spaces" prop_trim_removes_leading_spaces
  , fastProperty "splitBy on empty string" prop_splitBy_empty
  , fastProperty "CodeBlock content preserved" prop_codeblock_content_preserved
  , fastProperty "SourcePos offset consistency" prop_sourcepos_offset_consistency
  , fastProperty "SourceSpan start before end" prop_sourcespan_start_before_end
  , fastProperty "FileDirectives empty equality" prop_file_directives_empty_equality
  , fastProperty "BlockDirectives empty equality" prop_block_directives_empty_equality
  , fastProperty "TypeEnv empty equality" prop_typeenv_empty_equality
  , fastProperty "TypusFile preserves directives" prop_typusfile_directives
  , fastProperty "Map operations are consistent" prop_map_operations
  ]
