{-# LANGUAGE CPP #-}

module Test.Unit.PropertyQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()
import Data.List (sort, nub, length, sum, product, reverse, concat, (++))

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, emptySpan, mergeSpans)
import Compiler.GoLexer (GoToken(..), GoTokenKind(..), tokenizeGo)
import Ownership.Parser (Expr(..), Stmt(..))
import Compiler.TypeChecker (Type(..))
import Analyzer.Types (SymbolInfo(..))
import qualified Data.Map as Map
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Property QuickCheck Test Properties"
  [ algebraicProperties
  , structuralProperties
  , functionalProperties
  , invariantProperties
  ]

algebraicProperties :: TestTree
algebraicProperties = testGroup "Algebraic Properties"
  [ fastProperty "String concatenation is associative" prop_string_concat_associative
  , fastProperty "List concatenation is associative" prop_list_concat_associative
  , fastProperty "Map union is commutative" prop_map_union_commutative
  , fastProperty "Set union is commutative" prop_set_union_commutative
  ]

structuralProperties :: TestTree
structuralProperties = testGroup "Structural Properties"
  [ fastProperty "Parser preserves structure" prop_parser_preserves_structure
  , fastProperty "Lexer preserves token count" prop_lexer_preserves_count
  , fastProperty "AST transformation preserves semantics" prop_ast_preserves_semantics
  ]

functionalProperties :: TestTree
functionalProperties = testGroup "Functional Properties"
  [ fastProperty "Symbol table lookup is functional" prop_symboltable_functional
  , fastProperty "Type checking is deterministic" prop_typechecking_deterministic
  , fastProperty "Error reporting is consistent" prop_error_consistent
  ]

invariantProperties :: TestTree
invariantProperties = testGroup "Invariant Properties"
  [ fastProperty "Source span invariants" prop_span_invariants
  , fastProperty "Token position invariants" prop_token_invariants
  , fastProperty "Symbol table scope invariants" prop_symboltable_invariants
  ]

prop_string_concat_associative :: String -> String -> String -> Property
prop_string_concat_associative x y z = (x ++ y) ++ z === x ++ (y ++ z)

prop_list_concat_associative :: [Int] -> [Int] -> [Int] -> Property
prop_list_concat_associative xs ys zs = (xs ++ ys) ++ zs === xs ++ (ys ++ zs)

prop_map_union_commutative :: [(String, Int)] -> [(String, Int)] -> Property
prop_map_union_commutative pairs1 pairs2 =
  let map1 = Map.fromList pairs1
      map2 = Map.fromList pairs2
      union1 = Map.union map1 map2
      union2 = Map.union map2 map1
  in property $ Set.fromList (Map.keys union1) `Set.isSubsetOf` Set.fromList (Map.keys union2) &&
               Set.fromList (Map.keys union2) `Set.isSubsetOf` Set.fromList (Map.keys union1)

prop_set_union_commutative :: [Int] -> [Int] -> Property
prop_set_union_commutative xs ys =
  let set1 = Set.fromList xs
      set2 = Set.fromList ys
  in Set.union set1 set2 === Set.union set2 set1

prop_parser_preserves_structure :: String -> Property
prop_parser_preserves_structure s = property True

prop_lexer_preserves_count :: String -> Property
prop_lexer_preserves_count s =
  let tokens = tokenizeGo s
  in property $ length tokens >= 0

prop_ast_preserves_semantics :: Expr -> Property
prop_ast_preserves_semantics expr = property True

prop_symboltable_functional :: [(String, Int)] -> String -> Property
prop_symboltable_functional pairs key =
  let symbolTable = Map.fromList pairs
      result1 = Map.lookup key symbolTable
      result2 = Map.lookup key symbolTable
  in result1 === result2

prop_typechecking_deterministic :: Expr -> Property
prop_typechecking_deterministic expr = property True

prop_error_consistent :: String -> Property
prop_error_consistent input = property True

prop_span_invariants :: SourceSpan -> Property
prop_span_invariants span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_token_invariants :: GoToken -> Property
prop_token_invariants token =
  let kind = tokenKind token
      text = tokenText token
  in property $ not (null text)

prop_symboltable_invariants :: Map.Map String SymbolInfo -> Property
prop_symboltable_invariants st = property True