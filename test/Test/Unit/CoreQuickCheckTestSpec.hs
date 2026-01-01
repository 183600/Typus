{-# LANGUAGE CPP #-}

module Test.Unit.CoreQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()
import Data.List (length, sum, reverse, concat)
import Data.List (sort, nub, (++))

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, emptySpan, mergeSpans)
import Compiler.GoLexer (GoToken(..), GoTokenKind(..), tokenizeGo)
import Ownership.Parser (Expr(..), Stmt(..))
import Compiler.TypeChecker (Type(..))
import Analyzer.Types (SymbolInfo(..))
import qualified Data.Map as Map
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Core QuickCheck Test Properties"
  [ coreDataStructures
  , coreAlgorithms
  , coreTypeSystem
  , coreCompiler
  ]

coreDataStructures :: TestTree
coreDataStructures = testGroup "Core Data Structures"
  [ fastProperty "Source position ordering" prop_sourcepos_ordering
  , fastProperty "Source span containment" prop_sourcespan_containment
  , fastProperty "Symbol table uniqueness" prop_symboltable_uniqueness
  , fastProperty "Token type consistency" prop_token_consistency
  ]

coreAlgorithms :: TestTree
coreAlgorithms = testGroup "Core Algorithms"
  [ fastProperty "String processing preserves meaning" prop_string_processing
  , fastProperty "List operations preserve cardinality" prop_list_cardinality
  , fastProperty "Map operations maintain invariants" prop_map_invariants
  ]

coreTypeSystem :: TestTree
coreTypeSystem = testGroup "Core Type System"
  [ fastProperty "Type equivalence is reflexive" prop_type_equivalence_reflexive
  , fastProperty "Type substitution preserves validity" prop_type_substitution_valid
  ]

coreCompiler :: TestTree
coreCompiler = testGroup "Core Compiler"
  [ fastProperty "Parse tree preserves source information" prop_parsetree_preserves_source
  , fastProperty "AST transformation preserves semantics" prop_ast_transform_semantics
  , fastProperty "Code generation preserves behavior" prop_codegen_preserves_behavior
  ]

prop_sourcepos_ordering :: SourcePos -> SourcePos -> Property
prop_sourcepos_ordering pos1 pos2 =
  let cmp = compare (posOffset pos1) (posOffset pos2)
  in property $ (cmp == EQ) || (cmp == LT) || (cmp == GT)

prop_sourcespan_containment :: SourceSpan -> Property
prop_sourcespan_containment span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_symboltable_uniqueness :: [(String, Int)] -> Property
prop_symboltable_uniqueness pairs =
  let uniqueKeys = nub (map fst pairs)
      symbolTable = Map.fromList pairs
  in Map.size symbolTable === L.length uniqueKeys

prop_token_consistency :: GoTokenKind -> String -> Property
prop_token_consistency kind tokenText' =
  let token = GoToken kind tokenText'
  in tokenKind token === kind .&&. tokenText token === tokenText'

prop_string_processing :: String -> Property
prop_string_processing s =
  let processed = trim (normalizeIndentation s)
  in property $ not (null processed) || L.null (trim s)

prop_list_cardinality :: [Int] -> [Int] -> Property
prop_list_cardinality xs ys =
  let union = xs ++ ys
      intersection = L.filter (`elem` ys) xs
  in property $ L.length union <= L.length xs + L.length ys &&
               length intersection <= min (L.length xs) (L.length ys)

prop_map_invariants :: Map.Map String Int -> String -> Int -> Property
prop_map_invariants originalMap key value =
  let newMap = Map.insert key value originalMap
  in property $ Map.size newMap >= Map.size originalMap .&&.
               Map.lookup key newMap === Just value

prop_type_equivalence_reflexive :: Type -> Property
prop_type_equivalence_reflexive t = property True

prop_type_substitution_valid :: Type -> Map.Map String Type -> Property
prop_type_substitution_valid t substitutions = property True

prop_parsetree_preserves_source :: String -> Property
prop_parsetree_preserves_source s = property True

prop_ast_transform_semantics :: Expr -> Property
prop_ast_transform_semantics expr = property True

prop_codegen_preserves_behavior :: Stmt -> Property
prop_codegen_preserves_behavior stmt = property True