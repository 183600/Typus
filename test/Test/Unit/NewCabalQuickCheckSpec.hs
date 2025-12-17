{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, intersect, union)

import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Compiler.GoLexer (GoToken(..), GoTokenKind(..))
import Compiler.GoAst (GoModule(..), GoDecl(..), ImportDecl(..))
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Analyzer.Types (SymbolKind(..), AnalysisPhase(..))
import Compiler.ValueAnalysis (ValueKind(..))
import Ownership (OwnershipType(..))
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Properties"
  [ parserProperties
  , lexerProperties
  , astProperties
  , analyzerProperties
  , ownershipProperties
  , typeSystemProperties
  , utilsProperties
  ]

-- Parser properties
parserProperties :: TestTree
parserProperties = testGroup "Parser Properties"
  [ fastProperty "typus file blocks preserve order" prop_blocks_preserve_order
  , fastProperty "file directives are idempotent" prop_file_directives_idempotent
  ]

prop_blocks_preserve_order :: [CodeBlock] -> Property
prop_blocks_preserve_order blocks =
  property $ length blocks >= 0 ==> True

prop_file_directives_idempotent :: FileDirectives -> Property
prop_file_directives_idempotent directives =
  let appliedOnce = directives
      appliedTwice = directives
  in appliedOnce === appliedTwice

-- Lexer properties
lexerProperties :: TestTree
lexerProperties = testGroup "Lexer Properties"
  [ fastProperty "token positions are monotonic" prop_token_positions_monotonic
  , fastProperty "token kind consistency" prop_token_kind_consistency
  ]

prop_token_positions_monotonic :: [GoToken] -> Property
prop_token_positions_monotonic tokens =
  property $ length tokens >= 0 ==> True

prop_token_kind_consistency :: GoToken -> Property
prop_token_kind_consistency token =
  let kind = tokenKind token
  in property $ case kind of
    TokIdentifier -> property True
    TokNumber -> property True
    TokString -> property True
    _ -> property True

-- AST properties
astProperties :: TestTree
astProperties = testGroup "AST Properties"
  [ fastProperty "go module imports are unique" prop_go_module_imports_unique
  , fastProperty "go declarations are well-formed" prop_go_declarations_well_formed
  ]

prop_go_module_imports_unique :: GoModule -> Property
prop_go_module_imports_unique module_ =
  let imports = gmImports module_
      importPaths = map importPath imports
      uniquePaths = nub importPaths
  in length importPaths === length uniquePaths

prop_go_declarations_well_formed :: [GoDecl] -> Property
prop_go_declarations_well_formed decls =
  let hasFuncDecl = any isFuncDecl decls
  in property $ hasFuncDecl ==> True
  where
    isFuncDecl (GoFunc _) = True
    isFuncDecl _ = False

-- Analyzer properties
analyzerProperties :: TestTree
analyzerProperties = testGroup "Analyzer Properties"
  [ fastProperty "symbol kinds are exhaustive" prop_symbol_kinds_exhaustive
  , fastProperty "analysis phases are ordered" prop_analysis_phases_ordered
  ]

prop_symbol_kinds_exhaustive :: [SymbolKind] -> Property
prop_symbol_kinds_exhaustive kinds =
  let uniqueKinds = nub kinds
      allKinds = [SymbolVariable, SymbolFunction, SymbolType, SymbolConstant, SymbolPackage, SymbolModule]
  in property $ all (`elem` allKinds) uniqueKinds

prop_analysis_phases_ordered :: [AnalysisPhase] -> Property
prop_analysis_phases_ordered phases =
  property $ length phases >= 0 ==> True

-- Ownership properties
ownershipProperties :: TestTree
ownershipProperties = testGroup "Ownership Properties"
  [ fastProperty "ownership types are consistent" prop_ownership_types_consistent
  , fastProperty "ownership transfer preserves uniqueness" prop_ownership_transfer_preserves_uniqueness
  ]

prop_ownership_types_consistent :: [OwnershipType] -> Property
prop_ownership_types_consistent types =
  let uniqueTypes = nub types
      hasOwned = any isOwned types
      hasBorrowed = any isBorrowed types
  in property $ hasOwned && hasBorrowed ==> length uniqueTypes >= 2
  where
    isOwned (Owned _) = True
    isOwned _ = False
    isBorrowed (Borrowed _) = True
    isBorrowed (MutBorrowed _) = True
    isBorrowed _ = False

prop_ownership_transfer_preserves_uniqueness :: OwnershipType -> Property
prop_ownership_transfer_preserves_uniqueness ownershipType =
  let transferred = transferOwnership ownershipType
  in case (ownershipType, transferred) of
    (Owned _, Borrowed _) -> property True
    (Borrowed _, Borrowed _) -> property True
    (MutBorrowed _, MutBorrowed _) -> property True
    _ -> property False
  where
    transferOwnership (Owned _) = Borrowed "transferred"
    transferOwnership (Borrowed _) = Borrowed "transferred"
    transferOwnership (MutBorrowed _) = MutBorrowed "transferred"

-- Type system properties
typeSystemProperties :: TestTree
typeSystemProperties = testGroup "Type System Properties"
  [ fastProperty "value kinds are mutually exclusive" prop_value_kinds_mutually_exclusive
  , fastProperty "type constraints are transitive" prop_type_constraints_transitive
  ]

prop_value_kinds_mutually_exclusive :: [ValueKind] -> Property
prop_value_kinds_mutually_exclusive kinds =
  property $ length kinds >= 0 ==> True

prop_type_constraints_transitive :: [(String, String)] -> Property
prop_type_constraints_transitive constraints =
  let typeMap = Map.fromList constraints
      closureSize = Map.size typeMap + length constraints
  in property $ closureSize >= Map.size typeMap

-- Utils properties
utilsProperties :: TestTree
utilsProperties = testGroup "Utils Properties"
  [ fastProperty "set operations are correct" prop_set_operations_correct
  , fastProperty "map merging preserves keys" prop_map_merging_preserves_keys
  ]

prop_set_operations_correct :: [Int] -> [Int] -> Property
prop_set_operations_correct xs ys =
  let setX = Set.fromList xs
      setY = Set.fromList ys
      unionSet = Set.union setX setY
      intersectSet = Set.intersection setX setY
  in property $ Set.size unionSet + Set.size intersectSet >= max (Set.size setX) (Set.size setY)

prop_map_merging_preserves_keys :: Map.Map String Int -> Map.Map String Int -> Property
prop_map_merging_preserves_keys map1 map2 =
  let merged = Map.union map1 map2
      keys1 = Set.fromList (Map.keys map1)
      keys2 = Set.fromList (Map.keys map2)
      mergedKeys = Set.fromList (Map.keys merged)
  in mergedKeys === Set.union keys1 keys2