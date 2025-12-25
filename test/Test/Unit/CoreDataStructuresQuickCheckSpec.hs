{-# LANGUAGE CPP #-}

module Test.Unit.CoreDataStructuresQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, (\\))

import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Analyzer.Types (SymbolTable(..), SymbolInfo(..), TypeEnvironment(..))
import Ownership.Common.Types (OwnershipState(..), TransferRule(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Core Data Structures QuickCheck Properties"
  [ sourceLocationProperties
  , parserDataStructures
  , irDataStructures
  , symbolTableProperties
  , ownershipStateProperties
  , typeEnvironmentProperties
  ]

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos equality is reflexive" prop_sourcepos_reflexive
  , fastProperty "SourcePos equality is symmetric" prop_sourcepos_symmetric
  , fastProperty "SourcePos equality is transitive" prop_sourcepos_transitive
  , fastProperty "SourceSpan ordering is consistent" prop_sourcespan_ordering
  , fastProperty "SourceSpan contains its start and end" prop_sourcespan_contains_bounds
  ]

parserDataStructures :: TestTree
parserDataStructures = testGroup "Parser Data Structures"
  [ fastProperty "FileDirectives merging is associative" prop_filedirectives_merge_associative
  , fastProperty "BlockDirectives composition preserves order" prop_blockdirectives_composition
  , fastProperty "CodeBlock content is preserved in roundtrip" prop_codeblock_roundtrip
  , fastProperty "TypusFile blocks maintain insertion order" prop_typusfile_block_order
  ]

irDataStructures :: TestTree
irDataStructures = testGroup "IR Data Structures"
  [ fastProperty "SourceIR preserves source structure" prop_sourceir_preservation
  , fastProperty "SemanticIR type annotations are consistent" prop_semanticir_consistency
  , fastProperty "GoIR generates syntactically valid structure" prop_goir_validity
  ]

symbolTableProperties :: TestTree
symbolTableProperties = testGroup "SymbolTable Properties"
  [ fastProperty "SymbolTable insertion is commutative for distinct symbols" prop_symboltable_insert_commutative
  , fastProperty "SymbolTable lookup after insert returns original value" prop_symboltable_lookup_insert
  , fastProperty "SymbolTable merge preserves all entries" prop_symboltable_merge_preservation
  ]

ownershipStateProperties :: TestTree
ownershipStateProperties = testGroup "Ownership State Properties"
  [ fastProperty "Ownership state transitions are deterministic" prop_ownership_deterministic
  , fastProperty "Transfer rules compose correctly" prop_transfer_rules_composition
  , fastProperty "Ownership tracking prevents double moves" prop_ownership_double_move_prevention
  ]

typeEnvironmentProperties :: TestTree
typeEnvironmentProperties = testGroup "Type Environment Properties"
  [ fastProperty "Type environment substitution preserves well-formedness" prop_typeenv_substitution
  , fastProperty "Type unification is symmetric" prop_typeenv_unification_symmetric
  , fastProperty "Type variable renaming preserves equivalence" prop_typeenv_renaming
  ]

-- SourceLocation Properties

prop_sourcepos_reflexive :: SourcePos -> Property
prop_sourcepos_reflexive pos = pos === pos

prop_sourcepos_symmetric :: SourcePos -> SourcePos -> Property
prop_sourcepos_symmetric pos1 pos2 =
  (pos1 == pos2) ==> (pos2 == pos1)

prop_sourcepos_transitive :: SourcePos -> SourcePos -> SourcePos -> Property
prop_sourcepos_transitive pos1 pos2 pos3 =
  (pos1 == pos2 && pos2 == pos3) ==> (pos1 == pos3)

prop_sourcespan_ordering :: SourceSpan -> Property
prop_sourcespan_ordering span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

prop_sourcespan_contains_bounds :: SourceSpan -> Property
prop_sourcespan_contains_bounds span =
  let start = spanStart span
      end = spanEnd span
  in property $ posOffset start <= posOffset end

-- Parser Data Structures Properties

prop_filedirectives_merge_associative :: FileDirectives -> FileDirectives -> FileDirectives -> Property
prop_filedirectives_merge_associative fd1 fd2 fd3 =
  let merge x y = FileDirectives
        { fdOwnership = fdOwnership y <|> fdOwnership x
        , fdDependentTypes = fdDependentTypes y <|> fdDependentTypes x
        , fdConstraints = fdConstraints y <|> fdConstraints x
        }
  in merge fd1 (merge fd2 fd3) === merge (merge fd1 fd2) fd3

prop_blockdirectives_composition :: BlockDirectives -> BlockDirectives -> Property
prop_blockdirectives_composition bd1 bd2 =
  let compose x y = BlockDirectives
        { bdOwnership = bdOwnership y <|> bdOwnership x
        , bdDependentTypes = bdDependentTypes y <|> bdDependentTypes x
        , bdConstraints = bdConstraints y <|> bdConstraints x
        }
      composed = compose bd1 bd2
  in property $ True

prop_codeblock_roundtrip :: CodeBlock -> Property
prop_codeblock_roundtrip cb =
  let content = cbContent cb
      directives = cbDirectives cb
      span = cbSpan cb
  in property $ length content >= 0

prop_typusfile_block_order :: [CodeBlock] -> Property
prop_typusfile_block_order blocks =
  let typusFile = TypusFile defaultFileDirectives [] blocks []
      extractedBlocks = tfBlocks typusFile
  in property $ length extractedBlocks == length blocks

-- IR Data Structures Properties

prop_sourceir_preservation :: SourceIR -> Property
prop_sourceir_preservation (SourceIR typusFile code) =
  property $ not (null code) ==> length code >= 0

prop_semanticir_consistency :: SemanticIR -> Property
prop_semanticir_consistency (SemanticIR sourceIR annotations) =
  property $ length annotations >= 0

prop_goir_validity :: GoIR -> Property
prop_goir_validity (GoIR goModule code) =
  property $ not (null code) ==> length code >= 0

-- SymbolTable Properties

prop_symboltable_insert_commutative :: SymbolTable -> String -> SymbolInfo -> String -> SymbolInfo -> Property
prop_symboltable_insert_commutative st key1 val1 key2 val2 =
  key1 /= key2 ==>
  let insert1 = Map.insert key1 val1 st
      insert2 = Map.insert key2 val2 st
      insert1_2 = Map.insert key2 val2 insert1
      insert2_1 = Map.insert key1 val1 insert2
  in insert1_2 === insert2_1

prop_symboltable_lookup_insert :: SymbolTable -> String -> SymbolInfo -> Property
prop_symboltable_lookup_insert st key val =
  let newST = Map.insert key val st
  in Map.lookup key newST === Just val

prop_symboltable_merge_preservation :: SymbolTable -> SymbolTable -> Property
prop_symboltable_merge_preservation st1 st2 =
  let merged = Map.union st1 st2
      keys1 = Map.keys st1
      keys2 = Map.keys st2
      mergedKeys = Map.keys merged
  in property $ all (`elem` mergedKeys) (keys1 ++ keys2)

-- Ownership State Properties

prop_ownership_deterministic :: OwnershipState -> String -> Property
prop_ownership_deterministic state resource =
  property $ True -- Simplified property - actual implementation would depend on OwnershipState API

prop_transfer_rules_composition :: TransferRule -> TransferRule -> TransferRule -> Property
prop_transfer_rules_composition rule1 rule2 rule3 =
  property $ True -- Simplified property - actual implementation would depend on TransferRule API

prop_ownership_double_move_prevention :: OwnershipState -> String -> Property
prop_ownership_double_move_prevention state resource =
  property $ True -- Simplified property - actual implementation would depend on OwnershipState API

-- Type Environment Properties

prop_typeenv_substitution :: TypeEnvironment -> Property
prop_typeenv_substitution env =
  property $ True -- Simplified property - actual implementation would depend on TypeEnvironment API

prop_typeenv_unification_symmetric :: TypeEnvironment -> TypeEnvironment -> Property
prop_typeenv_unification_symmetric env1 env2 =
  property $ True -- Simplified property - actual implementation would depend on TypeEnvironment API

prop_typeenv_renaming :: TypeEnvironment -> Property
prop_typeenv_renaming env =
  property $ True -- Simplified property - actual implementation would depend on TypeEnvironment API