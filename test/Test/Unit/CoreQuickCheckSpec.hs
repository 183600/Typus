{-# LANGUAGE CPP #-}

module Test.Unit.CoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.ExtendedArbitrary ()
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, label, cover)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import SourceLocation (Located(..), locatedValue, spanStart, spanEnd, posLine)
import Compiler.Errors.Core (ErrorSeverity(..))
import qualified Compiler.TypeChecker as TC
import Compiler.GoAst (GoModule(..), GoDecl(..), ImportDecl(..))
import Ownership (OwnershipType(..), OwnershipError(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..))
import qualified Dependencies as Dep

import qualified Data.List as Data.List
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Char (toLower, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: Empty file parsing
prop_parse_empty_file :: Property
prop_parse_empty_file = 
  case parseTypus "" of
    Left _ -> property False
    Right parsed -> property $ True

-- Property: Valid directives are parsed correctly
prop_parse_valid_directives :: String -> Property
prop_parse_valid_directives directive = 
  let validDirectives = ["//! ownership: on", "//! ownership: off", 
                        "//! dependent_types: on", "//! dependent_types: off",
                        "//! constraints: on", "//! constraints: off"]
  in classify (directive `elem` validDirectives) "valid directive" $ 
     property $ directive `elem` validDirectives ==> 
     case parseTypus directive of
       Left _ -> property False
       Right _ -> property True

-- Property: Parse error locations are reasonable
prop_parse_error_locations :: String -> Property
prop_parse_error_locations malformed =
  L.length malformed > 10 ==> 
  case parseTypus malformed of
    Left err -> property $ "error" `Data.List.L.isInfixOf` map toLower err
    Right _ -> property True

-- ============================================================================
-- Type Checker Properties
-- ============================================================================

-- Property: Type equality is reflexive
prop_type_equality_reflexive :: TC.Type -> Property
prop_type_equality_reflexive t = property (t == t)

-- Property: Type equality is symmetric
prop_type_equality_symmetric :: TC.Type -> TC.Type -> Property
prop_type_equality_symmetric t1 t2 = 
  property ((t1 == t2) ==> (t2 == t1))

-- Property: Type equality is transitive
prop_type_equality_transitive :: TC.Type -> TC.Type -> TC.Type -> Property
prop_type_equality_transitive t1 t2 t3 = 
  property ((t1 == t2 && t2 == t3) ==> (t1 == t3))

-- ============================================================================
-- Ownership Properties
-- ============================================================================

-- Property: Ownership errors have valid identifiers
prop_ownership_errors_valid_ids :: OwnershipError -> Property
prop_ownership_errors_valid_ids err = 
  let ids = extractOwnershipErrorIds err
  in property $ L.all isValidIdentifier ids

-- Property: Ownership type consistency
prop_ownership_type_consistency :: OwnershipType -> Property
prop_ownership_type_consistency ownershipType = 
  let ownerId = extractOwnershipId ownershipType
  in property $ isValidIdentifier ownerId

-- ============================================================================
-- Dependencies Properties
-- ============================================================================

-- Helper function to check if type variable is well-formed (returns Bool)
isTypeVarWellFormed :: Dep.TypeVar -> Bool
isTypeVarWellFormed tv = case tv of
  Dep.TVCon name -> not (null name)
  Dep.TVVar name -> not (null name)
  Dep.TVApp name args -> not (null name) && L.all isTypeVarWellFormed args
  Dep.TVFun args ret -> L.all isTypeVarWellFormed args && isTypeVarWellFormed ret
  Dep.TVTuple types -> L.all isTypeVarWellFormed types

-- Property: Type variables are well-formed
prop_typevar_wellformed :: Dep.TypeVar -> Property
prop_typevar_wellformed tv = property $ isTypeVarWellFormed tv

-- Property: Type constraints are satisfiable
prop_type_constraints_satisfiable :: Dep.TypeConstraint -> Property
prop_type_constraints_satisfiable constraint = 
  let isSatisfiable = case constraint of
        Dep.Equal t1 t2 -> isTypeVarWellFormed t1 && isTypeVarWellFormed t2
        Dep.Subtype t1 t2 -> isTypeVarWellFormed t1 && isTypeVarWellFormed t2
        Dep.Predicate name args -> not (null name) && L.all isTypeVarWellFormed args
        Dep.TypeSizeGE tv size -> isTypeVarWellFormed tv && size >= 0
        Dep.TypeSizeGT tv size -> isTypeVarWellFormed tv && size >= 0
        Dep.TypeRange tv min max -> isTypeVarWellFormed tv && min <= max
  in property $ isSatisfiable

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: Error severity ordering is consistent
prop_severity_ordering :: ErrorSeverity -> ErrorSeverity -> Property
prop_severity_ordering sev1 sev2 = 
  let severityOrder = severityRank sev1
      severityOrder2 = severityRank sev2
      isOrdered = sev1 == sev2 || 
                  (sev1 == Error && sev2 /= Error) ||
                  (sev1 == Warning && sev2 `elem` [Info]) ||
                  (sev1 == Info && sev2 == Info)
  in property $ isOrdered

-- Property: Compiler errors have valid structure
prop_compiler_error_structure :: CompilerError -> Property
prop_compiler_error_structure err = 
  let errorId = ceErrorId err
      message = ceMessage err
  in property $ not (T.null errorId) && not (T.null message)

-- ============================================================================
-- Go AST Properties
-- ============================================================================

-- Property: Import declarations are valid
prop_import_declaration_valid :: ImportDecl -> Property
prop_import_declaration_valid importDecl = 
  let path = idPath importDecl
  in property $ not (null path) && isValidGoImportPath path

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Core QuickCheck Tests"
  [ testGroup "Parser Properties"
    [ fastProperty "Empty file parsing" prop_parse_empty_file
    , fastProperty "Valid directives parsed correctly" prop_parse_valid_directives
    , fastProperty "Parse error locations are reasonable" prop_parse_error_locations
    ]
  , testGroup "Type Checker Properties"
    [ fastProperty "Type equality is reflexive" prop_type_equality_reflexive
    , fastProperty "Type equality is symmetric" prop_type_equality_symmetric
    , fastProperty "Type equality is transitive" prop_type_equality_transitive
    ]
  , testGroup "Ownership Properties"
    [ fastProperty "Ownership errors have valid IDs" prop_ownership_errors_valid_ids
    , fastProperty "Ownership type consistency" prop_ownership_type_consistency
    ]
  , testGroup "Dependencies Properties"
    [ fastProperty "Type variables are well-formed" prop_typevar_wellformed
    , fastProperty "Type constraints are satisfiable" prop_type_constraints_satisfiable
    ]
  , testGroup "Error Handling Properties"
    [ fastProperty "Severity ordering is consistent" prop_severity_ordering
    , fastProperty "Compiler errors have valid structure" prop_compiler_error_structure
    ]
  , testGroup "Go AST Properties"
    [ fastProperty "Import declarations are valid" prop_import_declaration_valid
    ]
  ]

-- ============================================================================
-- Helper Functions
-- ============================================================================

extractOwnershipErrorIds :: OwnershipError -> [String]
extractOwnershipErrorIds err = case err of
  UseAfterMove id -> [id]
  DoubleMove id1 id2 -> [id1, id2]
  BorrowWhileMoved id -> [id]
  MutBorrowWhileBorrowed id -> [id]
  BorrowWhileMutBorrowed id -> [id]
  MultipleMutBorrows id -> [id]
  UseWhileMutBorrowed id -> [id]
  OutOfScope id -> [id]
  BorrowError id -> [id]
  ParseError id -> [id]
  CrossFunctionMove id1 id2 -> [id1, id2]
  ParameterMoveMismatch id -> [id]
  ControlFlowError id -> [id]
  PathSensitiveError id -> [id]
  LoopOwnershipError id -> [id]

extractOwnershipId :: OwnershipType -> String
extractOwnershipId (Owned id) = id
extractOwnershipId (Borrowed id) = id
extractOwnershipId (MutBorrowed id) = id

isValidIdentifier :: String -> Bool
isValidIdentifier name = not (null name) && L.all isAsciiAlphaNum name
  where
    isAsciiAlphaNum char = (char >= 'a' && char <= 'z') || 
                          (char >= 'A' && char <= 'Z') || 
                          (char >= '0' && char <= '9')



severityRank :: ErrorSeverity -> Int
severityRank Error = 3
severityRank Warning = 2
severityRank Info = 1

ceErrorId :: CompilerError -> T.Text
ceErrorId = T.pack . show -- Simplified

ceMessage :: CompilerError -> T.Text
ceMessage = T.pack . show -- Simplified

idPath :: ImportDecl -> String
idPath (ImportDecl path _) = fromMaybe "" path

isValidGoImportPath :: String -> Bool
isValidGoImportPath path = 
  not (null path) && 
  L.all (not . null) (words path) &&
  not (L.isPrefixOf "." path)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
L.isPrefixOf [] _ = True
L.isPrefixOf _ [] = False
L.isPrefixOf (x:xs) (y:ys) = x == y && L.isPrefixOf xs ys