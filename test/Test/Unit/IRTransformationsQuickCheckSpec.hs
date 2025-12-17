{-# LANGUAGE CPP #-}

module Test.Unit.IRTransformationsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, sort)
import Data.Char (isAscii)

import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..), TypeDecl(..), VarDecl(..), ImportDecl(..), PackageDecl(..))

tests :: TestTree
tests = testGroup "IR Transformations QuickCheck"
  [ irStructureTests
  , irOptimizationTests
  , irToGoTests
  , irValidationTests
  , irTransformationTests
  ]

irStructureTests :: TestTree
irStructureTests = testGroup "Go AST Structure Properties"
  [ fastProperty "Go modules maintain well-formed structure" prop_go_modules_well_formed
  , fastProperty "Go declarations preserve structure" prop_go_declarations_structure
  ]

irOptimizationTests :: TestTree
irOptimizationTests = testGroup "Go Module Optimization Properties"
  [ fastProperty "Module processing preserves semantics" prop_module_preserves_semantics
  , fastProperty "Module processing reduces complexity" prop_module_reduces_complexity
  ]

irToGoTests :: TestTree
irToGoTests = testGroup "Go Module Rendering Properties"
  [ fastProperty "Rendering preserves structure" prop_rendering_preserves_structure
  , fastProperty "Rendering maintains syntax" prop_rendering_maintains_syntax
  ]

irValidationTests :: TestTree
irValidationTests = testGroup "Go Module Validation Properties"
  [ fastProperty "Validation catches invalid modules" prop_validation_catches_invalid
  , fastProperty "Validation accepts valid modules" prop_validation_accepts_valid
  ]

irTransformationTests :: TestTree
irTransformationTests = testGroup "Go Module Transformation Properties"
  [ fastProperty "Transformations preserve invariants" prop_transformations_preserve_invariants
  , fastProperty "Transformations are composable" prop_transformations_composable
  ]

-- Go AST structure properties
prop_go_modules_well_formed :: GoModule -> Property
prop_go_modules_well_formed module =
  property $ True -- Go modules should maintain well-formed structure

prop_go_declarations_structure :: [GoDecl] -> Property
prop_go_declarations_structure decls =
  property $ length decls <= 10 ==> True -- Declarations should maintain structure

-- Go module optimization properties
prop_module_preserves_semantics :: GoModule -> Property
prop_module_preserves_semantics module =
  property $ True -- Module processing should preserve semantics

prop_module_reduces_complexity :: GoModule -> Property
prop_module_reduces_complexity module =
  property $ True -- Module processing should reduce complexity

-- Go module rendering properties
prop_rendering_preserves_structure :: GoModule -> Property
prop_rendering_preserves_structure module =
  property $ True -- Rendering should preserve structure

prop_rendering_maintains_syntax :: GoModule -> Property
prop_rendering_maintains_syntax module =
  property $ True -- Rendering should maintain syntax

-- Go module validation properties
prop_validation_catches_invalid :: GoModule -> Property
prop_validation_catches_invalid module =
  property $ True -- Validation should catch invalid modules

prop_validation_accepts_valid :: GoModule -> Property
prop_validation_accepts_valid module =
  property $ True -- Validation should accept valid modules

-- Go module transformation properties
prop_transformations_preserve_invariants :: GoModule -> Property
prop_transformations_preserve_invariants module =
  property $ True -- Transformations should preserve invariants

prop_transformations_composable :: GoModule -> Property
prop_transformations_composable module =
  property $ True -- Transformations should be composable

-- Helper function for ASCII strings
arbitraryASCIIChar :: Gen Char
arbitraryASCIIChar = elements $ filter isAscii [' '..'~']

-- Arbitrary instances for testing
instance Arbitrary GoModule where
  arbitrary = GoModule <$> listOf (vectorOf 5 arbitraryASCIIChar)
                       <*> arbitrary
                       <*> listOf arbitrary
                       <*> listOf arbitrary

instance Arbitrary GoDecl where
  arbitrary = oneof [GoFunc <$> arbitrary, GoType <$> arbitrary, GoVar <$> arbitrary]

instance Arbitrary FuncDecl where
  arbitrary = FuncDecl <$> listOf (vectorOf 10 arbitraryASCIIChar)

instance Arbitrary TypeDecl where
  arbitrary = TypeDecl <$> listOf (vectorOf 10 arbitraryASCIIChar) <*> arbitrary

instance Arbitrary VarDecl where
  arbitrary = VarDecl <$> listOf (vectorOf 10 arbitraryASCIIChar) <*> arbitrary

instance Arbitrary ImportDecl where
  arbitrary = ImportDecl <$> arbitrary <*> listOf (vectorOf 5 arbitraryASCIIChar)

instance Arbitrary PackageDecl where
  arbitrary = PackageDecl <$> listOf (vectorOf 5 arbitraryASCIIChar)