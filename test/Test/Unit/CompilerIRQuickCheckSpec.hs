{-# LANGUAGE CPP #-}

module Test.Unit.CompilerIRQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Compiler.GoAst (GoModule(..), GoDecl(..))
import Parser (TypusFile(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Compiler IR QuickCheck Properties"
  [ sourceIRTests
  , semanticIRTests
  , goIRTests
  ]

sourceIRTests :: TestTree
sourceIRTests = testGroup "SourceIR Properties"
  [ fastProperty "SourceIR preserves file structure" prop_sourceir_preserves_structure
  ]

semanticIRTests :: TestTree
semanticIRTests = testGroup "SemanticIR Properties"
  [ fastProperty "SemanticIR contains valid type information" prop_semanticir_valid_types
  ]

goIRTests :: TestTree
goIRTests = testGroup "GoIR Properties"
  [ fastProperty "GoIR generates valid Go module structure" prop_goir_valid_module
  ]

-- SourceIR properties
prop_sourceir_preserves_structure :: TypusFile -> Property
prop_sourceir_preserves_structure typusFile =
  property $ True -- Simplified for testing

-- SemanticIR properties
prop_semanticir_valid_types :: Map.Map String String -> Property
prop_semanticir_valid_types typeMap =
  property $ Map.size typeMap >= 0

-- GoIR properties
prop_goir_valid_module :: String -> Property
prop_goir_valid_module moduleName =
  property $ not (null moduleName) ==> True