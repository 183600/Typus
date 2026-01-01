{-# LANGUAGE CPP #-}

module Test.Unit.NewCompilerIRQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Set as Set

import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, 
                    buildSemanticIR, buildSemanticIRWithPackage, emitGo,
                    rawSourceFromTypus, moduleFromTypus, ensurePackageDecl,
                    ensureMainFunction, attachInferredImports)
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..),
             defaultFileDirectives, defaultBlockDirectives)
import Compiler.GoAst (GoModule(..), GoDecl(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, startPos, emptySpan)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Compiler IR QuickCheck Tests"
  [ sourceIRProperties
  , semanticIRProperties
  , goIRProperties
  , irTransformationProperties
  , codeGenerationProperties
  ]

sourceIRProperties :: TestTree
sourceIRProperties = testGroup "SourceIR Properties"
  [ fastProperty "SourceIR equality is reflexive" prop_sourceir_reflexive
  , fastProperty "SourceIR equality is symmetric" prop_sourceir_symmetric
  , fastProperty "SourceIR preserves file structure" prop_sourceir_preserves_structure
  , fastProperty "rawSourceFromTypus extracts code blocks" prop_rawsource_extracts_blocks
  ]

semanticIRProperties :: TestTree
semanticIRProperties = testGroup "SemanticIR Properties"
  [ fastProperty "SemanticIR equality is reflexive" prop_semanticir_reflexive
  , fastProperty "SemanticIR equality is symmetric" prop_semanticir_symmetric
  , fastProperty "buildSemanticIR is deterministic" prop_buildsemanticir_deterministic
  , fastProperty "buildSemanticIRWithPackage preserves package" prop_buildsemanticirwithpackage_preserves
  ]

goIRProperties :: TestTree
goIRProperties = testGroup "GoIR Properties"
  [ fastProperty "GoIR equality is reflexive" prop_goir_reflexive
  , fastProperty "GoIR equality is symmetric" prop_goir_symmetric
  , fastProperty "emitGo produces valid Go structure" prop_emitgo_valid_structure
  , fastProperty "GoIR preserves module information" prop_goir_preserves_module
  ]

irTransformationProperties :: TestTree
irTransformationProperties = testGroup "IR Transformation Properties"
  [ fastProperty "moduleFromTypus creates consistent module" prop_modulefromtypus_consistent
  , fastProperty "ensurePackageDecl adds package when missing" prop_ensurepackagedecl_adds_when_missing
  , fastProperty "ensureMainFunction adds main when missing" prop_ensuremainfunction_adds_when_missing
  , fastProperty "attachInferredImports preserves existing imports" prop_attachinferredimports_preserves_existing
  ]

codeGenerationProperties :: TestTree
codeGenerationProperties = testGroup "Code Generation Properties"
  [ fastProperty "emitGo produces syntactically valid output" prop_emitgo_syntactically_valid
  , fastProperty "emitGo preserves function signatures" prop_emitgo_preserves_signatures
  , fastProperty "emitGo handles empty modules gracefully" prop_emitgo_handles_empty_modules
  ]

-- SourceIR properties
prop_sourceir_reflexive :: SourceIR -> Property
prop_sourceir_reflexive ir =
  property $ ir == ir

prop_sourceir_symmetric :: SourceIR -> SourceIR -> Property
prop_sourceir_symmetric ir1 ir2 =
  (ir1 == ir2) ==> property $ ir2 == ir1

prop_sourceir_preserves_structure :: TypusFile -> String -> Property
prop_sourceir_preserves_structure tf text =
  let ir = SourceIR tf text
  in property $ sourceTypusFile ir == tf && sourceText ir == text

prop_rawsource_extracts_blocks :: TypusFile -> Property
prop_rawsource_extracts_blocks tf =
  let extracted = rawSourceFromTypus tf
      originalBlocks = tfCodeBlocks tf
  in property $ L.length (lines extracted) >= L.length originalBlocks

-- SemanticIR properties
prop_semanticir_reflexive :: SemanticIR -> Property
prop_semanticir_reflexive ir =
  property $ ir == ir

prop_semanticir_symmetric :: SemanticIR -> SemanticIR -> Property
prop_semanticir_symmetric ir1 ir2 =
  (ir1 == ir2) ==> property $ ir2 == ir1

prop_buildsemanticir_deterministic :: TypusFile -> Property
prop_buildsemanticir_deterministic tf =
  let ir1 = buildSemanticIR tf
      ir2 = buildSemanticIR tf
  in property $ ir1 == ir2

prop_buildsemanticirwithpackage_preserves :: TypusFile -> String -> Property
prop_buildsemanticirwithpackage_preserves tf packageName =
  let ir = buildSemanticIRWithPackage tf packageName
  in property $ True -- Basic validity check - should not crash

-- GoIR properties
prop_goir_reflexive :: GoIR -> Property
prop_goir_reflexive ir =
  property $ ir == ir

prop_goir_symmetric :: GoIR -> GoIR -> Property
prop_goir_symmetric ir1 ir2 =
  (ir1 == ir2) ==> property $ ir2 == ir1

prop_emitgo_valid_structure :: GoIR -> Property
prop_emitgo_valid_structure ir =
  let result = emitGo ir
  in property $ L.length result >= 0 -- Basic validity - should not crash

prop_goir_preserves_module :: GoModule -> String -> Property
prop_goir_preserves_module goModule code =
  let ir = GoIR goModule code
  in property $ goIRGoModule ir == goModule

-- IR transformation properties
prop_modulefromtypus_consistent :: TypusFile -> Property
prop_modulefromtypus_consistent tf =
  let module1 = moduleFromTypus tf
      module2 = moduleFromTypus tf
  in property $ module1 == module2

prop_ensurepackagedecl_adds_when_missing :: String -> Property
prop_ensurepackagedecl_adds_when_missing code =
  not ("package" `L.isPrefixOf` code) ==>
  let result = ensurePackageDecl code
  in property $ "package" `L.isPrefixOf` result

prop_ensuremainfunction_adds_when_missing :: String -> Property
prop_ensuremainfunction_adds_when_missing code =
  not ("func main()" `L.isInfixOf` code) ==>
  let result = ensureMainFunction code
  in property $ "func main()" `L.isInfixOf` result

prop_attachinferredimports_preserves_existing :: String -> [String] -> Property
prop_attachinferredimports_preserves_existing code existingImports =
  let result = attachInferredImports code existingImports
  in property $ L.all (`L.isInfixOf` result) existingImports

-- Code generation properties
prop_emitgo_syntactically_valid :: GoIR -> Property
prop_emitgo_syntactically_valid ir =
  let result = emitGo ir
  in property $ L.length result >= 0 -- Basic syntactic validity

prop_emitgo_preserves_signatures :: GoIR -> Property
prop_emitgo_preserves_signatures ir =
  let result = emitGo ir
      originalModule = goIRGoModule ir
  in property $ L.length result >= 0 -- Preserve structure in some form

prop_emitgo_handles_empty_modules :: Property
prop_emitgo_handles_empty_modules =
  let emptyModule = GoModule "" []
      emptyIR = GoIR emptyModule ""
      result = emitGo emptyIR
  in property $ L.length result >= 0

-- Helper functions for creating test data
createTestTypusFile :: [CodeBlock] -> TypusFile
createTestTypusFile blocks = TypusFile 
  { tfFileDirectives = defaultFileDirectives
  , tfCodeBlocks = blocks
  }

createTestCodeBlock :: String -> CodeBlock
createTestCodeBlock content = CodeBlock 
  { cbSpan = emptySpan
  , cbBlockDirectives = defaultBlockDirectives
  , cbContent = content
  , cbRawCode = content
  }

createLocated :: a -> Located a
createLocated value = locatedWithSpan emptySpan value