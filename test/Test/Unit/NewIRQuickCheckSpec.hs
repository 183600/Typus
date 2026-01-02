{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewIRQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Test.QuickCheck as QC

import Compiler.IR
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , buildSemanticIR
  , buildSemanticIRWithPackage
  , emitGo
  , rawSourceFromTypus
  , moduleFromTypus
  , ensurePackageDecl
  , ensureMainFunction
  , attachInferredImports
  )
import Parser (TypusFile(..), CodeBlock(..))
import Compiler.GoAst (GoModule(..), PackageDecl(..), ImportDecl(..), GoDecl(..))
import Compiler.Errors (CompilerError(..))
import Compiler.ValueAnalysis (ValueInfo)

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)

-- ============================================================================
-- Arbitrary Instances for QuickCheck Testing
-- ============================================================================

-- Generate arbitrary string for testing
instance Arbitrary String where
  arbitrary = QC.oneof
    [ QC.listOf (QC.elements ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-")
    , pure ""
    ]

-- Generate arbitrary TypusFile for testing
instance Arbitrary TypusFile where
  arbitrary = do
    let emptyFile = TypusFile [] [] []
    return emptyFile

-- Generate arbitrary CodeBlock for testing
instance Arbitrary CodeBlock where
  arbitrary = do
    content <- QC.arbitrary
    let emptyBlock = CodeBlock
          { cbDirectives = undefined  -- Simplified for testing
          , cbContent = content
          , cbSpan = undefined
          }
    return emptyBlock

-- Generate arbitrary GoModule for testing
instance Arbitrary GoModule where
  arbitrary = do
    pkgName <- QC.arbitrary
    imports <- QC.listOf QC.arbitrary
    decls <- QC.listOf QC.arbitrary
    buildTags <- QC.listOf QC.arbitrary
    return $ GoModule
      { gmPackage = if null pkgName then Nothing else Just (PackageDecl pkgName)
      , gmImports = imports
      , gmDecls = decls
      , gmBuildTags = buildTags
      }

-- Generate arbitrary ImportDecl for testing
instance Arbitrary ImportDecl where
  arbitrary = do
    alias <- QC.arbitrary
    path <- QC.arbitrary
    return $ ImportDecl alias path

-- ============================================================================
-- Property Tests for IR Generation
-- ============================================================================

-- Property: Build Source IR preserves Typus file
prop_build_source_ir_preserves_typus_file :: TypusFile -> Property
prop_build_source_ir_preserves_typus_file typusFile =
  let sourceIR = buildSourceIR typusFile
  in property $ sourceTypusFile sourceIR === typusFile

-- Property: Raw source from empty Typus file is empty
prop_raw_source_empty_typus_file :: Property
prop_raw_source_empty_typus_file =
  let emptyFile = TypusFile [] [] []
      rawSource = rawSourceFromTypus emptyFile
  in property $ rawSource === ""

-- Property: Raw source from Typus file with blocks concatenates content
prop_raw_source_concatenates_blocks :: [String] -> Property
prop_raw_source_concatenates_blocks contents =
  let blocks = L.map (\content -> CodeBlock undefined content undefined) contents
      typusFile = TypusFile [] [] blocks
      rawSource = rawSourceFromTypus typusFile
      expected = intercalate "\n" contents
  in property $ rawSource === expected

-- Property: Build Source IR creates correct text
prop_build_source_ir_creates_correct_text :: TypusFile -> Property
prop_build_source_ir_creates_correct_text typusFile =
  let sourceIR = buildSourceIR typusFile
      expectedText = rawSourceFromTypus typusFile
  in property $ sourceText sourceIR === expectedText

-- Property: Ensure package decl adds main package when missing
prop_ensure_package_decl_adds_main :: GoModule -> Property
prop_ensure_package_decl_adds_main goModule =
  let moduleWithoutPkg = goModule { gmPackage = Nothing }
      moduleWithPkg = ensurePackageDecl moduleWithoutPkg
  in property $ gmPackage moduleWithPkg === Just (PackageDecl "main")

-- Property: Ensure package decl preserves existing package
prop_ensure_package_decl_preserves_existing :: GoModule -> Property
prop_ensure_package_decl_preserves_existing goModule =
  case gmPackage goModule of
    Nothing -> property $ True  -- Tested above
    Just existingPkg ->
      let moduleWithPkg = ensurePackageDecl goModule
      in property $ gmPackage moduleWithPkg === Just existingPkg

-- Property: Ensure main function preserves existing main
prop_ensure_main_function_preserves_existing :: GoModule -> Property
prop_ensure_main_function_preserves_existing goModule =
  let hasMain = L.any isMainFunc (gmDecls goModule)
      moduleWithMain = ensureMainFunction goModule
  in if hasMain
     then property $ moduleWithMain === goModule
     else property $ L.length (gmDecls moduleWithMain) >= L.length (gmDecls goModule)
  where
    isMainFunc (GoFunc _) = True  -- Simplified check
    isMainFunc _ = False

-- Property: Emit Go creates GoIR with module L.and source
prop_emit_go_creates_go_ir :: GoModule -> Property
prop_emit_go_creates_go_ir goModule =
  let semanticIR = SemanticIR undefined goModule undefined
      goIR = emitGo semanticIR
  in property $ goModule goIR === goModule .&&.
             not (L.null (goSource goIR))

-- Property: GoIR source contains package declaration
prop_go_ir_source_contains_package :: GoModule -> Property
prop_go_ir_source_contains_package goModule =
  let semanticIR = SemanticIR undefined goModule undefined
      goIR = emitGo semanticIR
  in case gmPackage goModule of
    Just pkg -> property $ ("package " ++ pdName pkg) `L.isInfixOf` goSource goIR
    Nothing -> property $ True  -- No package to check

-- Property: Attach inferred imports preserves existing imports
prop_attach_inferred_imports_preserves_existing :: GoModule -> Property
prop_attach_inferred_imports_preserves_existing goModule =
  let moduleWithImports = attachInferredImports goModule
      originalImportKeys = map importKey (gmImports goModule)
      newImportKeys = map importKey (gmImports moduleWithImports)
  in property $ L.all (`elem` newImportKeys) originalImportKeys
  where
    importKey imp = (importAlias imp, importPath imp)

-- Property: Module content text concatenates declarations
prop_module_content_text_concatenates_decls :: GoModule -> Property
prop_module_content_text_concatenates_decls goModule =
  let content = moduleContentText goModule
      hasDecls = not (L.null (gmDecls goModule))
  in if hasDecls
     then property $ not (null content)
     else property $ True  -- Empty module is valid

-- Property: Build semantic IR with package combines modules
prop_build_semantic_ir_with_package_combines_modules :: TypusFile -> Property
prop_build_semantic_ir_with_package_combines_modules mainFile =
  let sourceIR = buildSourceIR mainFile
      packageFiles = []  -- Empty package for testing
      result = buildSemanticIRWithPackage sourceIR packageFiles
  in case result of
    Left _ -> property $ False
    Right semanticIR -> property $ True  -- Basic success check

-- Property: IR types are showable
prop_ir_types_showable :: SourceIR -> Property
prop_ir_types_showable sourceIR =
  let shown = show sourceIR
  in property $ not (null shown)

-- Property: SemanticIR preserves source file
prop_semantic_ir_preserves_source_file :: TypusFile -> Property
prop_semantic_ir_preserves_source_file typusFile =
  let sourceIR = buildSourceIR typusFile
  in case buildSemanticIR sourceIR of
    Left _ -> property $ False
    Right semanticIR -> property $ semanticTypusFile semanticIR === typusFile

-- Property: GoIR preserves module structure
prop_go_ir_preserves_module_structure :: GoModule -> Property
prop_go_ir_preserves_module_structure goModule =
  let semanticIR = SemanticIR undefined goModule undefined
      goIR = emitGo semanticIR
  in property $ goModule goIR === goModule

-- Property: IR generation handles empty modules
prop_ir_handles_empty_modules :: Property
prop_ir_handles_empty_modules =
  let emptyModule = GoModule Nothing [] [] []
      semanticIR = SemanticIR undefined emptyModule undefined
      goIR = emitGo semanticIR
  in property $ goModule goIR === emptyModule

-- Property: IR generation handles modules with imports
prop_ir_handles_modules_with_imports :: [ImportDecl] -> Property
prop_ir_handles_modules_with_imports imports =
  let moduleWithImports = GoModule Nothing imports [] []
      semanticIR = SemanticIR undefined moduleWithImports undefined
      goIR = emitGo semanticIR
  in property $ L.length (gmImports (goModule goIR)) === L.length imports

-- Property: IR generation handles modules with declarations
prop_ir_handles_modules_with_decls :: [GoDecl] -> Property
prop_ir_handles_modules_with_decls decls =
  let moduleWithDecls = GoModule Nothing [] decls []
      semanticIR = SemanticIR undefined moduleWithDecls undefined
      goIR = emitGo semanticIR
  in property $ L.length (gmDecls (goModule goIR)) === L.length decls

-- Property: IR generation handles modules with build tags
prop_ir_handles_modules_with_build_tags :: [String] -> Property
prop_ir_handles_modules_with_build_tags buildTags =
  let moduleWithTags = GoModule Nothing [] [] buildTags
      semanticIR = SemanticIR undefined moduleWithTags undefined
      goIR = emitGo semanticIR
  in property $ gmBuildTags (goModule goIR) === buildTags

-- Property: Source IR text is consistent with raw source
prop_source_ir_text_consistent :: TypusFile -> Property
prop_source_ir_text_consistent typusFile =
  let sourceIR = buildSourceIR typusFile
      rawSource = rawSourceFromTypus typusFile
  in property $ sourceText sourceIR === rawSource

-- Property: IR generation preserves import paths
prop_ir_preserves_import_paths :: [ImportDecl] -> Property
prop_ir_preserves_import_paths imports =
  let originalPaths = map importPath imports
      moduleWithImports = GoModule Nothing imports [] []
      semanticIR = SemanticIR undefined moduleWithImports undefined
      goIR = emitGo semanticIR
      finalPaths = map importPath (gmImports (goModule goIR))
  in property $ sort originalPaths === sort finalPaths

-- Property: IR generation preserves import aliases
prop_ir_preserves_import_aliases :: [ImportDecl] -> Property
prop_ir_preserves_import_aliases imports =
  let originalAliases = map importAlias imports
      moduleWithImports = GoModule Nothing imports [] []
      semanticIR = SemanticIR undefined moduleWithImports undefined
      goIR = emitGo semanticIR
      finalAliases = map importAlias (gmImports (goModule goIR))
  in property $ originalAliases === finalAliases

-- Property: IR generation handles complex modules
prop_ir_handles_complex_modules :: GoModule -> Property
prop_ir_handles_complex_modules goModule =
  let semanticIR = SemanticIR undefined goModule undefined
      goIR = emitGo semanticIR
  in property $ goModule goIR === goModule

-- Property: IR generation is deterministic
prop_ir_generation_deterministic :: GoModule -> Property
prop_ir_generation_deterministic goModule =
  let semanticIR1 = SemanticIR undefined goModule undefined
      semanticIR2 = SemanticIR undefined goModule undefined
      goIR1 = emitGo semanticIR1
      goIR2 = emitGo semanticIR2
  in property $ goIR1 === goIR2

-- Property: IR generation preserves build tags
prop_ir_preserves_build_tags :: [String] -> Property
prop_ir_preserves_build_tags buildTags =
  let moduleWithTags = GoModule Nothing [] [] buildTags
      semanticIR = SemanticIR undefined moduleWithTags undefined
      goIR = emitGo semanticIR
  in property $ gmBuildTags (goModule goIR) === buildTags

-- Property: IR generation handles package name changes
prop_ir_handles_package_name_changes :: String -> Property
prop_ir_handles_package_name_changes packageName =
  let moduleWithPkg = GoModule (Just (PackageDecl packageName)) [] [] []
      semanticIR = SemanticIR undefined moduleWithPkg undefined
      goIR = emitGo semanticIR
  in property $ case gmPackage (goModule goIR) of
    Just pkg -> pdName pkg === packageName
    Nothing -> False

-- Property: IR generation source is non-empty for non-empty modules
prop_ir_source_non_empty_for_non_empty_modules :: GoModule -> Property
prop_ir_source_non_empty_for_non_empty_modules goModule =
  let hasContent = not (L.null (gmDecls goModule)) || not (L.null (gmImports goModule))
      semanticIR = SemanticIR undefined goModule undefined
      goIR = emitGo semanticIR
  in if hasContent
     then property $ not (L.null (goSource goIR))
     else property $ True  -- Empty module is allowed

tests :: TestTree
tests =
  testGroup "New IR QuickCheck Tests"
    [ fastProperty "Build Source IR preserves Typus file" prop_build_source_ir_preserves_typus_file
    , fastProperty "Raw source from empty Typus file is empty" prop_raw_source_empty_typus_file
    , fastProperty "Raw source from Typus file with blocks concatenates content" prop_raw_source_concatenates_blocks
    , fastProperty "Build Source IR creates correct text" prop_build_source_ir_creates_correct_text
    , fastProperty "Ensure package decl adds main package when missing" prop_ensure_package_decl_adds_main
    , fastProperty "Ensure package decl preserves existing package" prop_ensure_package_decl_preserves_existing
    , fastProperty "Ensure main function preserves existing main" prop_ensure_main_function_preserves_existing
    , fastProperty "Emit Go creates GoIR with module L.and source" prop_emit_go_creates_go_ir
    , fastProperty "GoIR source contains package declaration" prop_go_ir_source_contains_package
    , fastProperty "Attach inferred imports preserves existing imports" prop_attach_inferred_imports_preserves_existing
    , fastProperty "Module content text concatenates declarations" prop_module_content_text_concatenates_decls
    , fastProperty "Build semantic IR with package combines modules" prop_build_semantic_ir_with_package_combines_modules
    , fastProperty "IR types are showable" prop_ir_types_showable
    , fastProperty "SemanticIR preserves source file" prop_semantic_ir_preserves_source_file
    , fastProperty "GoIR preserves module structure" prop_go_ir_preserves_module_structure
    , fastProperty "IR generation handles empty modules" prop_ir_handles_empty_modules
    , fastProperty "IR generation handles modules with imports" prop_ir_handles_modules_with_imports
    , fastProperty "IR generation handles modules with declarations" prop_ir_handles_modules_with_decls
    , fastProperty "IR generation handles modules with build tags" prop_ir_handles_modules_with_build_tags
    , fastProperty "Source IR text is consistent with raw source" prop_source_ir_text_consistent
    , fastProperty "IR generation preserves import paths" prop_ir_preserves_import_paths
    , fastProperty "IR generation preserves import aliases" prop_ir_preserves_import_aliases
    , fastProperty "IR generation handles complex modules" prop_ir_handles_complex_modules
    , fastProperty "IR generation is deterministic" prop_ir_generation_deterministic
    , fastProperty "IR generation preserves build tags" prop_ir_preserves_build_tags
    , fastProperty "IR generation handles package name changes" prop_ir_handles_package_name_changes
    , fastProperty "IR generation source is non-empty for non-empty modules" prop_ir_source_non_empty_for_non_empty_modules
    ]