{-# LANGUAGE CPP #-}
module Test.Unit.CompilerIRConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample, suchThat, (==>))
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Compiler.GoAst (GoModule(..), GoDecl(..), ImportDecl(..), FuncDecl(..), 
                       TypeDecl(..), VarDecl(..), PackageDecl(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import qualified Data.Text as T

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate valid identifiers
genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate import declarations
genImportDecl :: Gen ImportDecl
genImportDecl = do
  alias <- oneof [pure Nothing, Just <$> genIdentifier]
  path <- genIdentifier
  return $ ImportDecl alias path

-- Generate function declarations
genFuncDecl :: Gen FuncDecl
genFuncDecl = do
  name <- genIdentifier
  params <- listOf genIdentifier
  return $ FuncDecl (name : params)

-- Generate type declarations
genTypeDecl :: Gen TypeDecl
genTypeDecl = do
  name <- genIdentifier
  fields <- listOf genIdentifier
  return $ TypeDecl (name : fields)

-- Generate variable declarations
genVarDecl :: Gen VarDecl
genVarDecl = do
  name <- genIdentifier
  varType <- genIdentifier
  return $ VarDecl [name] varType

-- Generate package declarations
genPackageDecl :: Gen PackageDecl
genPackageDecl = do
  name <- genIdentifier
  return $ PackageDecl name

-- Generate Go declarations
genGoDecl :: Gen GoDecl
genGoDecl = oneof
  [ GoFunc <$> genFuncDecl
  , GoType <$> genTypeDecl
  , GoVar <$> genVarDecl
  , GoConst <$> genVarDecl  -- Using VarDecl for simplicity
  ]

-- Generate Go modules
genGoModule :: Gen GoModule
genGoModule = do
  imports <- listOf genImportDecl
  package <- oneof [pure Nothing, Just <$> genPackageDecl]
  declarations <- listOf genGoDecl
  buildTags <- listOf genIdentifier
  return $ GoModule buildTags package imports declarations

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  endLine <- choose (startLine, startLine + 50)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)
            else choose (1, 100)
  return $ SourceSpan (SourcePos startLine startCol 0) (SourcePos endLine endCol 0)

-- ============================================================================
-- Properties for SourceIR
-- ============================================================================

prop_source_ir_preserves_module :: GoModule -> String -> SourceSpan -> Property
prop_source_ir_preserves_module goModule sourceCode span =
  let sourceIR = SourceIR goModule sourceCode
  in sourceModule sourceIR === goModule &&
     sourceText sourceIR === sourceCode

prop_source_ir_content_length :: GoModule -> String -> Property
prop_source_ir_content_length goModule sourceCode =
  let sourceIR = SourceIR goModule sourceCode
  in length (sourceText sourceIR) === length sourceCode

-- ============================================================================
-- Properties for SemanticIR
-- ============================================================================

prop_semantic_ir_preserves_source :: SourceIR -> [String] -> Property
prop_semantic_ir_preserves_source sourceIR symbolTable =
  let semanticIR = SemanticIR sourceIR symbolTable []
  in sourceInfo semanticIR === sourceIR &&
     symbolTableInfo semanticIR === symbolTable

prop_semantic_ir_symbol_table_consistency :: SourceIR -> [String] -> Property
prop_semantic_ir_symbol_table_consistency sourceIR symbolTable =
  let semanticIR = SemanticIR sourceIR symbolTable []
      extractedSymbols = symbolTableInfo semanticIR
  in length extractedSymbols === length symbolTable

-- ============================================================================
-- Properties for GoIR
-- ============================================================================

prop_go_ir_preserves_semantic :: SemanticIR -> String -> Property
prop_go_ir_preserves_semantic semanticIR goCode =
  let goIR = GoIR semanticIR goCode
  in semanticInfo goIR === semanticIR &&
     goCodeOutput goIR === goCode

prop_go_ir_code_generation_consistency :: SemanticIR -> String -> Property
prop_go_ir_code_generation_consistency semanticIR goCode =
  let goIR = GoIR semanticIR goCode
  in length (goCodeOutput goIR) === length goCode

-- ============================================================================
-- Properties for IR transformation pipeline
-- ============================================================================

prop_ir_pipeline_preservation :: GoModule -> String -> [String] -> String -> Property
prop_ir_pipeline_preservation goModule sourceCode symbolTable goCode =
  let sourceIR = SourceIR goModule sourceCode
      semanticIR = SemanticIR sourceIR symbolTable []
      goIR = GoIR semanticIR goCode
  in sourceModule sourceIR === goModule &&
     sourceText sourceIR === sourceCode &&
     sourceInfo semanticIR === sourceIR &&
     symbolTableInfo semanticIR === symbolTable &&
     semanticInfo goIR === semanticIR &&
     goCodeOutput goIR === goCode

prop_ir_pipeline_roundtrip :: GoModule -> String -> Property
prop_ir_pipeline_roundtrip goModule sourceCode =
  let sourceIR = SourceIR goModule sourceCode
      semanticIR = SemanticIR sourceIR [] []
      goIR = GoIR semanticIR ""
      -- Reconstruct from GoIR
      reconstructedSource = sourceInfo $ semanticInfo goIR
      reconstructedModule = sourceModule reconstructedSource
      reconstructedText = sourceText reconstructedSource
  in reconstructedModule === goModule &&
     reconstructedText === sourceCode

-- ============================================================================
-- Properties for GoModule structure
-- ============================================================================

prop_go_module_imports_consistency :: GoModule -> Property
prop_go_module_imports_consistency goModule =
  let imports = goImports goModule
  in all isValidImport imports
  where
    isValidImport (ImportDecl alias path) = not (null path)

prop_go_module_declarations_consistency :: GoModule -> Property
prop_go_module_declarations_consistency goModule =
  let declarations = goDecls goModule
  in all isValidDeclaration declarations
  where
    isValidDeclaration (GoFunc (FuncDecl (name:_))) = not (null name)
    isValidDeclaration (GoType (TypeDecl (name:_))) = not (null name)
    isValidDeclaration (GoVar (VarDecl names _)) = all (not . null) names
    isValidDeclaration (GoConst (VarDecl names _)) = all (not . null) names

-- ============================================================================
-- Properties for IR invariants
-- ============================================================================

prop_source_ir_invariant :: GoModule -> String -> Property
prop_source_ir_invariant goModule sourceCode =
  let sourceIR = SourceIR goModule sourceCode
  in -- Invariant: SourceIR should never lose module information
     sourceModule sourceIR === goModule

prop_semantic_ir_invariant :: SourceIR -> [String] -> Property
prop_semantic_ir_invariant sourceIR symbolTable =
  let semanticIR = SemanticIR sourceIR symbolTable []
  in -- Invariant: SemanticIR should maintain reference to SourceIR
     sourceInfo semanticIR === sourceIR

prop_go_ir_invariant :: SemanticIR -> String -> Property
prop_go_ir_invariant semanticIR goCode =
  let goIR = GoIR semanticIR goCode
  in -- Invariant: GoIR should maintain reference to SemanticIR
     semanticInfo goIR === semanticIR

-- ============================================================================
-- Properties for IR transformation consistency
-- ============================================================================

prop_ir_transformation_idempotence :: GoModule -> String -> Property
prop_ir_transformation_idempotence goModule sourceCode =
  let sourceIR1 = SourceIR goModule sourceCode
      sourceIR2 = SourceIR (sourceModule sourceIR1) (sourceText sourceIR1)
  in sourceIR1 === sourceIR2

prop_ir_symbol_table_preservation :: SourceIR -> [String] -> [String] -> Property
prop_ir_symbol_table_preservation sourceIR originalSymbols newSymbols =
  let semanticIR1 = SemanticIR sourceIR originalSymbols []
      semanticIR2 = SemanticIR sourceIR newSymbols []
  in sourceInfo semanticIR1 === sourceInfo semanticIR2 &&
     symbolTableInfo semanticIR1 === originalSymbols &&
     symbolTableInfo semanticIR2 === newSymbols

-- ============================================================================
-- Edge case properties
-- ============================================================================

prop_empty_go_module_handling :: Property
prop_empty_go_module_handling =
  let emptyModule = GoModule [] Nothing [] []
      sourceIR = SourceIR emptyModule ""
      semanticIR = SemanticIR sourceIR [] []
      goIR = GoIR semanticIR ""
  in sourceModule sourceIR === emptyModule &&
     null (goImports emptyModule) &&
     null (goDecls emptyModule)

prop_minimal_ir_construction :: Property
prop_minimal_ir_construction =
  let minimalModule = GoModule [] (Just (PackageDecl "main")) [] []
      sourceIR = SourceIR minimalModule "package main"
      semanticIR = SemanticIR sourceIR ["main"] []
      goIR = GoIR semanticIR "package main\n"
  in sourceModule sourceIR === minimalModule &&
     sourceText sourceIR === "package main" &&
     symbolTableInfo semanticIR === ["main"] &&
     goCodeOutput goIR === "package main\n"

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Consistency QuickCheck Tests"
  [ testGroup "SourceIR properties"
    [ fastProperty "sourceIR preserves module" prop_source_ir_preserves_module
    , fastProperty "sourceIR content length" prop_source_ir_content_length
    ]
  , testGroup "SemanticIR properties"
    [ fastProperty "semanticIR preserves source" prop_semantic_ir_preserves_source
    , fastProperty "semanticIR symbol table consistency" prop_semantic_ir_symbol_table_consistency
    ]
  , testGroup "GoIR properties"
    [ fastProperty "goIR preserves semantic" prop_go_ir_preserves_semantic
    , fastProperty "goIR code generation consistency" prop_go_ir_code_generation_consistency
    ]
  , testGroup "IR transformation pipeline properties"
    [ fastProperty "IR pipeline preservation" prop_ir_pipeline_preservation
    , fastProperty "IR pipeline roundtrip" prop_ir_pipeline_roundtrip
    ]
  , testGroup "GoModule structure properties"
    [ fastProperty "GoModule imports consistency" prop_go_module_imports_consistency
    , fastProperty "GoModule declarations consistency" prop_go_module_declarations_consistency
    ]
  , testGroup "IR invariants"
    [ fastProperty "SourceIR invariant" prop_source_ir_invariant
    , fastProperty "SemanticIR invariant" prop_semantic_ir_invariant
    , fastProperty "GoIR invariant" prop_go_ir_invariant
    ]
  , testGroup "IR transformation consistency"
    [ fastProperty "IR transformation idempotence" prop_ir_transformation_idempotence
    , fastProperty "IR symbol table preservation" prop_ir_symbol_table_preservation
    ]
  , testGroup "Edge case properties"
    [ fastProperty "empty GoModule handling" prop_empty_go_module_handling
    , fastProperty "minimal IR construction" prop_minimal_ir_construction
    ]
  ]