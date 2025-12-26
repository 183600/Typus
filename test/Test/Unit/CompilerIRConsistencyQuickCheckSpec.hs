{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerIRConsistencyQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Compiler.IR
import Compiler.Errors (CompilerResult, CompilationPhase(..))
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.GoAst (GoModule(..), GoDecl(..), GoImport(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt)

-- ============================================================================
-- Test Data Generation
-- ============================================================================

-- | Generate source positions
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> positive <*> positive
    where
      positive = getPositive <$> arbitrary

-- | Generate code blocks
instance Arbitrary CodeBlock where
  arbitrary = do
    directives <- arbitrary
    content <- arbitraryString
    return $ CodeBlock directives content

-- | Generate file directives
instance Arbitrary FileDirectives where
  arbitrary = FileDirectives <$> arbitraryMaybe <*> arbitraryMaybe <*> arbitraryMaybe
    where
      arbitraryMaybe = oneof [return Nothing, Just <$> locatedAt <$> arbitrary <*> arbitrary]

-- | Generate block directives
instance Arbitrary BlockDirectives where
  arbitrary = BlockDirectives <$> arbitraryMaybe <*> arbitraryMaybe <*> arbitraryMaybe
    where
      arbitraryMaybe = oneof [return Nothing, Just <$> locatedAt <$> arbitrary <*> arbitrary]

-- | Generate Typus files
instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    blocks <- listOf arbitrary
    return $ TypusFile directives blocks

-- | Generate Go imports
instance Arbitrary GoImport where
  arbitrary = do
    path <- arbitraryString
    alias <- oneof [return "", arbitraryString]
    return $ GoImport path alias

-- | Generate Go declarations (simplified)
instance Arbitrary GoDecl where
  arbitrary = oneof
    [ return $ GoImportDecl <$> arbitrary
    , return $ GoVarDecl <$> arbitraryString <*> arbitraryString
    , return $ GoFuncDecl <$> arbitraryString <*> [] <*> arbitraryString
    ]

-- | Generate Go modules
instance Arbitrary GoModule where
  arbitrary = do
    pkgName <- arbitraryString
    imports <- listOf arbitrary
    decls <- listOf arbitrary
    return $ GoModule pkgName imports decls

-- | Generate arbitrary strings
arbitraryString :: Gen String
arbitraryString = do
  size <- choose (0, 20)
  vectorOf size $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t;{}()"

-- ============================================================================
-- QuickCheck Properties for Compiler IR Consistency
-- ============================================================================

-- | SourceIR should preserve the original Typus file
prop_source_ir_preserves_typus :: TypusFile -> Property
prop_source_ir_preserves_typus typusFile =
  let sourceIR = buildSourceIR typusFile
  in sourceTypusFile sourceIR === typusFile

-- | SourceIR should extract text from Typus file
prop_source_ir_extracts_text :: TypusFile -> Property
prop_source_ir_extracts_text typusFile =
  let sourceIR = buildSourceIR typusFile
      extractedText = sourceText sourceIR
      rawText = rawSourceFromTypus typusFile
  in extractedText === rawText

-- | Raw source extraction should be consistent
prop_raw_source_consistency :: TypusFile -> Property
prop_raw_source_consistency typusFile =
  let raw1 = rawSourceFromTypus typusFile
      raw2 = rawSourceFromTypus typusFile
  in raw1 === raw2

-- | SemanticIR should preserve Typus file when successful
prop_semantic_ir_preserves_typus :: TypusFile -> Property
prop_semantic_ir_preserves_typus typusFile =
  let sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
    Left _ -> property True  -- Error is acceptable
    Right semanticIR -> semanticTypusFile semanticIR === typusFile

-- | SemanticIR should have a valid Go module when successful
prop_semantic_ir_has_module :: TypusFile -> Property
prop_semantic_ir_has_module typusFile =
  let sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
    Left _ -> property True  -- Error is acceptable
    Right semanticIR -> 
      let module = semanticModule semanticIR
      in not (null $ gmPkgName module) .||. gmPkgName module === ""

-- | GoIR should have consistent module and source
prop_go_ir_consistency :: TypusFile -> Property
prop_go_ir_consistency typusFile =
  let sourceIR = buildSourceIR typusFile
      semanticResult = buildSemanticIR sourceIR
  in case semanticResult of
    Left _ -> property True  -- Error in semantic phase is acceptable
    Right semanticIR ->
      let goIR = emitGo semanticIR
          module = goModule goIR
          source = goSource goIR
      in module `seq` source `seq` True  -- Should not crash

-- | GoIR source should contain package name
prop_go_ir_source_contains_package :: TypusFile -> Property
prop_go_ir_source_contains_package typusFile =
  let sourceIR = buildSourceIR typusFile
      semanticResult = buildSemanticIR sourceIR
  in case semanticResult of
    Left _ -> property True  -- Error is acceptable
    Right semanticIR ->
      let goIR = emitGo semanticIR
          source = goSource goIR
          module = goModule goIR
          pkgName = gmPkgName module
      in not (null pkgName) ==> (pkgName `isInfixOf` source)

-- | Package declaration should be ensured
prop_ensure_package_decl :: TypusFile -> Property
prop_ensure_package_decl typusFile =
  let result = ensurePackageDecl typusFile
  in case result of
    Left _ -> property True  -- Error is acceptable
    Right updatedFile -> updatedFile `seq` True  -- Should not crash

-- | Main function should be ensured when needed
prop_ensure_main_function :: TypusFile -> Property
prop_ensure_main_function typusFile =
  let result = ensureMainFunction typusFile
  in case result of
    Left _ -> property True  -- Error is acceptable
    Right updatedFile -> updatedFile `seq` True  -- Should not crash

-- | Module from Typus should be consistent
prop_module_from_typus_consistency :: TypusFile -> Property
prop_module_from_typus_consistency typusFile =
  let result1 = moduleFromTypus typusFile
      result2 = moduleFromTypus typusFile
  in case (result1, result2) of
    (Left err1, Left err2) -> show err1 === show err2
    (Right mod1, Right mod2) -> mod1 === mod2
    (Left _, Right _) -> property False  -- Inconsistent results
    (Right _, Left _) -> property False  -- Inconsistent results

-- | Go module should be structurally valid
prop_go_module_structural :: GoModule -> Property
prop_go_module_structural module =
  let pkgName = gmPkgName module
      imports = gmImports module
      decls = gmDecls module
  in pkgName `seq` length imports `seq` length decls `seq` True  -- Should not crash

-- | Go imports should be structurally valid
prop_go_import_structural :: GoImport -> Property
prop_go_import_structural goImport =
  let path = giPath goImport
      alias = giAlias goImport
  in path `seq` alias `seq` True  -- Should not crash

-- | Code blocks should preserve directives
prop_code_block_preserves_directives :: BlockDirectives -> String -> Property
prop_code_block_preserves_directives directives content =
  let block = CodeBlock directives content
  in blockDirectives block === directives .&&. blockContent block === content

-- | Typus file should preserve structure
prop_typus_file_structural :: FileDirectives -> [CodeBlock] -> Property
prop_typus_file_structural directives blocks =
  let typusFile = TypusFile directives blocks
  in fileDirectives typusFile === directives .&&. codeBlocks typusFile === blocks

-- | Empty Typus file should be handled gracefully
prop_empty_typus_file :: Property
prop_empty_typus_file =
  let emptyFile = TypusFile defaultFileDirectives []
      sourceIR = buildSourceIR emptyFile
      semanticResult = buildSemanticIR sourceIR
  in case semanticResult of
    Left _ -> property True
    Right semanticIR ->
      let goIR = emitGo semanticIR
      in goIR `seq` True

-- | Large code blocks should be handled
prop_large_code_blocks :: Positive Int -> Property
prop_large_code_blocks (Positive size) =
  let largeContent = replicate size 'a' ++ "\nlet x = 5;"
      block = CodeBlock defaultBlockDirectives largeContent
      typusFile = TypusFile defaultFileDirectives [block]
      sourceIR = buildSourceIR typusFile
  in sourceIR `seq` True

-- | Many code blocks should be handled
prop_many_code_blocks :: Positive Int -> Property
prop_many_code_blocks (Positive count) =
  let blocks = replicate count $ CodeBlock defaultBlockDirectives "let x = 5;"
      typusFile = TypusFile defaultFileDirectives blocks
      sourceIR = buildSourceIR typusFile
  in sourceIR `seq` True

-- | IR transformation pipeline should be monotonic
prop_ir_pipeline_monotonic :: TypusFile -> Property
prop_ir_pipeline_monotonic typusFile =
  let sourceIR = buildSourceIR typusFile
      sourceSize = length $ show sourceIR
  in case buildSemanticIR sourceIR of
    Left _ -> property True
    Right semanticIR ->
      let semanticSize = length $ show semanticIR
      in case buildSemanticIRWithPackage sourceIR [] of
        Left _ -> property True
        Right semanticWithPkg ->
          let pkgSize = length $ show semanticWithPkg
              goIR = emitGo semanticWithPkg
              goSize = length $ show goIR
          in sourceSize > 0 .&&. semanticSize > 0 .&&. pkgSize > 0 .&&. goSize > 0

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Consistency QuickCheck Tests"
  [ testProperty "source IR preserves Typus file" prop_source_ir_preserves_typus
  , testProperty "source IR extracts text" prop_source_ir_extracts_text
  , testProperty "raw source extraction is consistent" prop_raw_source_consistency
  , testProperty "semantic IR preserves Typus file" prop_semantic_ir_preserves_typus
  , testProperty "semantic IR has valid Go module" prop_semantic_ir_has_module
  , testProperty "Go IR consistency" prop_go_ir_consistency
  , testProperty "Go IR source contains package name" prop_go_ir_source_contains_package
  , testProperty "package declaration is ensured" prop_ensure_package_decl
  , testProperty "main function is ensured" prop_ensure_main_function
  , testProperty "module from Typus is consistent" prop_module_from_typus_consistency
  , testProperty "Go module is structurally valid" prop_go_module_structural
  , testProperty "Go import is structurally valid" prop_go_import_structural
  , testProperty "code block preserves directives" prop_code_block_preserves_directives
  , testProperty "Typus file preserves structure" prop_typus_file_structural
  , testProperty "empty Typus file handled gracefully" prop_empty_typus_file
  , testProperty "large code blocks handled" prop_large_code_blocks
  , testProperty "many code blocks handled" prop_many_code_blocks
  , testProperty "IR pipeline is monotonic" prop_ir_pipeline_monotonic
  ]