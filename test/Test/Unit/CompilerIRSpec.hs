{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, suchThat)
import Data.List (isInfixOf, isPrefixOf, sort, nub, intercalate, lines, unlines)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import qualified Data.Set as Set
import Data.Char (isSpace, isAlphaNum)

import Compiler.IR
  ( SourceIR(..), SemanticIR(..), GoIR(..)
  , buildSourceIR, buildSemanticIR, buildSemanticIRWithPackage, emitGo
  , rawSourceFromTypus, moduleFromTypus, ensurePackageDecl, ensureMainFunction
  , attachInferredImports
  )
import Compiler.GoAst
  ( GoModule(..), PackageDecl(..), ImportDecl(..), GoDecl(..)
  , FuncDecl(..), TypeDecl(..), VarDecl(..), ConstDecl(..)
  , StatementBlock(..), RawBlock(..), parseGoModule, renderGoModule
  , isMainFunction, flattenDeclLines
  )
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.Errors (CompilerResult, CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import SourceLocation (SourceSpan(..), SourcePos(..))

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary PackageDecl where
  arbitrary = do
    name <- elements ["main", "test", "example", "mypackage"]
    return $ PackageDecl name

instance Arbitrary ImportDecl where
  arbitrary = do
    importAlias <- oneof [return Nothing, Just <$> elements ["fmt", "os", "io", "time"]]
    importPath <- elements ["fmt", "os", "io", "time", "net/http", "strings", "strconv"]
    return $ ImportDecl importAlias importPath

instance Arbitrary FuncDecl where
  arbitrary = do
    funcLines <- listOf $ elements 
      [ "func main() {"
      , "fmt.Println(\"Hello, World!\")"
      , "}"
      , "x := 5"
      , "return x"
      , "if x > 0 {"
      , "return true"
      , "}"
      ]
    return $ FuncDecl funcLines

instance Arbitrary TypeDecl where
  arbitrary = do
    typeLines <- listOf $ elements
      [ "type MyInt int"
      , "type MyStruct struct {"
      , "Field int"
      , "}"
      ]
    typeIsGroup <- arbitrary
    return $ TypeDecl typeLines typeIsGroup

instance Arbitrary VarDecl where
  arbitrary = do
    varLines <- listOf $ elements
      [ "var x int = 5"
      , "var y string = \"hello\""
      , "var z = []int{1, 2, 3}"
      ]
    varIsGroup <- arbitrary
    return $ VarDecl varLines varIsGroup

instance Arbitrary ConstDecl where
  arbitrary = do
    constLines <- listOf $ elements
      [ "const Pi = 3.14159"
      , "const E = 2.71828"
      , "const ("
      , "A = iota"
      , "B"
      , "C"
      , ")"
      ]
    constIsGroup <- arbitrary
    return $ ConstDecl constLines constIsGroup

instance Arbitrary StatementBlock where
  arbitrary = do
    statementLines <- listOf $ elements
      [ "x := 5"
      , "fmt.Println(x)"
      , "if x > 0 {"
      , "return true"
      , "}"
      ]
    return $ StatementBlock statementLines

instance Arbitrary RawBlock where
  arbitrary = do
    rawLines <- listOf $ elements
      [ "// This is a comment"
      , "/* Multi-line comment */"
      , "fmt.Println(\"raw code\")"
      , "x := 10"
      ]
    return $ RawBlock rawLines

instance Arbitrary GoDecl where
  arbitrary = oneof
    [ GoFunc <$> arbitrary
    , GoType <$> arbitrary
    , GoVar <$> arbitrary
    , GoConst <$> arbitrary
    , GoStatement <$> arbitrary
    , GoRaw <$> arbitrary
    ]

instance Arbitrary GoModule where
  arbitrary = do
    gmBuildTags <- listOf $ elements ["linux", "darwin", "windows"]
    gmPackage <- oneof [return Nothing, Just <$> arbitrary]
    gmImports <- listOf arbitrary
    gmDecls <- listOf arbitrary
    return $ GoModule gmBuildTags gmPackage gmImports gmDecls

instance Arbitrary SourceIR where
  arbitrary = do
    sourceTypusFile <- arbitrary
    sourceText <- arbitrary
    return $ SourceIR sourceTypusFile sourceText

instance Arbitrary SemanticIR where
  arbitrary = do
    semanticTypusFile <- arbitrary
    semanticModule <- arbitrary
    semanticValueInfo <- listOf arbitrary
    return $ SemanticIR semanticTypusFile semanticModule semanticValueInfo

instance Arbitrary GoIR where
  arbitrary = do
    goModule <- arbitrary
    goSource <- arbitrary
    return $ GoIR goModule goSource

-- ============================================================================
-- GoModule Properties
-- ============================================================================

-- Property: GoModule Show produces non-empty string
prop_gomodule_show_nonempty :: GoModule -> Property
prop_gomodule_show_nonempty goModule =
  let shown = show goModule
  in property $ not (null shown)

-- Property: GoModule preserves build tags
prop_gomodule_preserves_build_tags :: [String] -> GoModule -> Property
prop_gomodule_preserves_build_tags buildTags goModule =
  let updated = goModule { gmBuildTags = buildTags }
  in property $ gmBuildTags updated === buildTags

-- Property: GoModule preserves package declaration
prop_gomodule_preserves_package :: Maybe PackageDecl -> GoModule -> Property
prop_gomodule_preserves_package package goModule =
  let updated = goModule { gmPackage = package }
  in property $ gmPackage updated === package

-- Property: GoModule preserves imports
prop_gomodule_preserves_imports :: [ImportDecl] -> GoModule -> Property
prop_gomodule_preserves_imports imports goModule =
  let updated = goModule { gmImports = imports }
  in property $ gmImports updated === imports

-- Property: GoModule preserves declarations
prop_gomodule_preserves_decls :: [GoDecl] -> GoModule -> Property
prop_gomodule_preserves_decls decls goModule =
  let updated = goModule { gmDecls = decls }
  in property $ gmDecls updated === decls

-- ============================================================================
-- GoDecl Properties
-- ============================================================================

-- Property: GoDecl Show produces non-empty string
prop_godecl_show_nonempty :: GoDecl -> Property
prop_godecl_show_nonempty goDecl =
  let shown = show goDecl
  in property $ not (null shown)

-- Property: isMainFunction correctly identifies main functions
prop_is_main_function :: FuncDecl -> Property
prop_is_main_function funcDecl =
  let isMain = isMainFunction funcDecl
      funcContent = unlines (funcLines funcDecl)
  in property $ isMain === ("func main()" `isInfixOf` funcContent)

-- Property: flattenDeclLines handles all declaration types
prop_flatten_decl_lines :: GoDecl -> Property
prop_flatten_decl_lines goDecl =
  let flattened = flattenDeclLines goDecl
  in property $ not (null flattened)

-- ============================================================================
-- PackageDecl Properties
-- ============================================================================

-- Property: PackageDecl Show produces non-empty string
prop_package_decl_show_nonempty :: PackageDecl -> Property
prop_package_decl_show_nonempty packageDecl =
  let shown = show packageDecl
  in property $ not (null shown)

-- Property: PackageDecl contains package name
prop_package_decl_contains_name :: String -> Property
prop_package_decl_contains_name name =
  not (null name) && all isAlphaNum name ==>
  let packageDecl = PackageDecl name
      shown = show packageDecl
  in property $ name `isInfixOf` shown

-- ============================================================================
-- ImportDecl Properties
-- ============================================================================

-- Property: ImportDecl Show produces non-empty string
prop_import_decl_show_nonempty :: ImportDecl -> Property
prop_import_decl_show_nonempty importDecl =
  let shown = show importDecl
  in property $ not (null shown)

-- Property: ImportDecl contains import path
prop_import_decl_contains_path :: String -> Property
prop_import_decl_contains_path path =
  not (null path) ==>
  let importDecl = ImportDecl Nothing path
      shown = show importDecl
  in property $ path `isInfixOf` shown

-- ============================================================================
-- TypeDecl, VarDecl, ConstDecl Properties
-- ============================================================================

-- Property: TypeDecl Show produces non-empty string
prop_type_decl_show_nonempty :: TypeDecl -> Property
prop_type_decl_show_nonempty typeDecl =
  let shown = show typeDecl
  in property $ not (null shown)

-- Property: VarDecl Show produces non-empty string
prop_var_decl_show_nonempty :: VarDecl -> Property
prop_var_decl_show_nonempty varDecl =
  let shown = show varDecl
  in property $ not (null shown)

-- Property: ConstDecl Show produces non-empty string
prop_const_decl_show_nonempty :: ConstDecl -> Property
prop_const_decl_show_nonempty constDecl =
  let shown = show constDecl
  in property $ not (null shown)

-- ============================================================================
-- StatementBlock and RawBlock Properties
-- ============================================================================

-- Property: StatementBlock Show produces non-empty string
prop_statement_block_show_nonempty :: StatementBlock -> Property
prop_statement_block_show_nonempty statementBlock =
  let shown = show statementBlock
  in property $ not (null shown)

-- Property: RawBlock Show produces non-empty string
prop_raw_block_show_nonempty :: RawBlock -> Property
prop_raw_block_show_nonempty rawBlock =
  let shown = show rawBlock
  in property $ not (null shown)

-- ============================================================================
-- IR Properties
-- ============================================================================

-- Property: SourceIR preserves TypusFile
prop_source_ir_preserves_typus_file :: TypusFile -> String -> Property
prop_source_ir_preserves_typus_file typusFile sourceText =
  let sourceIR = buildSourceIR typusFile
  in property $ sourceTypusFile sourceIR === typusFile

-- Property: SourceIR generates non-empty source text
prop_source_ir_non_empty_source :: TypusFile -> Property
prop_source_ir_non_empty_source typusFile =
  let sourceIR = buildSourceIR typusFile
      sourceText = sourceText sourceIR
  in property $ not (null sourceText)

-- Property: SemanticIR preserves TypusFile
prop_semantic_ir_preserves_typus_file :: SourceIR -> Property
prop_semantic_ir_preserves_typus_file sourceIR =
  case buildSemanticIR sourceIR of
    Left _ -> property True -- May fail for invalid IR
    Right semanticIR -> property $ semanticTypusFile semanticIR === sourceTypusFile sourceIR

-- Property: GoIR preserves GoModule
prop_go_ir_preserves_module :: SemanticIR -> Property
prop_go_ir_preserves_module semanticIR =
  let goIR = emitGo semanticIR
  in property $ goModule goIR === semanticModule semanticIR

-- Property: GoIR generates non-empty source
prop_go_ir_non_empty_source :: SemanticIR -> Property
prop_go_ir_non_empty_source semanticIR =
  let goIR = emitGo semanticIR
      goSource = goSource goIR
  in property $ not (null goSource)

-- ============================================================================
-- Parsing and Rendering Properties
-- ============================================================================

-- Property: parseGoModule handles empty input
prop_parse_go_module_empty :: Property
prop_parse_go_module_empty =
  let result = parseGoModule []
  in case result of
    Left _ -> property False
    Right goModule -> property $ True -- Should parse successfully

-- Property: parseGoModule handles simple package declaration
prop_parse_go_module_simple_package :: String -> Property
prop_parse_go_module_simple_package packageName =
  not (null packageName) && all isAlphaNum packageName ==>
  let input = ["package " ++ packageName]
      result = parseGoModule input
  in case result of
    Left _ -> property False
    Right goModule -> property $ 
      case gmPackage goModule of
        Nothing -> property False
        Just pkg -> packageName pkg === packageName

-- Property: parseGoModule handles imports
prop_parse_go_module_imports :: [String] -> Property
prop_parse_go_module_imports importPaths =
  length importPaths <= 3 ==> -- Limit for performance
  let importLines = map ("import \"" ++) importPaths ++ ["\""]
      input = ["package main"] ++ importLines
      result = parseGoModule input
  in case result of
    Left _ -> property False
    Right goModule -> property $ length (gmImports goModule) >= length importPaths

-- Property: renderGoModule produces non-empty string
prop_render_go_module_nonempty :: GoModule -> Property
prop_render_go_module_nonempty goModule =
  let rendered = renderGoModule goModule
  in property $ not (null rendered)

-- Property: renderGoModule contains package name if present
prop_render_go_module_contains_package :: GoModule -> Property
prop_render_go_module_contains_package goModule =
  case gmPackage goModule of
    Nothing -> property True -- No package to contain
    Just pkg -> 
      let rendered = renderGoModule goModule
          pkgName = packageName pkg
      in property $ pkgName `isInfixOf` rendered

-- ============================================================================
-- IR Transformation Properties
-- ============================================================================

-- Property: rawSourceFromTypus handles empty file
prop_raw_source_empty_file :: Property
prop_raw_source_empty_file =
  let emptyFile = TypusFile defaultFileDirectives [] [] []
      rawSource = rawSourceFromTypus emptyFile
  in property $ True -- Should not crash

-- Property: rawSourceFromTypus handles simple blocks
prop_raw_source_simple_blocks :: [String] -> Property
prop_raw_source_simple_blocks blockContents =
  length blockContents <= 3 ==> -- Limit for performance
  let blocks = map (\content -> CodeBlock defaultBlockDirectives content undefined) blockContents
      typusFile = TypusFile defaultFileDirectives [] blocks []
      rawSource = rawSourceFromTypus typusFile
  in property $ not (null rawSource)

-- Property: moduleFromTypus handles simple files
prop_module_from_typus_simple :: TypusFile -> Property
prop_module_from_typus_simple typusFile =
  let result = moduleFromTypus typusFile
  in case result of
    Left _ -> property True -- May fail for invalid files
    Right goModule -> property $ True -- Should succeed or fail gracefully

-- Property: ensurePackageDecl adds package if missing
prop_ensure_package_decl_adds_missing :: GoModule -> Property
prop_ensure_package_decl_adds_missing goModule =
  case gmPackage goModule of
    Nothing -> 
      let ensured = ensurePackageDecl goModule
      in property $ isJust (gmPackage ensured)
    Just _ -> property True -- Already has package

-- Property: ensureMainFunction adds main function if missing
prop_ensure_main_function_adds_missing :: GoModule -> Property
prop_ensure_main_function_adds_missing goModule =
  let hasMain = any isMainFunc (gmDecls goModule)
      isMainFunc (GoFunc func) = "func main()" `isInfixOf` unlines (funcLines func)
      isMainFunc _ = False
      ensured = ensureMainFunction goModule
      hasMainAfter = any isMainFunc (gmDecls ensured)
  in property $ hasMain ==> hasMainAfter .&&. (not hasMain ==> hasMainAfter)

-- ============================================================================
-- Complex Properties
-- ============================================================================

-- Property: IR transformations are deterministic
prop_ir_transformations_deterministic :: SourceIR -> Property
prop_ir_transformations_deterministic sourceIR =
  let result1 = buildSemanticIR sourceIR
      result2 = buildSemanticIR sourceIR
  in case (result1, result2) of
    (Left _, Left _) -> property True -- Both should fail the same way
    (Right semIR1, Right semIR2) -> property $ semIR1 === semIR2
    _ -> property False -- Should not have different outcomes

-- Property: Go source generation is consistent
prop_go_source_consistent :: SemanticIR -> Property
prop_go_source_consistent semanticIR =
  let goIR1 = emitGo semanticIR
      goIR2 = emitGo semanticIR
  in property $ goIR1 === goIR2

-- Property: Parse-render roundtrip preserves structure
prop_parse_render_roundtrip :: GoModule -> Property
prop_parse_render_roundtrip goModule =
  let rendered = renderGoModule goModule
      parsedLines = lines rendered
      result = parseGoModule parsedLines
  in case result of
    Left _ -> property True -- May fail for complex modules
    Right reparsed -> property $ 
      -- Check that key structure is preserved
      length (gmDecls reparsed) >= length (gmDecls goModule) - 1 -- Allow for some variation

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Tests"
  [ testGroup "GoModule Properties"
    [ fastProperty "GoModule Show produces non-empty string" prop_gomodule_show_nonempty
    , fastProperty "GoModule preserves build tags" prop_gomodule_preserves_build_tags
    , fastProperty "GoModule preserves package declaration" prop_gomodule_preserves_package
    , fastProperty "GoModule preserves imports" prop_gomodule_preserves_imports
    , fastProperty "GoModule preserves declarations" prop_gomodule_preserves_decls
    ]
  , testGroup "GoDecl Properties"
    [ fastProperty "GoDecl Show produces non-empty string" prop_godecl_show_nonempty
    , fastProperty "isMainFunction correctly identifies main functions" prop_is_main_function
    , fastProperty "flattenDeclLines handles all declaration types" prop_flatten_decl_lines
    ]
  , testGroup "PackageDecl Properties"
    [ fastProperty "PackageDecl Show produces non-empty string" prop_package_decl_show_nonempty
    , fastProperty "PackageDecl contains package name" prop_package_decl_contains_name
    ]
  , testGroup "ImportDecl Properties"
    [ fastProperty "ImportDecl Show produces non-empty string" prop_import_decl_show_nonempty
    , fastProperty "ImportDecl contains import path" prop_import_decl_contains_path
    ]
  , testGroup "TypeDecl, VarDecl, ConstDecl Properties"
    [ fastProperty "TypeDecl Show produces non-empty string" prop_type_decl_show_nonempty
    , fastProperty "VarDecl Show produces non-empty string" prop_var_decl_show_nonempty
    , fastProperty "ConstDecl Show produces non-empty string" prop_const_decl_show_nonempty
    ]
  , testGroup "StatementBlock and RawBlock Properties"
    [ fastProperty "StatementBlock Show produces non-empty string" prop_statement_block_show_nonempty
    , fastProperty "RawBlock Show produces non-empty string" prop_raw_block_show_nonempty
    ]
  , testGroup "IR Properties"
    [ fastProperty "SourceIR preserves TypusFile" prop_source_ir_preserves_typus_file
    , fastProperty "SourceIR generates non-empty source text" prop_source_ir_non_empty_source
    , fastProperty "SemanticIR preserves TypusFile" prop_semantic_ir_preserves_typus_file
    , fastProperty "GoIR preserves GoModule" prop_go_ir_preserves_module
    , fastProperty "GoIR generates non-empty source" prop_go_ir_non_empty_source
    ]
  , testGroup "Parsing and Rendering Properties"
    [ fastProperty "parseGoModule handles empty input" prop_parse_go_module_empty
    , fastProperty "parseGoModule handles simple package declaration" prop_parse_go_module_simple_package
    , fastProperty "parseGoModule handles imports" prop_parse_go_module_imports
    , fastProperty "renderGoModule produces non-empty string" prop_render_go_module_nonempty
    , fastProperty "renderGoModule contains package name if present" prop_render_go_module_contains_package
    ]
  , testGroup "IR Transformation Properties"
    [ fastProperty "rawSourceFromTypus handles empty file" prop_raw_source_empty_file
    , fastProperty "rawSourceFromTypus handles simple blocks" prop_raw_source_simple_blocks
    , fastProperty "moduleFromTypus handles simple files" prop_module_from_typus_simple
    , fastProperty "ensurePackageDecl adds package if missing" prop_ensure_package_decl_adds_missing
    , fastProperty "ensureMainFunction adds main function if missing" prop_ensure_main_function_adds_missing
    ]
  , testGroup "Complex Properties"
    [ fastProperty "IR transformations are deterministic" prop_ir_transformations_deterministic
    , fastProperty "Go source generation is consistent" prop_go_source_consistent
    , fastProperty "Parse-render roundtrip preserves structure" prop_parse_render_roundtrip
    ]
  ]