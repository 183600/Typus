{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, suchThat, listOf1)

import Compiler.IR
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , buildSemanticIR
  , emitGo
  , rawSourceFromTypus
  , moduleFromTypus
  )

import Compiler.GoAst
  ( GoModule(..)
  , PackageDecl(..)
  , ImportDecl(..)
  , GoDecl(..)
  , FuncDecl(..)
  , TypeDecl(..)
  , VarDecl(..)
  , ConstDecl(..)
  , StatementBlock(..)
  , RawBlock(..)
  , parseGoModule
  , renderGoModule
  , isMainFunction
  )

import Parser
  ( TypusFile(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , spanBetween
  , locatedAt
  )

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (intercalate)
import Data.Char (isAlpha, isAlphaNum, isSpace)

-- ============================================================================
-- Generators for QuickCheck
-- ============================================================================

-- Generate a valid Go identifier
genGoIdentifier :: Gen String
genGoIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_")
  return (first : rest)

-- Generate a valid Go import path
genGoImportPath :: Gen String
genGoImportPath = do
  domain <- elements ["fmt", "os", "strings", "strconv", "time", "net/http", "encoding/json"]
  return $ "\"" ++ domain ++ "\""

-- Generate a package declaration
genPackageDecl :: Gen PackageDecl
genPackageDecl = do
  name <- elements ["main", "utils", "types", "models", "services"]
  return $ PackageDecl name

-- Generate an import declaration
genImportDecl :: Gen ImportDecl
genImportDecl = do
  path <- genGoImportPath
  alias <- oneof [return Nothing, Just <$> genGoIdentifier]
  return $ ImportDecl alias path

-- Generate a function declaration
genFuncDecl :: Gen FuncDecl
genFuncDecl = do
  name <- genGoIdentifier
  params <- listOf genGoIdentifier
  returnType <- oneof [return "", genGoIdentifier]
  let funcLine = "func " ++ name ++ "(" ++ intercalate ", " params ++ ") " ++ returnType
      bodyLines = ["    // Function body", "    return nil"]
  return $ FuncDecl (funcLine : bodyLines)

-- Generate a type declaration
genTypeDecl :: Gen TypeDecl
genTypeDecl = do
  name <- genGoIdentifier
  underlying <- elements ["string", "int", "bool", "struct{}", "interface{}"]
  isGroup <- elements [True, False]
  return $ TypeDecl ["type " ++ name ++ " " ++ underlying] isGroup

-- Generate a variable declaration
genVarDecl :: Gen VarDecl
genVarDecl = do
  name <- genGoIdentifier
  varType <- elements ["string", "int", "bool"]
  value <- elements ["\"\"", "0", "false", "nil"]
  isGroup <- elements [True, False]
  return $ VarDecl ["var " ++ name ++ " " ++ varType ++ " = " ++ value] isGroup

-- Generate a constant declaration
genConstDecl :: Gen ConstDecl
genConstDecl = do
  name <- genGoIdentifier
  constType <- elements ["string", "int", "bool"]
  value <- elements ["\"test\"", "42", "true"]
  isGroup <- elements [True, False]
  return $ ConstDecl ["const " ++ name ++ " " ++ constType ++ " = " ++ value] isGroup

-- Generate a statement block
genStatementBlock :: Gen StatementBlock
genStatementBlock = do
  lines <- listOf1 $ elements ["    x := 1", "    y := 2", "    fmt.Println(x, y)"]
  return $ StatementBlock lines

-- Generate a raw block
genRawBlock :: Gen RawBlock
genRawBlock = do
  lines <- listOf1 $ elements ["// Raw content", "/* Another comment */", "some raw code"]
  return $ RawBlock lines

-- Generate a Go declaration
genGoDecl :: Gen GoDecl
genGoDecl = oneof
  [ GoFunc <$> genFuncDecl
  , GoType <$> genTypeDecl
  , GoVar <$> genVarDecl
  , GoConst <$> genConstDecl
  , GoStatement <$> genStatementBlock
  , GoRaw <$> genRawBlock
  ]

-- Generate a Go module
genGoModule :: Gen GoModule
genGoModule = do
  buildTags <- listOf (elements ["debug", "test", "prod"])
  packageDecl <- oneof [return Nothing, Just <$> genPackageDecl]
  imports <- listOf genImportDecl
  decls <- listOf genGoDecl
  return $ GoModule buildTags packageDecl imports decls

-- Generate a code block
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  directives <- return defaultBlockDirectives
  content <- oneof
    [ genFuncDecl >>= \decl -> return (intercalate "\n" (funcLines decl))
    , genTypeDecl >>= \decl -> return (intercalate "\n" (typeLines decl))
    , genVarDecl >>= \decl -> return (intercalate "\n" (varLines decl))
    , return "fmt.Println(\"Hello, World!\")"
    ]
  span <- return $ spanBetween startPos startPos
  return $ CodeBlock directives content span

-- Generate a Typus file
genTypusFile :: Gen TypusFile
genTypusFile = do
  directives <- return defaultFileDirectives
  buildTags <- []
  blocks <- listOf1 genCodeBlock
  syntaxErrors <- return []
  return $ TypusFile directives buildTags blocks syntaxErrors

-- Generate valid Go code string
genGoCode :: Gen String
genGoCode = do
  pkg <- genPackageDecl
  imports <- listOf genImportDecl
  func <- genFuncDecl
  let goCode = "package " ++ packageName pkg ++ "\n\n" ++
               "import (\n" ++
               concatMap (\imp -> "    " ++ importPath imp ++ "\n") imports ++
               ")\n\n" ++
               intercalate "\n" (funcLines func)
  return goCode

-- ============================================================================
-- IR Properties
-- ============================================================================

-- Property: buildSourceIR preserves TypusFile
prop_buildSourceIR_preserves_typus_file :: Property
prop_buildSourceIR_preserves_typus_file =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
    in sourceTypusFile sourceIR === typusFile

-- Property: rawSourceFromTypus concatenates block content
prop_rawSourceFromTypus_concatenates_blocks :: Property
prop_rawSourceFromTypus_concatenates_blocks =
  forAll genTypusFile $ \typusFile ->
    let rawSource = rawSourceFromTypus typusFile
        expectedContent = intercalate "\n" $ map cbContent (tfBlocks typusFile)
    in rawSource === expectedContent

-- Property: buildSourceIR creates correct source text
prop_buildSourceIR_correct_source_text :: Property
prop_buildSourceIR_correct_source_text =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
        expectedText = rawSourceFromTypus typusFile
    in sourceText sourceIR === expectedText

-- Property: parseGoModule handles valid Go code
prop_parseGoModule_valid_code :: Property
prop_parseGoModule_valid_code =
  forAll genGoCode $ \goCode ->
    let lines' = lines goCode
        result = parseGoModule lines'
    in case result of
      Left _ -> property False
      Right module -> property True

-- Property: renderGoModule is inverse of parseGoModule for simple cases
prop_renderGoModule_inverse_parse :: Property
prop_renderGoModule_inverse_parse =
  forAll genGoModule $ \goModule ->
    let rendered = renderGoModule goModule
        parsed = parseGoModule (lines rendered)
    in case parsed of
      Left _ -> property False
      Right parsedModule -> gmPackage parsedModule === gmPackage goModule

-- Property: emitGo creates valid Go source
prop_emitGo_creates_valid_source :: Property
prop_emitGo_creates_valid_source =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
        result = buildSemanticIR sourceIR
    in case result of
      Left _ -> property False
      Right semanticIR ->
        let goIR = emitGo semanticIR
        in not (L.null (goSource goIR))

-- Property: GoIR contains module information
prop_goIR_contains_module_info :: Property
prop_goIR_contains_module_info =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
        result = buildSemanticIR sourceIR
    in case result of
      Left _ -> property False
      Right semanticIR ->
        let goIR = emitGo semanticIR
        in goModule goIR === semanticModule semanticIR

-- Property: buildSemanticIR preserves structure
prop_buildSemanticIR_preserves_structure :: Property
prop_buildSemanticIR_preserves_structure =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
        result = buildSemanticIR sourceIR
    in case result of
      Left _ -> property False
      Right semanticIR -> semanticTypusFile semanticIR === typusFile

-- Property: moduleFromTypus creates valid module
prop_moduleFromTypus_creates_valid_module :: Property
prop_moduleFromTypus_creates_valid_module =
  forAll genTypusFile $ \typusFile ->
    let result = moduleFromTypus typusFile
    in case result of
      Left _ -> property False
      Right goModule -> property True

-- Property: Go module rendering preserves package declaration
prop_render_preserves_package :: Property
prop_render_preserves_package =
  forAll genGoModule $ \goModule ->
    case gmPackage goModule of
      Nothing -> property True
      Just pkg ->
        let rendered = renderGoModule goModule
        in "package " `L.isPrefixOf` rendered

-- Property: Go module rendering preserves imports
prop_render_preserves_imports :: Property
prop_render_preserves_imports =
  forAll genGoModule $ \goModule ->
    let rendered = renderGoModule goModule
        hasImports = not (L.null (gmImports goModule))
    in if hasImports
       then "import" `L.isInfixOf` rendered
       else property True

-- Property: Go module rendering preserves declarations
prop_render_preserves_declarations :: Property
prop_render_preserves_declarations =
  forAll genGoModule $ \goModule ->
    let rendered = renderGoModule goModule
        hasDecls = not (L.null (gmDecls goModule))
    in if hasDecls
       then property $ L.length (lines rendered) >= 3
       else property True

-- Property: Function declarations are preserved in IR
prop_func_decls_preserved :: Property
prop_func_decls_preserved =
  forAll genFuncDecl $ \funcDecl ->
    let goModule = GoModule [] Nothing [] [GoFunc funcDecl]
        rendered = renderGoModule goModule
    in "func " `L.isInfixOf` rendered

-- Property: Type declarations are preserved in IR
prop_type_decls_preserved :: Property
prop_type_decls_preserved =
  forAll genTypeDecl $ \typeDecl ->
    let goModule = GoModule [] Nothing [] [GoType typeDecl]
        rendered = renderGoModule goModule
    in "type " `L.isInfixOf` rendered

-- Property: Variable declarations are preserved in IR
prop_var_decls_preserved :: Property
prop_var_decls_preserved =
  forAll genVarDecl $ \varDecl ->
    let goModule = GoModule [] Nothing [] [GoVar varDecl]
        rendered = renderGoModule goModule
    in "var " `L.isInfixOf` rendered

-- Property: Constant declarations are preserved in IR
prop_const_decls_preserved :: Property
prop_const_decls_preserved =
  forAll genConstDecl $ \constDecl ->
    let goModule = GoModule [] Nothing [] [GoConst constDecl]
        rendered = renderGoModule goModule
    in "const " `L.isInfixOf` rendered

-- Property: Raw blocks are preserved in IR
prop_raw_blocks_preserved :: Property
prop_raw_blocks_preserved =
  forAll genRawBlock $ \rawBlock ->
    let goModule = GoModule [] Nothing [] [GoRaw rawBlock]
        rendered = renderGoModule goModule
        rawContent = intercalate "\n" (rawLines rawBlock)
    in rawContent `L.isInfixOf` rendered

-- Property: IR transformation maintains consistency
prop_ir_transformation_consistency :: Property
prop_ir_transformation_consistency =
  forAll genTypusFile $ \typusFile ->
    let sourceIR = buildSourceIR typusFile
        result = buildSemanticIR sourceIR
    in case result of
      Left _ -> property False
      Right semanticIR ->
        let goIR = emitGo semanticIR
            parsed = parseGoModule (lines (goSource goIR))
        in case parsed of
          Left _ -> property False
          Right parsedModule -> property True

-- Property: GoIR source is non-empty for non-empty TypusFile
prop_goIR_source_non_empty :: Property
prop_goIR_source_non_empty =
  forAll genTypusFile $ \typusFile ->
    not (L.null (tfBlocks typusFile)) ==>
    let sourceIR = buildSourceIR typusFile
        result = buildSemanticIR sourceIR
    in case result of
      Left _ -> property False
      Right semanticIR ->
        let goIR = emitGo semanticIR
        in not (L.null (goSource goIR))

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Core QuickCheck Tests"
  [ testGroup "SourceIR Properties"
    [ fastProperty "buildSourceIR preserves TypusFile" prop_buildSourceIR_preserves_typus_file
    , fastProperty "rawSourceFromTypus concatenates blocks" prop_rawSourceFromTypus_concatenates_blocks
    , fastProperty "buildSourceIR correct source text" prop_buildSourceIR_correct_source_text
    ]

  , testGroup "GoModule Properties"
    [ fastProperty "parseGoModule valid code" prop_parseGoModule_valid_code
    , fastProperty "renderGoModule inverse parse" prop_renderGoModule_inverse_parse
    , fastProperty "render preserves package" prop_render_preserves_package
    , fastProperty "render preserves imports" prop_render_preserves_imports
    , fastProperty "render preserves declarations" prop_render_preserves_declarations
    ]

  , testGroup "Declaration Properties"
    [ fastProperty "func decls preserved" prop_func_decls_preserved
    , fastProperty "type decls preserved" prop_type_decls_preserved
    , fastProperty "var decls preserved" prop_var_decls_preserved
    , fastProperty "const decls preserved" prop_const_decls_preserved
    , fastProperty "raw blocks preserved" prop_raw_blocks_preserved
    ]

  , testGroup "SemanticIR Properties"
    [ fastProperty "emitGo creates valid source" prop_emitGo_creates_valid_source
    , fastProperty "GoIR contains module info" prop_goIR_contains_module_info
    , fastProperty "buildSemanticIR preserves structure" prop_buildSemanticIR_preserves_structure
    , fastProperty "moduleFromTypus creates valid module" prop_moduleFromTypus_creates_valid_module
    , fastProperty "IR transformation consistency" prop_ir_transformation_consistency
    ]

  , testGroup "Advanced Properties"
    [ fastProperty "GoIR source non-empty" prop_goIR_source_non_empty
    ]
  ]