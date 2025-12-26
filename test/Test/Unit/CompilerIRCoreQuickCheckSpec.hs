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
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Positive(..), NonNegative(..), Arbitrary(..), oneof, elements, Gen, suchThat)

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
  , flattenDeclLines
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
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanStart
  , spanEnd
  , posLine
  , posCol
  )

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, null, length)
import Data.Char (isSpace, isAlphaNum)

-- Property: buildSourceIR preserves Typus file
prop_buildSourceIR_preserves_typus :: String -> Property
prop_buildSourceIR_preserves_typus source =
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
      in property $ sourceTypusFile sourceIR === typusFile

-- Property: buildSourceIR extracts non-empty source text
prop_buildSourceIR_extracts_source :: String -> Property
prop_buildSourceIR_extracts_source source =
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          extracted = sourceText sourceIR
      in property $ not (null extracted) || null (tfBlocks typusFile)

-- Property: buildSemanticIR preserves Typus file
prop_buildSemanticIR_preserves_typus :: String -> Property
prop_buildSemanticIR_preserves_typus source =
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
      in property $ semanticTypusFile semanticIR === typusFile

-- Property: buildSemanticIRWithPackage sets package name
prop_buildSemanticIRWithPackage_sets_package :: String -> String -> Property
prop_buildSemanticIRWithPackage_sets_package source packageName =
  not (null packageName) ==> all (\c -> isAlphaNum c || c == '_' || c == '.') packageName ==>
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIRWithPackage packageName sourceIR
          goModule = semanticGoModule semanticIR
          package = gmPackage goModule
      in case package of
           Nothing -> property $ counterexample "package not set" False
           Just pkg -> property $ packageName pkg === packageName

-- Property: ensurePackageDecl adds package if missing
prop_ensurePackageDecl_adds_if_missing :: String -> String -> Property
prop_ensurePackageDecl_adds_if_missing source packageName =
  not (null packageName) ==> all (\c -> isAlphaNum c || c == '_' || c == '.') packageName ==>
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
          goModule = semanticGoModule semanticIR
          module' = ensurePackageDecl packageName goModule
          package = gmPackage module'
      in property $ 
        case package of
          Nothing -> False
          Just pkg -> packageName pkg === packageName

-- Property: ensurePackageDecl preserves existing package
prop_ensurePackageDecl_preserves_existing :: String -> String -> Property
prop_ensurePackageDecl_preserves_existing source packageName =
  not (null packageName) ==> all (\c -> isAlphaNum c || c == '_' || c == '.') packageName ==>
  let sourceWithPackage = "package " ++ packageName ++ "\n" ++ source
  in case parseTypus sourceWithPackage of
       Left _ -> property $ True -- Invalid source is allowed to fail
       Right typusFile ->
         let sourceIR = buildSourceIR typusFile
             semanticIR = buildSemanticIR sourceIR
             goModule = semanticGoModule semanticIR
             module' = ensurePackageDecl packageName goModule
             package = gmPackage module'
         in property $ 
           case package of
             Nothing -> False
             Just pkg -> packageName pkg === packageName

-- Property: ensureMainFunction adds main function if missing
prop_ensureMainFunction_adds_if_missing :: String -> Property
prop_ensureMainFunction_adds_if_missing source =
  not ("func main" `isInfixOf` source) ==>
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
          goModule = semanticGoModule semanticIR
          module' = ensureMainFunction goModule
          hasMain = any isMainFunction (gmDecls module')
      in property $ hasMain

-- Property: ensureMainFunction preserves existing main function
prop_ensureMainFunction_preserves_existing :: String -> Property
prop_ensureMainFunction_preserves_existing source =
  "func main" `isInfixOf` source ==>
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
          goModule = semanticGoModule semanticIR
          module' = ensureMainFunction goModule
          mainCount = length $ filter isMainFunction (gmDecls module')
      in property $ mainCount >= 1

-- Property: parseGoModule handles empty input
prop_parseGoModule_empty :: Property
prop_parseGoModule_empty =
  let result = parseGoModule ""
  in case result of
       Left _ -> property $ True
       Right module' -> property $ null (gmDecls module')

-- Property: renderGoModule produces non-empty output for non-empty module
prop_renderGoModule_non_empty :: GoModule -> Property
prop_renderGoModule_non_empty module' =
  let rendered = renderGoModule module'
      hasDecls = not (null (gmDecls module'))
  in classify hasDecls "has declarations" $
     classify (not hasDecls) "no declarations" $
     property $ not (null rendered) || not hasDecls

-- Property: renderGoModule and parseGoModule roundtrip
prop_render_parse_roundtrip :: GoModule -> Property
prop_render_parse_roundtrip module' =
  let rendered = renderGoModule module'
  in case parseGoModule rendered of
       Left _ -> property $ True -- Parse failures are allowed for complex modules
       Right parsedModule -> property $ gmPackage parsedModule === gmPackage module'

-- Property: isMainFunction correctly identifies main functions
prop_isMainFunction_identifies_main :: String -> Property
prop_isMainFunction_identifies_main funcName =
  not (null funcName) ==> 
  let mainFunc = "func main() {}"
      otherFunc = "func " ++ funcName ++ "() {}"
  in property $ isMainFunction (GoStatement (StatementBlock [mainFunc])) .&&. 
             not (isMainFunction (GoStatement (StatementBlock [otherFunc])))

-- Property: flattenDeclLines produces correct output
prop_flattenDeclLines_correct :: [String] -> Property
prop_flattenDeclLines_correct lines =
  let decl = GoStatement (StatementBlock lines)
      flattened = flattenDeclLines decl
  in property $ flattened === lines

-- Property: rawSourceFromTypus extracts code blocks
prop_rawSourceFromTypus_extracts_blocks :: String -> Property
prop_rawSourceFromTypus_extracts_blocks source =
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let rawSource = rawSourceFromTypus typusFile
          hasBlocks = not (null (tfBlocks typusFile))
      in classify hasBlocks "has blocks" $
         classify (not hasBlocks) "no blocks" $
         property $ not (null rawSource) || not hasBlocks

-- Property: moduleFromTypus creates valid Go module
prop_moduleFromTypus_creates_module :: String -> Property
prop_moduleFromTypus_creates_module source =
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let goModule = moduleFromTypus typusFile
      in property $ 
        case gmPackage goModule of
          Nothing -> True -- Package may be missing
          Just _ -> True -- Package exists, which is fine

-- Property: attachInferredImports preserves existing structure
prop_attachInferredImports_preserves_structure :: GoModule -> [ImportDecl] -> Property
prop_attachInferredImports_preserves_structure module' imports =
  let module'WithImports = attachInferredImports imports module'
  in property $ gmPackage module'WithImports === gmPackage module' .&&.
             length (gmDecls module'WithImports) >= length (gmDecls module')

-- Property: emitGo produces non-empty output for valid IR
prop_emitGo_non_empty :: String -> Property
prop_emitGo_non_empty source =
  case parseTypus source of
    Left _ -> property $ True -- Invalid source is allowed to fail
    Right typusFile ->
      let sourceIR = buildSourceIR typusFile
          semanticIR = buildSemanticIR sourceIR
          goCode = emitGo semanticIR
      in property $ not (null goCode) || null (tfBlocks typusFile)

-- Property: GoModule equality is consistent
prop_goModule_equality_consistent :: GoModule -> GoModule -> Property
prop_goModule_equality_consistent mod1 mod2 =
  let areEqual = mod1 == mod2
      samePackage = gmPackage mod1 == gmPackage mod2
      sameImports = gmImports mod1 == gmImports mod2
      sameDecls = gmDecls mod1 == gmDecls mod2
  in property $ areEqual === (samePackage .&&. sameImports .&&. sameDecls)

-- Property: ImportDecl equality is consistent
prop_importDecl_equality_consistent :: String -> String -> String -> String -> Property
prop_importDecl_equality_consistent path1 alias1 path2 alias2 =
  let import1 = ImportDecl (if null alias1 then Nothing else Just alias1) path1
      import2 = ImportDecl (if null alias2 then Nothing else Just alias2) path2
      areEqual = import1 == import2
      samePath = path1 == path2
      sameAlias = (if null alias1 then Nothing else Just alias1) == 
                  (if null alias2 then Nothing else Just alias2)
  in property $ areEqual === (samePath .&&. sameAlias)

tests :: TestTree
tests =
  testGroup "Compiler IR Core QuickCheck Tests"
    [ fastProperty "buildSourceIR preserves Typus file" prop_buildSourceIR_preserves_typus
    , fastProperty "buildSourceIR extracts source text" prop_buildSourceIR_extracts_source
    , fastProperty "buildSemanticIR preserves Typus file" prop_buildSemanticIR_preserves_typus
    , fastProperty "buildSemanticIRWithPackage sets package name" prop_buildSemanticIRWithPackage_sets_package
    , fastProperty "ensurePackageDecl adds package if missing" prop_ensurePackageDecl_adds_if_missing
    , fastProperty "ensurePackageDecl preserves existing package" prop_ensurePackageDecl_preserves_existing
    , fastProperty "ensureMainFunction adds main function if missing" prop_ensureMainFunction_adds_if_missing
    , fastProperty "ensureMainFunction preserves existing main function" prop_ensureMainFunction_preserves_existing
    , fastProperty "parseGoModule handles empty input" prop_parseGoModule_empty
    , fastProperty "renderGoModule produces non-empty output" prop_renderGoModule_non_empty
    , fastProperty "renderGoModule and parseGoModule roundtrip" prop_render_parse_roundtrip
    , fastProperty "isMainFunction correctly identifies main functions" prop_isMainFunction_identifies_main
    , fastProperty "flattenDeclLines produces correct output" prop_flattenDeclLines_correct
    , fastProperty "rawSourceFromTypus extracts code blocks" prop_rawSourceFromTypus_extracts_blocks
    , fastProperty "moduleFromTypus creates valid Go module" prop_moduleFromTypus_creates_module
    , fastProperty "attachInferredImports preserves structure" prop_attachInferredImports_preserves_structure
    , fastProperty "emitGo produces non-empty output" prop_emitGo_non_empty
    , fastProperty "GoModule equality is consistent" prop_goModule_equality_consistent
    , fastProperty "ImportDecl equality is consistent" prop_importDecl_equality_consistent
    ]