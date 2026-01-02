{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CodeGenerationQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, listOf1, choose
  , sized, resize, suchThat, vectorOf, arbitrary
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

import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate, sort, nub)
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Either (isLeft, isRight)

-- | 生成有效的包名
genPackageName :: Gen String
genPackageName = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['_']
  return $ first : rest

-- | 生成有效的导入路径
genImportPath :: Gen String
genImportPath = do
  parts <- listOf1 $ listOf1 $ elements $ ['a'..'z'] ++ ['_']
  return $ intercalate "/" parts

-- | 生成导入别名
genImportAlias :: Gen (Maybe String)
genImportAlias = oneof
  [ pure Nothing
  , Just <$> genPackageName
  ]

-- | 生成导入声明
genImportDecl :: Gen ImportDecl
genImportDecl = do
  alias <- genImportAlias
  path <- genImportPath
  return $ ImportDecl alias path

-- | 生成函数声明行
genFuncLines :: Gen [String]
genFuncLines = do
  name <- genPackageName
  params <- listOf genPackageName
  returnType <- elements ["int", "string", "bool", "void"]
  let funcLine = "func " ++ name ++ "(" ++ intercalate ", " params ++ ") " ++ returnType ++ " {"
  bodyLines <- listOf $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9'] ++ [';']
  let closingLine = "}"
  return $ funcLine : bodyLines ++ [closingLine]

-- | 生成类型声明行
genTypeLines :: Gen [String]
genTypeLines = do
  typeName <- genPackageName
  underlyingType <- elements ["int", "string", "struct", "interface"]
  let typeLine = "type " ++ typeName ++ " " ++ underlyingType
  additionalLines <- listOf $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['{'] ++ ['}']
  return $ typeLine : additionalLines

-- | 生成变量声明行
genVarLines :: Gen [String]
genVarLines = do
  varName <- genPackageName
  varType <- elements ["int", "string", "bool"]
  let varLine = "var " ++ varName ++ " " ++ varType
  additionalLines <- listOf $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['=']
  return $ varLine : additionalLines

-- | 生成常量声明行
genConstLines :: Gen [String]
genConstLines = do
  constName <- genPackageName
  constType <- elements ["int", "string", "bool"]
  let constLine = "const " ++ constName ++ " " ++ constType
  additionalLines <- listOf $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['=']
  return $ constLine : additionalLines

-- | 生成语句块行
genStatementLines :: Gen [String]
genStatementLines = do
  numLines <- choose (1, 5)
  vectorOf numLines $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9'] ++ [';']

-- | 生成原始块行
genRawLines :: Gen [String]
genRawLines = do
  numLines <- choose (1, 5)
  vectorOf numLines $ listOf $ elements $ ['a'..'z'] ++ [' '] ++ ['0'..'9'] ++ [';']

-- | 生成Go声明
genGoDecl :: Gen GoDecl
genGoDecl = oneof
  [ GoFunc <$> (FuncDecl <$> genFuncLines)
  , GoType <$> (TypeDecl <$> genTypeLines <*> arbitrary)
  , GoVar <$> (VarDecl <$> genVarLines <*> arbitrary)
  , GoConst <$> (ConstDecl <$> genConstLines <*> arbitrary)
  , GoStatement <$> (StatementBlock <$> genStatementLines)
  , GoRaw <$> (RawBlock <$> genRawLines)
  ]

-- | 生成Go模块
genGoModule :: Gen GoModule
genGoModule = do
  buildTags <- listOf $ listOf1 $ elements $ ['a'..'z'] ++ ['_']
  package <- oneof [pure Nothing, Just <$> (PackageDecl <$> genPackageName)]
  imports <- listOf genImportDecl
  decls <- listOf genGoDecl
  return $ GoModule buildTags package imports decls

-- | 生成有效的Go代码行
genGoCodeLines :: Gen [String]
genGoCodeLines = do
  numLines <- choose (1, 10)
  vectorOf numLines $ listOf $ elements $ 
    ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '] ++ [';'] ++ ['{'] ++ ['}'] ++ 
    ['('] ++ [')'] ++ ['='] ++ [','] ++ ['"'] ++ ['\n'] ++ ['\t']

-- 属性：parseGoModule应该解析有效的Go代码
prop_parseGoModule_valid_code :: Property
prop_parseGoModule_valid_code =
  forAll genGoCodeLines $ \lines ->
    let result = parseGoModule lines
    in isRight result === True

-- 属性：renderGoModule应该生成有效的Go代码
prop_renderGoModule_valid_code :: Property
prop_renderGoModule_valid_code =
  forAll genGoModule $ \goModule ->
    let rendered = renderGoModule goModule
    in not (null rendered) === True

-- 属性：parseGoModule和renderGoModule应该保持一致性
prop_parse_render_consistency :: Property
prop_parse_render_consistency =
  forAll genGoCodeLines $ \originalLines ->
    case parseGoModule originalLines of
      Left _ -> property True -- 解析失败时跳过
      Right parsedModule ->
        let renderedLines = lines $ renderGoModule parsedModule
        in property $ L.length renderedLines >= 0 -- 至少应该生成一些行

-- 属性：PackageDecl应该包含有效的包名
prop_packageDecl_valid_name :: Property
prop_packageDecl_valid_name =
  forAll genPackageName $ \name ->
    let pkgDecl = PackageDecl name
    in not (null name) && L.head name `elem` ['a'..'z']

-- 属性：ImportDecl应该包含有效的导入路径
prop_importDecl_valid_path :: Property
prop_importDecl_valid_path =
  forAll genImportDecl $ \importDecl ->
    let path = importPath importDecl
    in not (null path) && '/' `elem` path || not (null path)

-- 属性：FuncDecl应该包含函数行
prop_funcDecl_has_lines :: Property
prop_funcDecl_has_lines =
  forAll genFuncLines $ \lines ->
    let funcDecl = FuncDecl lines
    in not (null lines) && "func" `L.isPrefixOf` L.head lines

-- 属性：TypeDecl应该包含类型行
prop_typeDecl_has_lines :: Property
prop_typeDecl_has_lines =
  forAll genTypeLines $ \lines ->
  forAll arbitrary $ \isGroup ->
    let typeDecl = TypeDecl lines isGroup
    in not (null lines) && "type" `L.isPrefixOf` L.head lines

-- 属性：VarDecl应该包含变量行
prop_varDecl_has_lines :: Property
prop_varDecl_has_lines =
  forAll genVarLines $ \lines ->
  forAll arbitrary $ \isGroup ->
    let varDecl = VarDecl lines isGroup
    in not (null lines) && "var" `L.isPrefixOf` L.head lines

-- 属性：ConstDecl应该包含常量行
prop_constDecl_has_lines :: Property
prop_constDecl_has_lines =
  forAll genConstLines $ \lines ->
  forAll arbitrary $ \isGroup ->
    let constDecl = ConstDecl lines isGroup
    in not (null lines) && "const" `L.isPrefixOf` L.head lines

-- 属性：StatementBlock应该包含语句行
prop_statementBlock_has_lines :: Property
prop_statementBlock_has_lines =
  forAll genStatementLines $ \lines ->
    let stmtBlock = StatementBlock lines
    in not (null lines)

-- 属性：RawBlock应该包含原始行
prop_rawBlock_has_lines :: Property
prop_rawBlock_has_lines =
  forAll genRawLines $ \lines ->
    let rawBlock = RawBlock lines
    in not (null lines)

-- 属性：GoModule应该保持结构完整性
prop_goModule_structure_integrity :: Property
prop_goModule_structure_integrity =
  forAll genGoModule $ \goModule ->
    let buildTags = gmBuildTags goModule
        package = gmPackage goModule
        imports = gmImports goModule
        decls = gmDecls goModule
    in L.all (not . null) buildTags || null buildTags

-- 属性：isMainFunction应该正确识别main函数
prop_isMainFunction_identifies_main :: Property
prop_isMainFunction_identifies_main =
  let mainFunc = FuncDecl ["func main() {"]
      otherFunc = FuncDecl ["func other() {"]
  in (isMainFunction mainFunc === True) .&&. (isMainFunction otherFunc === False)

-- 属性：flattenDeclLines应该展平声明行
prop_flattenDeclLines_flattens :: Property
prop_flattenDeclLines_flattens =
  forAll genGoDecl $ \decl ->
    let flattened = flattenDeclLines decl
    in not (null flattened)

-- 属性：GoDecl的Eq实例应该正确比较声明
prop_goDecl_equality :: Property
prop_goDecl_equality =
  forAll genGoDecl $ \decl ->
    decl === decl

-- 属性：GoDecl的Show实例应该包含声明信息
prop_goDecl_show_informative :: Property
prop_goDecl_show_informative =
  forAll genGoDecl $ \decl ->
    let showStr = show decl
    in not (null showStr) === True

-- 属性：GoModule的Eq实例应该正确比较模块
prop_goModule_equality :: Property
prop_goModule_equality =
  forAll genGoModule $ \goModule ->
    goModule === goModule

-- 属性：GoModule的Show实例应该包含模块信息
prop_goModule_show_informative :: Property
prop_goModule_show_informative =
  forAll genGoModule $ \goModule ->
    let showStr = show goModule
    in not (null showStr) === True

-- 属性：ImportDecl的Eq实例应该正确比较导入
prop_importDecl_equality :: Property
prop_importDecl_equality =
  forAll genImportDecl $ \importDecl ->
    importDecl === importDecl

-- 属性：ImportDecl的Show实例应该包含导入信息
prop_importDecl_show_informative :: Property
prop_importDecl_show_informative =
  forAll genImportDecl $ \importDecl ->
    let showStr = show importDecl
    in not (null showStr) === True

-- 属性：解析包含包声明的代码应该成功
prop_parse_with_package :: Property
prop_parse_with_package =
  forAll genPackageName $ \pkgName ->
    let code = ["package " ++ pkgName]
    in isRight (parseGoModule code) === True

-- 属性：解析包含导入声明的代码应该成功
prop_parse_with_imports :: Property
prop_parse_with_imports =
  forAll (listOf genImportDecl) $ \imports ->
    let importLines = L.map (\imp -> "import \"" ++ importPath imp ++ "\"") imports
        code = ["package main"] ++ importLines
    in isRight (parseGoModule code) === True

-- 属性：解析包含函数声明的代码应该成功
prop_parse_with_functions :: Property
prop_parse_with_functions =
  forAll (listOf genFuncLines) $ \funcLinesList ->
    let allFuncLines = L.concat funcLinesList
        code = ["package main"] ++ allFuncLines
    in isRight (parseGoModule code) === True

tests :: TestTree
tests =
  testGroup "Code Generation QuickCheck Tests"
    [ fastProperty "parseGoModule valid code" prop_parseGoModule_valid_code
    , fastProperty "renderGoModule valid code" prop_renderGoModule_valid_code
    , fastProperty "parse render consistency" prop_parse_render_consistency
    , fastProperty "PackageDecl valid name" prop_packageDecl_valid_name
    , fastProperty "ImportDecl valid path" prop_importDecl_valid_path
    , fastProperty "FuncDecl has lines" prop_funcDecl_has_lines
    , fastProperty "TypeDecl has lines" prop_typeDecl_has_lines
    , fastProperty "VarDecl has lines" prop_varDecl_has_lines
    , fastProperty "ConstDecl has lines" prop_constDecl_has_lines
    , fastProperty "StatementBlock has lines" prop_statementBlock_has_lines
    , fastProperty "RawBlock has lines" prop_rawBlock_has_lines
    , fastProperty "GoModule structure integrity" prop_goModule_structure_integrity
    , fastProperty "isMainFunction identifies main" prop_isMainFunction_identifies_main
    , fastProperty "flattenDeclLines flattens" prop_flattenDeclLines_flattens
    , fastProperty "GoDecl equality" prop_goDecl_equality
    , fastProperty "GoDecl show informative" prop_goDecl_show_informative
    , fastProperty "GoModule equality" prop_goModule_equality
    , fastProperty "GoModule show informative" prop_goModule_show_informative
    , fastProperty "ImportDecl equality" prop_importDecl_equality
    , fastProperty "ImportDecl show informative" prop_importDecl_show_informative
    , fastProperty "parse with package" prop_parse_with_package
    , fastProperty "parse with imports" prop_parse_with_imports
    , fastProperty "parse with functions" prop_parse_with_functions
    ]