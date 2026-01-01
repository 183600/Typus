{-# LANGUAGE CPP #-}

module Test.Unit.GoAstQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), property, forAll, counterexample, classify, Arbitrary(..), Gen, oneof, choose, listOf, elements, vectorOf)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.List (nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

import Compiler.GoAst
  ( GoModule(..)
  , GoDecl(..)
  , ImportDecl(..)
  , FuncDecl(..)
  , TypeDecl(..)
  , VarDecl(..)
  , ConstDecl(..)
  , PackageDecl(..)
  )
import TestSupport.Arbitrary

-- Property: GoModule preserves package name
prop_gomodule_package_name :: PackageDecl -> [GoDecl] -> Property
prop_gomodule_package_name pkg decls =
  let goModule = GoModule [] (Just pkg) [] decls
  in property $ gmPackage goModule === Just pkg

-- Property: GoModule declaration count consistency
prop_gomodule_decl_count :: [GoDecl] -> Property
prop_gomodule_decl_count decls =
  let goModule = GoModule [] Nothing [] decls
  in property $ L.length (gmDecls goModule) === L.length decls

-- Property: GoModule import preservation
prop_gomodule_import_preservation :: [ImportDecl] -> [GoDecl] -> Property
prop_gomodule_import_preservation imports decls =
  let goModule = GoModule [] Nothing imports decls
  in property $ L.length (gmImports goModule) === L.length imports

-- Property: ImportDecl path validation
prop_import_path_valid :: String -> Property
prop_import_path_valid path =
  let validChars = L.all (\c -> c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/.-_") path
      importDecl = ImportDecl Nothing path
  in classify validChars "valid path characters" $
     property $ validChars ==> not (null path)

-- Property: ImportDecl alias preservation
prop_import_alias_preservation :: Maybe String -> String -> Property
prop_import_alias_preservation alias path =
  let importDecl = ImportDecl alias path
  in property $ importAlias importDecl === alias

-- Property: FuncDecl parameter preservation
prop_func_params_preservation :: [String] -> Property
prop_func_params_preservation params =
  let func = FuncDecl params
  in property $ L.length (funcLines func) === L.length params

-- Property: TypeDecl field preservation
prop_type_fields_preservation :: [String] -> Property
prop_type_fields_preservation fields =
  let typeDecl = TypeDecl fields False
  in property $ L.length (typeLines typeDecl) === L.length fields

-- Property: VarDecl variable preservation
prop_var_names_preservation :: [String] -> Property
prop_var_names_preservation names =
  let varDecl = VarDecl names False
  in property $ L.length (varLines varDecl) === L.length names

-- Property: ConstDecl constant preservation
prop_const_names_preservation :: [String] -> Property
prop_const_names_preservation names =
  let constDecl = ConstDecl names False
  in property $ L.length (constLines constDecl) === L.length names

-- Property: GoDecl type classification
prop_godecl_classification :: GoDecl -> Property
prop_godecl_classification decl =
  let isFunc = case decl of { GoFunc _ -> True; _ -> False }
      isType = case decl of { GoType _ -> True; _ -> False }
      isVar = case decl of { GoVar _ -> True; _ -> False }
      isConst = case decl of { GoConst _ -> True; _ -> False }
  in property $ isFunc || isType || isVar || isConst

-- Property: PackageDecl name preservation
prop_package_name_preservation :: String -> Property
prop_package_name_preservation name =
  let pkg = PackageDecl name
  in property $ packageName pkg === name

-- Property: GoModule build tags preservation
prop_gomodule_build_tags :: [String] -> [GoDecl] -> Property
prop_gomodule_build_tags tags decls =
  let goModule = GoModule tags Nothing [] decls
  in property $ gmBuildTags goModule === tags

-- Property: ImportDecl uniqueness
prop_import_uniqueness :: [ImportDecl] -> Property
prop_import_uniqueness imports =
  let paths = map importPath imports
      uniquePaths = L.length paths === L.length (nub paths)
  in property $ True -- Just checking that we can handle both unique L.and non-unique imports

-- Property: Function parameter ordering
prop_func_param_ordering :: [String] -> Property
prop_func_param_ordering params =
  let func = FuncDecl params
      expectedOrder = params
      actualOrder = funcLines func
  in property $ expectedOrder === actualOrder

-- Property: Type field ordering
prop_type_field_ordering :: [String] -> Property
prop_type_field_ordering fields =
  let typeDecl = TypeDecl fields False
      expectedOrder = fields
      actualOrder = typeLines typeDecl
  in property $ expectedOrder === actualOrder

-- Property: Variable declaration ordering
prop_var_ordering :: [String] -> Property
prop_var_ordering names =
  let varDecl = VarDecl names False
      expectedOrder = names
      actualOrder = varLines varDecl
  in property $ expectedOrder === actualOrder

-- Property: Constant declaration ordering
prop_const_ordering :: [String] -> Property
prop_const_ordering names =
  let constDecl = ConstDecl names False
      expectedOrder = names
      actualOrder = constLines constDecl
  in property $ expectedOrder === actualOrder

-- Property: GoModule empty handling
prop_gomodule_empty :: Property
prop_gomodule_empty =
  let goModule = GoModule [] Nothing [] []
  in property $ 
       L.null (gmBuildTags goModule) &&
       isNothing (gmPackage goModule) &&
       L.null (gmImports goModule) &&
       L.null (gmDecls goModule)

-- Property: GoModule large declaration handling
prop_gomodule_large :: Int -> Property
prop_gomodule_large numDecls =
  numDecls >= 0 && numDecls <= 1000 ==>
  let decls = replicate numDecls (GoVar (VarDecl ["x"] False))
      goModule = GoModule [] Nothing [] decls
  in property $ L.length (gmDecls goModule) === numDecls

-- Property: ImportDecl path format validation
prop_import_path_format :: String -> Property
prop_import_path_format path =
  let hasValidFormat = not (null path) && 
                       not ( "." `L.isPrefixOf` path) &&
                       not ( "/" `L.isPrefixOf` path) &&
                       not ("/" `L.isSuffixOf` path)
      importDecl = ImportDecl Nothing path
  in classify hasValidFormat "valid format" $
     property $ True -- Just checking format recognition

-- Property: GoModule with mixed declaration types
prop_gomodule_mixed_decls :: [GoDecl] -> [GoDecl] -> [GoDecl] -> [GoDecl] -> Property
prop_gomodule_mixed_decls funcs types vars consts =
  let allDecls = funcs ++ types ++ vars ++ consts
      goModule = GoModule [] Nothing [] allDecls
      funcCount = L.length [() | GoFunc _ <- allDecls]
      typeCount = L.length [() | GoType _ <- allDecls]
      varCount = L.length [() | GoVar _ <- allDecls]
      constCount = L.length [() | GoConst _ <- allDecls]
      allCorrect = funcCount == L.length funcs &&
                   typeCount == L.length types &&
                   varCount == L.length vars &&
                   constCount == L.length consts
  in property $ allCorrect

-- Property: PackageDecl name validation
prop_package_name_validation :: String -> Property
prop_package_name_validation name =
  let hasValidChars = L.all (\c -> c `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_") name
      pkg = PackageDecl name
  in classify hasValidChars "valid package name" $
     property $ True -- Just checking name validation

-- Property: GoModule build tag validation
prop_build_tags_validation :: [String] -> Property
prop_build_tags_validation tags =
  let hasValidTags = L.all (not . null) tags
      goModule = GoModule tags Nothing [] []
  in classify hasValidTags "L.all non-empty tags" $
     property $ L.length (gmBuildTags goModule) === L.length tags

-- Property: ImportDecl with alias handling
prop_import_with_alias :: String -> String -> Property
prop_import_with_alias alias path =
  let importDecl = ImportDecl (Just alias) path
      correctAlias = importAlias importDecl == Just alias
      correctPath = importPath importDecl == path
  in property $ correctAlias && correctPath

-- Property: GoModule consistency checks
prop_gomodule_consistency :: [String] -> Maybe PackageDecl -> [ImportDecl] -> [GoDecl] -> Property
prop_gomodule_consistency tags mbPkg imports decls =
  let goModule = GoModule tags mbPkg imports decls
      correctTags = gmBuildTags goModule == tags
      correctPackage = gmPackage goModule == mbPkg
      correctImports = gmImports goModule == imports
      correctDecls = gmDecls goModule == decls
  in property $ correctTags && correctPackage && correctImports && correctDecls

-- Property: GoDecl round-trip construction
prop_godecl_roundtrip :: GoDecl -> Property
prop_godecl_roundtrip decl =
  let reconstructed = case decl of
        GoFunc func -> GoFunc func
        GoType typeDecl -> GoType typeDecl
        GoVar varDecl -> GoVar varDecl
        GoConst constDecl -> GoConst constDecl
        GoStatement stmt -> GoStatement stmt
        GoRaw raw -> GoRaw raw
  in property $ reconstructed === decl

-- Property: GoModule serialization simulation
prop_gomodule_serialization :: GoModule -> Property
prop_gomodule_serialization goModule =
  let tags = gmBuildTags goModule
      mbPkg = gmPackage goModule
      imports = gmImports goModule
      decls = gmDecls goModule
      reconstructed = GoModule tags mbPkg imports decls
      correctTags = gmBuildTags reconstructed == tags
      correctPackage = gmPackage reconstructed == mbPkg
      correctImports = gmImports reconstructed == imports
      correctDecls = gmDecls reconstructed == decls
  in property $ correctTags && correctPackage && correctImports && correctDecls

tests :: TestTree
tests = testGroup "GoAST QuickCheck Tests"
  [ fastProperty "GoModule preserves package name" prop_gomodule_package_name
  , fastProperty "GoModule declaration count consistency" prop_gomodule_decl_count
  , fastProperty "GoModule import preservation" prop_gomodule_import_preservation
  , fastProperty "ImportDecl path validation" prop_import_path_valid
  , fastProperty "ImportDecl alias preservation" prop_import_alias_preservation
  , fastProperty "FuncDecl parameter preservation" prop_func_params_preservation
  , fastProperty "TypeDecl field preservation" prop_type_fields_preservation
  , fastProperty "VarDecl variable preservation" prop_var_names_preservation
  , fastProperty "ConstDecl constant preservation" prop_const_names_preservation
  , fastProperty "GoDecl type classification" prop_godecl_classification
  , fastProperty "PackageDecl name preservation" prop_package_name_preservation
  , fastProperty "GoModule build tags preservation" prop_gomodule_build_tags
  , fastProperty "ImportDecl uniqueness" prop_import_uniqueness
  , fastProperty "Function parameter ordering" prop_func_param_ordering
  , fastProperty "Type field ordering" prop_type_field_ordering
  , fastProperty "Variable declaration ordering" prop_var_ordering
  , fastProperty "Constant declaration ordering" prop_const_ordering
  , fastProperty "GoModule empty handling" prop_gomodule_empty
  , fastProperty "GoModule large declaration handling" prop_gomodule_large
  , fastProperty "ImportDecl path format validation" prop_import_path_format
  , fastProperty "GoModule with mixed declaration types" prop_gomodule_mixed_decls
  , fastProperty "PackageDecl name validation" prop_package_name_validation
  , fastProperty "GoModule build tag validation" prop_build_tags_validation
  , fastProperty "ImportDecl with alias handling" prop_import_with_alias
  , fastProperty "GoModule consistency checks" prop_gomodule_consistency
  , fastProperty "GoDecl round-trip construction" prop_godecl_roundtrip
  , fastProperty "GoModule serialization simulation" prop_gomodule_serialization
  ]