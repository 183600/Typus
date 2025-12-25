{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewDependentTypesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Test.QuickCheck as QC

import DependentTypesParser
  ( TypeRef(..)
  , TypeBody(..)
  , Field(..)
  , TypeParameter(..)
  , TypeConstraint(..)
  , DependentType(..)
  , DependentTypesParser(..)
  , DependentTypeError(..)
  , runDependentTypesParser
  , parseDependentType
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  , tInt
  , tVoid
  )

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub, intercalate)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- ============================================================================
-- Arbitrary Instances for QuickCheck Testing
-- ============================================================================

-- Generate valid identifiers (not keywords)
instance Arbitrary String where
  arbitrary = do
    first <- QC.elements ['a'..'z']
    rest <- QC.listOf (QC.elements (['a'..'z'] ++ ['0'..'9'] ++ "_"))
    let ident = first : rest
    -- Avoid keywords
    return $ if ident `elem` ["type", "func", "where", "alias", "struct", "len", "nonempty"]
             then "x" ++ ident
             else ident

-- Generate arbitrary type reference
instance Arbitrary TypeRef where
  arbitrary = QC.sized $ \size ->
    if size <= 1
    then TypeRef <$> QC.arbitrary <*> pure []
    else do
      name <- QC.arbitrary
      numArgs <- QC.choose (0, min 3 (size `div` 2))
      args <- QC.vectorOf numArgs (QC.resize (size `div` 2) QC.arbitrary)
      return $ TypeRef name args

-- Generate arbitrary field
instance Arbitrary Field where
  arbitrary = Field <$> QC.arbitrary <*> QC.arbitrary

-- Generate arbitrary type constraint
instance Arbitrary TypeConstraint where
  arbitrary = QC.oneof
    [ EqualityConstraint <$> QC.arbitrary <*> QC.arbitrary
    , InequalityConstraint <$> QC.arbitrary <*> QC.arbitrary
    , RangeConstraint <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    , SizeConstraint <$> QC.arbitrary <*> QC.arbitrary
    , NonEmptyConstraint <$> QC.arbitrary
    , PredicateConstraint <$> QC.arbitrary <*> QC.listOf QC.arbitrary
    , TypeClassConstraint <$> QC.arbitrary <*> QC.arbitrary
    , CustomConstraint <$> QC.arbitrary <*> QC.arbitrary
    ]

-- Generate arbitrary type parameter
instance Arbitrary TypeParameter where
  arbitrary = do
    name <- QC.arbitrary
    paramType <- QC.arbitrary
    constraints <- QC.listOf QC.arbitrary
    return $ TypeParameter name paramType constraints

-- Generate arbitrary type body
instance Arbitrary TypeBody where
  arbitrary = StructBody <$> QC.listOf QC.arbitrary

-- Generate arbitrary dependent type
instance Arbitrary DependentType where
  arbitrary = QC.oneof
    [ TypeDecl <$> QC.arbitrary <*> QC.listOf QC.arbitrary <*> QC.arbitrary <*> QC.listOf QC.arbitrary
    , DependentFunction <$> QC.arbitrary <*> QC.listOf ((,) <$> QC.arbitrary <*> QC.arbitrary) <*> QC.arbitrary <*> QC.listOf QC.arbitrary
    , TypeAlias <$> QC.arbitrary <*> QC.arbitrary <*> QC.listOf QC.arbitrary
    ]

-- ============================================================================
-- Property Tests for Dependent Types
-- ============================================================================

-- Property: TypeRef with no args is simple
prop_typeref_no_args_is_simple :: String -> Property
prop_typeref_no_args_is_simple name =
  let ref = TypeRef name []
  in property $ refArgs ref === []

-- Property: TypeRef preserves name and args
prop_typeref_preserves_name_args :: String -> [TypeRef] -> Property
prop_typeref_preserves_name_args name args =
  let ref = TypeRef name args
  in property $ refName ref === name .&&. refArgs ref === args

-- Property: Field preserves name and type
prop_field_preserves_name_type :: String -> TypeRef -> Property
prop_field_preserves_name_type name fieldType =
  let field = Field name fieldType
  in property $ fieldName field === name .&&. fieldType field === fieldType

-- Property: Equality constraint preserves both sides
prop_equality_constraint_preserves_sides :: String -> String -> Property
prop_equality_constraint_preserves_sides left right =
  let constraint = EqualityConstraint left right
  in case constraint of
       EqualityConstraint l r -> property $ l === left .&&. r === right
       _ -> property $ False

-- Property: Range constraint preserves range
prop_range_constraint_preserves_range :: String -> Int -> Int -> Property
prop_range_constraint_preserves_range name low high =
  let constraint = RangeConstraint name low high
  in case constraint of
       RangeConstraint n l h -> property $ n === name .&&. l === low .&&. h === high
       _ -> property $ False

-- Property: Size constraint preserves name and size
prop_size_constraint_preserves_size :: String -> Int -> Property
prop_size_constraint_preserves_size name size =
  let constraint = SizeConstraint name size
  in case constraint of
       SizeConstraint n s -> property $ n === name .&&. s === size
       _ -> property $ False

-- Property: NonEmpty constraint preserves name
prop_nonempty_constraint_preserves_name :: String -> Property
prop_nonempty_constraint_preserves_name name =
  let constraint = NonEmptyConstraint name
  in case constraint of
       NonEmptyConstraint n -> property $ n === name
       _ -> property $ False

-- Property: Predicate constraint preserves name and args
prop_predicate_constraint_preserves_name_args :: String -> [String] -> Property
prop_predicate_constraint_preserves_name_args name args =
  let constraint = PredicateConstraint name args
  in case constraint of
       PredicateConstraint n a -> property $ n === name .&&. a === args
       _ -> property $ False

-- Property: TypeParameter preserves all fields
prop_type_parameter_preserves_fields :: String -> TypeRef -> [TypeConstraint] -> Property
prop_type_parameter_preserves_fields name paramType constraints =
  let param = TypeParameter name paramType constraints
  in property $ paramName param === name .&&.
             paramType param === paramType .&&.
             paramConstraints param === constraints

-- Property: StructBody preserves fields
prop_struct_body_preserves_fields :: [Field] -> Property
prop_struct_body_preserves_fields fields =
  let body = StructBody fields
  in case body of
       StructBody f -> property $ f === fields
       _ -> property $ False

-- Property: TypeDecl preserves all components
prop_type_decl_preserves_components :: String -> [TypeParameter] -> TypeBody -> [TypeConstraint] -> Property
prop_type_decl_preserves_components name params body constraints =
  let decl = TypeDecl name params body constraints
  in case decl of
       TypeDecl n p b c -> property $ n === name .&&. p === params .&&. b === body .&&. c === constraints
       _ -> property $ False

-- Property: DependentFunction preserves all components
prop_dependent_function_preserves_components :: String -> [(String, TypeRef)] -> TypeRef -> [TypeConstraint] -> Property
prop_dependent_function_preserves_components name params retType constraints =
  let func = DependentFunction name params retType constraints
  in case func of
       DependentFunction n p r c -> property $ n === name .&&. p === params .&&. r === retType .&&. c === constraints
       _ -> property $ False

-- Property: TypeAlias preserves all components
prop_type_alias_preserves_components :: String -> TypeRef -> [TypeConstraint] -> Property
prop_type_alias_preserves_components name target constraints =
  let alias = TypeAlias name target constraints
  in case alias of
       TypeAlias n t c -> property $ n === name .&&. t === target .&&. c === constraints
       _ -> property $ False

-- Property: Basic type references are valid
prop_basic_types_valid :: Property
prop_basic_types_valid =
  property $ refName tInt === "int" .&&. refArgs tInt === [] .&&.
             refName tVoid === "void" .&&. refArgs tVoid === []

-- Property: Parsing simple type declaration works
prop_parse_simple_type_declaration :: String -> Property
prop_parse_simple_type_declaration name =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let input = "type " ++ name ++ " struct { x: int }"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (TypeDecl n _ (StructBody fields) _) -> 
         property $ n === name .&&. not (null fields)
       Right _ -> property $ False

-- Property: Parsing simple type alias works
prop_parse_simple_type_alias :: String -> Property
prop_parse_simple_type_alias name =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let input = "alias " ++ name ++ " = int"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (TypeAlias n target _) -> 
         property $ n === name .&&. refName target === "int"
       Right _ -> property $ False

-- Property: Parsing function declaration works
prop_parse_function_declaration :: String -> Property
prop_parse_function_declaration name =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let input = "func " ++ name ++ "(x: int) -> int"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (DependentFunction n params ret _) -> 
         property $ n === name .&&. not (null params) .&&. refName ret === "int"
       Right _ -> property $ False

-- Property: Parsing type with constraints works
prop_parse_type_with_constraints :: String -> Property
prop_parse_type_with_constraints name =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let input = "type " ++ name ++ " struct { x: int } where x >= 0"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (TypeDecl n _ _ constraints) -> 
         property $ n === name .&&. not (null constraints)
       Right _ -> property $ False

-- Property: Parsing generic type works
prop_parse_generic_type :: String -> String -> Property
prop_parse_generic_type typeName paramName =
  not (null typeName) && not (null paramName) &&
  typeName `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] &&
  paramName `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let input = "type " ++ typeName ++ "<" ++ paramName ++ "> struct { x: " ++ paramName ++ " }"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (TypeDecl n params _ _) -> 
         property $ n === typeName .&&. not (null params) .&&. paramName (head params) === paramName
       Right _ -> property $ False

-- Property: Validation catches syntax errors
prop_validation_catches_syntax_errors :: Property
prop_validation_catches_syntax_errors =
  let invalidInput = "type invalid struct { x int }"  -- Missing colon
      errors = validateDependentTypeSyntax invalidInput
  in property $ not (null errors)

-- Property: Validation accepts valid input
prop_validation_accepts_valid_input :: String -> Property
prop_validation_accepts_valid_input name =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let validInput = "type " ++ name ++ " struct { x: int }"
      errors = validateDependentTypeSyntax validInput
  in property $ null errors

-- Property: Parser state preserves type scope
prop_parser_state_preserves_scope :: [DependentType] -> Property
prop_parser_state_preserves_scope types =
  let typeNames = map getTypeName types
      -- Create a simple program with these types
      program = unlines $ map typeToProgram types
      result = runDependentTypesParser program
  in case result of
       Left _ -> property $ False
       Right (_, parser) -> 
         let scopeNames = Map.keys (typeScope parser)
         in property $ sort scopeNames === sort typeNames
  where
    getTypeName (TypeDecl n _ _ _) = n
    getTypeName (DependentFunction n _ _ _) = n
    getTypeName (TypeAlias n _ _) = n
    
    typeToProgram (TypeDecl n _ (StructBody fields) _) =
      "type " ++ n ++ " struct { " ++ unlines (map fieldToProgram fields) ++ " }"
    typeToProgram (DependentFunction n params ret _) =
      "func " ++ n ++ "(" ++ intercalate ", " (map paramToProgram params) ++ ") -> " ++ refName ret
    typeToProgram (TypeAlias n target _) =
      "alias " ++ n ++ " = " ++ typeRefToProgram target
    
    fieldToProgram (Field name typ) = name ++ ": " ++ typeRefToProgram typ
    paramToProgram (name, typ) = name ++ ": " ++ typeRefToProgram typ
    typeRefToProgram (TypeRef name args) = 
      if null args then name else name ++ "<" ++ intercalate ", " (map typeRefToProgram args) ++ ">"

-- Property: Error detection for duplicate definitions
prop_duplicate_definition_error :: String -> Property
prop_duplicate_definition_error name =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let program = unlines
        [ "type " ++ name ++ " struct { x: int }"
        , "type " ++ name ++ " struct { y: string }"
        ]
      result = runDependentTypesParser program
  in case result of
       Left _ -> property $ False
       Right (_, parser) -> 
         let errors = parserErrors parser
             hasDuplicateError = any isDuplicateError errors
         in property $ hasDuplicateError
  where
    isDuplicateError (InvalidTypeSyntax msg) = "重复定义" `isInfixOf` msg
    isDuplicateError _ = False

-- Property: Type parameter constraints are preserved
prop_type_parameter_constraints_preserved :: String -> [TypeConstraint] -> Property
prop_type_parameter_constraints_preserved name constraints =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let param = TypeParameter name tInt constraints
      input = "type Test<" ++ name ++ "> struct { x: " ++ name ++ " }"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (TypeDecl _ params _ _) -> 
         if null params then property $ False
         else property $ paramConstraints (head params) === constraints
       Right _ -> property $ False

-- Property: Nested generic types are parsed correctly
prop_nested_generic_types :: String -> String -> String -> Property
prop_nested_generic_types outerName innerName valueName =
  not (null outerName) && not (null innerName) && not (null valueName) &&
  all (`notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"]) 
       [outerName, innerName, valueName] ==>
  let input = "type " ++ outerName ++ " struct { x: " ++ innerName ++ "<" ++ valueName ++ "> }"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (TypeDecl n _ (StructBody fields) _) -> 
         if null fields then property $ False
         else case fieldType (head fields) of
                TypeRef name [TypeRef inner []] -> 
                  property $ n === outerName .&&. name === innerName .&&. inner === valueName
                _ -> property $ False
       Right _ -> property $ False

-- Property: Complex constraints are parsed correctly
prop_complex_constraints :: String -> Property
prop_complex_constraints name =
  not (null name) && name `notElem` ["type", "func", "where", "alias", "struct", "len", "nonempty"] ==>
  let input = "type " ++ name ++ " struct { x: int } where x >= 0 & x <= 100 & len name > 0"
  in case parseTypeDeclaration input of
       Left _ -> property $ False
       Right (TypeDecl _ _ _ constraints) -> 
         property $ length constraints >= 3
       Right _ -> property $ False

tests :: TestTree
tests =
  testGroup "New Dependent Types QuickCheck Tests"
    [ fastProperty "TypeRef with no args is simple" prop_typeref_no_args_is_simple
    , fastProperty "TypeRef preserves name and args" prop_typeref_preserves_name_args
    , fastProperty "Field preserves name and type" prop_field_preserves_name_type
    , fastProperty "Equality constraint preserves both sides" prop_equality_constraint_preserves_sides
    , fastProperty "Range constraint preserves range" prop_range_constraint_preserves_range
    , fastProperty "Size constraint preserves name and size" prop_size_constraint_preserves_size
    , fastProperty "NonEmpty constraint preserves name" prop_nonempty_constraint_preserves_name
    , fastProperty "Predicate constraint preserves name and args" prop_predicate_constraint_preserves_name_args
    , fastProperty "TypeParameter preserves all fields" prop_type_parameter_preserves_fields
    , fastProperty "StructBody preserves fields" prop_struct_body_preserves_fields
    , fastProperty "TypeDecl preserves all components" prop_type_decl_preserves_components
    , fastProperty "DependentFunction preserves all components" prop_dependent_function_preserves_components
    , fastProperty "TypeAlias preserves all components" prop_type_alias_preserves_components
    , fastProperty "Basic type references are valid" prop_basic_types_valid
    , fastProperty "Parsing simple type declaration works" prop_parse_simple_type_declaration
    , fastProperty "Parsing simple type alias works" prop_parse_simple_type_alias
    , fastProperty "Parsing function declaration works" prop_parse_function_declaration
    , fastProperty "Parsing type with constraints works" prop_parse_type_with_constraints
    , fastProperty "Parsing generic type works" prop_parse_generic_type
    , fastProperty "Validation catches syntax errors" prop_validation_catches_syntax_errors
    , fastProperty "Validation accepts valid input" prop_validation_accepts_valid_input
    , fastProperty "Parser state preserves type scope" prop_parser_state_preserves_scope
    , fastProperty "Error detection for duplicate definitions" prop_duplicate_definition_error
    , fastProperty "Type parameter constraints are preserved" prop_type_parameter_constraints_preserved
    , fastProperty "Nested generic types are parsed correctly" prop_nested_generic_types
    , fastProperty "Complex constraints are parsed correctly" prop_complex_constraints
    ]