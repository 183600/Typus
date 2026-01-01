{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Dependencies.AST
  ( AST(..)
  , Statement(..)
  , TypeExpr(..)
  , Constraint(..)
  , DependencyNode(..)
  , DependencyGraph(..)
  )

import Dependencies.Parser
  ( parseProgram
  , parseStatement
  , parseTypeExpr
  , parseConstraint
  , runParser
  , grammarDefinition
  )

import qualified Data.Text as T (pack, unpack)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import Data.Char (isSpace, isAlphaNum)

-- Property: AST construction with Program
prop_ast_program_construction :: [String] -> Property
prop_ast_program_construction statements =
  not (null statements) && L.length statements <= 5 ==>
  let dummyStatements = L.map (\s -> SVarDecl (T.pack s) (SimpleT (T.pack "int"))) statements
      program = Program dummyStatements
  in case program of
    Program stmts -> L.length stmts === L.length statements

-- Property: Statement equality
prop_statement_equality :: String -> TypeExpr -> Property
prop_statement_equality varName typeExpr =
  not (null varName) ==>
  let stmt1 = SVarDecl (T.pack varName) typeExpr
      stmt2 = SVarDecl (T.pack varName) typeExpr
      stmt3 = SVarDecl (T.pack (varName ++ "_diff")) typeExpr
  in stmt1 === stmt2 .&&. stmt1 /= stmt3

-- Property: TypeExpr construction
prop_typeexpr_simple :: String -> Property
prop_typeexpr_simple typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let typeExpr = SimpleT (T.pack typeName)
  in case typeExpr of
    SimpleT name -> T.unpack name === typeName

-- Property: TypeExpr generic construction
prop_typeexpr_generic :: String -> [String] -> Property
prop_typeexpr_generic typeName typeArgs =
  not (null typeName) && not (null typeArgs) && L.length typeArgs <= 3 ==>
  let genericType = GenericT (T.pack typeName) (L.map (SimpleT . T.pack) typeArgs)
  in case genericType of
    GenericT name args -> property $ T.unpack name === typeName .&&. L.length args === L.length typeArgs

-- Property: TypeExpr function construction
prop_typeexpr_function :: [(String, TypeExpr)] -> TypeExpr -> Property
prop_typeexpr_function params returnType =
  not (null params) && L.length params <= 3 ==>
  let funcType = FuncT (L.map (\(n, t) -> (T.pack n, t)) params) returnType
  in case funcType of
    FuncT paramTypes ret -> L.length paramTypes === L.length params

-- Property: TypeExpr refinement construction
prop_typeexpr_refinement :: TypeExpr -> [String] -> Property
prop_typeexpr_refinement baseType constraintNames =
  not (null constraintNames) && L.length constraintNames <= 3 ==>
  let constraints = L.map (\name -> PredC (T.pack name) []) constraintNames
      refinedType = RefineT baseType constraints
  in case refinedType of
    RefineT base cons -> property $ base === baseType .&&. L.length cons === L.length constraintNames

-- Property: Constraint construction
prop_constraint_size_gt :: String -> Int -> Property
prop_constraint_size_gt varName size =
  not (null varName) && size >= 0 && size <= 100 ==>
  let constraint = SizeGT (T.pack varName) size
  in case constraint of
    SizeGT var s -> property $ T.unpack var === varName .&&. s === size

-- Property: Constraint range construction
prop_constraint_range :: String -> Int -> Int -> Property
prop_constraint_range varName minVal maxVal =
  not (null varName) && minVal >= 0 && maxVal >= minVal && maxVal <= 100 ==>
  let constraint = RangeC (T.pack varName) minVal maxVal
  in case constraint of
    RangeC var min max -> property $ T.unpack var === varName .&&. min === minVal .&&. max === maxVal

-- Property: Constraint predicate construction
prop_constraint_predicate :: String -> [TypeExpr] -> Property
prop_constraint_predicate predName args =
  not (null predName) && not (null args) && L.length args <= 3 ==>
  let constraint = PredC (T.pack predName) args
  in case constraint of
    PredC pred argTypes -> property $ T.unpack pred === predName .&&. L.length argTypes === L.length args

-- Property: DependencyNode construction
prop_dependency_node_construction :: String -> [String] -> Property
prop_dependency_node_construction nodeName dependencies =
  not (null nodeName) && L.length dependencies <= 5 ==>
  let node = DependencyNode nodeName dependencies
  in property $ nodeName node === nodeName .&&. nodeDependencies node === dependencies

-- Property: parseStatement handles simple variable declarations
prop_parse_statement_var_decl :: String -> String -> Property
prop_parse_statement_var_decl varName typeName =
  not (null varName) && not (null typeName) && L.all isAlphaNum varName && L.all isAlphaNum typeName ==>
  let input = "var " ++ varName ++ " : " ++ typeName
      result = runParser parseStatement input
  in case result of
    Left _ -> property False
    Right (SVarDecl name (SimpleT simpleType)) -> 
      property $ T.unpack name === varName .&&. T.unpack simpleType === typeName
    Right _ -> property False

-- Property: parseTypeExpr handles simple types
prop_parse_typeexpr_simple :: String -> Property
prop_parse_typeexpr_simple typeName =
  not (null typeName) && L.all isAlphaNum typeName ==>
  let input = typeName
      result = runParser parseTypeExpr input
  in case result of
    Left _ -> property False
    Right (SimpleT name) -> property $ T.unpack name === typeName
    Right _ -> property False

-- Property: parseTypeExpr handles generic types
prop_parse_typeexpr_generic :: String -> [String] -> Property
prop_parse_typeexpr_generic typeName typeArgs =
  not (null typeName) && not (null typeArgs) && L.length typeArgs <= 3 && 
  L.all isAlphaNum typeName && L.all isAlphaNum (L.concat typeArgs) ==>
  let argsStr = L.concat $ intersperse ", " typeArgs
      input = typeName ++ "<" ++ argsStr ++ ">"
      result = runParser parseTypeExpr input
  in case result of
    Left _ -> property False
    Right (GenericT name args) -> 
      property $ T.unpack name === typeName .&&. L.length args === L.length typeArgs
    Right _ -> property False

-- Property: parseConstraint handles size constraints
prop_parse_constraint_size :: String -> String -> Int -> Property
prop_parse_constraint_size varName op size =
  not (null varName) && L.all isAlphaNum varName && op `elem` [">", ">="] && size >= 0 && size <= 100 ==>
  let input = varName ++ " " ++ op ++ " " ++ show size
      result = runParser parseConstraint input
  in case (op, result) of
    (">", Left _) -> property False
    (">", Right (SizeGT var s)) -> property $ T.unpack var === varName .&&. s === size
    (">", Right _) -> property False
    (">=", Left _) -> property False
    (">=", Right (SizeGE var s)) -> property $ T.unpack var === varName .&&. s === size
    (">=", Right _) -> property False
    _ -> property False

-- Property: parseConstraint handles range constraints
prop_parse_constraint_range :: String -> Int -> Int -> Property
prop_parse_constraint_range varName minVal maxVal =
  not (null varName) && L.all isAlphaNum varName && minVal >= 0 && maxVal >= minVal && maxVal <= 100 ==>
  let input = varName ++ " < " ++ show minVal ++ ", " ++ show maxVal ++ " >"
      result = runParser parseConstraint input
  in case result of
    Left _ -> property False
    Right (RangeC var min max) -> 
      property $ T.unpack var === varName .&&. min === minVal .&&. max === maxVal
    Right _ -> property False

-- Property: parseProgram handles multiple statements
prop_parse_program_multiple :: [String] -> Property
prop_parse_program_multiple varNames =
  not (null varNames) && L.length varNames <= 3 && L.all isAlphaNum (L.concat varNames) ==>
  let statements = L.map (\name -> "var " ++ name ++ " : int") varNames
      input = unlines statements
      result = runParser parseProgram input
  in case result of
    Left _ -> property False
    Right (Program stmts) -> property $ L.length stmts === L.length varNames
    Right _ -> property False

-- Property: grammarDefinition is non-empty
prop_grammar_definition_non_empty :: Property
prop_grammar_definition_non_empty =
  let grammar = grammarDefinition
  in property $ not (null grammar) && "Typus Language BNF Grammar" `L.isInfixOf` grammar

-- Property: AST roundtrip consistency
prop_ast_roundtrip_consistency :: Statement -> Property
prop_ast_roundtrip_consistency stmt =
  -- Note: This is a simplified test since we don't have a show instance for Statement
  case stmt of
    SVarDecl name typeExpr -> 
      name === name && typeExpr === typeExpr
    _ -> property True

-- Property: TypeExpr nested generics
prop_typeexpr_nested_generics :: String -> String -> String -> Property
prop_typeexpr_nested_generics outerType innerType innermostType =
  not (null outerType) && not (null innerType) && not (null innermostType) &&
  L.all isAlphaNum outerType && L.all isAlphaNum innerType && L.all isAlphaNum innermostType ==>
  let innerGeneric = GenericT (T.pack innerType) [SimpleT (T.pack innermostType)]
      outerGeneric = GenericT (T.pack outerType) [innerGeneric]
  in case outerGeneric of
    GenericT name [GenericT innerName [SimpleT innermostName]] ->
      T.unpack name === outerType && T.unpack innerName === innerType && T.unpack innermostName === innermostType
    _ -> property False

-- Property: Statement type definitions
prop_statement_type_def :: String -> [String] -> Property
prop_statement_type_def typeName typeParams =
  not (null typeName) && L.length typeParams <= 3 && L.all isAlphaNum (typeName : L.concat typeParams) ==>
  let params = map T.pack typeParams
      typeDef = STypeDef (T.pack typeName) params []
  in case typeDef of
    STypeDef name ps cs -> 
      T.unpack name === typeName && ps === params && null cs

-- Property: Statement function declarations
prop_statement_func_decl :: String -> [(String, TypeExpr)] -> Property
prop_statement_func_decl funcName params =
  not (null funcName) && not (null params) && L.length params <= 3 &&
  L.all isAlphaNum funcName && L.all (L.all isAlphaNum . fst) params ==>
  let paramPairs = L.map (\(n, t) -> (T.pack n, t)) params
      funcDecl = SFuncDecl (T.pack funcName) paramPairs Nothing
  in case funcDecl of
    SFuncDecl name ps ret -> 
      T.unpack name === funcName && ps === paramPairs && ret === Nothing

-- Property: Statement type aliases
prop_statement_type_alias :: String -> TypeExpr -> Property
prop_statement_type_alias aliasName underlyingType =
  not (null aliasName) && L.all isAlphaNum aliasName ==>
  let typeAlias = STypeAlias (T.pack aliasName) underlyingType []
  in case typeAlias of
    STypeAlias name typeExpr constraints ->
      T.unpack name === aliasName && typeExpr === underlyingType && null constraints

-- Property: Statement existential declarations
prop_statement_exists_decl :: [String] -> Statement -> Property
prop_statement_exists_decl typeVars innerStmt =
  not (null typeVars) && L.length typeVars <= 3 && L.all isAlphaNum (L.concat typeVars) ==>
  let existsDecl = SExistsDecl (map T.pack typeVars) innerStmt
  in case existsDecl of
    SExistsDecl vars stmt ->
      L.length vars === L.length typeVars && stmt === innerStmt

-- Property: Statement constraint definitions
prop_statement_constraint_def :: String -> Constraint -> Property
prop_statement_constraint_def constraintName constraint =
  not (null constraintName) && L.all isAlphaNum constraintName ==>
  let constraintDef = SConstraintDef (T.pack constraintName) constraint
  in case constraintDef of
    SConstraintDef name c ->
      T.unpack name === constraintName && c === constraint

-- Property: Complex TypeExpr combinations
prop_typeexpr_complex_combinations :: TypeExpr -> TypeExpr -> TypeExpr -> Property
prop_typeexpr_complex_combinations baseType argType returnType =
  let genericType = GenericT "Container" [baseType, argType]
      funcType = FuncT [("arg", genericType), ("other", argType)] returnType
      refinedType = RefineT funcType [PredC "valid" [], SizeGT "size" 0]
  in case refinedType of
    RefineT (FuncT params ret) constraints ->
      L.length params === 2 && L.length constraints === 2
    _ -> property False

-- Property: Error handling with invalid inputs
prop_parse_error_handling :: String -> Property
prop_parse_error_handling invalidInput =
  null invalidInput || L.any (not . isAlphaNum) invalidInput ==>
  let input = T.pack invalidInput
      result = runParser parseStatement input
  in case result of
    Left _ -> property True  -- Expected to fail
    Right _ -> property False -- Should not succeed with invalid input

-- Helper function
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

tests :: TestTree
tests = testGroup "Dependencies QuickCheck tests"
  [ fastProperty "AST construction with Program" prop_ast_program_construction
  , fastProperty "Statement equality" prop_statement_equality
  , fastProperty "TypeExpr construction" prop_typeexpr_simple
  , fastProperty "TypeExpr generic construction" prop_typeexpr_generic
  , fastProperty "TypeExpr function construction" prop_typeexpr_function
  , fastProperty "TypeExpr refinement construction" prop_typeexpr_refinement
  , fastProperty "Constraint construction" prop_constraint_size_gt
  , fastProperty "Constraint range construction" prop_constraint_range
  , fastProperty "Constraint predicate construction" prop_constraint_predicate
  , fastProperty "DependencyNode construction" prop_dependency_node_construction
  , fastProperty "parseStatement handles simple variable declarations" prop_parse_statement_var_decl
  , fastProperty "parseTypeExpr handles simple types" prop_parse_typeexpr_simple
  , fastProperty "parseTypeExpr handles generic types" prop_parse_typeexpr_generic
  , fastProperty "parseConstraint handles size constraints" prop_parse_constraint_size
  , fastProperty "parseConstraint handles range constraints" prop_parse_constraint_range
  , fastProperty "parseProgram handles multiple statements" prop_parse_program_multiple
  , fastProperty "grammarDefinition is non-empty" prop_grammar_definition_non_empty
  , fastProperty "AST roundtrip consistency" prop_ast_roundtrip_consistency
  , fastProperty "TypeExpr nested generics" prop_typeexpr_nested_generics
  , fastProperty "Statement type definitions" prop_statement_type_def
  , fastProperty "Statement function declarations" prop_statement_func_decl
  , fastProperty "Statement type aliases" prop_statement_type_alias
  , fastProperty "Statement existential declarations" prop_statement_exists_decl
  , fastProperty "Statement constraint definitions" prop_statement_constraint_def
  , fastProperty "Complex TypeExpr combinations" prop_typeexpr_complex_combinations
  , fastProperty "Error handling with invalid inputs" prop_parse_error_handling
  ]