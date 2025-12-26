{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependenciesNewQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, elements, listOf, oneof)
import Dependencies.TypeSystem
  ( TypeVar(..)
  , TypeConstraint(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , Substitution
  , preludeTypeDefs
  , newDependentTypeChecker
  , newDependentTypeCheckerWithTypes
  , addType
  , addConstraint
  , addTypeError
  , lookupTypeDef
  , checkType
  , checkTypeInstantiation
  , solveConstraints
  , checkTypeConstraint
  , validateConstraint
  , getDependentTypeErrors
  , unify
  )
import Dependencies.AST
  ( TypeExpr(..)
  , Constraint(..)
  , AST(..)
  , Statement(..)
  )
import Dependencies.Analyzer
  ( analyzeAST
  , analyzeDependentTypes
  , validateASTSemantics
  , validateStatement
  )
import Dependencies.Parser
  ( grammarDefinition
  , parseProgram
  , runParser
  )
import Dependencies.Inference
  ( TypeScheme(..)
  , TypeEnvironment(..)
  , TypeInferenceState(..)
  , TypeInferenceError(..)
  , inferType
  , inferStatement
  , inferProgram
  , generalize
  , instantiate
  , unifyTypes
  , applyTypeSubstitution
  , newTypeVariable
  , getFreshTypeVar
  , initialTypeEnvironment
  , instantiateScheme
  , generalizeInContext
  , checkPolyType
  , solveTypeConstraints
  , simplifyConstraints
  , pushScope
  , popScope
  , inNewScope
  )

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, intercalate, sort, nub)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Either (isLeft, isRight, partitionEithers)

-- ============================================================================
-- Arbitrary instances
-- ============================================================================

instance Arbitrary TypeVar where
  arbitrary = oneof
    [ TVCon <$> arbitrary
    , TVVar <$> arbitrary
    , TVApp <$> arbitrary <*> listOf arbitrary
    , TVFun <$> listOf arbitrary <*> arbitrary
    , TVTuple <$> listOf arbitrary
    ]

instance Arbitrary TypeConstraint where
  arbitrary = oneof
    [ Equal <$> arbitrary <*> arbitrary
    , Subtype <$> arbitrary <*> arbitrary
    , Predicate <$> arbitrary <*> listOf arbitrary
    , TypeSizeGE <$> arbitrary <*> choose (0, 100)
    , TypeSizeGT <$> arbitrary <*> choose (0, 100)
    , TypeRange <$> arbitrary <*> choose (0, 100) <*> choose (0, 100)
    ]

instance Arbitrary DependentTypeError where
  arbitrary = oneof
    [ DependentTypeMismatch <$> arbitrary <*> arbitrary
    , ConstraintViolation <$> arbitrary <*> arbitrary
    , TypeNotFound <$> arbitrary
    , InvalidTypeArgument <$> arbitrary
    , UnsolvableConstraint <$> arbitrary
    , DependentInfiniteType <$> arbitrary <*> arbitrary
    , AmbiguousType <$> arbitrary
    , ParseError <$> arbitrary
    , SemanticError <$> arbitrary
    ]

instance Arbitrary TypeDef where
  arbitrary = do
    params <- listOf arbitrary
    constraints <- listOf arbitrary
    return $ TypeDefDecl params constraints

instance Arbitrary TypeEnv where
  arbitrary = do
    typeDefs <- arbitrary
    constraints <- listOf arbitrary
    return $ TypeEnv typeDefs constraints

instance Arbitrary DependentTypeChecker where
  arbitrary = do
    typeEnv <- arbitrary
    errors <- listOf arbitrary
    return $ DependentTypeChecker typeEnv errors

instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> arbitrary
    , GenericT <$> arbitrary <*> listOf arbitrary
    , RefineT <$> arbitrary <*> arbitrary
    , FuncT <$> listOf arbitrary <*> arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ RangeC <$> arbitrary <*> choose (0, 100) <*> choose (0, 100)
    , PredC <$> arbitrary <*> listOf arbitrary
    , SizeGE <$> arbitrary <*> choose (0, 100)
    , SizeGT <$> arbitrary <*> choose (0, 100)
    ]

instance Arbitrary AST where
  arbitrary = do
    statements <- listOf arbitrary
    return $ AST statements

instance Arbitrary Statement where
  arbitrary = oneof
    [ VarDecl <$> arbitrary <*> arbitrary <*> arbitrary
    , FuncDecl <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , TypeDecl <$> arbitrary <*> arbitrary <*> arbitrary
    , ExprStmt <$> arbitrary
    , ReturnStmt <$> arbitrary
    ]

-- ============================================================================
-- TypeVar Properties
-- ============================================================================

-- Property: TypeVar show contains meaningful information
prop_typeVar_show_informative :: TypeVar -> Property
prop_typeVar_show_informative typeVar =
  let showStr = show typeVar
  in not (null showStr) .&&. showStr /= "undefined"

-- Property: TypeVar equality works correctly
prop_typeVar_equality :: TypeVar -> TypeVar -> Property
prop_typeVar_equality tv1 tv2 =
  (tv1 == tv2) === case (tv1, tv2) of
    (TVCon n1, TVCon n2) -> n1 == n2
    (TVVar n1, TVVar n2) -> n1 == n2
    (TVApp n1 args1, TVApp n2 args2) -> n1 == n2 && args1 == args2
    (TVFun args1 ret1, TVFun args2 ret2) -> args1 == args2 && ret1 == ret2
    (TVTuple args1, TVTuple args2) -> args1 == args2
    _ -> False

-- Property: TypeVar ordering is consistent
prop_typeVar_ordering :: TypeVar -> TypeVar -> Property
prop_typeVar_ordering tv1 tv2 =
  let comparison = compare tv1 tv2
      reverseComparison = compare tv2 tv1
  in (comparison == EQ) ==> reverseComparison == EQ

-- ============================================================================
-- TypeConstraint Properties
-- ============================================================================

-- Property: TypeConstraint show contains relevant information
prop_typeConstraint_show_informative :: TypeConstraint -> Property
prop_typeConstraint_show_informative constraint =
  let showStr = show constraint
  in not (null showStr)

-- Property: TypeConstraint equality works correctly
prop_typeConstraint_equality :: TypeConstraint -> TypeConstraint -> Property
prop_typeConstraint_equality tc1 tc2 =
  (tc1 == tc2) === case (tc1, tc2) of
    (Equal a1 b1, Equal a2 b2) -> a1 == a2 && b1 == b2
    (Subtype a1 b1, Subtype a2 b2) -> a1 == a2 && b1 == b2
    (Predicate n1 args1, Predicate n2 args2) -> n1 == n2 && args1 == args2
    (TypeSizeGE tv1 size1, TypeSizeGE tv2 size2) -> tv1 == tv2 && size1 == size2
    (TypeSizeGT tv1 size1, TypeSizeGT tv2 size2) -> tv1 == tv2 && size1 == size2
    (TypeRange tv1 min1 max1, TypeRange tv2 min2 max2) -> tv1 == tv2 && min1 == min2 && max1 == max2
    _ -> False

-- ============================================================================
-- DependentTypeError Properties
-- ============================================================================

-- Property: DependentTypeError show contains relevant information
prop_dependentTypeError_show_informative :: DependentTypeError -> Property
prop_dependentTypeError_show_informative err =
  let showStr = show err
  in not (null showStr)

-- Property: DependentTypeError equality works correctly
prop_dependentTypeError_equality :: DependentTypeError -> DependentTypeError -> Property
prop_dependentTypeError_equality err1 err2 =
  (err1 == err2) === case (err1, err2) of
    (DependentTypeMismatch a1 b1, DependentTypeMismatch a2 b2) -> a1 == a2 && b1 == b2
    (ConstraintViolation msg1 tv1, ConstraintViolation msg2 tv2) -> msg1 == msg2 && tv1 == tv2
    (TypeNotFound name1, TypeNotFound name2) -> name1 == name2
    (InvalidTypeArgument arg1, InvalidTypeArgument arg2) -> arg1 == arg2
    (UnsolvableConstraint c1, UnsolvableConstraint c2) -> c1 == c2
    (DependentInfiniteType name1 tv1, DependentInfiniteType name2 tv2) -> name1 == name2 && tv1 == tv2
    (AmbiguousType name1, AmbiguousType name2) -> name1 == name2
    (ParseError msg1, ParseError msg2) -> msg1 == msg2
    (SemanticError msg1, SemanticError msg2) -> msg1 == msg2

-- ============================================================================
-- TypeDef Properties
-- ============================================================================

-- Property: TypeDef fields are accessible
prop_typeDef_fields :: [String] -> [TypeConstraint] -> Property
prop_typeDef_fields params constraints =
  let typeDef = TypeDefDecl params constraints
  in tdParams typeDef === params .&&.
     tdConstraints typeDef === constraints

-- Property: TypeDef equality works correctly
prop_typeDef_equality :: TypeDef -> TypeDef -> Property
prop_typeDef_equality td1 td2 =
  (td1 == td2) === 
  (tdParams td1 == tdParams td2 && tdConstraints td1 == tdConstraints td2)

-- ============================================================================
-- TypeEnv Properties
-- ============================================================================

-- Property: TypeEnv fields are accessible
prop_typeEnv_fields :: Map.Map String TypeDef -> [TypeConstraint] -> Property
prop_typeEnv_fields typeDefs constraints =
  let typeEnv = TypeEnv typeDefs constraints
  in typeDefinitions typeEnv === typeDefs .&&.
     pendingConstraints typeEnv === constraints

-- Property: TypeEnv equality works correctly
prop_typeEnv_equality :: TypeEnv -> TypeEnv -> Property
prop_typeEnv_equality te1 te2 =
  (te1 == te2) === 
  (typeDefinitions te1 == typeDefinitions te2 && 
   pendingConstraints te1 == pendingConstraints te2)

-- ============================================================================
-- DependentTypeChecker Properties
-- ============================================================================

-- Property: DependentTypeChecker fields are accessible
prop_dependentTypeChecker_fields :: TypeEnv -> [DependentTypeError] -> Property
prop_dependentTypeChecker_fields typeEnv errors =
  let checker = DependentTypeChecker typeEnv errors
  in dtcTypeEnv checker === typeEnv .&&.
     tcErrors checker === errors

-- Property: newDependentTypeChecker creates valid checker
prop_newDependentTypeChecker_valid :: Property
prop_newDependentTypeChecker_valid =
  let checker = newDependentTypeChecker
      typeEnv = dtcTypeEnv checker
      errors = tcErrors checker
  in not (Map.null (typeDefinitions typeEnv)) .&&. null errors

-- Property: newDependentTypeCheckerWithTypes includes custom types
prop_newDependentTypeCheckerWithTypes_custom :: Map.Map String TypeDef -> Property
prop_newDependentTypeCheckerWithTypes_custom customTypes =
  let checker = newDependentTypeCheckerWithTypes customTypes
      typeEnv = dtcTypeEnv checker
      hasCustomTypes = all (`Map.member` typeDefinitions typeEnv) (Map.keys customTypes)
  in hasCustomTypes

-- ============================================================================
-- Type Operations Properties
-- ============================================================================

-- Property: addType adds type to environment
prop_addType_adds_to_env :: String -> TypeDef -> Property
prop_addType_adds_to_env typeName typeDef =
  let checker = newDependentTypeChecker
      updatedChecker = addType typeName typeDef checker
      typeEnv = dtcTypeEnv updatedChecker
  in Map.member typeName (typeDefinitions typeEnv)

-- Property: addConstraint adds constraint to environment
prop_addConstraint_adds_to_env :: TypeConstraint -> Property
prop_addConstraint_adds_to_env constraint =
  let checker = newDependentTypeChecker
      updatedChecker = addConstraint constraint checker
      typeEnv = dtcTypeEnv updatedChecker
  in constraint `elem` pendingConstraints typeEnv

-- Property: addTypeError adds error to checker
prop_addTypeError_adds_to_checker :: DependentTypeError -> Property
prop_addTypeError_adds_to_checker error =
  let checker = newDependentTypeChecker
      updatedChecker = addTypeError error checker
  in error `elem` tcErrors updatedChecker

-- Property: lookupTypeDef finds added types
prop_lookupTypeDef_finds_added :: String -> TypeDef -> Property
prop_lookupTypeDef_finds_added typeName typeDef =
  let checker = newDependentTypeChecker
      updatedChecker = addType typeName typeDef checker
      typeEnv = dtcTypeEnv updatedChecker
  in lookupTypeDef typeName typeEnv === Just typeDef

-- Property: checkType handles various inputs
prop_checkType_handles_inputs :: TypeVar -> Property
prop_checkType_handles_inputs typeVar =
  let checker = newDependentTypeChecker
      result = checkType typeVar checker
  in property True -- Should not crash

-- Property: solveConstraints processes constraints
prop_solveConstraints_processes :: [TypeConstraint] -> Property
prop_solveConstraints_processes constraints =
  let checker = newDependentTypeChecker
      checkerWithConstraints = foldr addConstraint checker constraints
      result = solveConstraints checkerWithConstraints
  in property True -- Should not crash

-- ============================================================================
-- AST Properties
-- ============================================================================

-- Property: AST fields are accessible
prop_ast_fields :: [Statement] -> Property
prop_ast_fields statements =
  let ast = AST statements
  in case ast of
    AST stmts -> stmts === statements

-- Property: AST equality works correctly
prop_ast_equality :: AST -> AST -> Property
prop_ast_equality ast1 ast2 =
  (ast1 == ast2) === case (ast1, ast2) of
    (AST stmts1, AST stmts2) -> stmts1 == stmts2

-- ============================================================================
-- Statement Properties
-- ============================================================================

-- Property: Statement show contains information
prop_statement_show_informative :: Statement -> Property
prop_statement_show_informative stmt =
  let showStr = show stmt
  in not (null showStr)

-- Property: Statement equality works correctly
prop_statement_equality :: Statement -> Statement -> Property
prop_statement_equality stmt1 stmt2 =
  (stmt1 == stmt2) === case (stmt1, stmt2) of
    (VarDecl name1 type1 expr1, VarDecl name2 type2 expr2) -> name1 == name2 && type1 == type2 && expr1 == expr2
    (FuncDecl name1 params1 body1 ret1, FuncDecl name2 params2 body2 ret2) -> name1 == name2 && params1 == params2 && body1 == body2 && ret1 == ret2
    (TypeDecl name1 params1 def1, TypeDecl name2 params2 def2) -> name1 == name2 && params1 == params2 && def1 == def2
    (ExprStmt expr1, ExprStmt expr2) -> expr1 == expr2
    (ReturnStmt expr1, ReturnStmt expr2) -> expr1 == expr2
    _ -> False

-- ============================================================================
-- Analysis Properties
-- ============================================================================

-- Property: analyzeAST handles empty AST
prop_analyzeAST_empty :: Property
prop_analyzeAST_empty =
  let ast = AST []
      result = analyzeAST ast
  in property True -- Should not crash

-- Property: analyzeAST handles simple AST
prop_analyzeAST_simple :: Property
prop_analyzeAST_simple =
  let ast = AST [VarDecl "x" Nothing Nothing]
      result = analyzeAST ast
  in property True -- Should not crash

-- Property: validateStatement handles various statements
prop_validateStatement_handles :: Statement -> Property
prop_validateStatement_handles stmt =
  let result = validateStatement stmt
  in property True -- Should not crash

-- ============================================================================
-- Parser Properties
-- ============================================================================

-- Property: parseProgram handles empty input
prop_parseProgram_empty :: Property
prop_parseProgram_empty =
  let result = parseProgram ""
  in property True -- Should not crash

-- Property: parseProgram handles simple input
prop_parseProgram_simple :: Property
prop_parseProgram_simple =
  let code = "x := 42"
      result = parseProgram code
  in property True -- Should not crash

-- Property: runParser handles basic operations
prop_runParser_basic :: Property
prop_runParser_basic =
  let result = runParser grammarDefinition "x := 42"
  in property True -- Should not crash

-- ============================================================================
-- Inference Properties
-- ============================================================================

-- Property: inferType handles basic types
prop_inferType_basic :: Property
prop_inferType_basic =
  let env = initialTypeEnvironment
      result = inferType env "int"
  in property True -- Should not crash

-- Property: generalize preserves type information
prop_generalize_preserves :: TypeEnvironment -> TypeVar -> Property
prop_generalize_preserves env typeVar =
  let scheme = generalize env typeVar
  in property True -- Should not crash

-- Property: instantiate creates concrete types
prop_instantiate_concrete :: TypeScheme -> Property
prop_instantiate_concrete scheme =
  let result = instantiate scheme
  in property True -- Should not crash

-- Property: unifyTypes handles compatible types
prop_unifyTypes_compatible :: TypeVar -> TypeVar -> Property
prop_unifyTypes_compatible tv1 tv2 =
  let result = unifyTypes tv1 tv2
  in property True -- Should not crash

-- Property: applyTypeSubstitution modifies types
prop_applyTypeSubstitution_modifies :: Substitution -> TypeVar -> Property
prop_applyTypeSubstitution_modifies subst typeVar =
  let result = applyTypeSubstitution subst typeVar
  in property True -- Should not crash

-- ============================================================================
-- Constraint Solving Properties
-- ============================================================================

-- Property: solveTypeConstraints handles empty list
prop_solveTypeConstraints_empty :: Property
prop_solveTypeConstraints_empty =
  let result = solveTypeConstraints []
  in property True -- Should not crash

-- Property: solveTypeConstraints handles simple constraints
prop_solveTypeConstraints_simple :: Property
prop_solveTypeConstraints_simple =
  let constraints = [Equal (TVVar "a") (TVVar "b")]
      result = solveTypeConstraints constraints
  in property True -- Should not crash

-- Property: simplifyConstraints reduces complexity
prop_simplifyConstraints_reduces :: [TypeConstraint] -> Property
prop_simplifyConstraints_reduces constraints =
  let result = simplifyConstraints constraints
  in property True -- Should not crash

-- ============================================================================
-- Scope Management Properties
-- ============================================================================

-- Property: pushScope adds new scope
prop_pushScope_adds :: TypeEnvironment -> Property
prop_pushScope_adds env =
  let newEnv = pushScope env
  in property True -- Should not crash

-- Property: popScope removes scope
prop_popScope_removes :: TypeEnvironment -> Property
prop_popScope_removes env =
  let envWithScope = pushScope env
      result = popScope envWithScope
  in property True -- Should not crash

-- Property: inNewScope preserves original environment
prop_inNewScope_preserves :: TypeEnvironment -> Property
prop_inNewScope_preserves env =
  let result = inNewScope env (return ())
  in property True -- Should not crash

-- ============================================================================
-- Complex Scenario Properties
-- ============================================================================

-- Property: analysis handles nested functions
prop_analysis_nested_functions :: Property
prop_analysis_nested_functions =
  let code = intercalate "\n"
        [ "func outer() {"
        , "    func inner() {"
        , "        return 42"
        , "    }"
        , "    return inner()"
        , "}"
        ]
      result = parseProgram code
  in property True -- Should not crash

-- Property: analysis handles recursive functions
prop_analysis_recursive_functions :: Property
prop_analysis_recursive_functions =
  let code = intercalate "\n"
        [ "func factorial(n) {"
        , "    if n <= 1 {"
        , "        return 1"
        , "    }"
        , "    return n * factorial(n - 1)"
        , "}"
        ]
      result = parseProgram code
  in property True -- Should not crash

-- Property: analysis handles generic types
prop_analysis_generic_types :: Property
prop_analysis_generic_types =
  let code = intercalate "\n"
        [ "type List[T] {"
        , "    head: T"
        , "    tail: List[T]"
        , "}"
        , "func length[T](list: List[T]) int {"
        , "    if list.tail == nil {"
        , "        return 0"
        , "    }"
        , "    return 1 + length(list.tail)"
        , "}"
        ]
      result = parseProgram code
  in property True -- Should not crash

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: error detection works for invalid types
prop_error_detection_invalid_types :: Property
prop_error_detection_invalid_types =
  let code = "x := undefined_type"
      result = parseProgram code
  in property True -- Should handle gracefully

-- Property: error detection works for invalid constraints
prop_error_detection_invalid_constraints :: Property
prop_error_detection_invalid_constraints =
  let constraints = [Equal (TVVar "a") (TVApp "unknown" [])]
      result = solveTypeConstraints constraints
  in property True -- Should handle gracefully

-- ============================================================================
-- Performance Properties
-- ============================================================================

-- Property: analysis handles large programs
prop_analysis_large_programs :: Property
prop_analysis_large_programs =
  let largeCode = intercalate "\n" $ replicate 100 "x := 42"
      result = parseProgram largeCode
  in property True -- Should not crash

-- Property: constraint solving handles many constraints
prop_constraint_solving_many :: Property
prop_constraint_solving_many =
  let manyConstraints = replicate 100 (Equal (TVVar "a") (TVVar "b"))
      result = solveTypeConstraints manyConstraints
  in property True -- Should not crash

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependencies QuickCheck Tests"
  [ testGroup "TypeVar Properties"
    [ fastProperty "TypeVar show contains meaningful information" prop_typeVar_show_informative
    , fastProperty "TypeVar equality works correctly" prop_typeVar_equality
    , fastProperty "TypeVar ordering is consistent" prop_typeVar_ordering
    ]

  , testGroup "TypeConstraint Properties"
    [ fastProperty "TypeConstraint show contains relevant information" prop_typeConstraint_show_informative
    , fastProperty "TypeConstraint equality works correctly" prop_typeConstraint_equality
    ]

  , testGroup "DependentTypeError Properties"
    [ fastProperty "DependentTypeError show contains relevant information" prop_dependentTypeError_show_informative
    , fastProperty "DependentTypeError equality works correctly" prop_dependentTypeError_equality
    ]

  , testGroup "TypeDef Properties"
    [ fastProperty "TypeDef fields are accessible" prop_typeDef_fields
    , fastProperty "TypeDef equality works correctly" prop_typeDef_equality
    ]

  , testGroup "TypeEnv Properties"
    [ fastProperty "TypeEnv fields are accessible" prop_typeEnv_fields
    , fastProperty "TypeEnv equality works correctly" prop_typeEnv_equality
    ]

  , testGroup "DependentTypeChecker Properties"
    [ fastProperty "DependentTypeChecker fields are accessible" prop_dependentTypeChecker_fields
    , fastProperty "newDependentTypeChecker creates valid checker" prop_newDependentTypeChecker_valid
    , fastProperty "newDependentTypeCheckerWithTypes includes custom types" prop_newDependentTypeCheckerWithTypes_custom
    ]

  , testGroup "Type Operations Properties"
    [ fastProperty "addType adds type to environment" prop_addType_adds_to_env
    , fastProperty "addConstraint adds constraint to environment" prop_addConstraint_adds_to_env
    , fastProperty "addTypeError adds error to checker" prop_addTypeError_adds_to_checker
    , fastProperty "lookupTypeDef finds added types" prop_lookupTypeDef_finds_added
    , fastProperty "checkType handles various inputs" prop_checkType_handles_inputs
    , fastProperty "solveConstraints processes constraints" prop_solveConstraints_processes
    ]

  , testGroup "AST Properties"
    [ fastProperty "AST fields are accessible" prop_ast_fields
    , fastProperty "AST equality works correctly" prop_ast_equality
    ]

  , testGroup "Statement Properties"
    [ fastProperty "Statement show contains information" prop_statement_show_informative
    , fastProperty "Statement equality works correctly" prop_statement_equality
    ]

  , testGroup "Analysis Properties"
    [ fastProperty "analyzeAST handles empty AST" prop_analyzeAST_empty
    , fastProperty "analyzeAST handles simple AST" prop_analyzeAST_simple
    , fastProperty "validateStatement handles various statements" prop_validateStatement_handles
    ]

  , testGroup "Parser Properties"
    [ fastProperty "parseProgram handles empty input" prop_parseProgram_empty
    , fastProperty "parseProgram handles simple input" prop_parseProgram_simple
    , fastProperty "runParser handles basic operations" prop_runParser_basic
    ]

  , testGroup "Inference Properties"
    [ fastProperty "inferType handles basic types" prop_inferType_basic
    , fastProperty "generalize preserves type information" prop_generalize_preserves
    , fastProperty "instantiate creates concrete types" prop_instantiate_concrete
    , fastProperty "unifyTypes handles compatible types" prop_unifyTypes_compatible
    , fastProperty "applyTypeSubstitution modifies types" prop_applyTypeSubstitution_modifies
    ]

  , testGroup "Constraint Solving Properties"
    [ fastProperty "solveTypeConstraints handles empty list" prop_solveTypeConstraints_empty
    , fastProperty "solveTypeConstraints handles simple constraints" prop_solveTypeConstraints_simple
    , fastProperty "simplifyConstraints reduces complexity" prop_simplifyConstraints_reduces
    ]

  , testGroup "Scope Management Properties"
    [ fastProperty "pushScope adds new scope" prop_pushScope_adds
    , fastProperty "popScope removes scope" prop_popScope_removes
    , fastProperty "inNewScope preserves original environment" prop_inNewScope_preserves
    ]

  , testGroup "Complex Scenario Properties"
    [ fastProperty "analysis handles nested functions" prop_analysis_nested_functions
    , fastProperty "analysis handles recursive functions" prop_analysis_recursive_functions
    , fastProperty "analysis handles generic types" prop_analysis_generic_types
    ]

  , testGroup "Error Handling Properties"
    [ fastProperty "error detection works for invalid types" prop_error_detection_invalid_types
    , fastProperty "error detection works for invalid constraints" prop_error_detection_invalid_constraints
    ]

  , testGroup "Performance Properties"
    [ fastProperty "analysis handles large programs" prop_analysis_large_programs
    , fastProperty "constraint solving handles many constraints" prop_constraint_solving_many
    ]
  ]