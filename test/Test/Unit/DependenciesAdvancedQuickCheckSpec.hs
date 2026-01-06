module Test.Unit.DependenciesAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck ((===), property, testProperty, Property, forAll, Gen, choose, arbitrary, listOf, elements, oneof, suchThat)
import TestSupport.QuickCheck (fastProperty)

import Dependencies.TypeSystem (TypeVar(..), TypeConstraint(..), DependentTypeError(..), 
                               TypeDef(..), TypeEnv(..), DependentTypeChecker(..),
                               newDependentTypeChecker, newDependentTypeCheckerWithTypes,
                               addType, addConstraint, lookupTypeDef, checkType,
                               preludeTypeDefs)
import Dependencies.AST (TypeExpr(..), Constraint(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State (runState, evalState)

-- ============================================================================
-- Generators
-- ============================================================================

-- Generate type variable names
genTypeVarName :: Gen String
genTypeVarName = do
  first <- elements ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

-- Generate constructor names
genConstructorName :: Gen String
genConstructorName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

-- Generate simple type variables
genSimpleTypeVar :: Gen TypeVar
genSimpleTypeVar = oneof
  [ TVCon <$> genConstructorName
  , TVVar <$> genTypeVarName
  ]

-- Generate type variable applications
genTypeVarApp :: Gen TypeVar
genTypeVarApp = do
  name <- genConstructorName
  args <- listOf genSimpleTypeVar
  return $ TVApp name args

-- Generate function type variables
genTypeVarFun :: Gen TypeVar
genTypeVarFun = do
  params <- listOf genSimpleTypeVar
  returnType <- genSimpleTypeVar
  return $ TVFun params returnType

-- Generate tuple type variables
genTypeVarTuple :: Gen TypeVar
genTypeVarTuple = do
  elements <- listOf genSimpleTypeVar
  return $ TVTuple elements

-- Generate L.any type variable
genTypeVar :: Gen TypeVar
genTypeVar = oneof
  [ genSimpleTypeVar
  , genTypeVarApp
  , genTypeVarFun
  , genTypeVarTuple
  ]

-- Generate type constraints
genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
  [ Equal <$> genTypeVar <*> genTypeVar
  , Subtype <$> genTypeVar <*> genTypeVar
  , Predicate <$> genTypeVarName <*> listOf genTypeVar
  , TypeSizeGE <$> genTypeVar <*> choose (0, 100)
  , TypeSizeGT <$> genTypeVar <*> choose (0, 100)
  , TypeRange <$> genTypeVar <*> choose (0, 100) <*> choose (0, 100)
  ]

-- Generate dependent type errors
genDependentTypeError :: Gen DependentTypeError
genDependentTypeError = oneof
  [ DependentTypeMismatch <$> genTypeVar <*> genTypeVar
  , ConstraintViolation <$> genTypeVarName <*> genTypeVar
  , TypeNotFound <$> genTypeVarName
  , InvalidTypeArgument <$> genTypeVarName
  , UnsolvableConstraint <$> genTypeConstraint
  , DependentInfiniteType <$> genTypeVarName <*> genTypeVar
  , AmbiguousType <$> genTypeVarName
  , ParseError <$> genTypeVarName
  , SemanticError <$> genTypeVarName
  ]

-- Generate type definitions
genTypeDef :: Gen TypeDef
genTypeDef = do
  params <- listOf genTypeVarName
  constraints <- listOf genTypeConstraint
  return $ TypeDefDecl params constraints

-- Generate type environments
genTypeEnv :: Gen TypeEnv
genTypeEnv = do
  typeDefs <- listOf $ do
    name <- genConstructorName
    typeDef <- genTypeDef
    return (name, typeDef)
  pendingConstraints <- listOf genTypeConstraint
  return $ TypeEnv (Map.fromList typeDefs) pendingConstraints

-- Generate type expressions
genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
  [ SimpleT <$> (T.pack <$> genConstructorName)
  , do
      name <- T.pack <$> genConstructorName
      args <- listOf genTypeExpr
      return $ GenericT name args
  , do
      params <- listOf $ (,) <$> genTypeVarName <*> genTypeExpr
      returnType <- genTypeExpr
      return $ FuncT params returnType
  , do
      base <- genTypeExpr
      constraints <- listOf genConstraint
      return $ RefineT base constraints
  ]

-- Generate constraints
genConstraint :: Gen Constraint
genConstraint = oneof
  [ SizeGE <$> (T.pack <$> genTypeVarName) <*> choose (0, 100)
  , SizeGT <$> (T.pack <$> genTypeVarName) <*> choose (0, 100)
  , RangeC <$> (T.pack <$> genTypeVarName) <*> choose (0, 100) <*> choose (0, 100)
  , PredC <$> (T.pack <$> genTypeVarName) <*> listOf genTypeExpr
  ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: TypeVar equality is reflexive
prop_typeVarReflexive :: TypeVar -> Bool
prop_typeVarReflexive tv = tv == tv

-- Property: TypeVar equality is symmetric
prop_typeVarSymmetric :: TypeVar -> TypeVar -> Bool
prop_typeVarSymmetric tv1 tv2 = (tv1 == tv2) == (tv2 == tv1)

-- Property: TypeVar equality is transitive
prop_typeVarTransitive :: TypeVar -> TypeVar -> TypeVar -> Bool
prop_typeVarTransitive tv1 tv2 tv3 =
  (tv1 == tv2 && tv2 == tv3) ==> (tv1 == tv3)

-- Property: TypeConstraint equality is reflexive
prop_typeConstraintReflexive :: TypeConstraint -> Bool
prop_typeConstraintReflexive tc = tc == tc

-- Property: TypeConstraint equality is symmetric
prop_typeConstraintSymmetric :: TypeConstraint -> TypeConstraint -> Bool
prop_typeConstraintSymmetric tc1 tc2 = (tc1 == tc2) == (tc2 == tc1)

-- Property: TypeConstraint equality is transitive
prop_typeConstraintTransitive :: TypeConstraint -> TypeConstraint -> TypeConstraint -> Bool
prop_typeConstraintTransitive tc1 tc2 tc3 =
  (tc1 == tc2 && tc2 == tc3) ==> (tc1 == tc3)

-- Property: DependentTypeError equality is reflexive
prop_dependentTypeErrorReflexive :: DependentTypeError -> Bool
prop_dependentTypeErrorReflexive dte = dte == dte

-- Property: DependentTypeError equality is symmetric
prop_dependentTypeErrorSymmetric :: DependentTypeError -> DependentTypeError -> Bool
prop_dependentTypeErrorSymmetric dte1 dte2 = (dte1 == dte2) == (dte2 == dte1)

-- Property: DependentTypeError equality is transitive
prop_dependentTypeErrorTransitive :: DependentTypeError -> DependentTypeError -> DependentTypeError -> Bool
prop_dependentTypeErrorTransitive dte1 dte2 dte3 =
  (dte1 == dte2 && dte2 == dte3) ==> (dte1 == dte3)

-- Property: newDependentTypeChecker creates valid checker
prop_newDependentTypeCheckerValid :: Bool
prop_newDependentTypeCheckerValid =
  let checker = newDependentTypeChecker
      env = dtcTypeEnv checker
      errors = tcErrors checker
  in Map.isSubmapOf preludeTypeDefs (typeDefinitions env) && null errors

-- Property: newDependentTypeCheckerWithTypes preserves custom types
prop_newDependentTypeCheckerWithTypesPreserves :: [(String, [String], [TypeConstraint])] -> Bool
prop_newDependentTypeCheckerWithTypesPreserves typeDefs =
  let checker = newDependentTypeCheckerWithTypes typeDefs
      env = dtcTypeEnv checker
      customDefs = Map.fromList [(n, TypeDefDecl ps cs) | (n, ps, cs) <- typeDefs]
  in Map.isSubmapOf customDefs (typeDefinitions env)

-- Property: TypeEnv preserves type definitions
prop_typeEnvPreservesTypeDefs :: [(String, TypeDef)] -> [TypeConstraint] -> Bool
prop_typeEnvPreservesTypeDefs typeDefs constraints =
  let typeDefMap = Map.fromList typeDefs
      env = TypeEnv typeDefMap constraints
  in typeDefinitions env == typeDefMap && pendingConstraints env == constraints

-- Property: Show instances produce non-empty strings
prop_typeVarShowNonEmpty :: TypeVar -> Bool
prop_typeVarShowNonEmpty tv = not (L.null (show tv))

prop_typeConstraintShowNonEmpty :: TypeConstraint -> Bool
prop_typeConstraintShowNonEmpty tc = not (L.null (show tc))

prop_dependentTypeErrorShowNonEmpty :: DependentTypeError -> Bool
prop_dependentTypeErrorShowNonEmpty dte = not (L.null (show dte))

-- Property: TypeVar constructors produce correct types
prop_tvConstructorCorrect :: String -> Bool
prop_tvConstructorCorrect name = not (null name) ==>
  let tv = TVCon name
  in case tv of
       TVCon n -> n == name
       _ -> False

prop_tvVarCorrect :: String -> Bool
prop_tvVarCorrect name = not (null name) ==>
  let tv = TVVar name
  in case tv of
       TVVar n -> n == name
       _ -> False

prop_tvAppCorrect :: String -> [TypeVar] -> Bool
prop_tvAppCorrect name args = not (null name) ==>
  let tv = TVApp name args
  in case tv of
       TVApp n a -> n == name && a == args
       _ -> False

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Dependencies Advanced QuickCheck Tests"
  [ testGroup "TypeVar Properties"
    [ testProperty "TypeVar equality is reflexive" prop_typeVarReflexive
    , testProperty "TypeVar equality is symmetric" prop_typeVarSymmetric
    , testProperty "TypeVar equality is transitive" prop_typeVarTransitive
    , testProperty "Show instances produce non-empty strings" prop_typeVarShowNonEmpty
    , testProperty "TVCon constructor produces correct types" prop_tvConstructorCorrect
    , testProperty "TVVar constructor produces correct types" prop_tvVarCorrect
    , testProperty "TVApp constructor produces correct types" prop_tvAppCorrect
    ]

  , testGroup "TypeConstraint Properties"
    [ testProperty "TypeConstraint equality is reflexive" prop_typeConstraintReflexive
    , testProperty "TypeConstraint equality is symmetric" prop_typeConstraintSymmetric
    , testProperty "TypeConstraint equality is transitive" prop_typeConstraintTransitive
    , testProperty "Show instances produce non-empty strings" prop_typeConstraintShowNonEmpty
    ]

  , testGroup "DependentTypeError Properties"
    [ testProperty "DependentTypeError equality is reflexive" prop_dependentTypeErrorReflexive
    , testProperty "DependentTypeError equality is symmetric" prop_dependentTypeErrorSymmetric
    , testProperty "DependentTypeError equality is transitive" prop_dependentTypeErrorTransitive
    , testProperty "Show instances produce non-empty strings" prop_dependentTypeErrorShowNonEmpty
    ]

  , testGroup "TypeChecker Properties"
    [ testProperty "newDependentTypeChecker creates valid checker" prop_newDependentTypeCheckerValid
    , testProperty "newDependentTypeCheckerWithTypes preserves custom types" prop_newDependentTypeCheckerWithTypesPreserves
    ]

  , testGroup "TypeEnv Properties"
    [ testProperty "TypeEnv preserves type definitions" prop_typeEnvPreservesTypeDefs
    ]

  , testGroup "Unit Tests"
    [ testCase "Create simple type variables" $ do
        let tvCon = TVCon "Int"
            tvVar = TVVar "a"
        case tvCon of
          TVCon name -> name @?= "Int"
          _ -> assertBool "Should be TVCon" False
        case tvVar of
          TVVar name -> name @?= "a"
          _ -> assertBool "Should be TVVar" False

    , testCase "Create type variable application" $ do
        let tvApp = TVApp "List" [TVVar "a"]
        case tvApp of
          TVApp name args -> do
            name @?= "List"
            args @?= [TVVar "a"]
          _ -> assertBool "Should be TVApp" False

    , testCase "Create function type variable" $ do
        let tvFun = TVFun [TVVar "a", TVVar "b"] (TVCon "Int")
        case tvFun of
          TVFun params returnType -> do
            params @?= [TVVar "a", TVVar "b"]
            returnType @?= TVCon "Int"
          _ -> assertBool "Should be TVFun" False

    , testCase "Create tuple type variable" $ do
        let tvTuple = TVTuple [TVVar "a", TVVar "b"]
        case tvTuple of
          TVTuple elements -> elements @?= [TVVar "a", TVVar "b"]
          _ -> assertBool "Should be TVTuple" False

    , testCase "Create type constraints" $ do
        let equal = Equal (TVVar "a") (TVCon "Int")
            subtype = Subtype (TVVar "a") (TVCon "Int")
            predicate = Predicate "Num" [TVVar "a"]
            sizeGE = TypeSizeGE (TVVar "a") 0
            sizeGT = TypeSizeGT (TVVar "a") 0
            range = TypeRange (TVVar "a") 0 100
        case equal of
          Equal t1 t2 -> do
            t1 @?= TVVar "a"
            t2 @?= TVCon "Int"
          _ -> assertBool "Should be Equal" False
        case subtype of
          Subtype t1 t2 -> do
            t1 @?= TVVar "a"
            t2 @?= TVCon "Int"
          _ -> assertBool "Should be Subtype" False
        case predicate of
          Predicate name args -> do
            name @?= "Num"
            args @?= [TVVar "a"]
          _ -> assertBool "Should be Predicate" False
        case sizeGE of
          TypeSizeGE tv k -> do
            tv @?= TVVar "a"
            k @?= 0
          _ -> assertBool "Should be TypeSizeGE" False
        case sizeGT of
          TypeSizeGT tv k -> do
            tv @?= TVVar "a"
            k @?= 0
          _ -> assertBool "Should be TypeSizeGT" False
        case range of
          TypeRange tv min max -> do
            tv @?= TVVar "a"
            min @?= 0
            max @?= 100
          _ -> assertBool "Should be TypeRange" False

    , testCase "Create dependent type errors" $ do
        let mismatch = DependentTypeMismatch (TVVar "a") (TVCon "Int")
            violation = ConstraintViolation "Size" (TVVar "a")
            notFound = TypeNotFound "MyType"
            invalidArg = InvalidTypeArgument "arg"
            unsolvable = UnsolvableConstraint (Equal (TVVar "a") (TVCon "Int"))
            infinite = DependentInfiniteType "rec" (TVVar "a")
            ambiguous = AmbiguousType "x"
            parseError = ParseError "syntax error"
            semanticError = SemanticError "type error"
        case mismatch of
          DependentTypeMismatch t1 t2 -> do
            t1 @?= TVVar "a"
            t2 @?= TVCon "Int"
          _ -> assertBool "Should be DependentTypeMismatch" False
        case violation of
          ConstraintViolation name tv -> do
            name @?= "Size"
            tv @?= TVVar "a"
          _ -> assertBool "Should be ConstraintViolation" False

    , testCase "Create type definition" $ do
        let typeDef = TypeDefDecl ["a", "b"] [Equal (TVVar "a") (TVVar "b")]
        tdParams typeDef @?= ["a", "b"]
        tdConstraints typeDef @?= [Equal (TVVar "a") (TVVar "b")]

    , testCase "Create type environment" $ do
        let typeDefs = Map.fromList [("Int", TypeDefDecl [] [])]
            constraints = [Equal (TVVar "a") (TVCon "Int")]
            env = TypeEnv typeDefs constraints
        typeDefinitions env @?= typeDefs
        pendingConstraints env @?= constraints

    , testCase "Create dependent type checker" $ do
        let checker = newDependentTypeChecker
            env = dtcTypeEnv checker
            errors = tcErrors checker
        Map.member "int" (typeDefinitions env) @?= True
        Map.member "bool" (typeDefinitions env) @?= True
        Map.member "float64" (typeDefinitions env) @?= True
        errors @?= []

    , testCase "Create dependent type checker with custom types" $ do
        let typeDefs = [("MyType", ["a"], [Equal (TVVar "a") (TVCon "Int")])]
            checker = newDependentTypeCheckerWithTypes typeDefs
            env = dtcTypeEnv checker
        Map.member "MyType" (typeDefinitions env) @?= True
        case Map.lookup "MyType" (typeDefinitions env) of
          Just (TypeDefDecl params constraints) -> do
            params @?= ["a"]
            constraints @?= [Equal (TVVar "a") (TVCon "Int")]
          _ -> assertBool "Should find MyType" False

    , testCase "Show instances" $ do
        let tv = TVVar "a"
            tc = Equal (TVVar "a") (TVCon "Int")
            dte = DependentTypeMismatch (TVVar "a") (TVCon "Int")
        show tv @?= "TVVar \"a\""
        show tc @?= "Equal (TVVar \"a\") (TVCon \"Int\")"
        show dte @?= "DependentTypeMismatch (TVVar \"a\") (TVCon \"Int\")"

    , testCase "Type variable ordering" $ do
        let tv1 = TVVar "a"
            tv2 = TVVar "b"
            tv3 = TVCon "Int"
        compare tv1 tv2 @?= LT
        compare tv1 tv3 @?= GT
        compare tv3 tv1 @?= LT
    ]
  ]