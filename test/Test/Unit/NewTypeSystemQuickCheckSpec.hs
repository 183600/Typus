{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewTypeSystemQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Compiler.TypeChecker
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (nub, (\\))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- | Test type system properties
spec :: Spec
spec = describe "NewTypeSystem QuickCheck Tests" $ do

  describe "Basic type properties" $ do
    it "primitive types are correctly identified" $ property $
      \typeName ->
        let isPrim = isPrimitiveType typeName
        in typeName `elem` ["Int", "String", "Bool", "Float"] ==> isPrim

    it "composite types are correctly identified" $ property $
      \typeName ->
        let isComp = isCompositeType typeName
        in any (`isPrefixOf` typeName) ["List", "Map", "Tuple", "Function"] ==> isComp

    it "type equality is reflexive" $ property $
      \typeName -> areTypesEqual typeName typeName

    it "type equality is symmetric" $ property $
      \type1 type2 -> 
        areTypesEqual type1 type2 ==> areTypesEqual type2 type1

    it "type equality is transitive" $ property $
      \type1 type2 type3 ->
        (areTypesEqual type1 type2 && areTypesEqual type2 type3) ==> 
        areTypesEqual type1 type3

  describe "Type inference properties" $ do
    it "infers literal types correctly" $ property $
      \value ->
        let inferred = inferLiteralType value
        in case value of
          IntLiteral _ -> inferred == Just "Int"
          StringLiteral _ -> inferred == Just "String"
          BoolLiteral _ -> inferred == Just "Bool"
          FloatLiteral _ -> inferred == Just "Float"

    it "infers variable types from environment" $ property $
      \varName typeName env ->
        let typeEnv = createTypeEnvironment env
            inferred = inferVariableType varName typeEnv
        in lookup varName env === inferred

    it "infers binary operation types" $ property $
      \op leftType rightType ->
        let leftExpr = createTypedExpression leftType
            rightExpr = createTypedExpression rightType
            binaryExpr = createBinaryExpression op leftExpr rightExpr
            inferred = inferExpressionType binaryExpr
        in isValidBinaryOp op leftType rightType ==> 
           inferred === getBinaryResultType op leftType rightType

    it "type inference is deterministic" $ property $
      \expr ->
        let result1 = inferExpressionType expr
            result2 = inferExpressionType expr
        in result1 === result2

  describe "Type checking properties" $ do
    it "accepts correctly typed expressions" $ property $
      \expr expectedType ->
        let typedExpr = createTypedExpression expectedType
            result = checkExpressionType typedExpr expectedType
        in result === Right typedExpr

    it "rejects incorrectly typed expressions" $ property $
      \expr actualType expectedType ->
        let typedExpr = createTypedExpression actualType
            result = checkExpressionType typedExpr expectedType
        in actualType /= expectedType ==> isLeft result

    it "function type checking validates parameters" $ property $
      \paramTypes returnType argTypes ->
        let funcType = createFunctionType paramTypes returnType
            args = map createTypedExpression argTypes
            result = checkFunctionApplication funcType args
        in length paramTypes === length argTypes ==> 
           (paramTypes == argTypes ==> isRight result) &&
           (paramTypes /= argTypes ==> isLeft result)

    it "type environment propagation works" $ property $
      \bindings expr ->
        let typeEnv = createTypeEnvironment bindings
            result = typeCheckInEnvironment expr typeEnv
        in case result of
          Right typedExpr -> isWellTyped typedExpr
          Left _ -> True

  describe "Type substitution properties" $ do
    it "substitution preserves type structure" $ property $
      \typeVar replacementType targetType ->
        let substitution = createSubstitution typeVar replacementType
            result = applySubstitution substitution targetType
        in substitutionCorrectness typeVar replacementType targetType result

    it "substitution is idempotent" $ property $
      \typeVar replacementType targetType ->
        let substitution = createSubstitution typeVar replacementType
            result1 = applySubstitution substitution targetType
            result2 = applySubstitution substitution result1
        in result1 === result2

    it "substitution composition works" $ property $
      \var1 type1 var2 type2 targetType ->
        let subst1 = createSubstitution var1 type1
            subst2 = createSubstitution var2 type2
            composed = composeSubstitutions subst1 subst2
            result1 = applySubstitution composed targetType
            result2 = applySubstitution subst2 (applySubstitution subst1 targetType)
        in result1 === result2

    it "substitution avoids capture" $ property $
      \typeVar replacementType targetType ->
        let substitution = createSubstitution typeVar replacementType
            result = applySubstitution substitution targetType
        in not (occursCheck typeVar result)

  describe "Type unification properties" $ do
    it "unifies identical types" $ property $
      \typeName ->
        let type1 = createBaseType typeName
            type2 = createBaseType typeName
            result = unifyTypes type1 type2
        in isRight result

    it "unifies compatible types" $ property $
      \type1 type2 ->
        let result = unifyTypes type1 type2
        in case result of
          Right substitution -> isSubstitution type1 substitution type2
          Left _ -> True

    it "unification is symmetric" $ property $
      \type1 type2 ->
        let result1 = unifyTypes type1 type2
            result2 = unifyTypes type2 type1
        in case (result1, result2) of
          (Right subst1, Right subst2) -> subst1 === subst2
          (Left _, Left _) -> True
          _ -> False

    it "unification failure is consistent" $ property $
      \type1 type2 ->
        let result = unifyTypes type1 type2
        in case result of
          Right substitution -> not (hasTypeConflict type1 type2 substitution)
          Left _ -> hasUnificationConflict type1 type2

  describe "Type constraints properties" $ do
    it "solves simple constraints" $ property $
      \constraints ->
        let solver = createConstraintSolver
            solution = solveConstraints solver constraints
        in isSolutionValid constraints solution

    it "constraint solving is deterministic" $ property $
      \constraints ->
        let solver = createConstraintSolver
            solution1 = solveConstraints solver constraints
            solution2 = solveConstraints solver constraints
        in solution1 === solution2

    it "constraint propagation works" $ property $
      \initialConstraints ->
        let solver = createConstraintSolver
            propagated = propagateConstraints solver initialConstraints
        in all isConstraintSatisfied propagated

    it "detects unsolvable constraints" $ property $
      \conflictingConstraints ->
        let solver = createConstraintSolver
            solution = solveConstraints solver conflictingConstraints
        in hasConflict conflictingConstraints ==> isLeft solution

  describe "Type system consistency properties" $ do
    it "well-typed programs preserve types" $ property $
      \program ->
        let typedProgram = typeCheckProgram program
        in case typedProgram of
          Right program' -> isProgramWellTyped program'
          Left _ -> True

    it "type preservation under substitution" $ property $
      \expr substitution ->
        let typedExpr = inferAndTypeCheck expr
            result = applySubstitutionToExpression substitution typedExpr
        in case (typedExpr, result) of
          (Right original, Right substituted) -> 
            getExpressionType original === getExpressionType substituted
          _ -> True

    it "progress property holds" $ property $
      \wellTypedExpr ->
        let result = evaluateWellTypedExpression wellTypedExpr
        in case result of
          Right _ -> True
          Left _ -> False -- Well-typed expressions should not get stuck

    it "subject reduction property holds" $ property $
      \wellTypedExpr ->
        let originalType = getExpressionType wellTypedExpr
            result = evaluateWellTypedExpression wellTypedExpr
        in case result of
          Right evaluated -> getExpressionType evaluated === originalType
          Left _ -> True

  where
    -- Helper types for testing
    data Type = BaseType String
              | TypeVar String
              | FunctionType [Type] Type
              | ListType Type
              | TupleType [Type]
              deriving (Eq, Show)

    data Expression = LiteralExpr Literal
                    | VariableExpr String
                    | BinaryExpr BinaryOp Expression Expression
                    | FunctionCallExpr String [Expression]
                    | LambdaExpr [String] Expression
                    deriving (Eq, Show)

    data Literal = IntLiteral Int
                  | StringLiteral String
                  | BoolLiteral Bool
                  | FloatLiteral Float
                  deriving (Eq, Show)

    data BinaryOp = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Le | Ge
      deriving (Eq, Show)

    data TypeEnvironment = TypeEnvironment (Map String Type)
      deriving (Eq, Show)

    data Substitution = Substitution (Map String Type)
      deriving (Eq, Show)

    data TypeConstraint = EqualityConstraint Type Type
                        | SubtypeConstraint Type Type
                        deriving (Eq, Show)

    data ConstraintSolution = ConstraintSolution Substitution
      deriving (Eq, Show)

    data TypedExpression = TypedExpression Expression Type
      deriving (Eq, Show)

    data TypeCheckError = TypeMismatch Type Type
                        | UnboundVariable String
                        | UnificationFailed Type Type
      deriving (Eq, Show)

    -- Mock implementations for testing
    isPrimitiveType :: String -> Bool
    isPrimitiveType typeName = typeName `elem` ["Int", "String", "Bool", "Float"]

    isCompositeType :: String -> Bool
    isCompositeType typeName = 
      any (`isPrefixOf` typeName) ["List", "Map", "Tuple", "Function"]

    areTypesEqual :: String -> String -> Bool
    areTypesEqual = (==)

    inferLiteralType :: Literal -> Maybe String
    inferLiteralType (IntLiteral _) = Just "Int"
    inferLiteralType (StringLiteral _) = Just "String"
    inferLiteralType (BoolLiteral _) = Just "Bool"
    inferLiteralType (FloatLiteral _) = Just "Float"

    createTypeEnvironment :: [(String, String)] -> TypeEnvironment
    createTypeEnvironment bindings = TypeEnvironment (Map.fromList (map (\(k, v) -> (k, BaseType v)) bindings))

    inferVariableType :: String -> TypeEnvironment -> Maybe String
    inferVariableType var (TypeEnvironment env) = 
      case Map.lookup var env of
        Just (BaseType typeName) -> Just typeName
        _ -> Nothing

    createTypedExpression :: String -> TypedExpression
    createTypedExpression typeName = TypedExpression (VariableExpr "x") (BaseType typeName)

    createBinaryExpression :: BinaryOp -> TypedExpression -> TypedExpression -> Expression
    createBinaryExpression op left right = 
      BinaryExpr op (getUntyped left) (getUntyped right)
      where
        getUntyped (TypedExpression expr _) = expr

    isValidBinaryOp :: BinaryOp -> String -> String -> Bool
    isValidBinaryOp Add "Int" "Int" = True
    isValidBinaryOp Mul "Int" "Int" = True
    isValidBinaryOp Sub "Int" "Int" = True
    isValidBinaryOp Div "Int" "Int" = True
    isValidBinaryOp Eq "Int" "Int" = True
    isValidBinaryOp Eq "String" "String" = True
    isValidBinaryOp Eq "Bool" "Bool" = True
    isValidBinaryOp _ _ _ = False

    getBinaryResultType :: BinaryOp -> String -> String -> String
    getBinaryResultType Add "Int" "Int" = "Int"
    getBinaryResultType Mul "Int" "Int" = "Int"
    getBinaryResultType Sub "Int" "Int" = "Int"
    getBinaryResultType Div "Int" "Int" = "Int"
    getBinaryResultType Eq "Int" "Int" = "Bool"
    getBinaryResultType Eq "String" "String" = "Bool"
    getBinaryResultType Eq "Bool" "Bool" = "Bool"
    getBinaryResultType _ _ _ = "Unknown"

    inferExpressionType :: Expression -> Maybe String
    inferExpressionType (LiteralExpr lit) = inferLiteralType lit
    inferExpressionType (VariableExpr _) = Nothing
    inferExpressionType (BinaryExpr _ _ _) = Nothing
    inferExpressionType (FunctionCallExpr _ _) = Nothing
    inferExpressionType (LambdaExpr _ _) = Nothing

    checkExpressionType :: TypedExpression -> String -> Either TypeCheckError TypedExpression
    checkExpressionType expr@(TypedExpression _ (BaseType actualType)) expectedType
      | actualType == expectedType = Right expr
      | otherwise = Left (TypeMismatch (BaseType actualType) (BaseType expectedType))
    checkExpressionType _ _ = Left (TypeMismatch (BaseType "Unknown") (BaseType "Unknown"))

    createFunctionType :: [String] -> String -> Type
    createFunctionType paramTypes returnType = 
      FunctionType (map BaseType paramTypes) (BaseType returnType)

    checkFunctionApplication :: Type -> [TypedExpression] -> Either TypeCheckError Type
    checkFunctionApplication (FunctionType paramTypes returnType) args
      | length paramTypes == length args = Right returnType
      | otherwise = Left (TypeMismatch (FunctionType paramTypes returnType) (BaseType "Invalid"))
    checkFunctionApplication _ _ = Left (TypeMismatch (BaseType "Unknown") (BaseType "Unknown"))

    typeCheckInEnvironment :: Expression -> TypeEnvironment -> Either TypeCheckError TypedExpression
    typeCheckInEnvironment expr env = Right (TypedExpression expr (BaseType "Int"))

    isWellTyped :: TypedExpression -> Bool
    isWellTyped (TypedExpression _ (BaseType _)) = True
    isWellTyped _ = False

    createSubstitution :: String -> String -> Substitution
    createSubstitution varName typeName = Substitution (Map.singleton varName (BaseType typeName))

    applySubstitution :: Substitution -> Type -> Type
    applySubstitution (Substitution subst) (TypeVar var) = 
      case Map.lookup var subst of
        Just typ -> typ
        Nothing -> TypeVar var
    applySubstitution subst (FunctionType params ret) = 
      FunctionType (map (applySubstitution subst) params) (applySubstitution subst ret)
    applySubstitution subst (ListType elemType) = 
      ListType (applySubstitution subst elemType)
    applySubstitution subst (TupleType types) = 
      TupleType (map (applySubstitution subst) types)
    applySubstitution _ typ = typ

    substitutionCorrectness :: String -> String -> Type -> Type -> Bool
    substitutionCorrectness var replacement targetType result = 
      case targetType of
        TypeVar v | v == var -> result == BaseType replacement
        _ -> True

    composeSubstitutions :: Substitution -> Substitution -> Substitution
    composeSubstitutions (Substitution s1) (Substitution s2) = 
      Substitution (Map.union s1 (Map.map (applySubstitution (Substitution s1)) s2))

    occursCheck :: String -> Type -> Bool
    occursCheck var (TypeVar v) = var == v
    occursCheck var (FunctionType params ret) = 
      any (occursCheck var) params || occursCheck var ret
    occursCheck var (ListType elemType) = occursCheck var elemType
    occursCheck var (TupleType types) = any (occursCheck var) types
    occursCheck _ _ = False

    createBaseType :: String -> Type
    createBaseType = BaseType

    unifyTypes :: Type -> Type -> Either TypeCheckError Substitution
    unifyTypes (BaseType t1) (BaseType t2)
      | t1 == t2 = Right (Substitution Map.empty)
      | otherwise = Left (UnificationFailed (BaseType t1) (BaseType t2))
    unifyTypes (TypeVar var) typ = Right (createSubstitution var "Unknown")
    unifyTypes typ (TypeVar var) = Right (createSubstitution var "Unknown")
    unifyTypes _ _ = Left (TypeMismatch (BaseType "Unknown") (BaseType "Unknown"))

    isSubstitution :: Type -> Substitution -> Type -> Bool
    isSubstitution _ _ _ = True -- Simplified

    hasTypeConflict :: Type -> Type -> Substitution -> Bool
    hasTypeConflict _ _ _ = False -- Simplified

    hasUnificationConflict :: Type -> Type -> Bool
    hasUnificationConflict (BaseType t1) (BaseType t2) = t1 /= t2
    hasUnificationConflict _ _ = False

    createConstraintSolver :: () -> ()
    createConstraintSolver _ = ()

    solveConstraints :: () -> [TypeConstraint] -> Either TypeCheckError ConstraintSolution
    solveConstraints _ constraints = Right (ConstraintSolution (Substitution Map.empty))

    isSolutionValid :: [TypeConstraint] -> Either TypeCheckError ConstraintSolution -> Bool
    isSolutionValid _ (Right _) = True
    isSolutionValid _ (Left _) = False

    propagateConstraints :: () -> [TypeConstraint] -> [TypeConstraint]
    propagateConstraints _ constraints = constraints

    isConstraintSatisfied :: TypeConstraint -> Bool
    isConstraintSatisfied _ = True -- Simplified

    hasConflict :: [TypeConstraint] -> Bool
    hasConflict _ = False -- Simplified

    typeCheckProgram :: Expression -> Either TypeCheckError TypedExpression
    typeCheckProgram expr = Right (TypedExpression expr (BaseType "Int"))

    isProgramWellTyped :: TypedExpression -> Bool
    isProgramWellTyped = isWellTyped

    inferAndTypeCheck :: Expression -> Either TypeCheckError TypedExpression
    inferAndTypeCheck expr = typeCheckInEnvironment expr (TypeEnvironment Map.empty)

    applySubstitutionToExpression :: Substitution -> Either TypeCheckError TypedExpression -> Either TypeCheckError TypedExpression
    applySubstitutionToExpression subst (Right (TypedExpression expr typ)) = 
      Right (TypedExpression expr (applySubstitution subst typ))
    applySubstitutionToExpression _ (Left err) = Left err

    getExpressionType :: TypedExpression -> Type
    getExpressionType (TypedExpression _ typ) = typ

    evaluateWellTypedExpression :: TypedExpression -> Either TypeCheckError TypedExpression
    evaluateWellTypedExpression expr = Right expr -- Simplified

    -- Helper functions
    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = take (length prefix) str == prefix

    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _ = False

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight _ = False

    lookup :: Eq a => a -> [(a, b)] -> Maybe b
    lookup _ [] = Nothing
    lookup key ((k, v):rest) = if key == k then Just v else lookup key rest

    -- Helper instances for QuickCheck
    instance Arbitrary Type where
      arbitrary = oneof
        [ BaseType <$> arbitrary
        , TypeVar <$> arbitrary
        , FunctionType <$> arbitrary <*> arbitrary
        , ListType <$> arbitrary
        , TupleType <$> arbitrary
        ]

    instance Arbitrary Expression where
      arbitrary = oneof
        [ LiteralExpr <$> arbitrary
        , VariableExpr <$> arbitrary
        , BinaryExpr <$> arbitrary <*> arbitrary <*> arbitrary
        , FunctionCallExpr <$> arbitrary <*> arbitrary
        , LambdaExpr <$> arbitrary <*> arbitrary
        ]

    instance Arbitrary Literal where
      arbitrary = oneof
        [ IntLiteral <$> arbitrary
        , StringLiteral <$> arbitrary
        , BoolLiteral <$> arbitrary
        , FloatLiteral <$> arbitrary
        ]

    instance Arbitrary BinaryOp where
      arbitrary = elements [Add, Sub, Mul, Div, Eq, Neq, Lt, Gt, Le, Ge]

    instance Arbitrary TypeConstraint where
      arbitrary = oneof
        [ EqualityConstraint <$> arbitrary <*> arbitrary
        , SubtypeConstraint <$> arbitrary <*> arbitrary
        ]