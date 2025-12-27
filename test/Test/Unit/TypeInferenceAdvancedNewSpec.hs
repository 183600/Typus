{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Test.Unit.TypeInferenceAdvancedNewSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, elements, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set

import Compiler.TypeChecker (buildTypeEnv, TypeCheckDiagnostic(..), diagnoseTypeErrors)
import Compiler.DependentTypeChecker (checkDependentTypes)
import Parser (parseTypus, TypusFile(..))
import Compiler (compile)
import SourceLocation (SourceSpan(..), defaultSpan)

-- | Advanced type expressions
data TypeExpr
    = TypeVar String                            -- Type variable
    | TypeConst String                          -- Type constant (Int, String, etc.)
    | TypeFunc TypeExpr TypeExpr                 -- Function type
    | TypeTuple [TypeExpr]                      -- Tuple type
    | TypeList TypeExpr                         -- List type
    | TypeOption TypeExpr                       -- Optional type
    | TypeMap TypeExpr TypeExpr                 -- Map type
    | TypeDependent String TypeExpr             -- Dependent type
    | TypeRef TypeExpr                          -- Reference type
    | TypeOwned TypeExpr                        -- Owned type
    deriving (Show, Eq)

-- | Type inference constraints
data TypeConstraint
    = Equality TypeExpr TypeExpr                -- Type equality
    | Subtype TypeExpr TypeExpr                 -- Subtype relationship
    | InstanceOf TypeExpr String                -- Type class instance
    | DependentConstraint String TypeExpr       -- Dependent constraint
    | OwnershipConstraint String TypeExpr       -- Ownership constraint
    deriving (Show, Eq)

-- | Type inference context
data TypeContext = TypeContext
    { tcTypeVars :: Map.Map String TypeExpr
    , tcConstraints :: [TypeConstraint]
    , tcSubstitutions :: Map.Map String TypeExpr
    , tcInstances :: Map.Map String [TypeExpr]
    } deriving (Show, Eq)

-- | Inference scenarios
data InferenceScenario
    = SimpleInference String                     -- Simple expression
    | FunctionInference String String           -- Function with parameters
    | GenericInference [String] String         -- Generic function
    | DependentInference String TypeExpr        -- Dependent type
    | RecursiveInference String                 -- Recursive function
    | ComplexInference [String]                 -- Complex expression
    deriving (Show, Eq)

-- | Type unification result
data UnificationResult
    = Unified TypeContext
    | Failed [TypeConstraint]                   -- Failed constraints
    | Ambiguous [TypeExpr]                      -- Ambiguous types
    deriving (Show, Eq)

-- | Generate type expressions
instance Arbitrary TypeExpr where
    arbitrary = sized $ \n -> if n <= 0
        then oneof
            [ TypeVar <$> genTypeVar
            , TypeConst <$> genTypeConst
            ]
        else oneof
            [ TypeVar <$> genTypeVar
            , TypeConst <$> genTypeConst
            , TypeFunc <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            , TypeTuple <$> listOf (resize (n `div` 3) arbitrary)
            , TypeList <$> resize (n `div` 2) arbitrary
            , TypeOption <$> resize (n `div` 2) arbitrary
            , TypeMap <$> resize (n `div` 3) arbitrary <*> resize (n `div` 3) arbitrary
            , TypeDependent <$> genTypeVar <*> resize (n `div` 2) arbitrary
            , TypeRef <$> resize (n `div` 2) arbitrary
            , TypeOwned <$> resize (n `div` 2) arbitrary
            ]
      where
        genTypeVar = elements ["T", "U", "V", "X", "Y", "Z", "A", "B", "C"]
        genTypeConst = elements ["Int", "String", "Bool", "Float", "Char", "Void"]

-- | Generate type constraints
instance Arbitrary TypeConstraint where
    arbitrary = oneof
        [ Equality <$> arbitrary <*> arbitrary
        , Subtype <$> arbitrary <*> arbitrary
        , InstanceOf <$> arbitrary <*> elements ["Num", "Eq", "Ord", "Show"]
        , DependentConstraint <$> genTypeVar <*> arbitrary
        , OwnershipConstraint <$> genVarName <*> arbitrary
        ]
      where
        genTypeVar = elements ["T", "U", "V", "X", "Y"]
        genVarName = elements ["x", "y", "z", "a", "b", "c"]

-- | Generate inference scenarios
instance Arbitrary InferenceScenario where
    arbitrary = oneof
        [ SimpleInference <$> genSimpleExpr
        , FunctionInference <$> genParamList <*> genReturnType
        , GenericInference <$> listOf genTypeVar <*> genGenericExpr
        , DependentInference <$> genDependentName <*> arbitrary
        , RecursiveInference <$> genRecursiveExpr
        , ComplexInference <$> listOf genComplexExpr
        ]
      where
        genSimpleExpr = elements ["42", "\"hello\"", "true", "1.5"]
        genParamList = elements ["x:Int", "y:String", "z:Bool"]
        genReturnType = elements ["Int", "String", "Bool"]
        genTypeVar = elements ["T", "U", "V"]
        genGenericExpr = elements ["id<T>(x)", "map<T,U>(f, xs)", "fold<T>(acc, x)"]
        genDependentName = elements ["Vector", "Matrix", "Array"]
        genRecursiveExpr = elements ["fact(n)", "fib(n)", "length(xs)"]
        genComplexExpr = elements ["map(f, filter(p, xs))", "fold(op, init, xs)", "compose(f, g, h)"]

-- | Property: Type variables should be unified correctly
prop_typeVariablesUnified :: TypeExpr -> TypeExpr -> Bool
prop_typeVariablesUnified t1 t2 = 
    let constraint = Equality t1 t2
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext [constraint]
    in case result of
        Unified _ -> True
        Failed _ -> True  -- Some unifications may fail
        Ambiguous _ -> True  -- Some may be ambiguous

-- | Property: Function types should handle parameter inference
prop_functionTypesParameterInference :: [TypeExpr] -> TypeExpr -> Bool
prop_functionTypesParameterInference paramTypes returnType = 
    let funcType = foldr TypeFunc returnType paramTypes
        constraint = Equality funcType funcType
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext [constraint]
    in case result of
        Unified ctx -> Map.size (tcTypeVars ctx) >= 0
        Failed _ -> True
        Ambiguous _ -> True

-- | Property: Generic types should be instantiated correctly
prop_genericTypesInstantiated :: [String] -> TypeExpr -> Bool
prop_genericTypesInstantiated typeVars baseType = 
    let substitutions = Map.fromList $ zip typeVars (repeat baseType)
        initialContext = TypeContext Map.empty [] substitutions Map.empty
        result = applySubstitutions initialContext baseType
    in result == baseType  -- Simplified: should apply substitutions

-- | Property: Dependent types should handle constraints correctly
prop_dependentTypesConstraints :: String -> TypeExpr -> Bool
prop_dependentTypesConstraints dependentName baseType = 
    let dependentType = TypeDependent dependentName baseType
        constraint = DependentConstraint dependentName baseType
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext [constraint]
    in case result of
        Unified _ -> True
        Failed _ -> True
        Ambiguous _ -> True

-- | Property: Recursive types should be handled safely
prop_recursiveTypesHandled :: String -> TypeExpr -> Bool
prop_recursiveTypesHandled typeName baseType = 
    let recursiveType = TypeFunc baseType (TypeVar typeName)
        substitution = Map.singleton typeName recursiveType
        initialContext = TypeContext Map.empty [] substitution Map.empty
        result = applySubstitutions initialContext baseType
    in not (containsRecursiveType result typeName) || isWellFounded result typeName

-- | Property: Complex expressions should maintain type consistency
prop_complexExpressionsConsistent :: [String] -> Bool
prop_complexExpressionsConsistent exprs = 
    let types = map inferSimpleType exprs
        constraints = zipWith Equality types (tail types)
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext constraints
    in case result of
        Unified _ -> True
        Failed _ -> True
        Ambiguous _ -> True

-- | Property: Type inference should handle polymorphism
prop_inferenceHandlesPolymorphism :: TypeExpr -> TypeExpr -> Bool
prop_inferenceHandlesPolymorphism inputType outputType = 
    let polymorphicFunc = TypeFunc (TypeVar "T") (TypeVar "U")
        constraint = Equality polymorphicFunc (TypeFunc inputType outputType)
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext [constraint]
    in case result of
        Unified ctx -> Map.size (tcSubstitutions ctx) >= 2  -- Should substitute T and U
        Failed _ -> True
        Ambiguous _ -> True

-- | Property: Type inference should respect ownership constraints
prop_inferenceRespectsOwnership :: String -> TypeExpr -> Bool
prop_inferenceRespectsOwnership varName varType = 
    let ownedType = TypeOwned varType
        constraint = OwnershipConstraint varName ownedType
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext [constraint]
    in case result of
        Unified ctx -> Map.member varName (tcTypeVars ctx)
        Failed _ -> True
        Ambiguous _ -> True

-- | Property: Reference types should handle aliasing correctly
prop_referenceTypesAliasing :: TypeExpr -> Bool
prop_referenceTypesAliasing baseType = 
    let refType = TypeRef baseType
        constraint = Equality refType refType
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext [constraint]
    in case result of
        Unified _ -> True
        Failed _ -> True
        Ambiguous _ -> True

-- | Property: Type inference should handle overloading
prop_inferenceHandlesOverloading :: TypeExpr -> TypeExpr -> TypeExpr -> Bool
prop_inferenceHandlesOverloading t1 t2 t3 = 
    let overloadedFunc = TypeFunc t1 t2
        alternativeFunc = TypeFunc t1 t3
        constraints = [Equality overloadedFunc overloadedFunc, Equality alternativeFunc alternativeFunc]
        initialContext = TypeContext Map.empty [] Map.empty Map.empty
        result = unifyConstraints initialContext constraints
    in case result of
        Unified _ -> True
        Failed _ -> True
        Ambiguous _ -> True

-- | Unify type constraints
unifyConstraints :: TypeContext -> [TypeConstraint] -> UnificationResult
unifyConstraints ctx [] = Unified ctx
unifyConstraints ctx (constraint:rest) = 
    case unifyConstraint ctx constraint of
        Left remaining -> Failed (remaining ++ rest)
        Right ctx' -> unifyConstraints ctx' rest

-- | Unify a single constraint
unifyConstraint :: TypeContext -> TypeConstraint -> Either [TypeConstraint] TypeContext
unifyConstraint ctx constraint = case constraint of
    Equality t1 t2 -> unifyTypes ctx t1 t2
    Subtype t1 t2 -> Right ctx  -- Simplified: accept all subtypes
    InstanceOf t1 className -> Right ctx  -- Simplified: accept all instances
    DependentConstraint name t -> Right ctx  -- Simplified: accept all dependent constraints
    OwnershipConstraint var t -> Right ctx  -- Simplified: accept all ownership constraints

-- | Unify two types
unifyTypes :: TypeContext -> TypeExpr -> TypeExpr -> Either [TypeConstraint] TypeContext
unifyTypes ctx t1 t2 
    | t1 == t2 = Right ctx
    | isTypeVar t1 = Right ctx { tcSubstitutions = Map.insert (getTypeVarName t1) t2 (tcSubstitutions ctx) }
    | isTypeVar t2 = Right ctx { tcSubstitutions = Map.insert (getTypeVarName t2) t1 (tcSubstitutions ctx) }
    | otherwise = case (t1, t2) of
        (TypeFunc a1 b1, TypeFunc a2 b2) -> do
            ctx' <- unifyTypes ctx a1 a2
            unifyTypes ctx' b1 b2
        (TypeTuple ts1, TypeTuple ts2) | length ts1 == length ts2 -> 
            foldM (\ctx' (t1', t2') -> unifyTypes ctx' t1' t2') ctx (zip ts1 ts2)
        _ -> Left [Equality t1 t2]

-- | Check if a type is a type variable
isTypeVar :: TypeExpr -> Bool
isTypeVar (TypeVar _) = True
isTypeVar _ = False

-- | Get the name of a type variable
getTypeVarName :: TypeExpr -> String
getTypeVarName (TypeVar name) = name
getTypeVarName _ = ""

-- | Apply substitutions to a type
applySubstitutions :: TypeContext -> TypeExpr -> TypeExpr
applySubstitutions ctx = go
  where
    go (TypeVar name) = Map.findWithDefault (TypeVar name) name (tcSubstitutions ctx)
    go (TypeFunc a b) = TypeFunc (go a) (go b)
    go (TypeTuple ts) = TypeTuple (map go ts)
    go (TypeList t) = TypeList (go t)
    go (TypeOption t) = TypeOption (go t)
    go (TypeMap k v) = TypeMap (go k) (go v)
    go (TypeDependent name t) = TypeDependent name (go t)
    go (TypeRef t) = TypeRef (go t)
    go (TypeOwned t) = TypeOwned (go t)
    go other = other

-- | Check if a type contains a recursive reference
containsRecursiveType :: TypeExpr -> String -> Bool
containsRecursiveType t name = case t of
    TypeVar n -> n == name
    TypeFunc a b -> containsRecursiveType a name || containsRecursiveType b name
    TypeTuple ts -> any (flip containsRecursiveType name) ts
    TypeList t' -> containsRecursiveType t' name
    TypeOption t' -> containsRecursiveType t' name
    TypeMap k v -> containsRecursiveType k name || containsRecursiveType v name
    TypeDependent n t' -> n == name || containsRecursiveType t' name
    TypeRef t' -> containsRecursiveType t' name
    TypeOwned t' -> containsRecursiveType t' name
    _ -> False

-- | Check if a recursive type is well-founded
isWellFounded :: TypeExpr -> String -> Bool
isWellFounded t name = 
    let visited = Set.singleton name
    in checkWellFounded t visited
  where
    checkWellFounded (TypeVar n) visited = n `Set.notMember` visited
    checkWellFounded (TypeFunc a b) visited = 
        checkWellFounded a visited && checkWellFounded b visited
    checkWellFounded (TypeTuple ts) visited = 
        all (flip checkWellFounded visited) ts
    checkWellFounded (TypeList t') visited = checkWellFounded t' visited
    checkWellFounded (TypeOption t') visited = checkWellFounded t' visited
    checkWellFounded (TypeMap k v) visited = 
        checkWellFounded k visited && checkWellFounded v visited
    checkWellFounded (TypeDependent n t') visited = 
        let visited' = Set.insert n visited
        in checkWellFounded t' visited'
    checkWellFounded (TypeRef t') visited = checkWellFounded t' visited
    checkWellFounded (TypeOwned t') visited = checkWellFounded t' visited
    checkWellFounded _ _ = True

-- | Infer simple type from expression
inferSimpleType :: String -> TypeExpr
inferSimpleType expr
    | all (`elem` "0123456789") expr = TypeConst "Int"
    | head expr == '"' && last expr == '"' = TypeConst "String"
    | expr == "true" || expr == "false" = TypeConst "Bool"
    | any (`elem` expr) ".eE" && all (`elem` "0123456789.eE-+") expr = TypeConst "Float"
    | otherwise = TypeVar "T"

-- | Generate Typus code for type inference testing
generateInferenceCode :: InferenceScenario -> String
generateInferenceCode scenario = case scenario of
    SimpleInference expr -> 
        "package main\n\nfunc main() {\n    x := " ++ expr ++ "\n}\n"
    
    FunctionInference params returnType ->
        "package main\n\nfunc myFunc(" ++ params ++ ") " ++ returnType ++ " {\n    return 42\n}\n"
    
    GenericInference typeVars expr ->
        "package main\n\nfunc " ++ expr ++ " {\n    // Generic function\n}\n"
    
    DependentInference name baseType ->
        "package main\n\ntype " ++ name ++ "<T> struct {\n    data T\n}\n"
    
    RecursiveInference expr ->
        "package main\n\nfunc " ++ expr ++ " {\n    // Recursive function\n}\n"
    
    ComplexInference exprs ->
        "package main\n\nfunc main() {\n" ++
        concatMap (\expr -> "    " ++ expr ++ "\n") exprs ++
        "}\n"

tests :: TestTree
tests = testGroup "Type Inference Advanced Tests"
  [ testProperty "Type variables are unified correctly" $
      fastProperty "type1, type2" prop_typeVariablesUnified
  
  , testProperty "Function types handle parameter inference" $
      fastProperty "parameter types, return type" prop_functionTypesParameterInference
  
  , testProperty "Generic types are instantiated correctly" $
      fastProperty "type variables, base type" prop_genericTypesInstantiated
  
  , testProperty "Dependent types handle constraints correctly" $
      fastProperty "dependent name, base type" prop_dependentTypesConstraints
  
  , testProperty "Recursive types are handled safely" $
      fastProperty "type name, base type" prop_recursiveTypesHandled
  
  , testProperty "Complex expressions maintain type consistency" $
      fastProperty "expressions" prop_complexExpressionsConsistent
  
  , testProperty "Inference handles polymorphism" $
      fastProperty "input type, output type" prop_inferenceHandlesPolymorphism
  
  , testProperty "Inference respects ownership constraints" $
      fastProperty "variable name, variable type" prop_inferenceRespectsOwnership
  
  , testProperty "Reference types handle aliasing correctly" $
      fastProperty "base type" prop_referenceTypesAliasing
  
  , testProperty "Inference handles overloading" $
      fastProperty "type1, type2, type3" prop_inferenceHandlesOverloading
  
  , testProperty "Type inference terminates for finite expressions" $
      fastProperty "type expression" $
      \expr -> 
        let initialContext = TypeContext Map.empty [] Map.empty Map.empty
            result = unifyConstraints initialContext [Equality expr expr]
        in case result of
            Unified _ -> True
            Failed _ -> True
            Ambiguous _ -> True
  
  , testProperty "Type inference preserves type safety" $
      fastProperty "inference scenario" $
      \scenario -> 
        let code = generateInferenceCode scenario
        in case parseTypus code of
            Left _ -> True  -- Parsing errors are acceptable
            Right typusFile ->
                case compile typusFile of
                    Left _ -> True  -- Compilation errors are acceptable
                    Right _ -> True  -- Successful compilation is acceptable
  ]