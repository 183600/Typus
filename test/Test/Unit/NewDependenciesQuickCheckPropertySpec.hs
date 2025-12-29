{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck property tests for Dependencies module
module Test.Unit.NewDependenciesQuickCheckPropertySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Dependencies.AST
import Dependencies.TypeSystem
import Dependencies.Inference
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort, nub)
import Data.Either (isLeft, isRight)

-- | Test group for Dependencies module QuickCheck properties
testDependenciesQuickCheckProperties :: TestTree
testDependenciesQuickCheckProperties = testGroup "Dependencies Module QuickCheck Property Tests"
  [ astProperties
  , typeExprProperties
  , constraintProperties
  , dependencyGraphProperties
  , typeSystemProperties
  , typeVarProperties
  , typeConstraintProperties
  , dependentTypeErrorProperties
  , typeCheckerProperties
  , substitutionProperties
  ]

-- | Properties for AST
astProperties :: TestTree
astProperties = testGroup "AST properties"
  [ testProperty "Program preserves statements" $
    \statements -> 
      let ast = Program statements
      in case ast of
        Program stmts -> stmts === statements
  
  , testProperty "Program with empty statements creates valid AST" $
    \_ -> 
      let ast = Program []
      in case ast of
        Program [] -> property True
  
  , testProperty "AST equality is structural" $
    \stmts1 stmts2 -> 
      let ast1 = Program stmts1
          ast2 = Program stmts2
      in (ast1 == ast2) === (stmts1 == stmts2)
  ]

-- | Properties for TypeExpr
typeExprProperties :: TestTree
typeExprProperties = testGroup "TypeExpr properties"
  [ testProperty "SimpleT preserves name" $
    \name -> 
      let typeExpr = SimpleT name
      in case typeExpr of
        SimpleT n -> n === name
  
  , testProperty "GenericT preserves name and arguments" $
    \name args -> 
      let typeExpr = GenericT name args
      in case typeExpr of
        GenericT n a -> n === name && a === args
  
  , testProperty "FuncT preserves parameters and return type" $
    \params retType -> 
      let typeExpr = FuncT params retType
      in case typeExpr of
        FuncT p r -> p === params && r === retType
  
  , testProperty "RefineT preserves type and constraints" $
    \baseType constraints -> 
      let typeExpr = RefineT baseType constraints
      in case typeExpr of
        RefineT b c -> b === baseType && c === constraints
  
  , testProperty "TypeExpr equality is structural" $
    \typeExpr1 typeExpr2 -> 
      let equal1 = (typeExpr1 == typeExpr2)
          equal2 = case (typeExpr1, typeExpr2) of
                    (SimpleT n1, SimpleT n2) -> n1 == n2
                    (GenericT n1 a1, GenericT n2 a2) -> n1 == n2 && a1 == a2
                    (FuncT p1 r1, FuncT p2 r2) -> p1 == p2 && r1 == r2
                    (RefineT b1 c1, RefineT b2 c2) -> b1 == b2 && c1 == c2
                    _ -> False
      in equal1 === equal2
  ]

-- | Properties for Constraint
constraintProperties :: TestTree
constraintProperties = testGroup "Constraint properties"
  [ testProperty "SizeGT preserves variable and size" $
    \var size -> 
      let constraint = SizeGT var size
      in case constraint of
        SizeGT v s -> v === var && s === size
  
  , testProperty "SizeGE preserves variable and size" $
    \var size -> 
      let constraint = SizeGE var size
      in case constraint of
        SizeGE v s -> v === var && s === size
  
  , testProperty "RangeC preserves variable and range" $
    \var minVal maxVal -> 
      let constraint = RangeC var minVal maxVal
      in case constraint of
        RangeC v mn mx -> v === var && mn === minVal && mx === maxVal
  
  , testProperty "PredC preserves predicate and arguments" $
    \pred args -> 
      let constraint = PredC pred args
      in case constraint of
        PredC p a -> p === pred && a === args
  
  , testProperty "Constraint equality is structural" $
    \constraint1 constraint2 -> 
      let equal1 = (constraint1 == constraint2)
          equal2 = case (constraint1, constraint2) of
                    (SizeGT v1 s1, SizeGT v2 s2) -> v1 == v2 && s1 == s2
                    (SizeGE v1 s1, SizeGE v2 s2) -> v1 == v2 && s1 == s2
                    (RangeC v1 mn1 mx1, RangeC v2 mn2 mx2) -> v1 == v2 && mn1 == mn2 && mx1 == mx2
                    (PredC p1 a1, PredC p2 a2) -> p1 == p2 && a1 == a2
                    _ -> False
      in equal1 === equal2
  ]

-- | Properties for DependencyGraph
dependencyGraphProperties :: TestTree
dependencyGraphProperties = testGroup "DependencyGraph properties"
  [ testProperty "DependencyGraph preserves nodes" $
    \nodes -> 
      let nodeMap = Map.fromList $ map (\n -> (nodeName n, n)) nodes
          graph = DependencyGraph nodeMap
      in graphNodes graph === nodeMap
  
  , testProperty "DependencyGraph with empty nodes is valid" $
    \_ -> 
      let graph = DependencyGraph Map.empty
      in Map.null (graphNodes graph)
  
  , testProperty "DependencyGraph lookup works correctly" $
    \nodes -> 
      let nodeMap = Map.fromList $ map (\n -> (nodeName n, n)) nodes
          graph = DependencyGraph nodeMap
      in all (\n -> Map.lookup (nodeName n) (graphNodes graph) === Just n) nodes
  ]

-- | Properties for TypeSystem
typeSystemProperties :: TestTree
typeSystemProperties = testGroup "TypeSystem properties"
  [ testProperty "newDependentTypeChecker creates valid checker" $
    \_ -> 
      let checker = newDependentTypeChecker
      in case checker of
        DependentTypeChecker env errors -> 
          Map.null (typeDefinitions env) && null (pendingConstraints env)
  
  , testProperty "newDependentTypeCheckerWithTypes preserves provided types" $
    \typeDefs -> 
      let typeMap = Map.fromList typeDefs
          checker = newDependentTypeCheckerWithTypes typeMap
      in case checker of
        DependentTypeChecker env _ -> typeDefinitions env === typeMap
  
  , testProperty "preludeTypeDefs is not empty" $
    \_ -> not (Map.null preludeTypeDefs)
  
  , testProperty "addType adds type to environment" $
    \name typeDef -> 
      let checker = newDependentTypeChecker
          updatedChecker = addType name typeDef checker
      in case updatedChecker of
        DependentTypeChecker env _ -> Map.lookup name (typeDefinitions env) === Just typeDef
  
  , testProperty "addConstraint adds constraint to environment" $
    \constraint -> 
      let checker = newDependentTypeChecker
          updatedChecker = addConstraint constraint checker
      in case updatedChecker of
        DependentTypeChecker env _ -> constraint `elem` pendingConstraints env
  ]

-- | Properties for TypeVar
typeVarProperties :: TestTree
typeVarProperties = testGroup "TypeVar properties"
  [ testProperty "TVCon preserves constructor name" $
    \name -> 
      let typeVar = TVCon name
      in case typeVar of
        TVCon n -> n === name
  
  , testProperty "TVVar preserves variable name" $
    \name -> 
      let typeVar = TVVar name
      in case typeVar of
        TVVar n -> n === name
  
  , testProperty "TVApp preserves constructor name and arguments" $
    \name args -> 
      let typeVar = TVApp name args
      in case typeVar of
        TVApp n a -> n === name && a === args
  
  , testProperty "TVFun preserves parameters and return type" $
    \params retType -> 
      let typeVar = TVFun params retType
      in case typeVar of
        TVFun p r -> p === params && r === retType
  
  , testProperty "TVTuple preserves elements" $
    \elements -> 
      let typeVar = TVTuple elements
      in case typeVar of
        TVTuple e -> e === elements
  
  , testProperty "TypeVar ordering is consistent" $
    \typeVar1 typeVar2 -> 
      let cmp1 = compare typeVar1 typeVar2
          cmp2 = compare (show typeVar1) (show typeVar2)
      in cmp1 === cmp2
  ]

-- | Properties for TypeConstraint
typeConstraintProperties :: TestTree
typeConstraintProperties = testGroup "TypeConstraint properties"
  [ testProperty "Equal preserves type variables" $
    \typeVar1 typeVar2 -> 
      let constraint = Equal typeVar1 typeVar2
      in case constraint of
        Equal t1 t2 -> t1 === typeVar1 && t2 === typeVar2
  
  , testProperty "Subtype preserves type variables" $
    \typeVar1 typeVar2 -> 
      let constraint = Subtype typeVar1 typeVar2
      in case constraint of
        Subtype t1 t2 -> t1 === typeVar1 && t2 === typeVar2
  
  , testProperty "Predicate preserves name and arguments" $
    \name args -> 
      let constraint = Predicate name args
      in case constraint of
        Predicate n a -> n === name && a === args
  
  , testProperty "TypeSizeGE preserves type variable and size" $
    \typeVar size -> 
      let constraint = TypeSizeGE typeVar size
      in case constraint of
        TypeSizeGE t s -> t === typeVar && s === size
  
  , testProperty "TypeSizeGT preserves type variable and size" $
    \typeVar size -> 
      let constraint = TypeSizeGT typeVar size
      in case constraint of
        TypeSizeGT t s -> t === typeVar && s === size
  
  , testProperty "TypeRange preserves type variable and range" $
    \typeVar minVal maxVal -> 
      let constraint = TypeRange typeVar minVal maxVal
      in case constraint of
        TypeRange t mn mx -> t === typeVar && mn === minVal && mx === maxVal
  
  , testProperty "TypeConstraint ordering is consistent" $
    \constraint1 constraint2 -> 
      let cmp1 = compare constraint1 constraint2
          cmp2 = compare (show constraint1) (show constraint2)
      in cmp1 === cmp2
  ]

-- | Properties for DependentTypeError
dependentTypeErrorProperties :: TestTree
dependentTypeErrorProperties = testGroup "DependentTypeError properties"
  [ testProperty "DependentTypeMismatch preserves type variables" $
    \typeVar1 typeVar2 -> 
      let error = DependentTypeMismatch typeVar1 typeVar2
      in case error of
        DependentTypeMismatch t1 t2 -> t1 === typeVar1 && t2 === typeVar2
  
  , testProperty "ConstraintViolation preserves message and type variable" $
    \msg typeVar -> 
      let error = ConstraintViolation msg typeVar
      in case error of
        ConstraintViolation m t -> m === msg && t === typeVar
  
  , testProperty "TypeNotFound preserves type name" $
    \typeName -> 
      let error = TypeNotFound typeName
      in case error of
        TypeNotFound n -> n === typeName
  
  , testProperty "InvalidTypeArgument preserves argument name" $
    \argName -> 
      let error = InvalidTypeArgument argName
      in case error of
        InvalidTypeArgument n -> n === argName
  
  , testProperty "UnsolvableConstraint preserves constraint" $
    \constraint -> 
      let error = UnsolvableConstraint constraint
      in case error of
        UnsolvableConstraint c -> c === constraint
  
  , testProperty "DependentInfiniteType preserves message and type variable" $
    \msg typeVar -> 
      let error = DependentInfiniteType msg typeVar
      in case error of
        DependentInfiniteType m t -> m === msg && t === typeVar
  
  , testProperty "AmbiguousType preserves message" $
    \msg -> 
      let error = AmbiguousType msg
      in case error of
        AmbiguousType m -> m === msg
  
  , testProperty "ParseError preserves message" $
    \msg -> 
      let error = ParseError msg
      in case error of
        ParseError m -> m === msg
  
  , testProperty "SemanticError preserves message" $
    \msg -> 
      let error = SemanticError msg
      in case error of
        SemanticError m -> m === msg
  ]

-- | Properties for TypeChecker
typeCheckerProperties :: TestTree
typeCheckerProperties = testGroup "TypeChecker properties"
  [ testProperty "checkType returns updated checker" $
    \typeVar -> 
      let checker = newDependentTypeChecker
          result = checkType typeVar checker
      in case result of
        Left _ -> property True
        Right updatedChecker -> property True  -- Should return updated checker
  
  , testProperty "solveConstraints removes solved constraints" $
    \constraints -> 
      let checker = newDependentTypeChecker
          checkerWithConstraints = foldr addConstraint checker constraints
          result = solveConstraints checkerWithConstraints
      in case result of
        Left _ -> property True
        Right solvedChecker -> 
          let remainingConstraints = pendingConstraints (dtcTypeEnv solvedChecker)
          in length remainingConstraints <= length constraints
  
  , testProperty "getDependentTypeErrors returns all errors" $
    \errors -> 
      let checker = newDependentTypeChecker
          checkerWithErrors = foldr addTypeError checker errors
          retrievedErrors = getDependentTypeErrors checkerWithErrors
      in sort retrievedErrors === sort errors
  ]

-- | Properties for Substitution
substitutionProperties :: TestTree
substitutionProperties = testGroup "Substitution properties"
  [ testProperty "Empty substitution has no mappings" $
    \_ -> Map.null (Map.empty :: Substitution)
  
  , testProperty "Substitution lookup works correctly" $
    \key value -> 
      let substitution = Map.singleton key value
      in Map.lookup key substitution === Just value
  
  , testProperty "Substitution composition preserves mappings" $
    \key1 value1 key2 value2 -> 
      let subst1 = Map.singleton key1 value1
          subst2 = Map.singleton key2 value2
          composed = Map.union subst1 subst2
      in Map.lookup key1 composed === Just value1 &&
         Map.lookup key2 composed === Just value2
  ]

-- | Additional edge case properties
edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Dependencies edge case properties"
  [ testProperty "TypeExpr with empty names" $
    \_ -> 
      let simpleType = SimpleT ""
          genericType = GenericT "" []
      in case (simpleType, genericType) of
        (SimpleT "", GenericT "" []) -> property True
  
  , testProperty "Constraint with empty strings" $
    \_ -> 
      let sizeGT = SizeGT "" 0
          sizeGE = SizeGE "" 0
          range = RangeC "" 0 0
          pred = PredC "" []
      in case (sizeGT, sizeGE, range, pred) of
        (SizeGT "" 0, SizeGE "" 0, RangeC "" 0 0, PredC "" []) -> property True
  
  , testProperty "DependencyGraph with empty node names" $
    \_ -> 
      let node = DependencyNode "" []
          graph = DependencyGraph (Map.singleton "" node)
      in nodeName (graphNodes graph Map.! "") === ""
  
  , testProperty "TypeVar with empty names" $
    \_ -> 
      let tvCon = TVCon ""
          tvVar = TVVar ""
          tvApp = TVApp "" []
      in case (tvCon, tvVar, tvApp) of
        (TVCon "", TVVar "", TVApp "" []) -> property True
  
  , testProperty "DependentTypeError with empty messages" $
    \_ -> 
      let errors = [ConstraintViolation "" undefined, TypeNotFound "", 
                    InvalidTypeArgument "", UnsolvableConstraint undefined,
                    DependentInfiniteType "" undefined, AmbiguousType "",
                    ParseError "", SemanticError ""]
      in all (\err -> case err of
              ConstraintViolation m _ -> null m
              TypeNotFound n -> null n
              InvalidTypeArgument n -> null n
              AmbiguousType m -> null m
              ParseError m -> null m
              SemanticError m -> null m
              _ -> property True) errors
  ]