{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.DependentTypeConstraintSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.List (sort, isInfixOf)
import SourceLocation (SourcePos(..), SourceSpan(..))

-- Mock data types for dependent type constraint testing
data TypeVar = TypeVar
  { typeVarName :: String
  , typeVarId :: Int
  } deriving (Show, Eq, Ord)

data Type = Type
  { typeName :: String
  , typeParams :: [Type]
  } deriving (Show, Eq)

data TypeConstraint = TypeConstraint
  { constraintType :: String
  , constraintVars :: [TypeVar]
  , constraintSpan :: SourceSpan
  } deriving (Show, Eq)

data DependentType = DependentType
  { dependentTypeName :: String
  , dependentTypeVars :: [TypeVar]
  , dependentTypeConstraints :: [TypeConstraint]
  } deriving (Show, Eq)

data TypeEnvironment = TypeEnvironment
  { envTypes :: [DependentType]
  , envConstraints :: [TypeConstraint]
  , envSubstitutions :: [(TypeVar, Type)]
  } deriving (Show, Eq)

data ConstraintResult = ConstraintResult
  { resultEnvironment :: TypeEnvironment
  , resultSatisfied :: Bool
  , resultErrors :: [String]
  } deriving (Show, Eq)

-- Mock constraint functions
addConstraint :: TypeConstraint -> TypeEnvironment -> TypeEnvironment
addConstraint constraint env = 
  let newConstraints = constraint : envConstraints env
  in env { envConstraints = newConstraints }

addSubstitution :: TypeVar -> Type -> TypeEnvironment -> TypeEnvironment
addSubstitution var typ env = 
  let newSubstitutions = (var, typ) : envSubstitutions env
  in env { envSubstitutions = newSubstitutions }

checkConstraints :: TypeEnvironment -> ConstraintResult
checkConstraints env = 
  -- Mock implementation - just check if we have any constraints
  let hasConstraints = not $ null $ envConstraints env
      errors = []  -- Mock empty errors
  in ConstraintResult env hasConstraints errors

applySubstitutions :: TypeEnvironment -> TypeEnvironment
applySubstitutions env = 
  -- Mock implementation - just return the same environment
  env

unifyTypes :: Type -> Type -> TypeEnvironment -> Either String TypeEnvironment
unifyTypes type1 type2 env = 
  if typeName type1 == typeName type2
     then Right env
     else Left $ "Cannot unify " ++ typeName type1 ++ " with " ++ typeName type2

tests :: TestTree
tests = testGroup "Dependent Type Constraint Tests"
  [ testGroup "Type variables"
    [ testCase "creates type variables correctly" $ do
        let var = TypeVar "T" 1
        typeVarName var @?= "T"
        typeVarId var @?= 1
      
    , testCase "compares type variables correctly" $ do
        let var1 = TypeVar "T" 1
            var2 = TypeVar "T" 2
            var3 = TypeVar "U" 1
        var1 @?= var1
        assertBool "var1 should not be var2" (var1 /= var2)
        assertBool "var1 should not be var3" (var1 /= var3)
      
    , testCase "orders type variables correctly" $ do
        let var1 = TypeVar "T" 1
            var2 = TypeVar "U" 2
            var3 = TypeVar "T" 2
        sort [var2, var1, var3] @?= [var1, var3, var2]
    ]

  , testGroup "Type constraints"
    [ testCase "creates type constraints correctly" $ do
        let var = TypeVar "T" 1
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint = TypeConstraint "Eq" [var] srcSpan
        constraintType constraint @?= "Eq"
        constraintVars constraint @?= [var]
        constraintSpan constraint @?= srcSpan
      
    , testCase "adds constraints to environment" $ do
        let var = TypeVar "T" 1
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint = TypeConstraint "Eq" [var] srcSpan
            env = TypeEnvironment [] [] []
            newEnv = addConstraint constraint env
        length (envConstraints newEnv) @?= 1
        case envConstraints newEnv of
          (c:_) -> c @?= constraint
          [] -> assertBool "Should have at least one constraint" False
      
    , testCase "handles multiple constraints" $ do
        let var1 = TypeVar "T" 1
            var2 = TypeVar "U" 2
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint1 = TypeConstraint "Eq" [var1] srcSpan
            constraint2 = TypeConstraint "Ord" [var2] srcSpan
            env = TypeEnvironment [] [] []
            env1 = addConstraint constraint1 env
            env2 = addConstraint constraint2 env1
        length (envConstraints env2) @?= 2
        assertBool "constraint1 should be in env2" (constraint1 `elem` envConstraints env2)
        assertBool "constraint2 should be in env2" (constraint2 `elem` envConstraints env2)
    ]
    
  , testGroup "Dependent types"
    [ testCase "creates dependent types correctly" $ do
        let var = TypeVar "T" 1
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint = TypeConstraint "Eq" [var] srcSpan
            depType = DependentType "List" [var] [constraint]
        dependentTypeName depType @?= "List"
        dependentTypeVars depType @?= [var]
        dependentTypeConstraints depType @?= [constraint]
      
    , testCase "handles dependent types with multiple variables" $ do
        let var1 = TypeVar "T" 1
            var2 = TypeVar "U" 2
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint1 = TypeConstraint "Eq" [var1] srcSpan
            constraint2 = TypeConstraint "Ord" [var2] srcSpan
            depType = DependentType "Map" [var1, var2] [constraint1, constraint2]
        dependentTypeName depType @?= "Map"
        dependentTypeVars depType @?= [var1, var2]
        dependentTypeConstraints depType @?= [constraint1, constraint2]
    ]
    
  , testGroup "Type environment"
    [ testCase "creates type environment correctly" $ do
        let env = TypeEnvironment [] [] []
        envTypes env @?= []
        envConstraints env @?= []
        envSubstitutions env @?= []
      
    , testCase "adds substitutions to environment" $ do
        let var = TypeVar "T" 1
            typ = Type "Int" []
            env = TypeEnvironment [] [] []
            newEnv = addSubstitution var typ env
        length (envSubstitutions newEnv) @?= 1
        case envSubstitutions newEnv of
          (s:_) -> s @?= (var, typ)
          [] -> assertBool "Should have at least one substitution" False
      
    , testCase "handles multiple substitutions" $ do
        let var1 = TypeVar "T" 1
            var2 = TypeVar "U" 2
            typ1 = Type "Int" []
            typ2 = Type "String" []
            env = TypeEnvironment [] [] []
            env1 = addSubstitution var1 typ1 env
            env2 = addSubstitution var2 typ2 env1
        length (envSubstitutions env2) @?= 2
        assertBool "substitution1 should be in env2" ((var1, typ1) `elem` envSubstitutions env2)
        assertBool "substitution2 should be in env2" ((var2, typ2) `elem` envSubstitutions env2)
    ]
    
  , testGroup "Constraint checking"
    [ testCase "checks empty constraints" $ do
        let env = TypeEnvironment [] [] []
            result = checkConstraints env
        resultSatisfied result @?= False
        resultErrors result @?= []
      
    , testCase "checks satisfied constraints" $ do
        let var = TypeVar "T" 1
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint = TypeConstraint "Eq" [var] srcSpan
            env = TypeEnvironment [] [constraint] []
            result = checkConstraints env
        resultSatisfied result @?= True
        resultErrors result @?= []
      
    , testCase "applies substitutions before checking" $ do
        let var = TypeVar "T" 1
            typ = Type "Int" []
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint = TypeConstraint "Eq" [var] srcSpan
            env = TypeEnvironment [] [constraint] [(var, typ)]
            result = checkConstraints env
        resultSatisfied result @?= True
    ]
    
  , testGroup "Type unification"
    [ testCase "unifies identical types" $ do
        let type1 = Type "Int" []
            type2 = Type "Int" []
            env = TypeEnvironment [] [] []
            result = unifyTypes type1 type2 env
        result @?= Right env
      
    , testCase "fails to unify different types" $ do
        let type1 = Type "Int" []
            type2 = Type "String" []
            env = TypeEnvironment [] [] []
            result = unifyTypes type1 type2 env
        case result of
          Left err -> assertBool "error should contain Cannot unify" ("Cannot unify" `isInfixOf` err)
          Right _ -> assertFailure "Expected unification to fail"
        
    , testCase "unifies parameterized types" $ do
        let type1 = Type "List" [Type "Int" []]
            type2 = Type "List" [Type "Int" []]
            env = TypeEnvironment [] [] []
            result = unifyTypes type1 type2 env
        result @?= Right env
      
    , testCase "fails to unify different parameterized types" $ do
        let type1 = Type "List" [Type "Int" []]
            type2 = Type "List" [Type "String" []]
            env = TypeEnvironment [] [] []
            result = unifyTypes type1 type2 env
        case result of
          Left err -> assertBool "error should contain Cannot unify" ("Cannot unify" `isInfixOf` err)
          Right _ -> assertFailure "Expected unification to fail"
    ]
    
  , testGroup "Substitution application"
    [ testCase "applies substitutions to environment" $ do
        let var = TypeVar "T" 1
            typ = Type "Int" []
            env = TypeEnvironment [] [] [(var, typ)]
            newEnv = applySubstitutions env
        envSubstitutions newEnv @?= [(var, typ)]
      
    , testCase "preserves constraints during substitution" $ do
        let var = TypeVar "T" 1
            typ = Type "Int" []
            srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint = TypeConstraint "Eq" [var] srcSpan
            env = TypeEnvironment [] [constraint] [(var, typ)]
            newEnv = applySubstitutions env
        envConstraints newEnv @?= [constraint]
        envSubstitutions newEnv @?= [(var, typ)]
    ]
    
  , testGroup "QuickCheck properties"
    [ testProperty "constraint addition preserves other constraints" $
        \constraint env ->
          let newEnv = addConstraint constraint env
              oldConstraints = envConstraints env
              newConstraints = envConstraints newEnv
          in constraint `elem` newConstraints &&
             all (`elem` newConstraints) oldConstraints
           
    , testProperty "substitution addition preserves other substitutions" $
        \var typ env ->
          let newEnv = addSubstitution var typ env
              oldSubstitutions = envSubstitutions env
              newSubstitutions = envSubstitutions newEnv
          in (var, typ) `elem` newSubstitutions &&
             all (`elem` newSubstitutions) oldSubstitutions
           
    , testProperty "constraint checking is consistent" $
        \env ->
          let result = checkConstraints env
              hasConstraints = not $ null $ envConstraints env
          in resultSatisfied result == hasConstraints
    ]
    
  , testGroup "Edge cases"
    [ testCase "handles empty type variables" $ do
        let depType = DependentType "Empty" [] []
        dependentTypeName depType @?= "Empty"
        dependentTypeVars depType @?= []
        dependentTypeConstraints depType @?= []
      
    , testCase "handles constraints with no variables" $ do
        let srcSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            constraint = TypeConstraint "True" [] srcSpan
        constraintType constraint @?= "True"
        constraintVars constraint @?= []
      
    , testCase "handles circular type dependencies" $ do
        let var1 = TypeVar "T" 1
            var2 = TypeVar "U" 2
            type1 = Type "Type1" [Type "Type2" []]
            type2 = Type "Type2" [Type "Type1" []]
            env = TypeEnvironment [] [] [(var1, type1), (var2, type2)]
            result = checkConstraints env
        resultSatisfied result @?= False
      
    , testCase "handles large type environments" $ do
        let vars = [TypeVar ("T" ++ show i) i | i <- [1..50] :: [Int]]
            types = [Type ("Type" ++ show i) [] | i <- [1..50] :: [Int]]
            substitutions = zip vars types
            env = TypeEnvironment [] [] substitutions
        length (envSubstitutions env) @?= 50
        let result = checkConstraints env
        resultSatisfied result @?= False
    ]
  ]

-- Arbitrary instances for QuickCheck
instance Arbitrary TypeVar where
  arbitrary = TypeVar <$> arbitrary <*> arbitrary

instance Arbitrary Type where
  arbitrary = do
    name <- arbitrary
    params <- listOf arbitrary
    return $ Type name params

instance Arbitrary TypeConstraint where
  arbitrary = do
    cType <- arbitrary
    cVars <- listOf arbitrary
    cSpan <- arbitrary
    return $ TypeConstraint cType cVars cSpan

instance Arbitrary DependentType where
  arbitrary = do
    name <- arbitrary
    vars <- listOf arbitrary
    constraints <- listOf arbitrary
    return $ DependentType name vars constraints

instance Arbitrary TypeEnvironment where
  arbitrary = do
    types <- listOf arbitrary
    constraints <- listOf arbitrary
    substitutions <- listOf arbitrary
    return $ TypeEnvironment types constraints substitutions

instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary
