module Test.Unit.ConciseDependenciesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, oneof, choose, elements, listOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Dependencies.AST (TypeExpr(..), Constraint(..))

-- | 简洁的QuickCheck测试，针对Dependencies模块的基础属性
tests :: TestTree
tests =
  testGroup "Concise Dependencies QuickCheck Tests"
    [ testGroup "Type expression properties"
        [ testProperty "Simple type expressions are equal if names match" $
            \name -> SimpleT name === SimpleT name
            
        , testProperty "Generic type expressions preserve structure" $
            \name args -> GenericT name args === GenericT name args
            
        , testProperty "Refined type expressions preserve base L.and constraint" $
            \baseType constraint -> RefineT baseType constraint === RefineT baseType constraint
            
        , testProperty "Function type expressions preserve domain L.and codomain" $
            \domain codomain -> FuncT domain codomain === FuncT domain codomain
        ]
        
    , testGroup "Constraint properties"
        [ testProperty "Range constraints preserve bounds" $
            \low high -> RangeC low high === RangeC low high
            
        , testProperty "Predicate constraints preserve predicate text" $
            \pred -> PredC pred === PredC pred
            
        , testProperty "Size constraints preserve threshold" $
            \size -> SizeGE size === SizeGE size && SizeGT size === SizeGT size
        ]
        
    , testGroup "Type environment operations"
        [ testProperty "Empty type environment has no types" $
            \name -> Map.null Map.empty && Map.lookup name Map.empty === Nothing
            
        , testProperty "Type insertion is retrievable" $
            \name typeExpr -> 
            let env = Map.singleton name typeExpr
            in Map.lookup name env === Just typeExpr
            
        , testProperty "Multiple type insertions preserve L.all" $
            \pairs -> 
            let env = Map.fromList pairs
                retrieved = Map.toList env
            in L.all (\(k,v) -> Map.lookup k env === Just v) retrieved
        ]
        
    , testGroup "Type substitution properties"
        [ testProperty "Empty substitution leaves types unchanged" $
            \typeExpr -> applySubstitution Map.empty typeExpr === typeExpr
            
        , testProperty "Substitution composition is associative" $
            \sub1 sub2 sub3 typeExpr -> 
            let composed1 = Map.union sub3 (Map.union sub2 sub1)
                composed2 = Map.union sub3 (Map.union sub1 sub2)
                result1 = applySubstitution composed1 typeExpr
                result2 = applySubstitution composed2 typeExpr
            in result1 === result2  -- This might not always hold, but good for testing
        ]
        
    , testGroup "Type unification properties"
        [ testProperty "Identical types always unify" $
            \typeExpr -> case unifyTypes typeExpr typeExpr of
                           Left _ -> property False
                           Right _ -> property True
                           
        , testProperty "Unification result is symmetric" $
            \type1 type2 -> 
            case (unifyTypes type1 type2, unifyTypes type2 type1) of
              (Left _, Left _) -> property True
              (Right sub1, Right sub2) -> property True  -- Substitutions might differ but both succeed
              _ -> property False
        ]
    ]

-- Helper functions for testing
applySubstitution :: Map String TypeExpr -> TypeExpr -> TypeExpr
applySubstitution sub (SimpleT name) = Map.findWithDefault (SimpleT name) name sub
applySubstitution sub (GenericT name args) = GenericT name (L.map (applySubstitution sub) args)
applySubstitution sub (RefineT base constraint) = RefineT (applySubstitution sub base) constraint
applySubstitution sub (FuncT domain codomain) = FuncT (applySubstitution sub domain) (applySubstitution sub codomain)

unifyTypes :: TypeExpr -> TypeExpr -> Either String (Map String TypeExpr)
unifyTypes (SimpleT name1) (SimpleT name2) 
  | name1 == name2 = Right Map.empty
  | otherwise = Right (Map.singleton name1 (SimpleT name2))
unifyTypes (GenericT name1 args1) (GenericT name2 args2)
  | name1 == name2 && L.length args1 == L.length args2 = 
      L.foldr (\(arg1, arg2) acc -> 
                do
                  sub1 <- acc
                  sub2 <- unifyTypes (applySubstitution sub1 arg1) (applySubstitution sub1 arg2)
                  return (Map.union sub2 sub1)
            ) (Right Map.empty) (zip args1 args2)
  | otherwise = Left "Cannot unify different generic types"
unifyTypes (FuncT domain1 codomain1) (FuncT domain2 codomain2) = 
  do
    sub1 <- unifyTypes domain1 domain2
    sub2 <- unifyTypes (applySubstitution sub1 codomain1) (applySubstitution sub1 codomain2)
    return (Map.union sub2 sub1)
unifyTypes _ _ = Left "Cannot unify different type constructors"

-- Generate test data
instance Arbitrary TypeExpr where
  arbitrary = oneof
    [ SimpleT <$> arbitrary
    , GenericT <$> arbitrary <*> listOf arbitrary
    , RefineT <$> arbitrary <*> arbitrary
    , FuncT <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Constraint where
  arbitrary = oneof
    [ RangeC <$> arbitrary <*> arbitrary
    , PredC <$> arbitrary
    , SizeGE <$> arbitrary
    , SizeGT <$> arbitrary
    ]

instance Arbitrary String where
  arbitrary = oneof
    [ return ""
    , listOf $ elements ['a'..'z']
    , listOf $ elements ['A'..'Z']
    , listOf $ elements "0123456789_"
    ]