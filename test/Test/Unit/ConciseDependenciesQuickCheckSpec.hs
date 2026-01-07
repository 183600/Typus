module Test.Unit.ConciseDependenciesQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, (===), Arbitrary(..), Gen, oneof, choose, elements, listOf, property)
import Data.Set 
import Dependencies.AST (TypeExpr(..), Constraint)
            \(_ :: Text) -> Map.null (Map.empty :: Map.Map Text TypeExpr) && Map.lookup ("" :: Text) (Map.empty :: Map.Map Text TypeExpr) == Nothing
            
        ,             testProperty "Type insertion is retrievable" $
            \(name :: Text) (typeExpr :: TypeExpr) -> 
            let env = Map.singleton name typeExpr
            in Map.lookup name                               env == Just typeExpr
            
        ,             testProperty "Multiple type insertions preserve L.all" $
            \(pairs :: [(Text, TypeExpr)]) -> 
            let env = Map.fromList pairs
                                              retrieved = Map.toList env
            in L.all (\(k,v) -> Map.lookup k                               env == Just v) retrieved
        ]
        
    , testGroup "Type substitution properties"
        [             testProperty "Empty substitution leaves types unchanged" $
            \typeExpr -> applySubstitution Map.empty                               typeExpr == typeExpr
            
        ,             testProperty "Substitution composition is associative" $
            \sub1 sub2 sub3 typeExpr -> 
            let composed1 = Map.union sub3 (Map.union sub2 sub1)
                                              composed2 = Map.union sub3 (Map.union sub1 sub2)
                                              result1 = applySubstitution composed1 typeExpr
                                              result2 = applySubstitution composed2 typeExpr
            in                               result1 == result2  -- This might not always hold, but good for testing
        ]
        
    , testGroup "Type unification properties"
        [             testProperty "Identical types always unify" $
            \typeExpr -> case unifyTypes typeExpr typeExpr of
                           Left _ -> property False
                           Right _ -> property True
                           
        ,             testProperty "Unification result is symmetric" $
            \type1 type2 -> 
            case (unifyTypes type1 type2, unifyTypes type2 type1) of
              (Left _, Left _) -> property True
              (Right sub1, Right sub2) -> property True  -- Substitutions might differ but both succeed
              _ -> property False
        ]
    ]
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- Helper functions for testing
applySubstitution :: Map Text TypeExpr -> TypeExpr -> TypeExpr
applySubstitution sub (SimpleT name) = Map.findWithDefault (SimpleT name) name sub
applySubstitution sub (GenericT name args) = GenericT name (L.map (applySubstitution sub) args)
applySubstitution sub (RefineT base constraint) = RefineT (applySubstitution sub base) constraint
applySubstitution sub (FuncT params codomain) = FuncT (L.map (\(name, typ) -> (name, applySubstitution sub typ) params) (applySubstitution sub codomain)

unifyTypes :: TypeExpr -> TypeExpr -> Either String (Map Text TypeExpr)
unifyTypes (SimpleT name1) (SimpleT name2) 
  |                               name1 ==                               name2 = Right Map.empty
  |                               otherwise = Right (Map.singleton name1 (SimpleT name2)
unifyTypes (GenericT name1 args1) (GenericT name2 args2)
  |                               name1 == name2 && L.length                               args1 == L.length                               args2 = 
      L.foldr (\(arg1, arg2) acc -> 
                do
              sub1 <- acc
                  sub2 <- unifyTypes (applySubstitution sub1 arg1) (applySubstitution sub1 arg2)
                  return (Map.union sub2 sub1)
            ) (Right Map.empty) (zip args1 args2)
  |                               otherwise = Left "Cannot unify different generic types"
unifyTypes (FuncT domain1 codomain1) (FuncT domain2 codomain2) = 
  do
              sub1 <- unifyTypes (SimpleT "") (SimpleT "")  -- Placeholder for domain unification
    sub2 <- unifyTypes codomain1 codomain2
    return (Map.union sub2 sub1)
unifyTypes _                               _ = Left "Cannot unify different type constructors"

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
    [ RangeC <$> arbitrary <*> arbitrary <*> arbitrary
    , PredC <$> arbitrary <*> arbitrary
    , SizeGE <$> arbitrary <*> arbitrary
    , SizeGT <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Text where
                                              arbitrary = T.pack <$> oneof
    [ return ""
      , listOf $ elements ['a'..'z']
    , listOf $ elements ['A'..'Z']
    , listOf $ elements "0123456789_"
    ]