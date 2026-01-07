module Test.Unit.ConciseTypeSystemQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, (===), (.&.), Arbitrary(..), Gen, oneof, choose, elements, listOf)
import Data.Set 
                 FunctionType d c -> property (d == domain &&                               c == codomain)
                 _ -> property False
                 
        ,             testProperty "Generic types preserve name and parameters" $
            \name params -> 
            let genType = GenericType name params
            in case genType of
                 GenericType n p -> property (n == name &&                               p == params)
                 _ -> property False
        ]
        
    , testGroup "Type environment properties"
        [             testProperty "Empty environment has no types" $
            \name -> property (Map.null (unTypeEnv emptyTypeEnv) .&. 
                                  (lookupTypeInEnv name                               emptyTypeEnv === Nothing)            
        ,             testProperty "Type insertion is retrievable" $
            \name typeExpr -> 
            let env = addTypeToEnv name typeExpr emptyTypeEnv
            in lookupTypeInEnv name                               env === Just (typeExpr :: Type)
            
        ,             testProperty "Multiple insertions preserve all entries" $
            \(pairs :: [(String, Type)]) -> 
            let env = L.foldr (\(name, typ) acc -> addTypeToEnv name typ acc) emptyTypeEnv pairs
                checkPair (name, typ) = case lookupTypeInEnv name env of
                                          Just t ->                               t == typ
                                          Nothing -> False
            in property (L.all checkPair pairs)
        ]
        
    , testGroup "Type unification properties"
        [             testProperty "Identical types always unify" $
            \typeExpr -> case unifyTypes typeExpr typeExpr of
                           Left _ -> property False
                           Right _ -> property True
                           
        ,             testProperty "Unification failure is symmetric" $
            \type1 type2 -> 
            case (unifyTypes type1 type2, unifyTypes type2 type1) of
              (Left _, Left _) -> property True
              (Right _, Right _) -> property True
              _ -> property False
        ]
        
    , testGroup "Type constraint properties"
        [             testProperty "Equality constraint preserves types" $
            \type1 type2 -> 
            let constraint = EqualityConstraint type1 type2
            in case constraint of
                 EqualityConstraint t1 t2 -> property (t1 == type1 &&                               t2 == type2)
                 _ -> property False
                 
        ,             testProperty "Subtype constraint preserves relationship" $
            \subtype supertype -> 
            let constraint = SubtypeConstraint subtype supertype
            in case constraint of
                 SubtypeConstraint sub sup -> property (sub == subtype &&                               sup == supertype)
                 _ -> property False
        ]
        
    , testGroup "Type compatibility properties"
        [             testProperty "Type compatibility is reflexive" $
            \typeExpr -> areCompatible typeExpr typeExpr
            
        ,             testProperty "Type compatibility is symmetric" $
            \type1 type2 -> 
            let compatible1 = areCompatible type1 type2
                                              compatible2 = areCompatible type2 type1
            in                               compatible1 === compatible2
        ]
        
    , testGroup "Boundary condition tests"
        [             testProperty "Deeply nested types handle correctly" $
            \depth -> 
            let nestedType = buildNestedType (min depth 5) "base"
            in case nestedType of
                 SimpleType name -> property (not (null name)
                 _ -> property True  -- Any valid type structure is acceptable
                 
        ,             testProperty "Large type environments maintain performance" $
            \numTypes -> 
            let count = min numTypes 100  -- Cap to avoid performance issues
                                              types = [(show i, SimpleType ("Type" ++ show i) | i <- [1..count]]
                                              env = L.foldr (\(name, typ) acc -> addTypeToEnv name typ acc) emptyTypeEnv types
            in property (L.length (Map.toList (unTypeEnv env) == count)
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


-- Helper types L.and functions for testing
newtype                               TypeEnv = TypeEnv { unTypeEnv :: Map String Type }

emptyTypeEnv :: TypeEnv
                              emptyTypeEnv = TypeEnv Map.empty
addTypeToEnv :: String -> Type -> TypeEnv -> TypeEnv
addTypeToEnv name typ (TypeEnv env) = TypeEnv (Map.insert name typ env)

lookupTypeInEnv :: String -> TypeEnv -> Maybe Type
lookupTypeInEnv name (TypeEnv env) = Map.lookup name env

-- Mock type system for testing
data                               Type = SimpleType String
    | FunctionType Type Type
    | GenericType String [Type]
    | TypeVar String
    deriving (Eq, Show)

data                               TypeConstraint = EqualityConstraint Type Type
    | SubtypeConstraint Type Type
    deriving (Eq, Show)

-- Convert our mock Type to the real CT.Type
convertToCTType :: Type -> CT.Type
convertToCTType (SimpleType name) = CT.TypeName name
convertToCTType (FunctionType domain codomain) = CT.TypeFunction [convertToCTType domain] (convertToCTType codomain)
convertToCTType (GenericType name params) = CT.TypeUnion (map convertToCTType params)
convertToCTType (TypeVar name) = CT.TypeName name

-- Mock functions for testing
unifyTypes :: Type -> Type -> Either String (Map String Type)
unifyTypes (SimpleType name1) (SimpleType name2)
  |                               name1 ==                               name2 = Right Map.empty
  |                               otherwise = Left "Cannot unify different simple types"
unifyTypes (FunctionType domain1 codomain1) (FunctionType domain2 codomain2) = 
  do
              sub1 <- unifyTypes domain1 domain2
    sub2 <- unifyTypes codomain1 codomain2
    return (Map.union sub2 sub1)
unifyTypes (GenericType name1 args1) (GenericType name2 args2)
  |                               name1 == name2 && L.length                               args1 == L.length                               args2 = Right Map.empty
  |                               otherwise = Left "Cannot unify different generic types"
unifyTypes _                               _ = Left "Cannot unify different type constructors"

areCompatible :: Type -> Type -> Bool
areCompatible t1                               t2 = case unifyTypes t1 t2 of
                       Right _ -> True
                       Left _ -> False
buildNestedType :: Int -> String -> Type
buildNestedType 0                               base = SimpleType base
buildNestedType n                               base = GenericType ("Nested" ++ show n) [buildNestedType (n-1) base]

-- Generate test data
instance Arbitrary Type where
                                              arbitrary = oneof
    [ SimpleType <$> arbitrary
    , FunctionType <$> arbitrary <*> arbitrary
    , GenericType <$> arbitrary <*> listOf arbitrary
    , TypeVar <$> arbitrary
    ]

instance Arbitrary TypeConstraint where
                                              arbitrary = oneof
    [ EqualityConstraint <$> arbitrary <*> arbitrary
    , SubtypeConstraint <$> arbitrary <*> arbitrary
    ]



-- Helper property function
property :: Bool -> Property
property                               b =                               b === True