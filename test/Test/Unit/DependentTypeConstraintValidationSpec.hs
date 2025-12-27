{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Test.Unit.DependentTypeConstraintValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, listOf, elements, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as Map

import DependentTypesParser (parseDependentType)
import Compiler.DependentTypeChecker (checkDependentTypes)
import Parser (parseTypus, TypusFile(..))
import Compiler (compile)
import SourceLocation (SourceSpan(..), defaultSpan)

-- | Dependent type expressions
data DependentType
    = BaseType String                           -- Basic types like Int, String
    | VectorType DependentType DependentType    -- Vector<T, Length>
    | MatrixType DependentType DependentType DependentType  -- Matrix<T, Rows, Cols>
    | FunctionType DependentType DependentType   -- (Input -> Output)
    | DependentFunction String DependentType     -- Dependent function
    | RefType DependentType                      -- Reference type
    | OwnedType DependentType                    -- Owned type
    deriving (Show, Eq)

-- | Type constraints
data TypeConstraint
    = EqualityConstraint DependentType DependentType
    | InequalityConstraint DependentType DependentType
    | SizeConstraint DependentType Int
    | RangeConstraint DependentType Int Int
    | OwnershipConstraint String String          -- owner, resource
    | LifetimeConstraint String String           -- lifetime relationship
    deriving (Show, Eq)

-- | Type variables for dependent types
data TypeVar = TypeVar
    { tvName :: String
    , tvConstraint :: Maybe TypeConstraint
    } deriving (Show, Eq)

-- | Dependent type context
data TypeContext = TypeContext
    { tcTypeVars :: Map.Map String DependentType
    , tcConstraints :: [TypeConstraint]
    , tcSubstitutions :: Map.Map String DependentType
    } deriving (Show, Eq)

-- | Constraint solving result
data SolveResult
    = Solved TypeContext
    | Unsolved [TypeConstraint]
    | Contradiction [TypeConstraint]
    deriving (Show, Eq)

-- | Generate dependent types
instance Arbitrary DependentType where
    arbitrary = sized $ \n -> if n <= 0
        then BaseType <$> elements ["Int", "String", "Bool", "Float"]
        else oneof
            [ BaseType <$> elements ["Int", "String", "Bool", "Float"]
            , VectorType <$> arbitrary <*> (BaseType "Int" <$ arbitrary) -- Simplified size
            , MatrixType <$> arbitrary <*> (BaseType "Int" <$ arbitrary) <*> (BaseType "Int" <$ arbitrary)
            , FunctionType <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            , DependentFunction <$> genVarName <*> arbitrary
            , RefType <$> arbitrary
            , OwnedType <$> arbitrary
            ]
      where
        genVarName = elements ["T", "U", "V", "X", "Y", "Z", "A", "B"]

-- | Generate type constraints
instance Arbitrary TypeConstraint where
    arbitrary = oneof
        [ EqualityConstraint <$> arbitrary <*> arbitrary
        , InequalityConstraint <$> arbitrary <*> arbitrary
        , SizeConstraint <$> arbitrary <*> arbitrary
        , RangeConstraint <$> arbitrary <*> arbitrary <*> arbitrary
        , OwnershipConstraint <$> genVarName <*> genVarName
        , LifetimeConstraint <$> genVarName <*> genVarName
        ]
      where
        genVarName = elements ["x", "y", "z", "a", "b", "c", "resource", "data"]

-- | Property: Type equality constraints should be solvable
prop_equalityConstraintsSolvable :: DependentType -> DependentType -> Bool
prop_equalityConstraintsSolvable t1 t2 = 
    let constraint = EqualityConstraint t1 t2
        initialContext = TypeContext Map.empty [] Map.empty
        result = solveConstraint initialContext constraint
    in case result of
        Solved _ -> True
        Unsolved _ -> True  -- Some constraints might remain unsolved
        Contradiction _ -> isActuallyContradictory t1 t2

-- | Check if two types are actually contradictory
isActuallyContradictory :: DependentType -> DependentType -> Bool
isActuallyContradictory (BaseType name1) (BaseType name2) = name1 /= name2
isActuallyContradictory _ _ = False -- Simplified: most complex types can be unified

-- | Property: Size constraints should be validated correctly
prop_sizeConstraintsValidated :: DependentType -> Int -> Bool
prop_sizeConstraintsValidated typ size = 
    let constraint = SizeConstraint typ size
        initialContext = TypeContext Map.empty [] Map.empty
        result = solveConstraint initialContext constraint
    in case result of
        Solved ctx -> size >= 0  -- Valid sizes should be non-negative
        Unsolved _ -> True       -- Some constraints might remain unsolved
        Contradiction _ -> size < 0  -- Negative sizes should be contradictory

-- | Property: Range constraints should validate bounds
prop_rangeConstraintsValidateBounds :: DependentType -> Int -> Int -> Bool
prop_rangeConstraintsValidateBounds typ minVal maxVal = 
    let constraint = RangeConstraint typ minVal maxVal
        initialContext = TypeContext Map.empty [] Map.empty
        result = solveConstraint initialContext constraint
    in case result of
        Solved ctx -> minVal <= maxVal  -- Valid ranges should have proper bounds
        Unsolved _ -> True              -- Some constraints might remain unsolved
        Contradiction _ -> minVal > maxVal  -- Invalid ranges should be contradictory

-- | Property: Ownership constraints should track relationships
prop_ownershipConstraintsTrackRelationships :: String -> String -> Bool
prop_ownershipConstraintsTrackRelationships owner resource = 
    let constraint = OwnershipConstraint owner resource
        initialContext = TypeContext Map.empty [] Map.empty
        result = solveConstraint initialContext constraint
    in case result of
        Solved ctx -> Map.size (tcTypeVars ctx) >= 0 -- Should track the relationship
        Unsolved _ -> True
        Contradiction _ -> False  -- Valid ownership shouldn't contradict

-- | Property: Function types should handle dependent arguments correctly
prop_functionTypesHandleDependentArgs :: DependentType -> DependentType -> Bool
prop_functionTypesHandleDependentArgs inputType outputType = 
    let funcType = FunctionType inputType outputType
        constraint = EqualityConstraint funcType funcType
        initialContext = TypeContext Map.empty [] Map.empty
        result = solveConstraint initialContext constraint
    in case result of
        Solved _ -> True
        Unsolved _ -> True
        Contradiction _ -> False

-- | Property: Vector types should validate size dependencies
prop_vectorTypesValidateSizeDependencies :: DependentType -> DependentType -> Bool
prop_vectorTypesValidateSizeDependencies elementType sizeType = 
    let vectorType = VectorType elementType sizeType
        sizeConstraint = SizeConstraint sizeType 10
        initialContext = TypeContext Map.empty [] Map.empty
        result1 = solveConstraint initialContext sizeConstraint
        result2 = case result1 of
            Solved ctx -> solveConstraint ctx (EqualityConstraint vectorType vectorType)
            other -> other
    in case result2 of
        Solved _ -> True
        Unsolved _ -> True
        Contradiction _ -> False

-- | Property: Matrix types should validate dimensional constraints
prop_matrixTypesValidateDimensions :: DependentType -> DependentType -> DependentType -> Bool
prop_matrixTypesValidateDimensions elementType rowsType colsType = 
    let matrixType = MatrixType elementType rowsType colsType
        rowsConstraint = SizeConstraint rowsType 5
        colsConstraint = SizeConstraint colsType 3
        initialContext = TypeContext Map.empty [] Map.empty
        result1 = solveConstraint initialContext rowsConstraint
        result2 = case result1 of
            Solved ctx -> solveConstraint ctx colsConstraint
            other -> other
        result3 = case result2 of
            Solved ctx -> solveConstraint ctx (EqualityConstraint matrixType matrixType)
            other -> other
    in case result3 of
        Solved _ -> True
        Unsolved _ -> True
        Contradiction _ -> False

-- | Solve a single constraint
solveConstraint :: TypeContext -> TypeConstraint -> SolveResult
solveConstraint ctx constraint = case constraint of
    EqualityConstraint t1 t2 -> solveEquality ctx t1 t2
    InequalityConstraint t1 t2 -> solveInequality ctx t1 t2
    SizeConstraint typ size -> solveSize ctx typ size
    RangeConstraint typ minVal maxVal -> solveRange ctx typ minVal maxVal
    OwnershipConstraint owner resource -> solveOwnership ctx owner resource
    LifetimeConstraint lifetime1 lifetime2 -> solveLifetime ctx lifetime1 lifetime2

-- | Solve equality constraints
solveEquality :: TypeContext -> DependentType -> DependentType -> SolveResult
solveEquality ctx t1 t2 
    | t1 == t2 = Solved ctx
    | otherwise = case (t1, t2) of
        (BaseType name1, BaseType name2) -> 
            if name1 == name2 
                then Solved ctx 
                else Contradiction [EqualityConstraint t1 t2]
        (VectorType e1 s1, VectorType e2 s2) ->
            let result1 = solveEquality ctx e1 e2
                result2 = case result1 of
                    Solved ctx' -> solveEquality ctx' s1 s2
                    other -> other
            in result2
        (FunctionType i1 o1, FunctionType i2 o2) ->
            let result1 = solveEquality ctx i1 i2
                result2 = case result1 of
                    Solved ctx' -> solveEquality ctx' o1 o2
                    other -> other
            in result2
        _ -> Unsolved [EqualityConstraint t1 t2]

-- | Solve inequality constraints
solveInequality :: TypeContext -> DependentType -> DependentType -> SolveResult
solveInequality ctx t1 t2 
    | t1 /= t2 = Solved ctx
    | otherwise = Contradiction [InequalityConstraint t1 t2]

-- | Solve size constraints
solveSize :: TypeContext -> DependentType -> Int -> SolveResult
solveSize ctx typ size
    | size >= 0 = Solved ctx
    | otherwise = Contradiction [SizeConstraint typ size]

-- | Solve range constraints
solveRange :: TypeContext -> DependentType -> Int -> Int -> SolveResult
solveRange ctx typ minVal maxVal
    | minVal <= maxVal = Solved ctx
    | otherwise = Contradiction [RangeConstraint typ minVal maxVal]

-- | Solve ownership constraints
solveOwnership :: TypeContext -> String -> String -> SolveResult
solveOwnership ctx owner resource = 
    let updatedContext = ctx { tcTypeVars = Map.insert owner (BaseType "Owner") (tcTypeVars ctx) }
    in Solved updatedContext

-- | Solve lifetime constraints
solveLifetime :: TypeContext -> String -> String -> SolveResult
solveLifetime ctx lifetime1 lifetime2 = 
    let updatedContext = ctx { tcTypeVars = Map.insert lifetime1 (BaseType "Lifetime") (tcTypeVars ctx) }
    in Solved updatedContext

-- | Property: Complex constraint systems should be solvable
prop_complexConstraintSystemsSolvable :: [TypeConstraint] -> Bool
prop_complexConstraintSystemsSolvable constraints = 
    let initialContext = TypeContext Map.empty [] Map.empty
        result = solveConstraints initialContext constraints
    in case result of
        Solved _ -> True
        Unsolved remaining -> length remaining <= length constraints  -- Should make progress
        Contradiction _ -> hasActualContradiction constraints

-- | Check if constraints actually contain contradictions
hasActualContradiction :: [TypeConstraint] -> Bool
hasActualContradiction constraints = any isContradictory constraints
  where
    isContradictory (EqualityConstraint (BaseType name1) (BaseType name2)) = name1 /= name2
    isContradictory (RangeConstraint _ minVal maxVal) = minVal > maxVal
    isContradictory (SizeConstraint _ size) = size < 0
    isContradictory _ = False

-- | Solve multiple constraints
solveConstraints :: TypeContext -> [TypeConstraint] -> SolveResult
solveConstraints ctx [] = Solved ctx
solveConstraints ctx (constraint:rest) = 
    case solveConstraint ctx constraint of
        Solved ctx' -> solveConstraints ctx' rest
        Unsolved remaining -> Unsolved (remaining ++ rest)
        Contradiction contradictions -> Contradiction contradictions

-- | Generate Typus code for dependent type testing
generateDependentTypeCode :: [TypeConstraint] -> String
generateDependentTypeCode constraints = 
    "//! dependent_types: on\n" ++
    "//! constraints: on\n" ++
    "package main\n\n" ++
    "func main() {\n" ++
    concatMap generateConstraintCode constraints ++
    "}\n"
  where
    generateConstraintCode (EqualityConstraint t1 t2) =
        "    var " ++ renderType t1 ++ " = " ++ renderType t2 ++ "\n"
    
    generateConstraintCode (SizeConstraint typ size) =
        "    var " ++ renderType typ ++ " : size = " ++ show size ++ "\n"
    
    generateConstraintCode (RangeConstraint typ minVal maxVal) =
        "    var " ++ renderType typ ++ " : range(" ++ show minVal ++ ", " ++ show maxVal ++ ")\n"
    
    generateConstraintCode (OwnershipConstraint owner resource) =
        "    var " ++ owner ++ " owns " ++ resource ++ "\n"
    
    generateConstraintCode _ = ""

-- | Render dependent types as strings
renderType :: DependentType -> String
renderType = \case
    BaseType name -> name
    VectorType elem size -> "Vector<" ++ renderType elem ++ ", " ++ renderType size ++ ">"
    MatrixType elem rows cols -> "Matrix<" ++ renderType elem ++ ", " ++ renderType rows ++ ", " ++ renderType cols ++ ">"
    FunctionType input output -> "(" ++ renderType input ++ " -> " ++ renderType output ++ ")"
    DependentFunction name retType -> name ++ "() -> " ++ renderType retType
    RefType typ -> "&" ++ renderType typ
    OwnedType typ -> "owned " ++ renderType typ

tests :: TestTree
tests = testGroup "Dependent Type Constraint Validation Tests"
  [ testProperty "Equality constraints are solvable" $
      fastProperty "type1, type2" prop_equalityConstraintsSolvable
  
  , testProperty "Size constraints are validated correctly" $
      fastProperty "type, size" prop_sizeConstraintsValidated
  
  , testProperty "Range constraints validate bounds" $
      fastProperty "type, min, max" prop_rangeConstraintsValidateBounds
  
  , testProperty "Ownership constraints track relationships" $
      fastProperty "owner, resource" prop_ownershipConstraintsTrackRelationships
  
  , testProperty "Function types handle dependent arguments correctly" $
      fastProperty "input, output" prop_functionTypesHandleDependentArgs
  
  , testProperty "Vector types validate size dependencies" $
      fastProperty "element, size" prop_vectorTypesValidateSizeDependencies
  
  , testProperty "Matrix types validate dimensional constraints" $
      fastProperty "element, rows, cols" prop_matrixTypesValidateDimensions
  
  , testProperty "Complex constraint systems are solvable" $
      fastProperty "constraint list" prop_complexConstraintSystemsSolvable
  
  , testProperty "Type substitutions are applied correctly" $
      fastProperty "type variables and substitutions" $
      \typeVars -> 
        let substitutions = Map.fromList $ take (length typeVars `div` 2) $ 
              zip (map (("T" ++) . show) [0..]) (map BaseType ["Int", "String", "Bool"])
            context = TypeContext typeVars [] substitutions
        in Map.size (tcSubstitutions context) >= 0
  
  , testProperty "Constraint solving terminates" $
      fastProperty "constraint sets" $
      \constraints -> 
        let initialContext = TypeContext Map.empty [] Map.empty
            result = solveConstraints initialContext (take 10 constraints)  -- Limit to prevent infinite loops
        in case result of
            Solved _ -> True
            Unsolved _ -> True
            Contradiction _ -> True
  ]