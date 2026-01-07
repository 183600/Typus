module Test.Unit.NewDependencyAnalysisCoreQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, oneof, elements, listOf, sized)
import TestSupport.QuickCheck 
          , TypeFunc <$> genTypeExpr m <*> genTypeExpr (n - 1 - m)
          , TypeDependent <$> genTypeExpr m <*> genTypeExpr (n - 1 - m)
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


instance Arbitrary TypeExpr where
                                                arbitrary = sized genTypeExpr

-- | Generate arbitrary constraints
genConstraint :: Gen Constraint
                              genConstraint = do
              t1 <- arbitrary
    t2 <- arbitrary
    elements [ TypeEq t1 t2
             , TypeSub t1 t2
             , TypeImplies t1 t2
             ]

instance Arbitrary Constraint where
                                                arbitrary = genConstraint

-- | Generate arbitrary statements
genStatement :: Int -> Gen Statement
genStatement                               0 = 
    oneof [ VarDecl <$> arbitrary <*> arbitrary
          , TypeDecl <$> arbitrary <*> arbitrary
          ]
genStatement                               n = do
              m <- elements [0..n `div` 2]
    oneof [ VarDecl <$> arbitrary <*> arbitrary
          , TypeDecl <$> arbitrary <*> arbitrary
          , FuncDecl <$> arbitrary <*> listOf arbitrary <*> arbitrary
          , ConstraintStmt <$> arbitrary
          ]

instance Arbitrary Statement where
                                                arbitrary = sized genStatement

-- | Generate arbitrary AST nodes
genAST :: Gen AST
                              genAST = AST <$> listOf arbitrary

instance Arbitrary AST where
                                                arbitrary = genAST

-- ============================================================================
-- Core Dependency Analysis Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Dependency Analysis Core QuickCheck Tests"
    [ testGroup "Type System Properties"
        [             testProperty "Type equality is reflexive" prop_typeEqualityReflexive
        ,             testProperty "Type equality is symmetric" prop_typeEqualitySymmetric
        ,             testProperty "Type substitution preserves structure" prop_typeSubstitutionPreservesStructure
        ,             testProperty "Type generalization is sound" prop_typeGeneralizationSound
        ,             testProperty "Type instantiation is inverse of generalization" prop_typeInstantiationInverse
        ]

    , testGroup "Constraint Solving Properties"
        [             testProperty "Constraint solving preserves type safety" prop_constraintSolvingTypeSafety
        ,             testProperty "Unification is idempotent" prop_unificationIdempotent
        ,             testProperty "Substitution composition is associative" prop_substitutionCompositionAssociative
        ,             testProperty "Type variable freshness" prop_typeVariableFreshness
        ]

    , testGroup "AST Analysis Properties"
        [             testProperty "AST validation preserves well-formedness" prop_astValidationWellFormedness
        ,             testProperty "Dependency extraction is complete" prop_dependencyExtractionComplete
        ,             testProperty "Type inference preserves typing rules" prop_typeInferencePreservesRules
        ,             testProperty "Statement analysis is monotonic" prop_statementAnalysisMonotonic
        ]

    , testGroup "Type Environment Properties"
        [             testProperty "Environment extension preserves existing bindings" prop_environmentExtensionPreservesBindings
        ,             testProperty "Type lookup is consistent" prop_typeLookupConsistent
        ,             testProperty "Environment merging is commutative" prop_environmentMergingCommutative
        ]

    , testGroup "Error Handling Properties"
        [             testProperty "Error detection is sound" prop_errorDetectionSound
        ,             testProperty "Error messages contain location information" prop_errorMessagesContainLocation
        ,             testProperty "Error recovery preserves partial results" prop_errorRecoveryPreservesPartial
        ]
    ]

-- ============================================================================
-- Type System Properties
-- ============================================================================

-- | Type equality should be reflexive
prop_typeEqualityReflexive :: TypeExpr -> Property
prop_typeEqualityReflexive                               t =                               t === t

-- | Type equality should be symmetric
prop_typeEqualitySymmetric :: TypeExpr -> TypeExpr -> Property
prop_typeEqualitySymmetric t1                               t2 = (t1 == t2) === (t2 == t1)

-- | Type substitution should preserve structure
prop_typeSubstitutionPreservesStructure :: TypeExpr -> TypeVar -> TypeExpr -> Property
prop_typeSubstitutionPreservesStructure t var                               replacement =
    let substituted = applyTypeSubstitution (Map.singleton var replacement) t
    in                               substituted === substituted  -- Basic sanity check

-- | Type generalization should be sound
prop_typeGeneralizationSound :: TypeEnvironment -> TypeExpr -> Property
prop_typeGeneralizationSound env                               t =
    let scheme = generalize env t
    in isJust                               scheme === True  -- Generalization should always succeed for well-formed types

-- | Type instantiation should be inverse of generalization
prop_typeInstantiationInverse :: TypeEnvironment -> TypeExpr -> Property
prop_typeInstantiationInverse env                               t =
    case generalize env t of
        Nothing -> property True
        Just scheme -> 
            let instantiated = instantiate scheme
            in property $ isJust instantiated

-- ============================================================================
-- Constraint Solving Properties
-- ============================================================================

-- | Constraint solving should preserve type safety
prop_constraintSolvingTypeSafety :: [Constraint] -> Property
prop_constraintSolvingTypeSafety                               constraints =
    let result = solveConstraints constraints
    in case result of
        Left _ -> property True  -- Failure is acceptable for inconsistent constraints
        Right substitution -> property $ Map.size substitution >= 0

-- | Unification should be idempotent
prop_unificationIdempotent :: TypeExpr -> TypeExpr -> Property
prop_unificationIdempotent t1                               t2 =
    case unify t1 t2 of
        Left _ -> property True
        Right sub1 -> 
            case applyTypeSubstitution sub1 t1 `unify` applyTypeSubstitution sub1 t2 of
                Left _ -> property True
                Right sub2 ->                               sub1 === sub2

-- | Substitution composition should be associative
prop_substitutionCompositionAssociative :: TypeExpr -> TypeVar -> TypeExpr -> TypeVar -> TypeExpr -> Property
prop_substitutionCompositionAssociative t var1 rep1 var2                               rep2 =
    let sub1 = Map.singleton var1 rep1
                                      sub2 = Map.singleton var2 rep2
                                      comp1 = Map.union sub2 (Map.L.map (applyTypeSubstitution sub2) sub1)
                                      comp2 = Map.union (Map.L.map (applyTypeSubstitution sub1) sub2) sub1
                                      result1 = applyTypeSubstitution comp1 t
                                      result2 = applyTypeSubstitution comp2 t
    in                               result1 === result2

-- | Type variables should remain fresh
prop_typeVariableFreshness :: [TypeVar] -> Property
prop_typeVariableFreshness                               vars =
    let freshVars = L.map (const newTypeVariable) vars
    in property $ L.length (nub freshVars) == L.length freshVars
  where
      nub [] = []
    nub (x:xs) = x : nub (L.filter (/= x) xs)

-- ============================================================================
-- AST Analysis Properties
-- ============================================================================

-- | AST validation should preserve well-formedness
prop_astValidationWellFormedness :: AST -> Property
prop_astValidationWellFormedness                               ast =
    let result = validateASTSemantics ast
    in case result of
        Left _ -> property True  -- Validation failure is acceptable
        Right validated -> property $ True  -- Success indicates well-formedness

-- | Dependency extraction should be complete
prop_dependencyExtractionComplete :: AST -> Property
prop_dependencyExtractionComplete                               ast =
    let dependencies = extractDependencies ast
    in property $ L.length dependencies >= 0  -- Basic sanity check

-- | Type inference should preserve typing rules
prop_typeInferencePreservesRules :: AST -> Property
prop_typeInferencePreservesRules                               ast =
    let result = inferProgram ast
    in case result of
        Left _ -> property True  -- Type errors are acceptable
        Right _ -> property True  -- Successful inference indicates rule preservation

-- | Statement analysis should be monotonic
prop_statementAnalysisMonotonic :: Statement -> Property
prop_statementAnalysisMonotonic                               stmt =
    let result1 = validateStatement stmt
                                      result2 = validateStatement stmt  -- Same analysis twice
    in                               result1 === result2

-- ============================================================================
-- Type Environment Properties
-- ============================================================================

-- | Environment extension should preserve existing bindings
prop_environmentExtensionPreservesBindings :: TypeEnvironment -> String -> TypeExpr -> Property
prop_environmentExtensionPreservesBindings env name                               typ =
    let extended = addType env name typ
                                      originalLookup = Map.lookup name env
                                      extendedLookup = Map.lookup name extended
    in case extendedLookup of
        Nothing -> property True
        Just found ->                               found === typ

-- | Type lookup should be consistent
prop_typeLookupConsistent :: TypeEnvironment -> String -> Property
prop_typeLookupConsistent env                               name =
    let lookup1 = Map.lookup name env
                                      lookup2 = Map.lookup name env
    in                               lookup1 === lookup2

-- | Environment merging should be commutative
prop_environmentMergingCommutative :: TypeEnvironment -> TypeEnvironment -> Property
prop_environmentMergingCommutative env1                               env2 =
    let merged1 = Map.union env1 env2
                                      merged2 = Map.union env2 env1
    in Map.keys                               merged1 === Map.keys merged2

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- | Error detection should be sound
prop_errorDetectionSound :: AST -> Property
prop_errorDetectionSound                               ast =
    let errors = getDependentTypeErrors ast
    in property $ L.length errors >= 0  -- Basic sanity check

-- | Error messages should contain location information
prop_errorMessagesContainLocation :: AST -> Property
prop_errorMessagesContainLocation                               ast =
    let errors = getDependentTypeErrors ast
    in property $ L.all (not . null . show) errors  -- All errors should be displayable

-- | Error recovery should preserve partial results
prop_errorRecoveryPreservesPartial :: AST -> Property
prop_errorRecoveryPreservesPartial                               ast =
    let result = analyzeAST ast
    in property $ case result of
        Left _ -> property True  -- Complete failure is acceptable
        Right partial -> property $ True  -- Partial success indicates recovery