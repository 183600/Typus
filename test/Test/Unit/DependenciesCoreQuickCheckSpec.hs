module Test.Unit.DependenciesCoreQuickCheckSpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Dependencies.TypeSystem
import Dependencies.AST (TypeExpr(..), Constraint)
  return (first:rest)

-- Generate type constructor names
genTypeConName :: Gen String
                              genTypeConName = do
              first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return (first:rest)

-- Generate simple TypeVars
genTypeVar :: Gen TypeVar
                              genTypeVar = oneof
  [ TVCon <$> genTypeConName
  , TVVar <$> genTypeVarName
  ]

-- Generate TypeVar applications
genTypeVarApp :: Gen TypeVar
                              genTypeVarApp = do
              name <- genTypeConName
  args <- listOf1 genTypeVar
  return $ TVApp name args

-- Generate function types
genTypeVarFun :: Gen TypeVar
                              genTypeVarFun = do
              params <- listOf1 genTypeVar
  returnType <- genTypeVar
  return $ TVFun params returnType

-- Generate tuple types
genTypeVarTuple :: Gen TypeVar
                              genTypeVarTuple = do
              elements <- listOf1 genTypeVar
  return $ TVTuple elements

-- Generate L.any TypeVar
genAnyTypeVar :: Gen TypeVar
                              genAnyTypeVar = oneof
  [ genTypeVar
  , genTypeVarApp
  , genTypeVarFun
  , genTypeVarTuple
  ]

-- Generate type constraints
genTypeConstraint :: Gen TypeConstraint
                              genTypeConstraint = oneof
  [ Equal <$> genAnyTypeVar <*> genAnyTypeVar
  , Subtype <$> genAnyTypeVar <*> genAnyTypeVar
  , Predicate <$> genTypeConName <*> listOf1 genAnyTypeVar
  , TypeSizeGE <$> genAnyTypeVar <*> choose (0, 100)
  , TypeSizeGT <$> genAnyTypeVar <*> choose (0, 100)
  , TypeRange <$> genAnyTypeVar <*> choose (0, 50) <*> choose (51, 100)
  ]

-- Generate type definitions
genTypeDef :: Gen (String, [String], [TypeConstraint])
                              genTypeDef = do
              name <- genTypeConName
  params <- listOf genTypeVarName
  constraints <- listOf genTypeConstraint
  return (name, params, constraints)

-- ============================================================================
-- TypeVar Properties
-- ============================================================================

prop_typeVarEqReflexive :: TypeVar -> Property
prop_typeVarEqReflexive                               tv =
  counterexample "TypeVar equality should be reflexive" $
                                  tv === tv

prop_typeVarEqSymmetric :: TypeVar -> TypeVar -> Property
prop_typeVarEqSymmetric tv1                               tv2 =
  counterexample "TypeVar equality should be symmetric" $
    (tv1 === tv2) === (tv2 === tv1)

prop_typeVarEqTransitive :: TypeVar -> TypeVar -> TypeVar -> Property
prop_typeVarEqTransitive tv1 tv2                               tv3 =
  counterexample "TypeVar equality should be transitive" $
    (tv1 === tv2 &&                               tv2 === tv3) ==> (tv1 === tv3)

prop_typeVarOrdering :: TypeVar -> TypeVar -> Property
prop_typeVarOrdering tv1                               tv2 =
  let ordering = compare tv1 tv2
in counterexample "TypeVar ordering should be consistent" $
    (tv1 <= tv2 && tv2 <= tv1) === (tv1 === tv2)

-- ============================================================================
-- TypeConstraint Properties
-- ============================================================================

prop_typeConstraintEqReflexive :: TypeConstraint -> Property
prop_typeConstraintEqReflexive                               tc =
  counterexample "TypeConstraint equality should be reflexive" $
                                  tc === tc

prop_typeConstraintEqSymmetric :: TypeConstraint -> TypeConstraint -> Property
prop_typeConstraintEqSymmetric tc1                               tc2 =
  counterexample "TypeConstraint equality should be symmetric" $
    (tc1 === tc2) === (tc2 === tc1)

prop_typeConstraintOrdering :: TypeConstraint -> TypeConstraint -> Property
prop_typeConstraintOrdering tc1                               tc2 =
  let ordering = compare tc1 tc2
in counterexample "TypeConstraint ordering should be consistent" $
    (tc1 <= tc2 && tc2 <= tc1) === (tc1 === tc2)

-- ============================================================================
-- Type Definition Properties
-- ============================================================================

prop_typeDefConstruction :: String -> [String] -> [TypeConstraint] -> Property
prop_typeDefConstruction name params                               constraints =
  let typeDef = TypeDefDecl params constraints
  in counterexample "TypeDefDecl should preserve constructor arguments" $
    tdParams                               typeDef === params .&.
    tdConstraints                               typeDef === constraints

-- ============================================================================
-- Type Environment Properties
-- ============================================================================

prop_typeEnvConstruction :: Map.Map String TypeDef -> [TypeConstraint] -> Property
prop_typeEnvConstruction defs                               constraints =
  let env = TypeEnv defs constraints
  in counterexample "TypeEnv should preserve constructor arguments" $
    typeDefinitions                               env === defs .&.
    pendingConstraints                               env === constraints

prop_preludeTypeDefsProperties :: Property
                              prop_preludeTypeDefsProperties =
  let expectedTypes = ["int", "string", "bool", "float64"]
                                    actualTypes = Map.keys preludeTypeDefs
  in counterexample "preludeTypeDefs should contain expected types" $
    sort                               actualTypes === sort expectedTypes

-- ============================================================================
-- DependentTypeChecker Properties
-- ============================================================================

prop_newDependentTypeCheckerProperties :: Property
                              prop_newDependentTypeCheckerProperties =
  let checker = newDependentTypeChecker
                                    env = dtcTypeEnv checker
                                    errors = tcErrors checker
  in counterexample "newDependentTypeChecker should initialize correctly" $
    typeDefinitions                               env === preludeTypeDefs .&.
    pendingConstraints                               env === [] .&.
                                  errors === []

prop_newDependentTypeCheckerWithTypesProperties :: Property
                              prop_newDependentTypeCheckerWithTypesProperties =
forAll (listOf1 genTypeDef) $ \typeDefs ->
    let checker = newDependentTypeCheckerWithTypes typeDefs
                                      env = dtcTypeEnv checker
                                      errors = tcErrors checker
                                      expectedDefs = Map.fromList [(n, TypeDefDecl ps cs) | (n, ps, cs) <- typeDefs]
                                      actualDefs = Map.difference (typeDefinitions env) preludeTypeDefs
    in counterexample "newDependentTypeCheckerWithTypes should add custom types" $
                                  actualDefs === expectedDefs .&.
                                  errors === []

-- ============================================================================
-- Type Environment Operations
-- ============================================================================

prop_addTypeProperties :: String -> [String] -> [TypeConstraint] -> Property
prop_addTypeProperties name params                               constraints =
  let checker = evalState (do
addType name params constraints
          get) newDependentTypeChecker
                                    env = dtcTypeEnv checker
                                    mTypeDef = Map.lookup name (typeDefinitions env)
  in counterexample "addType should add type definition to environment" $
    case mTypeDef of
      Just (TypeDefDecl actualParams actualConstraints) ->
                                      actualParams === params .&.                               actualConstraints === constraints
      Nothing -> property False

prop_addConstraintProperties :: TypeConstraint -> Property
prop_addConstraintProperties                               constraint =
  let checker = evalState (do
addConstraint constraint
          get) newDependentTypeChecker
                                    env = dtcTypeEnv checker
                                    constraints = pendingConstraints env
  in counterexample "addConstraint should add constraint to environment" $
    constraint `elem` constraints

prop_lookupTypeDefExisting :: Property
                              prop_lookupTypeDefExisting =
  let checker = newDependentTypeChecker
  in counterexample "lookupTypeDef should find prelude types" $
case evalState (lookupTypeDef "int") checker of
      Just _ -> property True
      Nothing -> property False

prop_lookupTypeDefNonExisting :: String -> Property
prop_lookupTypeDefNonExisting                               name =
  let checker = newDependentTypeChecker
                                    isPreludeType = name `elem` ["int", "string", "bool", "float64"]
  in counterexample "lookupTypeDef should return Nothing for non-existing types" $
not                               isPreludeType ==> case evalState (lookupTypeDef name) checker of
      Nothing -> property True
      Just _ -> property False

-- ============================================================================
-- Type Checking Properties
-- ============================================================================

prop_checkTypeConExisting :: Property
                              prop_checkTypeConExisting =
let checker = evalState (checkType (TVCon "int") newDependentTypeChecker
                                    errors = tcErrors checker
  in counterexample "checkType should accept existing type constructors" $
    null errors

prop_checkTypeConNonExisting :: String -> Property
prop_checkTypeConNonExisting                               name =
  let isPreludeType = name `elem` ["int", "string", "bool", "float64"]
                              checker = evalState (checkType (TVCon name) newDependentTypeChecker
                                    errors = tcErrors checker
  in counterexample "checkType should reject non-existing type constructors" $
    not                               isPreludeType ==> L.any isTypeNotFoundError errors
  where
      isTypeNotFoundError (TypeNotFound _) = True
    isTypeNotFoundError                               _ = False

prop_checkTypeVar :: Property
                              prop_checkTypeVar =
  forAll genTypeVarName $ \name ->
let checker = evalState (checkType (TVVar name) newDependentTypeChecker
                                      errors = tcErrors checker
    in counterexample "checkType should accept type variables" $
    null errors

-- ============================================================================
-- Constraint Solving Properties
-- ============================================================================

prop_solveConstraintsEmpty :: Property
                              prop_solveConstraintsEmpty =
  let result = evalState solveConstraints newDependentTypeChecker
  in counterexample "solveConstraints should succeed with no constraints" $
                                  result === True

prop_solveConstraintsValidEqualities :: Property
                              prop_solveConstraintsValidEqualities =
  let tv1 = TVCon "int"
                                    tv2 = TVCon "int"
                                    checker = evalState (do
addConstraint (Equal tv1 tv2)
          solveConstraints) newDependentTypeChecker
                                    errors = tcErrors checker
  in counterexample "solveConstraints should succeed with valid equalities" $
    null errors

prop_validateConstraintValidEqual :: Property
                              prop_validateConstraintValidEqual =
let constraint = Equal (TVCon "int") (TVCon "int")
  in counterexample "validateConstraint should accept valid equalities" $
    isRight (validateConstraint constraint)

prop_validateConstraintInvalidEqual :: Property
                              prop_validateConstraintInvalidEqual =
let constraint = Equal (TVCon "int") (TVCon "string")
  in counterexample "validateConstraint should reject invalid equalities" $
    isLeft (validateConstraint constraint)

prop_validateConstraintValidSize :: Property
                              prop_validateConstraintValidSize =
let constraint = TypeSizeGE (TVCon "int") 5
  in counterexample "validateConstraint should accept valid size constraints" $
    isRight (validateConstraint constraint)

prop_validateConstraintInvalidSize :: Property
                              prop_validateConstraintInvalidSize =
let constraint = TypeSizeGE (TVCon "int") (-1)
  in counterexample "validateConstraint should reject invalid size constraints" $
    isLeft (validateConstraint constraint)

prop_validateConstraintValidRange :: Property
                              prop_validateConstraintValidRange =
let constraint = TypeRange (TVCon "int") 0 10
  in counterexample "validateConstraint should accept valid ranges" $
    isRight (validateConstraint constraint)

prop_validateConstraintInvalidRange :: Property
                              prop_validateConstraintInvalidRange =
let constraint = TypeRange (TVCon "int") 10 0
  in counterexample "validateConstraint should reject invalid ranges" $
    isLeft (validateConstraint constraint)

-- ============================================================================
-- Unification Properties
-- ============================================================================

prop_unifyIdentical :: TypeVar -> Property
prop_unifyIdentical                               tv =
  counterexample "unify should succeed with identical types" $
isJust (unify [(tv, tv)])

prop_unifySimpleVars :: Property
                              prop_unifySimpleVars =
  let tv1 = TVVar "a"
                                    tv2 = TVVar "b"
  in counterexample "unify should succeed with simple variables" $
isJust (unify [(tv1, tv2)])

prop_unifySameConstructors :: Property
                              prop_unifySameConstructors =
  let tv1 = TVCon "int"
                                    tv2 = TVCon "int"
  in counterexample "unify should succeed with same constructors" $
isJust (unify [(tv1, tv2)])

prop_unifyDifferentConstructors :: Property
                              prop_unifyDifferentConstructors =
  let tv1 = TVCon "int"
                                    tv2 = TVCon "string"
  in counterexample "unify should fail with different constructors" $
isNothing (unify [(tv1, tv2)])

prop_unifyApplications :: Property
                              prop_unifyApplications =
  let tv1 = TVApp "List" [TVCon "int"]
                                    tv2 = TVApp "List" [TVCon "int"]
  in counterexample "unify should succeed with matching applications" $
isJust (unify [(tv1, tv2)])

prop_unifyDifferentArity :: Property
                              prop_unifyDifferentArity =
  let tv1 = TVApp "List" [TVCon "int"]
                                    tv2 =  TVApp "List" [TVCon "int", TVCon "string"]
  in property $ counterexample "unify should fail with different arities" $
isNothing (unify [(tv1, tv2)])

-- ============================================================================
-- Helper Functions
-- ============================================================================

sort :: Ord                               a => [a] -> [a]
sort [] = []
sort (x:xs) = insert x (sort xs)
  where
      insert y [] = [y]
    insert y (z:zs) = if y <= z then y:z:zs else z:insert y zs

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests =   testGroup "Dependencies Core QuickCheck Tests"
  [ testGroup "TypeVar Tests"
      [             testProperty "TypeVar equality is reflexive" prop_typeVarEqReflexive
      ,             testProperty "TypeVar equality is symmetric" prop_typeVarEqSymmetric
      ,             testProperty "TypeVar equality is transitive" prop_typeVarEqTransitive
      ,             testProperty "TypeVar ordering is consistent" prop_typeVarOrdering
      ]
  , testGroup "TypeConstraint Tests"
      [             testProperty "TypeConstraint equality is reflexive" prop_typeConstraintEqReflexive
      ,             testProperty "TypeConstraint equality is symmetric" prop_typeConstraintEqSymmetric
      ,             testProperty "TypeConstraint ordering is consistent" prop_typeConstraintOrdering
      ]
  , testGroup "Type Definition Tests"
      [             testProperty "TypeDefDecl preserves constructor arguments" prop_typeDefConstruction
      ]
  , testGroup "Type Environment Tests"
      [             testProperty "TypeEnv preserves constructor arguments" prop_typeEnvConstruction
      ,             testProperty "preludeTypeDefs contains expected types" prop_preludeTypeDefsProperties
      ]
  , testGroup "DependentTypeChecker Tests"
      [             testProperty "newDependentTypeChecker initializes correctly" prop_newDependentTypeCheckerProperties
      ,             testProperty "newDependentTypeCheckerWithTypes adds custom types" prop_newDependentTypeCheckerWithTypesProperties
      ]
  , testGroup "Type Environment Operations Tests"
      [             testProperty "addType adds type definition to environment" prop_addTypeProperties
      ,             testProperty "addConstraint adds constraint to environment" prop_addConstraintProperties
      ,             testProperty "lookupTypeDef finds prelude types" prop_lookupTypeDefExisting
      ,             testProperty "lookupTypeDef returns Nothing for non-existing types" prop_lookupTypeDefNonExisting
      ]
  , testGroup "Type Checking Tests"
      [             testProperty "checkType accepts existing type constructors" prop_checkTypeConExisting
      ,             testProperty "checkType rejects non-existing type constructors" prop_checkTypeConNonExisting
      ,             testProperty "checkType accepts type variables" prop_checkTypeVar
      ]
  , testGroup "Constraint Solving Tests"
      [             testProperty "solveConstraints succeeds with no constraints" prop_solveConstraintsEmpty
      ,             testProperty "solveConstraints succeeds with valid equalities" prop_solveConstraintsValidEqualities
      ,             testProperty "validateConstraint accepts valid equalities" prop_validateConstraintValidEqual
      ,             testProperty "validateConstraint rejects invalid equalities" prop_validateConstraintInvalidEqual
      ,             testProperty "validateConstraint accepts valid size constraints" prop_validateConstraintValidSize
      ,             testProperty "validateConstraint rejects invalid size constraints" prop_validateConstraintInvalidSize
      ,             testProperty "validateConstraint accepts valid ranges" prop_validateConstraintValidRange
      ,             testProperty "validateConstraint rejects invalid ranges" prop_validateConstraintInvalidRange
      ]
  , testGroup "Unification Tests"
      [             testProperty "unify succeeds with identical types" prop_unifyIdentical
      ,             testProperty "unify succeeds with simple variables" prop_unifySimpleVars
      ,             testProperty "unify succeeds with same constructors" prop_unifySameConstructors
      ,             testProperty "unify fails with different constructors" prop_unifyDifferentConstructors
      ,             testProperty "unify succeeds with matching applications" prop_unifyApplications
      ,             testProperty "unify fails with different arities" prop_unifyDifferentArity
      ]
  ]