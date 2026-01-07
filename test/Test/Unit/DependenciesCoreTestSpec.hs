module Test.Unit.DependenciesCoreTestSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, (==>), oneof, elements, listOf, choose)
import qualified Test.Tasty.QuickCheck as QC
import Dependencies.TypeSystem
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


y
  )
import Dependencies.AST (TypeExpr(..), Constraint)
    , TypeRange <$> QC.arbitrary <*> QC.choose (0, 100) <*> QC.choose (0, 100)
    ]

instance Arbitrary DependentTypeError where
                                              arbitrary = QC.oneof
    [ DependentTypeMismatch <$> QC.arbitrary <*> QC.arbitrary
    , ConstraintViolation <$> QC.arbitrary <*> QC.arbitrary
    , TypeNotFound <$> QC.arbitrary
    , InvalidTypeArgument <$> QC.arbitrary
    , UnsolvableConstraint <$> QC.arbitrary
    , DependentInfiniteType <$> QC.arbitrary <*> QC.arbitrary
    , AmbiguousType <$> QC.arbitrary
    , ParseError <$> QC.arbitrary
    , SemanticError <$> QC.arbitrary
    ]

instance Arbitrary TypeDef where
                                              arbitrary = TypeDefDecl <$> listOf QC.arbitrary <*> listOf QC.arbitrary

instance Arbitrary TypeEnv where
                                              arbitrary = TypeEnv <$> (Map.fromList <$> listOf (() <$> QC.arbitrary <*> QC.arbitrary) <*> listOf QC.arbitrary

instance Arbitrary DependentTypeChecker where
                                              arbitrary = DependentTypeChecker <$> QC.arbitrary <*> listOf QC.arbitrary

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =
    testGroup "Dependencies Core Tests"
    [ testGroup "TypeVar"
        [             testCase "TypeVar equality works correctly" $ do
                        let tv1 = TVCon "int"
                                              tv2 = TVCon "int"
                                              tv3 = TVCon "string"
                                              tv4 = TVVar "x"
                                              tv5 = TVVar "x"
            assertBool "Same constructors should be equal" $                               tv1 == tv2
            assertBool "Different constructors should not be equal" $ tv1 /= tv3
            assertBool "Same variables should be equal" $                               tv4 == tv5
            assertBool "Constructor L.and variable should not be equal" $ tv1 /= tv4

          ,             testCase "TypeVar ordering works consistently" $ do
                        let tv1 = TVCon "a"
                                              tv2 = TVCon "b"
                                              tv3 = TVVar "a"
            assertBool "TVCon 'a' < TVCon 'b'" $ tv1 < tv2
            assertBool "Ordering should be consistent" $ compare tv1                               tv2 == LT

          ,             testCase "TVFun creates function types correctly" $ do
                    let funcType = TVFun [TVCon "int", TVCon "string"] (TVCon "bool")
            case funcType of
              TVFun args ret -> do
                            assertBool "Should have 2 arguments" $ L.length                               args == 2
                L.head args @?= TVCon "int"
                args !! 1 @?= TVCon "string"
                ret @?= TVCon "bool"
              _ -> assertBool "Should be TVFun" False

          ,             testCase "TVTuple creates tuple types correctly" $ do
                        let tupleType = TVTuple [TVCon "int", TVCon "string", TVCon "bool"]
            case tupleType of
              TVTuple elems -> do
                            assertBool "Should have 3 elements" $ L.length                               elems == 3
                L.head elems @?= TVCon "int"
                elems !! 1 @?= TVCon "string"
                elems !! 2 @?= TVCon "bool"
              _ -> assertBool "Should be TVTuple" False
        ]

    , testGroup "TypeConstraint"
        [             testCase "Equal constraint works correctly" $ do
                        let tv1 = TVCon "int"
                                              tv2 = TVCon "int"
                                              constraint = Equal tv1 tv2
            case constraint of
              Equal a b -> do
                            a @?= tv1
                b @?= tv2
              _ -> assertBool "Should be Equal constraint" False

          ,             testCase "Subtype constraint works correctly" $ do
                        let parent = TVCon "animal"
                                              child = TVCon "dog"
                                              constraint = Subtype child parent
            case constraint of
              Subtype c p -> do
                            c @?= child
                p @?= parent
              _ -> assertBool "Should be Subtype constraint" False

          ,             testCase "Predicate constraint works correctly" $ do
                        let args = [TVCon "int", TVCon "string"]
                                              constraint = Predicate "hasProperty" args
            case constraint of
              Predicate name a -> do
                            name @?= "hasProperty"
                a @?= args
              _ -> assertBool "Should be Predicate constraint" False

          ,             testCase "Size constraints work correctly" $ do
                        let tv = TVVar "n"
                                              geConstraint = TypeSizeGE tv 5
                                              gtConstraint = TypeSizeGT tv 10
            case geConstraint of
              TypeSizeGE var k -> do
                            var @?= tv
                k @?= 5
              _ -> assertBool "Should be TypeSizeGE" False
            case gtConstraint of
              TypeSizeGT var k -> do
                            var @?= tv
                k @?= 10
              _ -> assertBool "Should be TypeSizeGT" False

          ,             testCase "Range constraint works correctly" $ do
                        let tv = TVVar "age"
                                              constraint = TypeRange tv 18 65
            case constraint of
              TypeRange var minVal maxVal -> do
                            var @?= tv
                minVal @?= 18
                maxVal @?= 65
              _ -> assertBool "Should be TypeRange" False
        ]

    , testGroup "DependentTypeChecker"
        [             testCase "newDependentTypeChecker initializes with prelude types" $ do
                        let checker = newDependentTypeChecker
            let env = dtcTypeEnv checker
            let defs = typeDefinitions env
            assertBool "Should have int type" $ Map.member "int" defs
            assertBool "Should have string type" $ Map.member "string" defs
            assertBool "Should have bool type" $ Map.member "bool" defs
            assertBool "Should have float64 type" $ Map.member "float64" defs
            assertBool "Should start with no errors" $ L.null $ tcErrors checker
            assertBool "Should start with no pending constraints" $ L.null $ pendingConstraints env

          ,             testCase "newDependentTypeCheckerWithTypes adds custom types" $ do
                        let customTypes = [("custom", ["T"], [TypeSizeGE (TVVar "T") 0])]
                                              checker = newDependentTypeCheckerWithTypes customTypes
            let env = dtcTypeEnv checker
            let defs = typeDefinitions env
            assertBool "Should have custom type" $ Map.member "custom" defs
            case Map.lookup "custom" defs of
              Just (TypeDefDecl params constraints) -> do
                            params @?= ["T"]
                assertBool "Should have one constraint" $ L.length                               constraints == 1
              _ -> assertBool "Should find custom type definition" False

          ,             testCase "addType adds type to environment" $ do
                        let initialChecker = newDependentTypeChecker
                ((), checker) = runState (addType "newtype" ["A", "B"] [Equal (TVVar "A") (TVVar "B")]) initialChecker
            let env = dtcTypeEnv checker
            let defs = typeDefinitions env
            assertBool "Should have newtype" $ Map.member "newtype" defs
            case Map.lookup "newtype" defs of
              Just (TypeDefDecl params constraints) -> do
                            params @?= ["A", "B"]
                assertBool "Should have one constraint" $ L.length                               constraints == 1
              _ -> assertBool "Should find newtype definition" False

          ,             testCase "addConstraint adds pending constraint" $ do
                        let initialChecker = newDependentTypeChecker
                                              constraint = Equal (TVCon "int") (TVCon "int")
                ((), checker) = runState (addConstraint constraint) initialChecker
            let env = dtcTypeEnv checker
            let constraints = pendingConstraints env
            assertBool "Should have one pending constraint" $ L.length                               constraints == 1
            L.head constraints @?= constraint

          ,             testCase "addTypeError adds error to checker" $ do
                        let initialChecker = newDependentTypeChecker
                                              error = TypeNotFound "unknownType"
                ((), checker) = runState (addTypeError error) initialChecker
            let errors = tcErrors checker
            assertBool "Should have one error" $ L.length                               errors == 1
            L.head errors @?= error

          ,             testCase "lookupTypeDef finds existing types" $ do
                        let checker = newDependentTypeChecker
                (result, _) = runState (lookupTypeDef "int") checker
            case result of
              Just (TypeDefDecl params constraints) -> do
                            assertBool "int should have no parameters" $ null params
                assertBool "int should have no constraints" $ null constraints
              Nothing -> assertBool "Should find int type" False

          ,             testCase "lookupTypeDef returns Nothing for non-existent types" $ do
                        let checker = newDependentTypeChecker
                (result, _) = runState (lookupTypeDef "nonexistent") checker
            result @?= Nothing
        ]

    , testGroup "Type Conversion"
        [             testCase "convertTypeExpr handles simple types" $ do
                        let params = Set.empty
                                              typeExpr = SimpleT (T.pack "int")
                                              typeVar = convertTypeExpr params typeExpr
            typeVar @?= TVCon "int"

          ,             testCase "convertTypeExpr handles generic types" $ do
                        let params = Set.empty
                                              typeExpr = GenericT "list" [SimpleT (T.pack "int")]
                                              typeVar = convertTypeExpr params typeExpr
            case typeVar of
              TVApp "list" args -> do
                            assertBool "Should have one argument" $ L.length                               args == 1
                L.head args @?= TVCon "int"
              _ -> assertBool "Should be TVApp" False

          ,             testCase "convertTypeExpr handles function types" $ do
                        let params = Set.empty
                                              typeExpr = FuncT [("x", SimpleT (T.pack "int")] (SimpleT (T.pack "bool")
                                              typeVar = convertTypeExpr params typeExpr
            case typeVar of
              TVFun args ret -> do
                            assertBool "Should have one argument" $ L.length                               args == 1
                L.head args @?= TVCon "int"
                ret @?= TVCon "bool"
              _ -> assertBool "Should be TVFun" False

          ,             testCase "convertTypeExprAndRefinements extracts constraints" $ do
                        let params = Set.fromList ["n"]
                                              typeExpr = RefineT (SimpleT (T.pack "n") [SizeGE "n" 0]
                (typeVar, constraints) = convertTypeExprAndRefinements params typeExpr
            typeVar @?= TVVar "n"
            assertBool "Should have one constraint" $ L.length                               constraints == 1
            case L.head constraints of
              TypeSizeGE (TVVar "n") k -> k @?= 0
              _ -> assertBool "Should be TypeSizeGE" False

          ,             testCase "convertConstraint handles size constraints" $ do
                        let params = Set.fromList ["x"]
                                              constraint = SizeGE "x" 10
                                              typeConstraint = convertConstraint params constraint
            case typeConstraint of
              TypeSizeGE (TVVar "x") k -> k @?= 10
              _ -> assertBool "Should be TypeSizeGE" False

          ,             testCase "convertConstraint handles predicate constraints" $ do
                        let params = Set.empty
                                              constraint = PredC "positive" [SimpleT (T.pack "int")]
                                              typeConstraint = convertConstraint params constraint
            case typeConstraint of
              Predicate "positive" args -> do
                            assertBool "Should have one argument" $ L.length                               args == 1
                L.head args @?= TVCon "int"
              _ -> assertBool "Should be Predicate" False
        ]

    , testGroup "Type Checking"
        [             testCase "checkType validates constructor types" $ do
                        let initialChecker = newDependentTypeChecker
                ((), checker) = runState (checkType (TVCon "int") initialChecker
            assertBool "Should not add errors for valid type" $ L.null $ tcErrors checker

          ,             testCase "checkType adds error for unknown constructor" $ do
                        let initialChecker = newDependentTypeChecker
                ((), checker) = runState (checkType (TVCon "unknown") initialChecker
            let errors = tcErrors checker
            assertBool "Should add error for unknown type" $ not $ null errors
            case L.head errors of
              TypeNotFound name -> name @?= "unknown"
              _ -> assertBool "Should be TypeNotFound error" False

          ,             testCase "getDependentTypeErrors returns L.all errors" $ do
                        let initialChecker = newDependentTypeChecker
                ((), checker1) = runState (addTypeError (TypeNotFound "error1") initialChecker
                ((), checker2) = runState (addTypeError (SemanticError "error2") checker1
            let errors = getDependentTypeErrors checker2
            assertBool "Should have 2 errors" $ L.length                               errors == 2
        ]

    , testGroup "QuickCheck Properties"
        [             testProperty "TypeVar equality is reflexive" $
            \tv ->                               tv == tv

        ,             testProperty "TypeVar equality is symmetric" $
            \tv1 tv2 -> (tv1 == tv2) == (tv2 == tv1)

        ,             testProperty "TypeConstraint equality is reflexive" $
            \constraint ->                               constraint == constraint

        ,             testProperty "DependentTypeError equality is reflexive" $
            \error ->                               error == error

        ,             testProperty "TypeEnv equality is reflexive" $
            \env ->                               env == env

        ,             testProperty "DependentTypeChecker equality is reflexive" $
            \checker ->                               checker == checker

        ,             testProperty "newDependentTypeChecker has no errors" $
            \_ -> L.null $ tcErrors newDependentTypeChecker

        ,             testProperty "newDependentTypeCheckerWithTypes preserves custom types" $
            \typeDefs ->
              let checker = newDependentTypeCheckerWithTypes typeDefs
                                                env = dtcTypeEnv checker
                                                defs = typeDefinitions env
                                                customNames = [name | (name, _, _) <- typeDefs]
              in L.all (`Map.member` defs) customNames

        ,             testProperty "addType preserves existing types" $
            \name params constraints ->
              let initialChecker = newDependentTypeChecker
                  ((), checker) = runState (addType name params constraints) initialChecker
                                                env = dtcTypeEnv checker
                                                defs = typeDefinitions env
              in Map.member "int" defs && Map.member "string" defs

        ,             testProperty "lookupTypeDef is deterministic" $
            \name ->
              let checker = newDependentTypeChecker
                  (result1, _) = runState (lookupTypeDef name) checker
                  (result2, _) = runState (lookupTypeDef name) checker
              in                               result1 == result2

        ,             testProperty "convertTypeExpr is deterministic" $
            \params typeExpr ->
              let tv1 = convertTypeExpr params typeExpr
                                                tv2 = convertTypeExpr params typeExpr
              in                               tv1 == tv2

        ,             testProperty "convertConstraint is deterministic" $
            \params constraint ->
              let tc1 = convertConstraint params constraint
                                                tc2 = convertConstraint params constraint
              in                               tc1 == tc2
        ]
    ]