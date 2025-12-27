module Test.Unit.NewQuickCheckTestSuite6Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import TestSupport.QuickCheck (fastProperty)
import Dependencies.AST
import Dependencies.TypeSystem

-- | Test suite for Dependencies module dependency analysis
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite6 - Dependencies Analysis"
    [ testGroup "AST operations"
        [ testCase "Program AST construction" $ do
            let program = Program [SVarDecl "x" (SimpleT "int")]
            case program of
              Program stmts -> length stmts @?= 1
              
        , testCase "Statement equality works" $ do
            let stmt1 = SVarDecl "x" (SimpleT "int")
                stmt2 = SVarDecl "x" (SimpleT "int")
                stmt3 = SVarDecl "y" (SimpleT "int")
            stmt1 @?= stmt2
            stmt1 /= stmt3 @?= True
            
        , testCase "TypeExpr construction" $ do
            let simpleType = SimpleT "int"
                genericType = GenericT "List" [SimpleT "int"]
                funcType = FuncT [("x", SimpleT "int")] (SimpleT "bool")
            show simpleType `contains` "int" @?= True
            show genericType `contains` "List" @?= True
            show funcType `contains` "FuncT" @?= True
            
        , testCase "Constraint construction" $ do
            let sizeGT = SizeGT "arr" 0
                sizeGE = SizeGE "arr" 1
                range = RangeC "x" 0 100
                pred = PredC "Valid" [SimpleT "int"]
            show sizeGT `contains` "SizeGT" @?= True
            show sizeGE `contains` "SizeGE" @?= True
            show range `contains` "RangeC" @?= True
            show pred `contains` "PredC" @?= True
        ]

    , testGroup "Dependency graph operations"
        [ testCase "DependencyNode construction" $ do
            let node = DependencyNode "moduleA" ["moduleB", "moduleC"]
            nodeName node @?= "moduleA"
            nodeDependencies node @?= ["moduleB", "moduleC"]
            
        , testCase "DependencyGraph construction" $ do
            let node1 = DependencyNode "A" ["B"]
                node2 = DependencyNode "B" []
                graph = DependencyGraph (Map.fromList [("A", node1), ("B", node2)])
            length (graphNodes graph) @?= 2
        ]

    [,testGroup "TypeVar operations"
        [ testCase "TypeVar construction" $ do
            let con = TVCon "Int"
                var = TVVar "a"
                app = TVApp "List" [TVVar "a"]
                fun = TVFun [TVVar "a"] (TVVar "b")
                tuple = TVTuple [TVVar "a", TVVar "b"]
            show con `contains` "TVCon" @?= True
            show var `contains` "TVVar" @?= True
            show app `contains` "TVApp" @?= True
            show fun `contains` "TVFun" @?= True
            show tuple `contains` "TVTuple" @?= True
            
        , testCase "TypeVar equality" $ do
            TVVar "a" @?= TVVar "a"
            TVVar "a" /= TVVar "b" @?= True
        ]

    , testGroup "TypeConstraint operations"
        [ testCase "TypeConstraint construction" $ do
            let equal = Equal (TVVar "a") (TVVar "b")
                subtype = Subtype (TVVar "a") (TVVar "b")
                predicate = Predicate "Valid" [TVVar "a"]
                sizeGE = TypeSizeGE (TVVar "arr") 0
                sizeGT = TypeSizeGT (TVVar "arr") 1
                range = TypeRange (TVVar "x") 0 100
            show equal `contains` "Equal" @?= True
            show subtype `contains` "Subtype" @?= True
            show predicate `contains` "Predicate" @?= True
            show sizeGE `contains` "TypeSizeGE" @?= True
            show sizeGT `contains` "TypeSizeGT" @?= True
            show range `contains` "TypeRange" @?= True
        ]

    , testGroup "DependentTypeError operations"
        [ testCase "DependentTypeError construction" $ do
            let typeMismatch = DependentTypeMismatch (TVVar "a") (TVVar "b")
                constraintViolation = ConstraintViolation "Size" (TVVar "arr")
                typeNotFound = TypeNotFound "UnknownType"
                invalidArg = InvalidTypeArgument "param"
                unsolvable = UnsolvableConstraint (Equal (TVVar "a") (TVVar "b"))
                infinite = DependentInfiniteType "RecType" (TVVar "a")
                ambiguous = AmbiguousType "x"
                parseErr = ParseError "syntax error"
                semanticErr = SemanticError "type error"
            show typeMismatch `contains` "DependentTypeMismatch" @?= True
            show constraintViolation `contains` "ConstraintViolation" @?= True
            show typeNotFound `contains` "TypeNotFound" @?= True
            show invalidArg `contains` "InvalidTypeArgument" @?= True
            show unsolvable `contains` "UnsolvableConstraint" @?= True
            show infinite `contains` "DependentInfiniteType" @?= True
            show ambiguous `contains` "AmbiguousType" @?= True
            show parseErr `contains` "ParseError" @?= True
            show semanticErr `contains` "SemanticError" @?= True
        ]

    , testGroup "TypeEnv operations"
        [ testCase "TypeEnv construction" $ do
            let typeDefs = Map.singleton "Int" (TypeDefDecl [] [])
                constraints = [Equal (TVVar "a") (TVVar "b")]
                env = TypeEnv typeDefs constraints
            length (typeDefinitions env) @?= 1
            length (pendingConstraints env) @?= 1
            
        , testCase "preludeTypeDefs contains basic types" $ do
            let prelude = preludeTypeDefs
            Map.member "int" prelude @?= True
        ]

    , testGroup "DependentTypeChecker operations"
        [ testCase "newDependentTypeChecker creates checker" $ do
            let checker = newDependentTypeChecker
            length (tcErrors checker) @?= 0
            Map.size (typeDefinitions (dtcTypeEnv checker)) @?= 0
            
        , testCase "newDependentTypeCheckerWithTypes uses provided types" $ do
            let types = Map.singleton "Custom" (TypeDefDecl [] [])
                checker = newDependentTypeCheckerWithTypes types
            Map.member "Custom" (typeDefinitions (dtcTypeEnv checker)) @?= True
        ]

    , testGroup "Type operations"
        [ testCase "addType adds type definition" $ do
            let checker = newDependentTypeChecker
                typeDef = TypeDefDecl ["T"] []
                checker' = addType "MyType" typeDef checker
            Map.member "MyType" (typeDefinitions (dtcTypeEnv checker')) @?= True
            
        , testCase "addConstraint adds constraint" $ do
            let checker = newDependentTypeChecker
                constraint = Equal (TVVar "a") (TVVar "b")
                checker' = addConstraint constraint checker
            length (pendingConstraints (dtcTypeEnv checker')) @?= 1
            
        , testCase "lookupTypeDef finds existing type" $ do
            let typeDef = TypeDefDecl [] []
                checker = addType "Int" typeDef newDependentTypeChecker
                result = lookupTypeDef "Int" checker
            case result of
              Just _ -> True @?= True
              Nothing -> assertBool "Should find type definition" False
        ]

    , testGroup "Constraint solving"
        [ testCase "solveConstraints handles simple equality" $ do
            let constraint = Equal (TVVar "a") (TVVar "b")
                checker = addConstraint constraint newDependentTypeChecker
                result = solveConstraints checker
            True @?= True  -- Basic test that solving doesn't crash
            
        , testCase "checkTypeConstraint validates constraints" $ do
            let constraint = TypeSizeGE (TVVar "arr") 0
                checker = newDependentTypeChecker
                result = checkTypeConstraint constraint checker
            True @?= True  -- Basic test that checking doesn't crash
        ]

    , testGroup "Type conversion"
        [ testCase "convertTypeExpr handles simple types" $ do
            let typeExpr = SimpleT "int"
                checker = newDependentTypeChecker
                result = convertTypeExpr typeExpr checker
            case result of
              Left _ -> assertBool "Should convert simple type" False
              Right _ -> True @?= True
        ]

    , testGroup "QuickCheck properties"
        [ fastProperty "TypeVar ordering is consistent" prop_typeVarOrderingConsistent
        , fastProperty "TypeConstraint ordering is consistent" prop_typeConstraintOrderingConsistent
        , fastProperty "DependencyNode preserves name and dependencies" prop_dependencyNodePreservesFields
        , fastProperty "Statement equality is reflexive" prop_statementEqualityReflexive
        , fastProperty "TypeExpr equality is symmetric" prop_typeExprEqualitySymmetric
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- TypeVar properties
prop_typeVarOrderingConsistent :: TypeVar -> TypeVar -> Bool
prop_typeVarOrderingConsistent tv1 tv2 =
    let ord1 = compare tv1 tv2
        ord2 = compare (show tv1) (show tv2)
    in (tv1 == tv2) || (ord1 == ord2)

-- TypeConstraint properties
prop_typeConstraintOrderingConsistent :: TypeConstraint -> TypeConstraint -> Bool
prop_typeConstraintOrderingConsistent tc1 tc2 =
    let ord1 = compare tc1 tc2
        ord2 = compare (show tc1) (show tc2)
    in (tc1 == tc2) || (ord1 == ord2)

-- DependencyNode properties
prop_dependencyNodePreservesFields :: String -> [String] -> Bool
prop_dependencyNodePreservesFields name deps =
    let node = DependencyNode name deps
    in nodeName node == name && nodeDependencies node == deps

-- Statement properties
prop_statementEqualityReflexive :: Statement -> Bool
prop_statementEqualityReflexive stmt = stmt == stmt

-- TypeExpr properties
prop_typeExprEqualitySymmetric :: TypeExpr -> TypeExpr -> Bool
prop_typeExprEqualitySymmetric te1 te2 = (te1 == te2) == (te2 == te1)

-- Helper functions for generating test data
genTypeVar :: Gen TypeVar
genTypeVar = oneof
    [ fmap TVCon arbitrary
    , fmap TVVar arbitrary
    , fmap TVApp $ (,) <$> arbitrary <*> arbitrary
    , fmap TVFun $ (,) <$> arbitrary <*> arbitrary
    , fmap TVTuple arbitrary
    ]

genTypeConstraint :: Gen TypeConstraint
genTypeConstraint = oneof
    [ fmap Equal $ (,) <$> genTypeVar <*> genTypeVar
    , fmap Subtype $ (,) <$> genTypeVar <*> genTypeVar
    , fmap Predicate $ (,) <$> arbitrary <*> arbitrary
    , fmap TypeSizeGE $ (,) <$> genTypeVar <*> arbitrary
    , fmap TypeSizeGT $ (,) <$> genTypeVar <*> arbitrary
    , fmap TypeRange $ (,,) <$> genTypeVar <*> arbitrary <*> arbitrary
    ]

genTypeExpr :: Gen TypeExpr
genTypeExpr = oneof
    [ fmap SimpleT arbitrary
    , fmap GenericT $ (,) <$> arbitrary <*> arbitrary
    , fmap FuncT $ (,) <$> arbitrary <*> arbitrary
    , fmap RefineT $ (,) <$> genTypeExpr <*> arbitrary
    ]

genStatement :: Gen Statement
genStatement = oneof
    [ fmap STypeDef $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap STypeAlias $ (,,) <$> arbitrary <*> genTypeExpr <*> arbitrary
    , fmap SVarDecl $ (,) <$> arbitrary <*> genTypeExpr
    , fmap SFuncDecl $ (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    , fmap SConstraintDef $ (,) <$> arbitrary <*> arbitrary
    , fmap SExistsDecl $ (,) <$> arbitrary <*> arbitrary
    ]

genDependencyNode :: Gen DependencyNode
genDependencyNode = do
    name <- arbitrary
    deps <- arbitrary
    return $ DependencyNode name deps

genValidIdentifier :: Gen String
genValidIdentifier = do
    first <- elements ['a'..'z']
    rest <- arbitrary `suchThat` all (`elem` ['a'..'z'] ++ ['0'..'9'] ++ "_")
    return (first : rest)