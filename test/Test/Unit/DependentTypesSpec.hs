{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.DependentTypesSpec (tests) where

import Control.Monad.State (execState, runState)
import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, assertFailure, testCase )

import Analyzer.DependentTypeBridge (extractTypeDefinitions)
import DependentTypesParser
  ( DependentType(..)
  , DependentTypeError(..)
  , TypeConstraint(..)
  , TypeParameter(..)
  , TypeRef(..)
  , parseTypeDeclaration
  , validateDependentTypeSyntax
  )
import qualified Dependencies as Dep
import qualified Dependencies.TypeSystem as TS

tests :: TestTree
tests =
  testGroup "Dependent types parser"
    [ testCase "parses type and function declarations" $ do
        let source = unlines
              [ "type Vector<T> struct {"
              , "    values: T"
              , "}"
              ]
        let errors = validateDependentTypeSyntax source
        errors @?= []

    , testCase "reports syntax problems" $ do
        let invalidSource = "alias Broken"
        let errors = validateDependentTypeSyntax invalidSource
        assertBool "expected dependent type parser to report errors" (not (null errors))

    , testCase "extracts constraints from multiline type declarations" $ do
        let source = unlines
              [ "type Vector<T> struct {"
              , "    values: T"
              , "}"
              , "where len values > 0"
              ]
        extractTypeDefinitions source
          @?= [ ( "Vector"
                , ["T"]
                , [Dep.TypeSizeGE (Dep.TVVar "values") 1]
                )
              ]

    , testCase "collects parameter and declaration constraints" $ do
        let source = unlines
              [ "type Matrix<T: Slice<int> | len T > 1 & T == Number & ensure(T)> struct {"
              , "    values: T"
              , "}"
              , "where values > 5 & NonEmpty(values)"
              ]
        extractTypeDefinitions source
          @?= [ ( "Matrix"
                , ["T"]
                , [ Dep.Subtype (Dep.TVVar "T") (Dep.TVApp "Slice" [Dep.TVCon "int"])
                  , Dep.TypeSizeGE (Dep.TVVar "T") 2
                  , Dep.Equal (Dep.TVVar "T") (Dep.TVCon "Number")
                  , Dep.Predicate "ensure" [Dep.TVVar "T"]
                  , Dep.TypeRange (Dep.TVVar "values") 6 maxBound
                  , Dep.Predicate "NonEmpty" [Dep.TVVar "values"]
                  ]
                )
              ]

    , testCase "supports type class constraints in type definitions" $ do
        let source = unlines
              [ "type Sorted<T | T: Comparable<int>> struct {"
              , "    values: T"
              , "}"
              , "where values: Comparable<int>"
              ]
        extractTypeDefinitions source
          @?= [ ( "Sorted"
                , ["T"]
                , [ Dep.Subtype (Dep.TVVar "T") (Dep.TVApp "Comparable" [Dep.TVCon "int"])
                  , Dep.Subtype (Dep.TVVar "values") (Dep.TVApp "Comparable" [Dep.TVCon "int"])
                  ]
                )
              ]

    , testCase "translates nonempty shorthand constraint to TypeSizeGT" $ do
        let source = unlines
              [ "type Bag<T> struct {"
              , "    values: T"
              , "}"
              , "where nonempty values"
              ]
        extractTypeDefinitions source
          @?= [ ( "Bag"
                , ["T"]
                , [ Dep.TypeSizeGT (Dep.TVVar "values") 0
                  ]
                )
              ]

    , testCase "extracts numeric equality constraints as concrete values" $ do
        let source = unlines
              [ "type Bounded<N> struct {}"
              , "where N == 1"
              ]
        extractTypeDefinitions source
          @?= [ ( "Bounded"
                , ["N"]
                , [ Dep.Equal (Dep.TVVar "N") (Dep.TVCon "1")
                  ]
                )
              ]

    , testCase "ignores aliases and dependent functions when extracting types" $ do
        let source = unlines
              [ "type Envelope<T> struct {"
              , "    payload: T"
              , "}"
              , "alias EnvelopeAlias = Envelope<int>"
              , "func enforce(x Envelope<int>) where len x > 0"
              ]
        extractTypeDefinitions source
          @?= [ ( "Envelope"
                , ["T"]
                , []
                )
              ]

    , testCase "analyzeAST accepts well-formed declarations" $ do
        let ast =
              Dep.Program
                [ Dep.STypeDef "Vector" ["T"] []
                , Dep.SVarDecl "items" (Dep.GenericT "Vector" [Dep.SimpleT "int"])
                ]
        Dep.analyzeAST ast @?= []

    , testCase "analyzeAST reports missing type references" $ do
        let ast = Dep.Program [Dep.SVarDecl "value" (Dep.SimpleT "Missing")]
        Dep.analyzeAST ast @?= [Dep.TypeNotFound "Missing"]

    , testCase "validateStatement registers dependent type definitions" $ do
        let stmt = Dep.STypeDef "Vector" ["T"] [Dep.SizeGE "T" 1]
            checker = execState (Dep.validateStatement stmt) Dep.newDependentTypeChecker
            defs = TS.typeDefinitions (TS.dtcTypeEnv checker)
        Map.lookup "Vector" defs
          @?= Just (TS.TypeDefDecl ["T"] [TS.TypeSizeGE (TS.TVVar "T") 1])

    , testCase "validateStatement reports missing alias targets" $ do
        let stmt = Dep.STypeAlias "Alias" (Dep.SimpleT "Missing") []
            checker = execState (Dep.validateStatement stmt) Dep.newDependentTypeChecker
        TS.getDependentTypeErrors checker
          @?= [TS.TypeNotFound "Missing"]

    , testCase "solveConstraints succeeds with satisfiable constraints" $ do
        let (result, checker) =
              runState
                ( do
                    Dep.addConstraint (Dep.Equal (Dep.TVCon "int") (Dep.TVCon "int"))
                    Dep.addConstraint (Dep.Subtype (Dep.TVCon "int") (Dep.TVCon "int"))
                    Dep.solveConstraints
                )
                Dep.newDependentTypeChecker
        assertBool "expected constraints to solve" result
        Dep.getDependentTypeErrors checker @?= []

    , testCase "solveConstraints reports invalid range constraints" $ do
        let stmt = Dep.SConstraintDef "invalidRange" (Dep.RangeC "x" 10 1)
            (result, checker) =
              runState (Dep.validateStatement stmt >> TS.solveConstraints) Dep.newDependentTypeChecker
        assertBool "expected constraint solving to fail" (not result)
        TS.getDependentTypeErrors checker
          @?= [TS.SemanticError "invalid range: min > max"]

    , testCase "extractTypeDefinitions preserves equality between parameters" $ do
        let source = unlines
              [ "type Pair<Left, Right> struct {"
              , "    first: Left"
              , "    second: Right"
              , "}"
              , "where Left == Right"
              ]
        extractTypeDefinitions source
          @?= [ ( "Pair"
                , ["Left", "Right"]
                , [Dep.Equal (Dep.TVVar "Left") (Dep.TVVar "Right")]
                )
              ]

    , testCase "extractTypeDefinitions treats uppercase constants as concrete types" $ do
        let source = unlines
              [ "type Limited<limit> struct {}"
              , "where limit == MaxSize"
              ]
        extractTypeDefinitions source
          @?= [ ( "Limited"
                , ["limit"]
                , [Dep.Equal (Dep.TVVar "limit") (Dep.TVCon "MaxSize")]
                )
              ]

    , testCase "extractTypeDefinitions converts custom constraints to predicates" $ do
        let source = unlines
              [ "type Verified<T> struct {"
              , "    data: T"
              , "}"
              , "where enforceVerified"
              ]
        extractTypeDefinitions source
          @?= [ ( "Verified"
                , ["T"]
                , [Dep.Predicate "enforceVerified" []]
                )
              ]

    , testCase "parseTypeDeclaration captures inline parameter constraints" $ do
        let source = unlines
              [ "type Sized<T | len T > 3 & requires(T)> struct {"
              , "    values: T"
              , "}"
              ]
        case parseTypeDeclaration source of
          Left err ->
            assertFailure ("failed to parse type declaration: " ++ err)
          Right (TypeDecl _ params _ _) -> do
            let expectedParam =
                  TypeParameter
                    { paramName = "T"
                    , paramType = TypeRef "int" []
                    , paramConstraints =
                        [ SizeConstraint "T" 4
                        , PredicateConstraint "requires" ["T"]
                        ]
                    }
            params @?= [expectedParam]
          Right _ ->
            assertFailure "parseTypeDeclaration did not return a type declaration"

    , testCase "validateDependentTypeSyntax reports duplicate type definitions" $ do
        let source = unlines
              [ "type Vector struct {}"
              , "type Vector struct {}"
              ]
        validateDependentTypeSyntax source
          @?= [InvalidTypeSyntax "重复定义: Vector"]
    ]
