module Test.Unit.DependentTypesSpec (tests) where

import Control.Monad.State (execState, runState)
import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, testCase )

import Analyzer.DependentTypeBridge (extractTypeDefinitions)
import DependentTypesParser
  ( validateDependentTypeSyntax
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

    , testCase "solveConstraints reports invalid range constraints" $ do
        let stmt = Dep.SConstraintDef "invalidRange" (Dep.RangeC "x" 10 1)
            (result, checker) =
              runState (Dep.validateStatement stmt >> TS.solveConstraints) Dep.newDependentTypeChecker
        assertBool "expected constraint solving to fail" (not result)
        TS.getDependentTypeErrors checker
          @?= [TS.SemanticError "invalid range: min > max"]
    ]
