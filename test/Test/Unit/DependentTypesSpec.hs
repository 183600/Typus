module Test.Unit.DependentTypesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, testCase )

import Analyzer.DependentTypeBridge (extractTypeDefinitions)
import DependentTypesParser
  ( validateDependentTypeSyntax
  )
import qualified Dependencies as Dep

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
    ]
