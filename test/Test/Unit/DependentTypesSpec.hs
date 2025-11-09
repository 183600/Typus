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
    ]

