module Test.Unit.DependentTypesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, assertFailure, testCase )

import DependentTypesParser
  ( validateDependentTypeSyntax
  )

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
    ]

