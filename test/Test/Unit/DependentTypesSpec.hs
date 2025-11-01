module Test.Unit.DependentTypesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ( (@?=), assertBool, assertFailure, testCase )

import DependentTypesParser
  ( DependentType(..)
  , DependentTypesParser(parserErrors, typeScope)
  , Field(..)
  , TypeBody(..)
  , runDependentTypesParser
  , validateDependentTypeSyntax
  )

tests :: TestTree
tests =
  testGroup "Dependent types parser"
    [ testCase "parses type and function declarations" $ do
        let source = unlines
              [ "type Vector<T> struct {"
              , "    values: T"
              , "}"
              , ""
              , "func SafeDivide(x int, y int) -> int where y > 0 { }"
              ]
        case runDependentTypesParser source of
          Left err -> assertFailure $ "runDependentTypesParser failed: " <> err
          Right (defs, parserState) -> do
            parserErrors parserState @?= []
            assertBool "expected Vector type declaration" (any isVectorType defs)
            assertBool "expected SafeDivide function declaration" (any isSafeDivide defs)
            assertBool "definitions should populate the scope" (not (null (typeScope parserState)))

    , testCase "reports syntax problems" $ do
        let invalidSource = "alias Broken"
        let errors = validateDependentTypeSyntax invalidSource
        assertBool "expected dependent type parser to report errors" (not (null errors))
    ]
  where
    isVectorType :: DependentType -> Bool
    isVectorType (TypeDecl name _ (StructBody fields) _) =
      name == "Vector" && any ((== "values") . fieldNameFrom) fields
    isVectorType _ = False

    isSafeDivide :: DependentType -> Bool
    isSafeDivide (DependentFunction name _ _ _) = name == "SafeDivide"
    isSafeDivide _ = False

    fieldNameFrom :: Field -> String
    fieldNameFrom (Field fieldName _) = fieldName
