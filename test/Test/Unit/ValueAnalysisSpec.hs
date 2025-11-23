module Test.Unit.ValueAnalysisSpec (tests) where

import Compiler.GoAst (GoModule, parseGoModule)
import Compiler.ValueAnalysis
import Data.List (sort)
import System.FilePath ((</>))
import qualified Test.QuickCheck as QC
import TestSupport.QuickCheck (fastProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

fixtureRoot :: FilePath
fixtureRoot = "test" </> "fixtures" </> "value_analysis_project"

tests :: TestTree
tests =
    testGroup "ValueAnalysis"
        [ testGroup "Composite fixture: values.go"
            [ testCase "classifies literal and reference bindings" $ do
                goModule <- loadFixtureModule "values.go"
                let infos = analyzeValueSemantics goModule
                    kindOf = kindsFor infos
                kindOf "value" @?= [ValueCopy]
                kindOf "text" @?= [ValueCopy]
                kindOf "numbers" @?= [Reference]
                kindOf "first" @?= [ValueCopy]
                kindOf "second" @?= [Reference]
                kindOf "groupedValue" @?= [ValueCopy]
                kindOf "groupedRef" @?= [Reference]
                kindOf "inner" @?= [Reference]
                kindOf "lookup" @?= [Reference]
                kindOf "fake" @?= []

            , testCase "recognises custom structs and grouped aliases" $ do
                goModule <- loadFixtureModule "values.go"
                let infos = analyzeValueSemantics goModule
                    kindOf = kindsFor infos
                kindOf "customStruct" @?= [ValueCopy]
                kindOf "customAlias" @?= [ValueCopy]
                kindOf "groupStruct" @?= [ValueCopy]
                kindOf "groupAlias" @?= [ValueCopy]
                kindOf "customPointer" @?= [Reference]

            , testCase "extracts ordered value copies from the composite fixture" $ do
                goModule <- loadFixtureModule "values.go"
                let sortedCopies = sort (extractValueCopyVars goModule)
                sortedCopies @?= ["customAlias", "customStruct", "first", "groupAlias", "groupStruct", "groupedValue", "text", "value"]
            ]

        , testCase "classifies short declarations inside guards" $ do
            goModule <- loadFixtureModule "control_flow.go"
            let infos = analyzeValueSemantics goModule
                kindOf = kindsFor infos
            kindOf "immediate" @?= [ValueCopy]
            kindOf "pointer" @?= [Reference]
            kindOf "alias" @?= [Reference]
            kindOf "ref" @?= [Reference]

        , testCase "handles composite literals and builder patterns" $ do
            goModule <- loadFixtureModule "composites.go"
            let infos = analyzeValueSemantics goModule
                kindOf = kindsFor infos
            kindOf "first" @?= [Reference]
            kindOf "second" @?= [ValueCopy]
            kindOf "alias" @?= [ValueCopy]
            kindOf "trailing" @?= [Reference]
            kindOf "pointer" @?= [Reference]
            kindOf "fromMake" @?= [Reference]

        , testGroup "Property-based guarantees"
            [ fastProperty "ampersand-prefixed expressions are references" prop_ampersandClassifiedAsReference
            , fastProperty "whitespace does not affect builtin value types" prop_builtinValueTypesRecognized
            , fastProperty "pointer-prefixed types are not treated as values" prop_pointerTypesRejected
            ]
        ]

loadFixtureModule :: FilePath -> IO GoModule
loadFixtureModule relative = do
    contents <- readFile (fixtureRoot </> relative)
    parseModule contents

parseModule :: String -> IO GoModule
parseModule source =
    case parseGoModule (lines source) of
        Left err -> assertFailure ("Failed to parse Go module: " ++ err)
        Right goMod -> pure goMod

kindsFor :: [ValueInfo] -> String -> [ValueKind]
kindsFor infos target =
    [ viKind info
    | info <- infos
    , viName info == target
    ]

identifierChars :: [Char]
identifierChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

genIdentifier :: QC.Gen String
genIdentifier = QC.listOf1 (QC.elements identifierChars)

builtinValueTypesSample :: [String]
builtinValueTypesSample =
    [ "int", "int8", "int16", "int32", "int64"
    , "uint", "uint8", "uint16", "uint32", "uint64"
    , "float32", "float64", "complex64", "complex128"
    , "bool", "byte", "rune", "string"
    ]

prop_ampersandClassifiedAsReference :: QC.Property
prop_ampersandClassifiedAsReference =
    QC.forAll genIdentifier $ \name ->
        let expr = "  &" ++ name
        in QC.counterexample expr (isReferenceInit expr)

prop_builtinValueTypesRecognized :: QC.Property
prop_builtinValueTypesRecognized =
    QC.forAll (QC.elements builtinValueTypesSample) $ \ty ->
        QC.counterexample ty (isValueType ("  " ++ ty ++ "  "))

prop_pointerTypesRejected :: QC.Property
prop_pointerTypesRejected =
    QC.forAll (QC.elements builtinValueTypesSample) $ \ty ->
        let pointerTy = '*' : ty
        in QC.counterexample pointerTy (not (isValueType pointerTy))
