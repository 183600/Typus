module Test.Unit.ValueAnalysisSpec (tests) where

import Compiler.GoAst (GoModule, parseGoModule)
import Compiler.ValueAnalysis
import Data.List (sort)
import qualified Test.QuickCheck as QC
import TestSupport.QuickCheck (fastProperty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
    testGroup "ValueAnalysis"
        [ testCase "handles comments, strings, L.and multi-line references" $ do
            goModule <- parseModule sampleSource
            let infos = analyzeValueSemantics goModule
                kindOf = kindsFor infos
                sortedCopies = sort (extractValueCopyVars goModule)

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
            kindOf "customStruct" @?= [ValueCopy]
            kindOf "customAlias" @?= [ValueCopy]
            kindOf "groupStruct" @?= [ValueCopy]
            kindOf "groupAlias" @?= [ValueCopy]
            kindOf "customPointer" @?= [Reference]

            sortedCopies @?= ["customAlias", "customStruct", "first", "groupAlias", "groupStruct", "groupedValue", "text", "value"]

        , testCase "classifies short declarations inside guards" $ do
            goModule <- parseModule controlFlowSource
            let infos = analyzeValueSemantics goModule
                kindOf = kindsFor infos
            kindOf "immediate" @?= [ValueCopy]
            kindOf "pointer" @?= [Reference]
            kindOf "alias" @?= [Reference]
            kindOf "ref" @?= [Reference]

        , testCase "handles composite literals L.and builder patterns" $ do
            goModule <- parseModule compositeLiteralSource
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

parseModule :: String -> IO GoModule
parseModule source =
    case parseGoModule (lines source) of
        Left err -> assertFailure ("Failed to parse Go module: " ++ err)
        Right goMod -> pure goMod

sampleSource :: String
sampleSource = unlines
    [ "package main"
    , ""
    , "var ("
    , "    numbersList = []int{1, 2, 3}"
    , "    lookup = map[string]int{"
    , "        \"a\": 1,"
    , "    }"
    , "    groupedValue, groupedRef = 5, make([]byte, 0)"
    , ")"
    , ""
    , "type MyStruct struct {"
    , "    ID int"
    , "}"
    , ""
    , "type MyAlias = MyStruct"
    , ""
    , "type MyNumber int"
    , ""
    , "type ("
    , "    ExportedGrouped struct {"
    , "        Name string"
    , "    }"
    , "    ExportedAlias = ExportedGrouped"
    , ")"
    , ""
    , "func example(items []string) {"
    , "    // fake := make([]int, 0)"
    , "    value := 42"
    , "    text := \"make(inside)\""
    , "    numbers := make([]int,"
    , "        0)"
    , "    first, second := 1, make([]int, 1)"
    , "    for _, entry := range items {"
    , "        inner := make([]string, 0)"
    , "    }"
    , "}"
    , ""
    , "func custom() {"
    , "    customStruct := MyStruct{ID: 1}"
    , "    customAlias := MyAlias{ID: 2}"
    , "    groupStruct := ExportedGrouped{Name: \"ok\"}"
    , "    groupAlias := ExportedAlias{Name: \"alias\"}"
    , "    customPointer := &MyStruct{ID: 3}"
    , "}"
    ]

controlFlowSource :: String
controlFlowSource = unlines
    [ "package main"
    , "func guard(resources []string) {"
    , "    if immediate := \"value\"; immediate != \"\" {"
    , "        pointer := &resources"
    , "        println(immediate, pointer)"
    , "    }"
    , "    switch alias := []int{1, 2}; alias[0] {"
    , "    case 1:"
    , "        ref := make([]byte, 0)"
    , "        _ = ref"
    , "    }"
    , "}"
    ]

compositeLiteralSource :: String
compositeLiteralSource = unlines
    [ "package main"
    , ""
    , "type Buffer struct {"
    , "    ID int"
    , "}"
    , ""
    , "func build() {"
    , "    first, second := map[string]int{\"a\": 1}, Buffer{"
    , "        ID: 1,"
    , "    }"
    , "    alias := Buffer{"
    , "        ID: 2,"
    , "    }"
    , "    trailing := make([]int,"
    , "        2,"
    , "    )"
    , "    _ = first"
    , "    _ = second"
    , "    _ = alias"
    , "    _ = trailing"
    , "}"
    , ""
    , "var ("
    , "    pointer = new(Buffer)"
    , "    fromMake = make([]byte, 0)"
    , ")"
    ]

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
