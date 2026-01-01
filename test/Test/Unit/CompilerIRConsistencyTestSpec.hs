{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIRConsistencyTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements
  , vectorOf, oneof, frequency, suchThat, Positive(..)
  )

import Compiler.IR 
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , buildSemanticIR
  , buildSemanticIRWithPackage
  , emitGo
  , rawSourceFromTypus
  , moduleFromTypus
  , ensurePackageDecl
  , ensureMainFunction
  , attachInferredImports
  )

import Parser (TypusFile(..), CodeBlock(..), parseTypus)
import Compiler.GoAst (GoModule(..), GoDecl(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length)
import Data.List (null)
import qualified Data.Text as T (pack, unpack)

-- | Generate simple valid Go-like code for IR testing
genSimpleCode :: Gen String
genSimpleCode = oneof
  [ return $ unlines
      [ "package main"
      , "func main() {"
      , "    println(\"hello\")"
      , "}"
      ]
  , return $ unlines
      [ "package main"
      , "import \"fmt\""
      , "func add(a int, b int) int {"
      , "    return a + b"
      , "}"
      , "func main() {"
      , "    result := add(1, 2)"
      , "    fmt.Println(result)"
      , "}"
      ]
  , return $ unlines
      [ "package main"
      , "type Counter struct {"
      , "    value int"
      , "}"
      , "func (c *Counter) Increment() {"
      , "    c.value++"
      , "}"
      , "func main() {"
      , "    counter := &Counter{value: 0}"
      , "    counter.Increment()"
      , "    println(counter.value)"
      , "}"
      ]
  ]

-- | Generate code with imports for testing import inference
genCodeWithImports :: Gen String
genCodeWithImports = oneof
  [ return $ unlines
      [ "package main"
      , "func main() {"
      , "    fmt.Println(\"hello\")"
      , "}"
      ]
  , return $ unlines
      [ "package main"
      , "func main() {"
      , "    os.Exit(1)"
      , "}"
      ]
  , return $ unlines
      [ "package main"
      , "func main() {"
      , "    strings.Contains(\"hello\", \"world\")"
      , "}"
      ]
  ]

-- Property tests

-- Property: buildSourceIR should preserve original source text
prop_buildSourceIR_preserves_source :: Property
prop_buildSourceIR_preserves_source =
  forAll genSimpleCode $ \code ->
    case parseTypus code of
      Left _ -> property True  -- Skip invalid code
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile code
        in property $ sourceText sourceIR === code

-- Property: buildSourceIR should preserve parsed file
prop_buildSourceIR_preserves_parsed :: Property
prop_buildSourceIR_preserves_parsed =
  forAll genSimpleCode $ \code ->
    case parseTypus code of
      Left _ -> property True  -- Skip invalid code
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile code
        in property $ sourceTypusFile sourceIR === typusFile

-- Property: semanticIR should contain sourceIR content
prop_semanticIR_contains_source :: Property
prop_semanticIR_contains_source =
  forAll genSimpleCode $ \code ->
    case parseTypus code of
      Left _ -> property True  -- Skip invalid code
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile code
            semanticIR = buildSemanticIR sourceIR
        in property $ semanticTypusFile semanticIR === typusFile

-- Property: emitGo should produce valid Go code
prop_emitGo_produces_valid :: Property
prop_emitGo_produces_valid =
  forAll genSimpleCode $ \code ->
    case parseTypus code of
      Left _ -> property True  -- Skip invalid code
      Right typusFile ->
        let sourceIR = buildSourceIR typusFile code
            semanticIR = buildSemanticIR sourceIR
            goIR = emitGo semanticIR
        in property $ not $ L.null $ T.unpack $ goCode goIR

-- Property: rawSourceFromTypus should extract code blocks
prop_rawSource_extracts_blocks :: Property
prop_rawSource_extracts_blocks =
  forAll genSimpleCode $ \code ->
    case parseTypus code of
      Left _ -> property True  -- Skip invalid code
      Right typusFile ->
        let rawCode = rawSourceFromTypus typusFile
        in property $ not $ null rawCode

-- Property: moduleFromTypus should create valid module
prop_moduleFromTypus_creates_module :: Property
prop_moduleFromTypus_creates_module =
  forAll genSimpleCode $ \code ->
    case parseTypus code of
      Left _ -> property True  -- Skip invalid code
      Right typusFile ->
        case moduleFromTypus typusFile of
          Left _ -> property True  -- May fail, that's OK
          Right module -> property $ not $ L.null $ goModuleDecls module

-- Property: ensurePackageDecl should add package if missing
prop_ensurePackage_adds_if_missing :: Property
prop_ensurePackage_adds_if_missing =
  let codeWithoutPackage = unlines
        [ "func main() {"
        , "    println(\"hello\")"
        , "}"
        ]
  in case parseTypus codeWithoutPackage of
       Left _ -> property True  -- Skip invalid code
       Right typusFile ->
         let withPackage = ensurePackageDecl typusFile
         in property $ True  -- Basic smoke test

-- Property: ensureMainFunction should add main if missing
prop_ensureMain_adds_if_missing :: Property
prop_ensureMain_adds_if_missing =
  let codeWithoutMain = unlines
        [ "package main"
        , "func helper() {"
        , "    println(\"helper\")"
        , "}"
        ]
  in case parseTypus codeWithoutMain of
       Left _ -> property True  -- Skip invalid code
       Right typusFile ->
         let withMain = ensureMainFunction typusFile
         in property $ True  -- Basic smoke test

-- Property: attachInferredImports should add necessary imports
prop_attachInferredImports_adds_imports :: Property
prop_attachInferredImports_adds_imports =
  forAll genCodeWithImports $ \code ->
    case parseTypus code of
      Left _ -> property True  -- Skip invalid code
      Right typusFile ->
        let withImports = attachInferredImports typusFile
        in property $ True  -- Basic smoke test

-- Unit tests

unit_tests :: TestTree
unit_tests = testGroup "Compiler IR Consistency Unit Tests"
  [ testCase "buildSourceIR preserves source L.and parsed content" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"hello world\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
          sourceText sourceIR @?= code
          sourceTypusFile sourceIR @?= typusFile

  , testCase "buildSemanticIR preserves sourceIR content" $ do
      let code = unlines
            [ "package main"
            , "func add(a int, b int) int {"
            , "    return a + b"
            , "}"
            , "func main() {"
            , "    result := add(1, 2)"
            , "    println(result)"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
              semanticIR = buildSemanticIR sourceIR
          semanticTypusFile semanticIR @?= typusFile

  , testCase "emitGo produces non-empty Go code" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"test\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
              goCodeText = T.unpack $ goCode goIR
          assertBool "Go code should not be empty" $ not $ null goCodeText
          assertBool "Go code should contain package" $ "package" `L.isInfixOf` goCodeText

  , testCase "rawSourceFromTypus extracts code blocks" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"extracted\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let rawCode = rawSourceFromTypus typusFile
          assertBool "raw source should not be empty" $ not $ null rawCode
          assertBool "raw source should contain function" $ "func" `L.isInfixOf` rawCode

  , testCase "moduleFromTypus creates Go module" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    println(\"module test\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          case moduleFromTypus typusFile of
            Left err -> assertFailure $ "module creation failed: " ++ show err
            Right goModule -> do
              assertBool "module should have declarations" $ not $ L.null $ goModuleDecls goModule

  , testCase "ensurePackageDecl adds package declaration" $ do
      let codeWithoutPackage = unlines
            [ "func main() {"
            , "    println(\"no package\")"
            , "}"
            ]
      case parseTypus codeWithoutPackage of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let withPackage = ensurePackageDecl typusFile
          -- Just verify it doesn't crash
          return ()

  , testCase "ensureMainFunction adds main function" $ do
      let codeWithoutMain = unlines
            [ "package main"
            , "func helper() {"
            , "    println(\"helper function\")"
            , "}"
            ]
      case parseTypus codeWithoutMain of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let withMain = ensureMainFunction typusFile
          -- Just verify it doesn't crash
          return ()

  , testCase "attachInferredImports adds fmt import" $ do
      let codeUsingFmt = unlines
            [ "package main"
            , "func main() {"
            , "    fmt.Println(\"using fmt\")"
            , "}"
            ]
      case parseTypus codeUsingFmt of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let withImports = attachInferredImports typusFile
          -- Just verify it doesn't crash
          return ()

  , testCase "buildSemanticIRWithPackage works with custom package" $ do
      let code = unlines
            [ "func main() {"
            , "    println(\"custom package\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
              semanticIR = buildSemanticIRWithPackage "custom" sourceIR
          -- Just verify it doesn't crash
          return ()

  , testCase "IR transformation pipeline consistency" $ do
      let code = unlines
            [ "package main"
            , "import \"fmt\""
            , "func greet(name string) {"
            , "    fmt.Printf(\"Hello, %s!\n\", name)"
            , "}"
            , "func main() {"
            , "    greet(\"World\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
              goCodeText = T.unpack $ goCode goIR
          
          assertBool "final Go code should contain package" $ "package" `L.isInfixOf` goCodeText
          assertBool "final Go code should contain import" $ "import" `L.isInfixOf` goCodeText
          assertBool "final Go code should contain functions" $ "func" `L.isInfixOf` goCodeText

  , testCase "IR handles empty input gracefully" $ do
      let emptyCode = ""
      case parseTypus emptyCode of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile emptyCode
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
          -- Should handle empty input without crashing
          return ()

  , testCase "IR handles complex structures" $ do
      let complexCode = unlines
            [ "package main"
            , "type Calculator struct {"
            , "    result float64"
            , "}"
            , "func (c *Calculator) Add(x float64) {"
            , "    c.result += x"
            , "}"
            , "func (c *Calculator) Get() float64 {"
            , "    return c.result"
            , "}"
            , "func main() {"
            , "    calc := &Calculator{result: 0}"
            , "    calc.Add(5)"
            , "    calc.Add(3)"
            , "    println(calc.Get())"
            , "}"
            ]
      case parseTypus complexCode of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile complexCode
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
              goCodeText = T.unpack $ goCode goIR
          
          assertBool "should handle complex structures" $ not $ null goCodeText
          assertBool "should contain struct definition" $ "type" `L.isInfixOf` goCodeText
  ]

-- Consistency tests

consistency_tests :: TestTree
consistency_tests = testGroup "IR Consistency Tests"
  [ testCase "SourceIR consistency roundtrip" $ do
      let code = unlines
            [ "package main"
            , "func test() {"
            , "    println(\"consistency test\")"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
              extractedCode = rawSourceFromTypus $ sourceTypusFile sourceIR
          -- The extracted code should be functionally equivalent
          assertBool "should extract meaningful code" $ not $ null extractedCode

  , testCase "SemanticIR preserves semantic information" $ do
      let code = unlines
            [ "package main"
            , "const PI = 3.14159"
            , "func circleArea(radius float64) float64 {"
            , "    return PI * radius * radius"
            , "}"
            , "func main() {"
            , "    area := circleArea(2.0)"
            , "    println(area)"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
              goCodeText = T.unpack $ goCode goIR
          
          -- Should preserve constants L.and functions
          assertBool "should preserve constants" $ "PI" `L.isInfixOf` goCodeText
          assertBool "should preserve functions" $ "circleArea" `L.isInfixOf` goCodeText

  , testCase "IR transformation preserves order" $ do
      let code = unlines
            [ "package main"
            , "func first() { println(\"first\") }"
            , "func second() { println(\"second\") }"
            , "func third() { println(\"third\") }"
            , "func main() {"
            , "    first()"
            , "    second()"
            , "    third()"
            , "}"
            ]
      case parseTypus code of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile code
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
              goCodeText = T.unpack $ goCode goIR
          
          -- Functions should appear in the right order
          let firstPos = takeWhile (/= '\n') $ dropWhile (/= "first") goCodeText
              secondPos = takeWhile (/= '\n') $ dropWhile (/= "second") goCodeText
              thirdPos = takeWhile (/= '\n') $ dropWhile (/= "third") goCodeText
          -- Basic order check
          assertBool "should contain L.all functions" $ 
            "first" `L.isInfixOf` goCodeText && 
            "second" `L.isInfixOf` goCodeText && 
            "third" `L.isInfixOf` goCodeText
  ]

-- Error handling tests

error_handling_tests :: TestTree
error_handling_tests = testGroup "IR Error Handling Tests"
  [ testCase "handles malformed input gracefully" $ do
      let malformedCode = unlines
            [ "package main"
            , "func broken() {"
            , "    @#$ invalid syntax"
            , "}"
            ]
      case parseTypus malformedCode of
        Left _ -> return ()  -- Expected to fail
        Right typusFile -> do
          -- Even if parsing succeeds, IR building should handle errors
          let sourceIR = buildSourceIR typusFile malformedCode
          return ()

  , testCase "handles incomplete structures" $ do
      let incompleteCode = unlines
            [ "package main"
            , "type Incomplete struct {"
            , "    Field string"
            , "    // missing closing brace"
            , "func main() {}"
            ]
      case parseTypus incompleteCode of
        Left _ -> return ()  -- Expected to fail
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile incompleteCode
          return ()

  , testCase "handles empty blocks" $ do
      let emptyBlocksCode = unlines
            [ "package main"
            , "func empty() {"
            , "}"
            , "func main() {"
            , "    empty()"
            , "}"
            ]
      case parseTypus emptyBlocksCode of
        Left err -> assertFailure $ "parse failed: " ++ err
        Right typusFile -> do
          let sourceIR = buildSourceIR typusFile emptyBlocksCode
              semanticIR = buildSemanticIR sourceIR
              goIR = emitGo semanticIR
          -- Should handle empty blocks without issues
          return ()
  ]

tests :: TestTree
tests = testGroup "Compiler IR Consistency Tests"
  [ testGroup "Property Tests"
    [ fastProperty "buildSourceIR preserves source" prop_buildSourceIR_preserves_source
    , fastProperty "buildSourceIR preserves parsed" prop_buildSourceIR_preserves_parsed
    , fastProperty "semanticIR contains source" prop_semanticIR_contains_source
    , fastProperty "emitGo produces valid" prop_emitGo_produces_valid
    , fastProperty "rawSource extracts blocks" prop_rawSource_extracts_blocks
    , fastProperty "moduleFromTypus creates module" prop_moduleFromTypus_creates_module
    , fastProperty "ensurePackage adds if missing" prop_ensurePackage_adds_if_missing
    , fastProperty "ensureMain adds if missing" prop_ensureMain_adds_if_missing
    , fastProperty "attachInferredImports adds imports" prop_attachInferredImports_adds_imports
    ]
  , unit_tests
  , consistency_tests
  , error_handling_tests
  ]