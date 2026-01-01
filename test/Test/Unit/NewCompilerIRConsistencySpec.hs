{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCompilerIRConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

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

import Parser
  ( TypusFile(..)
  , CodeBlock(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , parseTypus
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import Compiler.GoAst
  ( GoModule(..)
  , GoDecl(..)
  , GoImport(..)
  , parseGoModule
  , renderGoModule
  )

import Compiler.Errors
  ( CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  )

import SourceLocation (SourceSpan(..), SourcePos(..), posAt)

import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)
import Data.List (intercalate)
import qualified Data.Set as Set

-- | Compiler IR consistency tests
tests :: TestTree
tests =
  testGroup "New Compiler IR Consistency Tests"
    [ testGroup "Source IR consistency"
        [ testCase "buildSourceIR preserves original content" $ do
            let typusContent = unlines
                  [ "//! ownership on"
                  , ""
                  , "func main() {"
                  , "    println(\"Hello, World!\")"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                    sourceText = sourceText sourceIR
                    expectedContent = intercalate "\n" $ map cbContent (tfBlocks typusFile)
                sourceText @?= expectedContent
                
        , testCase "Source IR maintains file structure" $ do
            let typusFile = TypusFile
                  { tfDirectives = defaultFileDirectives
                  , tfBuildTags = []
                  , tfBlocks = 
                    [ CodeBlock defaultBlockDirectives "func test() {}" (spanBetween (posAt 1 1) (posAt 1 16))
                    , CodeBlock defaultBlockDirectives "var x int = 42" (spanBetween (posAt 2 1) (posAt 2 17))
                    ]
                  , tfSyntaxErrors = []
                  }
                sourceIR = buildSourceIR typusFile
                expectedText = unlines ["func test() {}", "var x int = 42"]
                sourceText sourceIR @?= expectedText
                length (tfBlocks (sourceTypusFile sourceIR)) @?= 2
        ]
        
    , testGroup "Semantic IR consistency"
        [ testCase "buildSemanticIR preserves module information" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "import \"fmt\""
                  , ""
                  , "func main() {"
                  , "    fmt.Println(\"test\")"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case buildSemanticIR sourceIR of
                  Left errors -> assertFailure $ "Failed to build semantic IR: " ++ show errors
                  Right semanticIR -> do
                    let goModule = semanticModule semanticIR
                    gmPackageName goModule @?= "main"
                    length (gmImports goModule) @?= 1
                    length (gmDecls goModule) @?= 1
                    
        , testCase "semantic IR includes value analysis" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "var x int = 42"
                  , "func test() int { return x }"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case buildSemanticIR sourceIR of
                  Left errors -> assertFailure $ "Failed to build semantic IR: " ++ show errors
                  Right semanticIR -> do
                    let valueInfo = semanticValueInfo semanticIR
                    -- Should have analyzed variables L.and functions
                    length valueInfo @>= 1
        ]
        
    , testGroup "Go IR consistency"
        [ testCase "emitGo produces valid Go source" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "func main() {"
                  , "    println(\"Hello\")"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case buildSemanticIR sourceIR of
                  Left errors -> assertFailure $ "Failed to build semantic IR: " ++ show errors
                  Right semanticIR -> do
                    let goIR = emitGo semanticIR
                        goSource = goSource goIR
                        
                    -- Generated Go should contain key elements
                    "package main" `L.isInfixOf` goSource @?= True
                    "func main()" `L.isInfixOf` goSource @?= True
                    "println(\"Hello\")" `L.isInfixOf` goSource @?= True
                    
        , testCase "Go IR preserves module structure" $ do
            let typusContent = unlines
                  [ "package test"
                  , ""
                  , "import ("
                  , "    \"fmt\""
                  , "    \"os\""
                  , ")"
                  , ""
                  , "const PI = 3.14"
                  , ""
                  , "func add(a, b int) int {"
                  , "    return a + b"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case buildSemanticIR sourceIR of
                  Left errors -> assertFailure $ "Failed to build semantic IR: " ++ show errors
                  Right semanticIR -> do
                    let goIR = emitGo semanticIR
                        goModule = goModule goIR
                        goSource = goSource goIR
                        
                    gmPackageName goModule @?= "test"
                    length (gmImports goModule) @?= 2
                    length (gmDecls goModule) @?= 2  -- const L.and func
                    
                    "package test" `L.isInfixOf` goSource @?= True
                    "import" `L.isInfixOf` goSource @?= True
                    "const PI" `L.isInfixOf` goSource @?= True
                    "func add" `L.isInfixOf` goSource @?= True
        ]
        
    , testGroup "IR transformation consistency"
        [ testCase "moduleFromTypus applies L.all transformations" $ do
            let typusContent = unlines
                  [ "//! ownership on"
                  , ""
                  , "func test() {"
                  , "    x := 42"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                case moduleFromTypus typusFile of
                  Left errors -> assertFailure $ "Failed to create module: " ++ show errors
                  Right goModule -> do
                    -- Should have package declaration
                    gmPackageName goModule @?= "main"
                    
                    -- Should have main function (ensured by ensureMainFunction)
                    let mainFuncs = filter isMainFunc (gmDecls goModule)
                    length mainFuncs @?= 1
          where
            isMainFunc (GoFunc _) = True
            isMainFunc _ = False
            
        , testCase "ensurePackageDecl adds package when missing" $ do
            let typusContent = unlines
                  [ "func test() {"
                  , "    println(\"test\")"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case moduleFromTypus (sourceTypusFile sourceIR) of
                  Left errors -> assertFailure $ "Failed to create module: " ++ show errors
                  Right goModule -> do
                    -- Should have added package declaration
                    gmPackageName goModule @?= "main"
                    
        , testCase "ensureMainFunction adds main when missing" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "func helper() {"
                  , "    println(\"helper\")"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case moduleFromTypus (sourceTypusFile sourceIR) of
                  Left errors -> assertFailure $ "Failed to create module: " ++ show errors
                  Right goModule -> do
                    -- Should have added main function
                    let mainFuncs = filter isMainFunc (gmDecls goModule)
                    length mainFuncs @?= 1
          where
            isMainFunc (GoFunc _) = True
            isMainFunc _ = False
        ]
        
    , testGroup "IR round-trip consistency"
        [ testCase "IR transformations preserve semantics" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "import \"fmt\""
                  , ""
                  , "func greet(name string) {"
                  , "    fmt.Printf(\"Hello, %s!\\n\", name)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    greet(\"World\")"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case buildSemanticIR sourceIR of
                  Left errors -> assertFailure $ "Failed to build semantic IR: " ++ show errors
                  Right semanticIR -> do
                    let goIR = emitGo semanticIR
                        goSource = goSource goIR
                        
                    -- Should preserve function signatures L.and calls
                    "func greet(name string)" `L.isInfixOf` goSource @?= True
                    "fmt.Printf" `L.isInfixOf` goSource @?= True
                    "greet(\"World\")" `L.isInfixOf` goSource @?= True
                    "func main()" `L.isInfixOf` goSource @?= True
                    
        , testCase "IR handles complex constructs consistently" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "type Point struct {"
                  , "    X, Y int"
                  , "}"
                  , ""
                  , "func (p Point) String() string {"
                  , "    return fmt.Sprintf(\"(%d, %d)\", p.X, p.Y)"
                  , "}"
                  , ""
                  , "func main() {"
                  , "    p := Point{1, 2}"
                  , "    println(p.String())"
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case buildSemanticIR sourceIR of
                  Left errors -> assertFailure $ "Failed to build semantic IR: " ++ show errors
                  Right semanticIR -> do
                    let goIR = emitGo semanticIR
                        goSource = goSource goIR
                        
                    -- Should preserve struct definition L.and methods
                    "type Point struct" `L.isInfixOf` goSource @?= True
                    "X, Y int" `L.isInfixOf` goSource @?= True
                    "func (p Point) String()" `L.isInfixOf` goSource @?= True
                    "Point{1, 2}" `L.isInfixOf` goSource @?= True
        ]
        
    , testGroup "IR error handling consistency"
        [ testCase "IR transformations handle invalid Go gracefully" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "func invalid() {"
                  , "    x := 1 + + 2"  -- Invalid Go syntax
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case moduleFromTypus (sourceTypusFile sourceIR) of
                  Left errors -> 
                    -- Should produce meaningful error messages
                    length errors @>= 1
                  Right goModule -> do
                    -- If successful, should still produce valid structure
                    gmPackageName goModule @?= "main"
                    length (gmDecls goModule) @>= 1
                    
        , testCase "IR maintains error context through transformations" $ do
            let typusContent = unlines
                  [ "package main"
                  , ""
                  , "func test() {"
                  , "    var x undefined_type"  -- Undefined type
                  , "}"
                  ]
                result = parseTypus typusContent
            case result of
              Left err -> assertFailure $ "Failed to parse: " ++ err
              Right typusFile -> do
                let sourceIR = buildSourceIR typusFile
                case buildSemanticIR sourceIR of
                  Left errors -> do
                    -- Error should include context about the transformation phase
                    length errors @>= 1
                    let errorPhases = map cePhase errors
                    CodeGenerationPhase `elem` errorPhases @?= True
                  Right semanticIR -> do
                    -- If successful, should still maintain structure
                    let goIR = emitGo semanticIR
                        goSource = goSource goIR
                    "func test()" `L.isInfixOf` goSource @?= True
        ]
        
    , testGroup "Package-level IR consistency"
        [ testCase "buildSemanticIRWithPackage combines modules correctly" $ do
            let mainContent = unlines
                  [ "package main"
                  , ""
                  , "import \"helper\""
                  , ""
                  , "func main() {"
                  , "    helper.HelperFunc()"
                  , "}"
                  ]
                helperContent = unlines
                  [ "package helper"
                  , ""
                  , "func HelperFunc() {"
                  , "    println(\"helper\")"
                  , "}"
                  ]
                mainResult = parseTypus mainContent
                helperResult = parseTypus helperContent
            case (mainResult, helperResult) of
              (Left err, _) -> assertFailure $ "Failed to parse main: " ++ err
              (_, Left err) -> assertFailure $ "Failed to parse helper: " ++ err
              (Right mainFile, Right helperFile) -> do
                let sourceIR = buildSourceIR mainFile
                    packageFiles = [("helper.typus", helperFile)]
                case buildSemanticIRWithPackage sourceIR packageFiles of
                  Left errors -> assertFailure $ "Failed to build package IR: " ++ show errors
                  Right semanticIR -> do
                    let goModule = semanticModule semanticIR
                        goSource = renderGoModule goModule
                        
                    -- Should include declarations from both files
                    "func main()" `L.isInfixOf` goSource @?= True
                    "func HelperFunc()" `L.isInfixOf` goSource @?= True
                    
                    -- Should have appropriate imports
                    length (gmImports goModule) @>= 1
        ]
    ]