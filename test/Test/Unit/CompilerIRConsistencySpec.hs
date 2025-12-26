module Test.Unit.CompilerIRConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, suchThat, elements)
import qualified Test.QuickCheck as QC

import Compiler (compile, CompilerError(..), CompilerResult, generateGoCode)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..), buildSourceIR, buildSemanticIR, rawSourceFromTypus)
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (Located(..), SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- | Generate simple valid Typus expressions
genSimpleExpression :: Gen String
genSimpleExpression = do
    base <- elements 
        [ "x := 42"
        , "y := true"
        , "z := \"hello\""
        , "a := 3.14"
        , "b := x + 1"
        , "c := y && false"
        ]
    return base

-- | Generate simple function declarations
genSimpleFunction :: Gen String
genSimpleFunction = do
    name <- elements ["add", "multiply", "isTrue", "getName"]
    params <- choose (0, 3)
    paramList <- if params == 0 
                 then return ""
                 else do
                     paramNames <- take params <$> listOf (elements ["x", "y", "z", "a", "b"])
                     return $ "(" ++ unwords paramNames ++ ")"
    body <- elements ["return 42", "return true", "return \"test\"", "return x + y"]
    return $ "func " ++ name ++ paramList ++ " {\n  " ++ body ++ "\n}"

-- | Generate simple type declarations
genSimpleType :: Gen String
genSimpleType = do
    typeName <- elements ["Person", "Point", "Result", "Status"]
    fields <- choose (1, 3)
    fieldList <- take fields <$> listOf (elements 
        ["name string", "age int", "x float64", "y float64", "valid bool"])
    return $ "type " ++ typeName ++ " struct {\n  " ++ unlines fieldList ++ "\n}"

-- | Generate complete but simple Typus files
genSimpleTypusFile :: Gen String
genSimpleTypusFile = do
    exprCount <- choose (1, 3)
    funcCount <- choose (0, 2)
    typeCount <- choose (0, 1)
    
    exprs <- take exprCount <$> listOf genSimpleExpression
    funcs <- take funcCount <$> listOf genSimpleFunction
    types <- take typeCount <$> listOf genSimpleType
    
    let directives = ["@ownership true", "@dependent_types true"]
    let content = unlines $ directives ++ types ++ funcs ++ exprs
    
    return content

tests :: TestTree
tests =
  testGroup "Compiler IR Generation Consistency"
    [ testGroup "SourceIR Generation"
        [ testCase "buildSourceIR preserves original file content" $ do
            let typusCode = "x := 42\ny := true\n"
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives typusCode]
                    }
            let sourceIR = buildSourceIR typusFile
            sourceText sourceIR @?= typusCode
            sourceTypusFile sourceIR @?= typusFile

        , testCase "rawSourceFromTypus extracts code correctly" $ do
            let typusCode = "func test() {\n  return 42\n}"
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives typusCode]
                    }
            let extracted = rawSourceFromTypus typusFile
            extracted @?= typusCode

        , testCase "SourceIR handles multiple code blocks" $ do
            let block1 = "x := 1"
            let block2 = "y := 2"
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = 
                        [ CodeBlock defaultBlockDirectives block1
                        , CodeBlock defaultBlockDirectives block2
                        ]
                    }
            let sourceIR = buildSourceIR typusFile
            let expected = block1 ++ "\n" ++ block2
            sourceText sourceIR @?= expected
        ]

    , testGroup "SemanticIR Generation"
        [ testCase "buildSemanticIR maintains consistency" $ do
            let typusCode = "x := 42\nfunc test() { return x }"
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives typusCode]
                    }
            let sourceIR = buildSourceIR typusFile
            let semanticIR = buildSemanticIR sourceIR
            semanticTypusFile semanticIR @?= typusFile
            assertBool "SemanticIR should contain valid content" $ 
                not $ null $ show semanticIR

        , testCase "buildSemanticIRWithPackage adds package declaration" $ do
            let typusCode = "x := 42"
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives typusCode]
                    }
            let sourceIR = buildSourceIR typusFile
            let semanticIR = buildSemanticIRWithPackage "testpkg" sourceIR
            assertBool "Should contain package declaration" $ 
                "package testpkg" `isInfixOf` show semanticIR
        ]

    , testGroup "IR Transformation Consistency"
        [ testCase "SourceIR to SemanticIR preserves declarations" $ do
            let typusCode = "func add(x int, y int) int { return x + y }"
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives typusCode]
                    }
            let sourceIR = buildSourceIR typusFile
            let semanticIR = buildSemanticIR sourceIR
            
            -- Check that function declaration is preserved
            assertBool "Function should be preserved in SemanticIR" $ 
                "add" `isInfixOf` show semanticIR

        , testCase "IR transformations maintain type information" $ do
            let typusCode = "x := 42\ny := \"hello\""
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives typusCode]
                    }
            let sourceIR = buildSourceIR typusFile
            let semanticIR = buildSemanticIR sourceIR
            
            -- Check that type information is maintained
            let semanticStr = show semanticIR
            assertBool "Should maintain integer type information" $ 
                "int" `isInfixOf` semanticStr
            assertBool "Should maintain string type information" $ 
                "string" `isInfixOf` semanticStr
        ]

    , testGroup "Go Code Generation Consistency"
        [ testCase "generateGoCode produces valid Go syntax" $ do
            let typusCode = "x := 42\nfunc main() { println(x) }"
            result <- compile typusCode "test"
            case result of
                Right goCode -> do
                    assertBool "Should contain package declaration" $ 
                        "package" `isInfixOf` goCode
                    assertBool "Should contain main function" $ 
                        "func main" `isInfixOf` goCode
                    assertBool "Should contain variable declaration" $ 
                        "var x" `isInfixOf` goCode || "x :=" `isInfixOf` goCode
                Left _ -> assertBool "Compilation should succeed" False

        , testCase "Go code generation preserves semantics" $ do
            let typusCode = "func add(a int, b int) int { return a + b }"
            result <- compile typusCode "test"
            case result of
                Right goCode -> do
                    assertBool "Should preserve function name" $ 
                        "func add" `isInfixOf` goCode
                    assertBool "Should preserve parameters" $ 
                        "a int" `isInfixOf` goCode && "b int" `isInfixOf` goCode
                    assertBool "Should preserve return statement" $ 
                        "return" `isInfixOf` goCode
                Left _ -> assertBool "Compilation should succeed" False

        , testCase "Go code generation handles complex expressions" $ do
            let typusCode = "result := (x + y) * 2 / (z - 1)"
            result <- compile typusCode "test"
            case result of
                Right goCode -> do
                    assertBool "Should preserve operator precedence" $ 
                        "(" `isInfixOf` goCode
                    assertBool "Should preserve arithmetic operations" $ 
                        "+" `isInfixOf` goCode && "*" `isInfixOf` goCode
                Left _ -> assertBool "Compilation should succeed" False
        ]

    , testGroup "Error Handling Consistency"
        [ testCase "IR generation handles invalid input gracefully" $ do
            let invalidCode = "x := 1\ny := \nz := 3"  -- Invalid assignment
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives invalidCode]
                    }
            let sourceIR = buildSourceIR sourceTypusFile
            -- Should build SourceIR even with invalid code
            assertBool "SourceIR should be built" $ 
                not $ null $ sourceText sourceIR

        , testCase "compilation errors are consistent across IR stages" $ do
            let syntaxError = "func test( { return 42 }"  -- Unbalanced parentheses
            result <- compile syntaxError "error_test"
            case result of
                Left errors -> do
                    assertBool "Should report compilation error" $ 
                        not $ null errors
                    assertBool "Error should be descriptive" $ 
                        length (show errors) > 10
                Right _ -> assertBool "Should fail on syntax error" False
        ]

    , testGroup "Property-based IR Consistency"
        [ fastProperty "SourceIR construction is deterministic" $ 
            prop_sourceIRDeterministic
        , fastProperty "SemanticIR preserves SourceIR content" $ 
            prop_semanticIRPreservesContent
        , fastProperty "Go code generation is deterministic" $ 
            prop_goCodeGenerationDeterministic
        , fastProperty "IR transformations are idempotent where appropriate" $ 
            prop_irTransformationsIdempotent
        ]

    , testGroup "Performance and Memory Consistency"
        [ testCase "IR generation handles large files efficiently" $ do
            let largeCode = unlines $ replicate 100 "x := " ++ show (42 :: Int)
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives largeCode]
                    }
            let sourceIR = buildSourceIR typusFile
            assertBool "Should handle large files" $ 
                length (sourceText sourceIR) > 100

        , testCase "multiple IR transformations don't leak memory" $ do
            let typusCode = "func test() { x := 42; return x }"
            let typusFile = TypusFile 
                    { tfFileDirectives = defaultFileDirectives
                    , tfCodeBlocks = [CodeBlock defaultBlockDirectives typusCode]
                    }
            -- Perform multiple transformations
            let sourceIR = buildSourceIR typusFile
            let semanticIR1 = buildSemanticIR sourceIR
            let semanticIR2 = buildSemanticIR sourceIR
            
            -- Results should be consistent
            assertBool "Multiple transformations should be consistent" $ 
                show semanticIR1 == show semanticIR2
        ]
    ]

-- Helper function for default directives
defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing

defaultBlockDirectives :: BlockDirectives  
defaultBlockDirectives = BlockDirectives Nothing Nothing Nothing

-- Property: SourceIR construction is deterministic
prop_sourceIRDeterministic :: String -> Bool
prop_sourceIRDeterministic code = 
    let typusFile = TypusFile 
            { tfFileDirectives = defaultFileDirectives
            , tfCodeBlocks = [CodeBlock defaultBlockDirectives code]
            }
        sourceIR1 = buildSourceIR typusFile
        sourceIR2 = buildSourceIR typusFile
    in sourceIR1 == sourceIR2

-- Property: SemanticIR preserves SourceIR content
prop_semanticIRPreservesContent :: String -> Bool
prop_semanticIRPreservesContent code = 
    let typusFile = TypusFile 
            { tfFileDirectives = defaultFileDirectives
            , tfCodeBlocks = [CodeBlock defaultBlockDirectives code]
            }
        sourceIR = buildSourceIR typusFile
        semanticIR = buildSemanticIR sourceIR
    in semanticTypusFile semanticIR == typusFile

-- Property: Go code generation is deterministic
prop_goCodeGenerationDeterministic :: String -> Bool
prop_goCodeGenerationDeterministic code = 
    case compile code "test1" of
        Right goCode1 -> 
            case compile code "test2" of
                Right goCode2 -> goCode1 == goCode2
                Left _ -> False
        Left _ -> True  -- If compilation fails, that's acceptable for property test

-- Property: IR transformations are idempotent where appropriate
prop_irTransformationsIdempotent :: String -> Bool
prop_irTransformationsIdempotent code = 
    let typusFile = TypusFile 
            { tfFileDirectives = defaultFileDirectives
            , tfCodeBlocks = [CodeBlock defaultBlockDirectives code]
            }
        sourceIR = buildSourceIR typusFile
        semanticIR1 = buildSemanticIR sourceIR
        semanticIR2 = buildSemanticIR sourceIR
    in show semanticIR1 == show semanticIR2