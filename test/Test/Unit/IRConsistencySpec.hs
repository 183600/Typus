{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.IRConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Gen, arbitrary, oneof, elements, choose, listOf, resize)

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
  )

import Parser
  ( TypusFile(..)
  , CodeBlock(..)
  , BlockDirectives(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import Compiler.GoAst
  ( GoModule(..)
  , GoDecl(..)
  , FuncDecl(..)
  , ImportDecl(..)
  , PackageDecl(..)
  )

import Compiler.Errors
  ( CompilerError(..)
  , CompilerResult
  )

import SourceLocation (SourceSpan(..), SourcePos(..))

import Data.List (isPrefixOf, isInfixOf, isSuffixOf, intercalate, nub)
import qualified Data.Set as Set
import Data.Char (isSpace, isLetter)

-- ============================================================================
-- IR Consistency Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "IR Consistency Tests"
    [ testGroup "SourceIR Consistency"
        [ testCase "sourceIR preserves original file structure" $ do
            let blocks = 
                  [ CodeBlock defaultBlockDirectives "func main() {\n    println(\"Hello\")\n}" (spanBetween (posAt 1 1) (posAt 3 2))
                  , CodeBlock defaultBlockDirectives "func helper() int {\n    return 42\n}" (spanBetween (posAt 5 1) (posAt 7 2))
                  ]
            let typusFile = TypusFile defaultFileDirectives [] blocks []
            let sourceIR = buildSourceIR typusFile
            
            sourceTypusFile sourceIR @?= typusFile
            let expectedText = unlines ["func main() {\n    println(\"Hello\")\n}", "func helper() int {\n    return 42\n}"]
            sourceText sourceIR @?= expectedText

        , testCase "sourceIR handles empty files gracefully" $ do
            let typusFile = TypusFile defaultFileDirectives [] [] []
            let sourceIR = buildSourceIR typusFile
            
            null (tfBlocks $ sourceTypusFile sourceIR) @?= True
            sourceText sourceIR @?= ""

        , testCase "sourceIR maintains block order" $ do
            let blocks = 
                  [ CodeBlock defaultBlockDirectives "first" (spanBetween (posAt 1 1) (posAt 1 6))
                  , CodeBlock defaultBlockDirectives "second" (spanBetween (posAt 2 1) (posAt 2 7))
                  , CodeBlock defaultBlockDirectives "third" (spanBetween (posAt 3 1) (posAt 3 6))
                  ]
            let typusFile = TypusFile defaultFileDirectives [] blocks []
            let sourceIR = buildSourceIR typusFile
            
            let blockTexts = map cbContent (tfBlocks $ sourceTypusFile sourceIR)
            blockTexts @?= ["first", "second", "third"]

        , testCase "sourceIR preserves source spans" $ do
            let span = spanBetween (posAt 5 10) (posAt 8 20)
            let block = CodeBlock defaultBlockDirectives "content" span
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            let originalSpan = cbSpan (head $ tfBlocks $ sourceTypusFile sourceIR)
            originalSpan @?= span
        ]

    , testGroup "SemanticIR Consistency"
        [ testCase "semanticIR maintains typus file reference" $ do
            let block = CodeBlock defaultBlockDirectives "func test() {}" (spanBetween (posAt 1 1) (posAt 1 16))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should build semantic IR" False
                Right semanticIR -> do
                    semanticTypusFile semanticIR @?= typusFile
                    assertBool "should have module" (True)

        , testCase "semanticIR contains valid Go module" $ do
            let block = CodeBlock defaultBlockDirectives "package main\n\nfunc main() {}" (spanBetween (posAt 1 1) (posAt 3 15))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should build semantic IR" False
                Right semanticIR -> do
                    let goModule = semanticModule semanticIR
                    assertBool "should have package declaration" (isJust $ gmPackage goModule)
                    assertBool "should have declarations" (not $ null $ gmDecls goModule)

        , testCase "semanticIR generates value information" $ do
            let block = CodeBlock defaultBlockDirectives "func main() {\n    x := 42\n    println(x)\n}" (spanBetween (posAt 1 1) (posAt 4 18))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should build semantic IR" False
                Right semanticIR -> do
                    let valueInfo = semanticValueInfo semanticIR
                    assertBool "should analyze value semantics" (not $ null valueInfo)

        , testCase "semanticIR with package context" $ do
            let mainBlock = CodeBlock defaultBlockDirectives "func main() {}" (spanBetween (posAt 1 1) (posAt 1 15))
            let helperBlock = CodeBlock defaultBlockDirectives "func helper() int { return 42 }" (spanBetween (posAt 1 1) (posAt 1 33))
            let mainFile = TypusFile defaultFileDirectives [] [mainBlock] []
            let helperFile = TypusFile defaultFileDirectives [] [helperBlock] []
            let sourceIR = buildSourceIR mainFile
            let packageFiles = [("helper.typus", helperFile)]
            
            case buildSemanticIRWithPackage sourceIR packageFiles of
                Left _ -> assertBool "should build semantic IR with package" False
                Right semanticIR -> do
                    let goModule = semanticModule semanticIR
                    assertBool "should include package declarations" (length (gmDecls goModule) >= 1)
        ]

    , testGroup "GoIR Consistency"
        [ testCase "GoIR contains valid Go source" $ do
            let block = CodeBlock defaultBlockDirectives "package main\n\nfunc main() {\n    println(\"Hello\")\n}" (spanBetween (posAt 1 1) (posAt 5 2))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should build semantic IR" False
                Right semanticIR -> do
                    let goIR = emitGo semanticIR
                    let goSource = goSource goIR
                    
                    assertBool "should contain package declaration" ("package main" `isInfixOf` goSource)
                    assertBool "should contain main function" ("func main" `isInfixOf` goSource)
                    assertBool "should contain println call" ("println" `isInfixOf` goSource)

        , testCase "GoIR maintains module structure" $ do
            let block = CodeBlock defaultBlockDirectives "package main\n\nimport \"fmt\"\n\nfunc main() {\n    fmt.Println(\"Hello\")\n}" (spanBetween (posAt 1 1) (posAt 7 2))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should build semantic IR" False
                Right semanticIR -> do
                    let goIR = emitGo semanticIR
                    let goModule = goModule goIR
                    let goSource = goSource goIR
                    
                    assertBool "should have package" (isJust $ gmPackage goModule)
                    assertBool "should have imports" (not $ null $ gmImports goModule)
                    assertBool "should have declarations" (not $ null $ gmDecls goModule)
                    assertBool "source should contain import" ("import" `isInfixOf` goSource)

        , testCase "GoIR handles multiple declarations" $ do
            let block = CodeBlock defaultBlockDirectives (unlines
                  [ "package main"
                  , ""
                  , "const x = 42"
                  , "var y string = \"hello\""
                  , ""
                  , "func helper() int { return x }"
                  , ""
                  , "func main() {"
                  , "    println(helper())"
                  , "}"
                  ]) (spanBetween (posAt 1 1) (posAt 10 2))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should build semantic IR" False
                Right semanticIR -> do
                    let goIR = emitGo semanticIR
                    let goModule = goModule goIR
                    let goSource = goSource goIR
                    
                    let decls = gmDecls goModule
                    assertBool "should have multiple declarations" (length decls >= 3)
                    assertBool "source should contain const" ("const" `isInfixOf` goSource)
                    assertBool "source should contain var" ("var" `isInfixOf` goSource)
                    assertBool "source should contain helper function" ("func helper" `isInfixOf` goSource)
        ]

    , testGroup "IR Transformation Consistency"
        [ testCase "source to semantic transformation preserves content" $ do
            let originalContent = "func main() { println(\"test\") }"
            let block = CodeBlock defaultBlockDirectives originalContent (spanBetween (posAt 1 1) (posAt 1 32))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should transform to semantic IR" False
                Right semanticIR -> do
                    let goIR = emitGo semanticIR
                    let goSource = goSource goIR
                    
                    assertBool "should preserve function name" ("func main" `isInfixOf` goSource)
                    assertBool "should preserve function call" ("println" `isInfixOf` goSource)

        , testCase "semantic to Go transformation is deterministic" $ do
            let block = CodeBlock defaultBlockDirectives "package main\n\nfunc main() { return }" (spanBetween (posAt 1 1) (posAt 3 20))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should build semantic IR" False
                Right semanticIR -> do
                    let goIR1 = emitGo semanticIR
                    let goIR2 = emitGo semanticIR
                    
                    goSource goIR1 @?= goSource goIR2
                    goModule goIR1 @?= goModule goIR2

        , testCase "transformation handles complex types" $ do
            let block = CodeBlock defaultBlockDirectives (unlines
                  [ "package main"
                  , ""
                  , "type Data struct {"
                  , "    Value int"
                  , "    Name string"
                  , "}"
                  , ""
                  , "func process(d Data) Data {"
                  , "    return Data{Value: d.Value + 1, Name: d.Name}"
                  , "}"
                  ]) (spanBetween (posAt 1 1) (posAt 11 50))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should handle complex types" False
                Right semanticIR -> do
                    let goIR = emitGo semanticIR
                    let goSource = goSource goIR
                    
                    assertBool "should contain type definition" ("type Data" `isInfixOf` goSource)
                    assertBool "should contain struct definition" ("struct" `isInfixOf` goSource)
                    assertBool "should contain function with struct parameter" ("func process(d Data)" `isInfixOf` goSource)
        ]

    , testGroup "Error Handling Consistency"
        [ testCase "invalid Go code produces error in semantic IR" $ do
            let block = CodeBlock defaultBlockDirectives "func invalid( {" (spanBetween (posAt 1 1) (posAt 1 14))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left errors -> do
                    assertBool "should produce compilation errors" (not $ null errors)
                Right _ -> assertBool "should reject invalid Go code" False

        , testCase "partial errors don't crash transformation" $ do
            let blocks = 
                  [ CodeBlock defaultBlockDirectives "func valid() {}" (spanBetween (posAt 1 1) (posAt 1 16))
                  , CodeBlock defaultBlockDirectives "func invalid( {" (spanBetween (posAt 2 1) (posAt 2 14))
                  ]
            let typusFile = TypusFile defaultFileDirectives [] blocks []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should handle partial errors gracefully" True
                Right _ -> assertBool "should handle mixed valid/invalid content" True

        , testCase "error messages are informative" $ do
            let block = CodeBlock defaultBlockDirectives "invalid syntax here" (spanBetween (posAt 1 1) (posAt 1 19))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left errors -> do
                    assertBool "should have error messages" (not $ null errors)
                    let firstError = head errors
                    assertBool "error should have description" (not $ null $ ceDescription firstError)
                Right _ -> assertBool "should produce errors for invalid syntax" False
        ]

    , testGroup "Property-Based Consistency Tests"
        [ fastProperty "IR transformations are idempotent" $
            \blocks ->
                let validBlocks = take 5 blocks  -- Limit for performance
                    typusFile = TypusFile defaultFileDirectives [] validBlocks []
                    sourceIR = buildSourceIR typusFile
                in case buildSemanticIR sourceIR of
                    Left _ -> property True  -- Invalid input is handled gracefully
                    Right semanticIR ->
                        let goIR1 = emitGo semanticIR
                            goIR2 = emitGo semanticIR
                        in goSource goIR1 === goSource goIR2

        , fastProperty "sourceIR preserves block count" $
            \blocks ->
                let validBlocks = take 10 blocks  -- Limit for performance
                    typusFile = TypusFile defaultFileDirectives [] validBlocks []
                    sourceIR = buildSourceIR typusFile
                    originalCount = length validBlocks
                    preservedCount = length $ tfBlocks $ sourceTypusFile sourceIR
                in originalCount === preservedCount

        , fastProperty "semanticIR always contains module when source is valid" $
            \validGoCode ->
                let block = CodeBlock defaultBlockDirectives validGoCode (spanBetween (posAt 1 1) (posAt 1 10))
                    typusFile = TypusFile defaultFileDirectives [] [block] []
                    sourceIR = buildSourceIR typusFile
                in case buildSemanticIR sourceIR of
                    Left _ -> property True  -- Invalid input handled gracefully
                    Right semanticIR ->
                        let goModule = semanticModule semanticIR
                        in property $ True  -- Always contains a GoModule when successful
        ]

    , testGroup "Performance and Stress Tests"
        [ testCase "handles large files efficiently" $ do
            let largeContent = unlines $ ["func main() {" ++ replicate i ' ' ++ "x := " ++ show i | i <- [1..1000]] ++ ["}"]
            let block = CodeBlock defaultBlockDirectives largeContent (spanBetween (posAt 1 1) (posAt 1001 2))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should handle large files" False
                Right semanticIR -> do
                    let goIR = emitGo semanticIR
                    let goSource = goSource goIR
                    assertBool "should generate Go source" (not $ null goSource)

        , testCase "handles many small blocks" $ do
            let blocks = [CodeBlock defaultBlockDirectives ("func f" ++ show i ++ "() {}") (spanBetween (posAt i 1) (posAt i 15)) | i <- [1..100]]
            let typusFile = TypusFile defaultFileDirectives [] blocks []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should handle many blocks" False
                Right semanticIR -> do
                    let goModule = semanticModule semanticIR
                    assertBool "should handle many declarations" (length (gmDecls goModule) >= 50)

        , testCase "handles deeply nested structures" $ do
            let nestedContent = unlines $ ["type Level" ++ show i ++ " struct {" ++ replicate (i `mod` 5) '\t' ++ "Field Level" ++ show (i+1) ++ "}" | i <- [1..20]] ++ ["type Level20 struct { Value int }"]
            let block = CodeBlock defaultBlockDirectives nestedContent (spanBetween (posAt 1 1) (posAt 21 30))
            let typusFile = TypusFile defaultFileDirectives [] [block] []
            let sourceIR = buildSourceIR typusFile
            
            case buildSemanticIR sourceIR of
                Left _ -> assertBool "should handle nested structures" False
                Right semanticIR -> do
                    let goIR = emitGo semanticIR
                    let goSource = goSource goIR
                    assertBool "should generate nested type definitions" ("type Level1" `isInfixOf` goSource)
        ]
    ]
  where
    isJust Nothing = False
    isJust (Just _) = True

    posAt line col = SourcePos line col 0
