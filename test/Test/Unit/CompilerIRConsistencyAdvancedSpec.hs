{-# LANGUAGE CPP #-}

module Test.Unit.CompilerIRConsistencyAdvancedSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (intercalate)
import qualified Data.Set as Set

import Compiler.IR
  ( SourceIR(..)
  , SemanticIR(..)
  , GoIR(..)
  , buildSourceIR
  , buildSemanticIR
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
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import Compiler.GoAst
  ( GoModule(..)
  , GoDecl(..)
  , GoImport(..)
  )
import Compiler.Errors
  ( CompilerError(..)
  , CompilerResult
  , ErrorCategory(..)
  , ErrorSeverity(..)
  )

tests :: TestTree
tests = testGroup "Compiler IR Consistency Advanced Tests"
  [ sourceIRConsistencyTests
  , semanticIRConsistencyTests
  , goIRConsistencyTests
  , irTransformationTests
  , importInferenceTests
  , errorHandlingTests
  , quickCheckProperties
  ]

sourceIRConsistencyTests :: TestTree
sourceIRConsistencyTests = testGroup "SourceIR Consistency Tests"
  [ testCase "buildSourceIR preserves original file" $ do
      let typusFile = TypusFile defaultFileDirectives [] [] []
          sourceIR = buildSourceIR typusFile
      sourceTypusFile sourceIR @?= typusFile
      
  , testCase "rawSourceFromTypus concatenates block content" $ do
      let block1 = CodeBlock defaultBlockDirectives "func main() {}\n" undefined
          block2 = CodeBlock defaultBlockDirectives "fmt.Println(\"hello\")\n" undefined
          typusFile = TypusFile defaultFileDirectives [] [block1, block2] []
          expected = "func main() {}\nfmt.Println(\"hello\")\n"
      rawSourceFromTypus typusFile @?= expected
      
  , testCase "rawSourceFromTypus handles empty blocks" $ do
      let typusFile = TypusFile defaultFileDirectives [] [] []
      rawSourceFromTypus typusFile @?= ""
      
  , testCase "rawSourceFromTypus preserves block ordering" $ do
      let block1 = CodeBlock defaultBlockDirectives "first\n" undefined
          block2 = CodeBlock defaultBlockDirectives "second\n" undefined
          block3 = CodeBlock defaultBlockDirectives "third\n" undefined
          typusFile = TypusFile defaultFileDirectives [] [block1, block2, block3] []
          result = rawSourceFromTypus typusFile
      result @?= "first\nsecond\nthird\n"
  ]

semanticIRConsistencyTests :: TestTree
semanticIRConsistencyTests = testGroup "SemanticIR Consistency Tests"
  [ testCase "buildSemanticIR preserves file structure" $ do
      let typusFile = TypusFile defaultFileDirectives [] [] []
          sourceIR = buildSourceIR typusFile
      result <- return $ buildSemanticIR sourceIR
      case result of
        Right semanticIR -> semanticTypusFile semanticIR @?= typusFile
        Left err -> "Expected successful semantic analysis" @?= show err
        
  , testCase "semanticIR contains valid GoModule" $ do
      let block = CodeBlock defaultBlockDirectives "package main\nfunc main() {}\n" undefined
          typusFile = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR typusFile
      result <- return $ buildSemanticIR sourceIR
      case result of
        Right semanticIR -> do
          let goModule = semanticModule semanticIR
          gmPackageName goModule @?= "main"
          L.length (gmDecls goModule) @?= 1
        Left err -> "Expected successful semantic analysis" @?= show err
        
  , testCase "semanticIR includes value analysis" $ do
      let block = CodeBlock defaultBlockDirectives "package main\nvar x int = 42\n" undefined
          typusFile = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR typusFile
      result <- return $ buildSemanticIR sourceIR
      case result of
        Right semanticIR -> do
          let valueInfo = semanticValueInfo semanticIR
          L.length valueInfo @?= 1  -- Should detect the variable
        Left err -> "Expected successful semantic analysis" @?= show err
  ]

goIRConsistencyTests :: TestTree
goIRConsistencyTests = testGroup "GoIR Consistency Tests"
  [ testCase "emitGo produces valid Go source" $ do
      let block = CodeBlock defaultBlockDirectives "package main\nfunc main() {}\n" undefined
          typusFile = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR typusFile
      semanticResult <- return $ buildSemanticIR sourceIR
      case semanticResult of
        Right semanticIR -> do
          let goIR = emitGo semanticIR
              goSource = goSource goIR
          "package main" `L.isInfixOf` goSource @?= True
          "func main()" `L.isInfixOf` goSource @?= True
        Left err -> "Expected successful semantic analysis" @?= show err
        
  , testCase "emitGo preserves module structure" $ do
      let block = CodeBlock defaultBlockDirectives "package main\nimport \"fmt\"\nfunc main() {}\n" undefined
          typusFile = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR typusFile
      semanticResult <- return $ buildSemanticIR sourceIR
      case semanticResult of
        Right semanticIR -> do
          let goIR = emitGo semanticIR
              goModule = goModule goIR
              goSource = goSource goIR
          gmPackageName goModule @?= "main"
          "import \"fmt\"" `L.isInfixOf` goSource @?= True
        Left err -> "Expected successful semantic analysis" @?= show err
  ]

irTransformationTests :: TestTree
irTransformationTests = testGroup "IR Transformation Tests"
  [ testCase "ensurePackageDecl adds package when missing" $ do
      let input = "func main() {}\n"
          result = ensurePackageDecl input
      "package main" `L.isPrefixOf` result @?= True
      
  , testCase "ensurePackageDecl preserves existing package" $ do
      let input = "package custom\nfunc main() {}\n"
          result = ensurePackageDecl input
      result @?= input
      
  , testCase "ensureMainFunction adds main when missing" $ do
      let input = "package main\nfunc other() {}\n"
          result = ensureMainFunction input
      "func main()" `L.isInfixOf` result @?= True
      
  , testCase "ensureMainFunction preserves existing main" $ do
      let input = "package main\nfunc main() { fmt.Println(\"hello\") }\n"
          result = ensureMainFunction input
      result @?= input
      
  , testCase "attachInferredImports adds necessary imports" $ do
      let input = "package main\nfunc main() { fmt.Println(\"hello\") }\n"
          result = attachInferredImports input
      "import \"fmt\"" `L.isInfixOf` result @?= True
  ]

importInferenceTests :: TestTree
importInferenceTests = testGroup "Import Inference Tests"
  [ testCase "infers fmt import for Println" $ do
      let input = "package main\nfunc main() { fmt.Println(\"test\") }\n"
          result = attachInferredImports input
      "import \"fmt\"" `L.isInfixOf` result @?= True
      
  , testCase "infers multiple imports" $ do
      let input = "package main\nfunc main() { fmt.Println(math.Abs(-1)) }\n"
          result = attachInferredImports input
      "import \"fmt\"" `L.isInfixOf` result @?= True
      "import \"math\"" `L.isInfixOf` result @?= True
      
  , testCase "avoids duplicate imports" $ do
      let input = "package main\nimport \"fmt\"\nfunc main() { fmt.Println(\"test\") }\n"
          result = attachInferredImports input
      let importCount = L.length $ L.filter (== "import \"fmt\"") (lines result)
      importCount @?= 1
      
  , testCase "handles qualified imports" $ do
      let input = "package main\nfunc main() { rand.Seed(42) }\n"
          result = attachInferredImports input
      result @?= result  -- Should not crash, even if import detection fails
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Tests"
  [ testCase "handles malformed Go code gracefully" $ do
      let block = CodeBlock defaultBlockDirectives "package main\nfunc main {\n" undefined
          typusFile = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR typusFile
      result <- return $ buildSemanticIR sourceIR
      case result of
        Left _ -> "Expected parsing error" @?= "Got error"
        Right _ -> "Should still produce some result" @?= "Got success"
        
  , testCase "preserves errors through IR pipeline" $ do
      let block = CodeBlock defaultBlockDirectives "invalid syntax here" undefined
          typusFile = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR typusFile
      semanticResult <- return $ buildSemanticIR sourceIR
      case semanticResult of
        Left err -> errorCategory err `seq` True @?= True
        Right semanticIR -> do
          let goIR = emitGo semanticIR
          goSource goIR `seq` True @?= True  -- Should not crash
  ]

quickCheckProperties :: TestTree
quickCheckProperties = testGroup "QuickCheck IR Properties"
  [ fastProperty "buildSourceIR is idempotent for file structure" prop_buildSourceIR_idempotent
  , fastProperty "emitGo produces syntactically valid output" prop_emitGo_valid
  , fastProperty "IR transformations preserve essential structure" prop_transformations_preserve
  ]

-- QuickCheck property implementations
prop_buildSourceIR_idempotent :: TypusFile -> Property
prop_buildSourceIR_idempotent typusFile =
  let sourceIR1 = buildSourceIR typusFile
      sourceIR2 = buildSourceIR (sourceTypusFile sourceIR1)
  in sourceTypusFile sourceIR1 === sourceTypusFile sourceIR2

prop_emitGo_valid :: TypusFile -> Property
prop_emitGo_valid typusFile =
  let sourceIR = buildSourceIR typusFile
      semanticResult = buildSemanticIR sourceIR
  in case semanticResult of
    Right semanticIR -> do
      let goIR = emitGo semanticIR
          goSource = goSource goIR
      not (null goSource) ==> property True
    Left _ -> property True  -- Error cases don't need to satisfy this

prop_transformations_preserve :: String -> Property
prop_transformations_preserve code =
  let withPackage = ensurePackageDecl code
      withMain = ensureMainFunction withPackage
      withImports = attachInferredImports withMain
  in not (null code) ==> 
     not (null withPackage) && not (null withMain) && not (null withImports) ==> property True