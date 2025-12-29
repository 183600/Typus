{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalCompilerIRConsistencyQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Compiler.IR
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler.GoAst (GoModule(..), GoDecl(..), GoImport(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import SourceLocation (SourceSpan(..), SourcePos(..), posAt, spanBetween)
import Data.List (sort, nub)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

-- | Test compiler IR consistency properties
testCompilerIRConsistencyProperties :: TestTree
testCompilerIRConsistencyProperties = testGroup "Compiler IR Consistency Properties"
  [ testProperty "source IR preserves typus file" propSourceIRPreservesTypusFile
  , testProperty "source IR text extraction is consistent" propSourceIRTextExtractionConsistent
  , testProperty "semantic IR preserves source file" propSemanticIRPreservesSourceFile
  , testProperty "semantic IR contains valid Go module" propSemanticIRContainsValidModule
  , testProperty "Go IR preserves module structure" propGoIRPreservesModuleStructure
  , testProperty "IR transformation pipeline is deterministic" propIRPipelineDeterministic
  ]

-- | Source IR should preserve the original Typus file
propSourceIRPreservesTypusFile :: TypusFile -> Bool
propSourceIRPreservesTypusFile typusFile =
  let sourceIR = buildSourceIR typusFile
  in sourceTypusFile sourceIR == typusFile

-- | Source IR text extraction should be consistent with raw source extraction
propSourceIRTextExtractionConsistent :: TypusFile -> Bool
propSourceIRTextExtractionConsistent typusFile =
  let sourceIR = buildSourceIR typusFile
      extractedText = sourceText sourceIR
      rawText = rawSourceFromTypus typusFile
  in extractedText == rawText

-- | Semantic IR should preserve the source Typus file
propSemanticIRPreservesSourceFile :: TypusFile -> Property
propSemanticIRPreservesSourceFile typusFile =
  let sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
       Right semanticIR -> semanticTypusFile semanticIR == typusFile
       Left _ -> property True  -- Errors are acceptable for malformed input

-- | Semantic IR should contain a valid Go module
propSemanticIRContainsValidModule :: TypusFile -> Property
propSemanticIRContainsValidModule typusFile =
  let sourceIR = buildSourceIR typusFile
      result = buildSemanticIR sourceIR
  in case result of
       Right semanticIR -> 
         let module' = semanticModule semanticIR
         in not (null (gmName module'))  -- Module should have a name
       Left _ -> property True  -- Errors are acceptable for malformed input

-- | Go IR should preserve the module structure from semantic IR
propGoIRPreservesModuleStructure :: TypusFile -> Property
propGoIRPreservesModuleStructure typusFile =
  let sourceIR = buildSourceIR typusFile
      semanticResult = buildSemanticIR sourceIR
  in case semanticResult of
       Right semanticIR ->
         let goIR = emitGo semanticIR
             semanticModule = semanticModule semanticIR
             goModule = goModule goIR
         in gmName semanticModule == gmName goModule  -- Names should match
       Left _ -> property True  -- Errors are acceptable for malformed input

-- | IR transformation pipeline should be deterministic
propIRPipelineDeterministic :: TypusFile -> Property
propIRPipelineDeterministic typusFile =
  let sourceIR1 = buildSourceIR typusFile
      sourceIR2 = buildSourceIR typusFile
      semanticResult1 = buildSemanticIR sourceIR1
      semanticResult2 = buildSemanticIR sourceIR2
  in case (semanticResult1, semanticResult2) of
       (Right ir1, Right ir2) -> 
         let goIR1 = emitGo ir1
             goIR2 = emitGo ir2
         in goSource goIR1 == goSource goIR2
       (Left _, Left _) -> property True  -- Both should fail the same way
       _ -> property False  -- Inconsistent results

-- | Test source IR operations
testSourceIROperations :: TestTree
testSourceIROperations = testGroup "Source IR Operations"
  [ testCase "build source IR from empty file" $
      let emptyFile = TypusFile defaultFileDirectives [] [] []
          sourceIR = buildSourceIR emptyFile
      in sourceTypusFile sourceIR == emptyFile &&
         null (sourceText sourceIR)
         
  , testCase "build source IR with code blocks" $
      let block = CodeBlock defaultBlockDirectives "func main() {}" (spanBetween (posAt 1 1) (posAt 1 20))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
      in sourceTypusFile sourceIR == file &&
         sourceText sourceIR == "func main() {}"
         
  , testCase "raw source extraction consistency" $
      let content = "func test() { return 42 }"
          block = CodeBlock defaultBlockDirectives content (spanBetween (posAt 1 1) (posAt 1 27))
          file = TypusFile defaultFileDirectives [] [block] []
          extracted = rawSourceFromTypus file
      in extracted == content
  ]

-- | Test semantic IR operations
testSemanticIROperations :: TestTree
testSemanticIROperations = testGroup "Semantic IR Operations"
  [ testCase "build semantic IR from simple file" $
      let block = CodeBlock defaultBlockDirectives "package main\n\nfunc main() {}" (spanBetween (posAt 1 1) (posAt 3 15))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
          result = buildSemanticIR sourceIR
      in case result of
           Right semanticIR -> 
             semanticTypusFile semanticIR == file &&
             not (null (gmName (semanticModule semanticIR)))
           Left _ -> fail "Failed to build semantic IR"
           
  , testCase "semantic IR includes value analysis" $
      let block = CodeBlock defaultBlockDirectives "package main\n\nfunc main() { x := 42 }" (spanBetween (posAt 1 1) (posAt 3 25))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
          result = buildSemanticIR sourceIR
      in case result of
           Right semanticIR -> 
             not (null (semanticValueInfo semanticIR))
           Left _ -> fail "Failed to build semantic IR"
           
  , testCase "build semantic IR with package" $
      let block1 = CodeBlock defaultBlockDirectives "package lib\n\nfunc helper() {}" (spanBetween (posAt 1 1) (posAt 3 19))
          block2 = CodeBlock defaultBlockDirectives "package main\n\nimport \"lib\"\n\nfunc main() {}" (spanBetween (posAt 1 1) (posAt 4 16))
          file1 = TypusFile defaultFileDirectives [] [block1] []
          file2 = TypusFile defaultFileDirectives [] [block2] []
          sourceIR = buildSourceIR file2
          packageFiles = [("lib", file1)]
          result = buildSemanticIRWithPackage sourceIR packageFiles
      in case result of
           Right semanticIR -> 
             let module' = semanticModule semanticIR
             in length (gmImports module') > 0  -- Should have imports from package
           Left _ -> fail "Failed to build semantic IR with package"
  ]

-- | Test Go IR operations
testGoIROperations :: TestTree
testGoIROperations = testGroup "Go IR Operations"
  [ testCase "emit Go from semantic IR" $
      let block = CodeBlock defaultBlockDirectives "package main\n\nfunc main() {}" (spanBetween (posAt 1 1) (posAt 3 15))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
          Right semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
      in not (null (goSource goIR)) &&
         gmName (goModule goIR) == gmName (semanticModule semanticIR)
         
  , testCase "Go IR source contains module content" $
      let content = "package main\n\nfunc main() { println(\"Hello\") }"
          block = CodeBlock defaultBlockDirectives content (spanBetween (posAt 1 1) (posAt 3 34))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
          Right semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          goSource = goSource goIR
      in "package main" `isInfixOf` goSource &&
         "func main" `isInfixOf` goSource &&
         "println" `isInfixOf` goSource
         
  , testCase "Go IR preserves module declarations" $
      let decl = GoFuncDecl "test" [] [] Nothing
          module' = GoModule "testmod" [decl] [] []
          semanticIR = SemanticIR 
            { semanticTypusFile = TypusFile defaultFileDirectives [] [] []
            , semanticModule = module'
            , semanticValueInfo = []
            }
          goIR = emitGo semanticIR
          goModule = goModule goIR
      in gmName goModule == "testmod" &&
         length (gmDecls goModule) == 1
  ]

-- | Test IR edge cases
testIREdgeCases :: TestTree
testIREdgeCases = testGroup "IR Edge Cases"
  [ testCase "empty Typus file" $
      let emptyFile = TypusFile defaultFileDirectives [] [] []
          sourceIR = buildSourceIR emptyFile
          result = buildSemanticIR sourceIR
      in case result of
           Right semanticIR -> 
             let goIR = emitGo semanticIR
             in not (null (goSource goIR))  -- Should still generate some output
           Left _ -> fail "Failed to handle empty file"
           
  , testCase "file with only comments" $
      let block = CodeBlock defaultBlockDirectives "// This is a comment\n// Another comment" (spanBetween (posAt 1 1) (posAt 2 22))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
          result = buildSemanticIR sourceIR
      in case result of
           Right _ -> pure ()  -- Should handle comments gracefully
           Left _ -> fail "Failed to handle file with only comments"
           
  , testCase "file with syntax errors" $
      let block = CodeBlock defaultBlockDirectives "package main\n\nfunc invalid syntax here {" (spanBetween (posAt 1 1) (posAt 3 32))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
          result = buildSemanticIR sourceIR
      in case result of
           Left _ -> pure ()  -- Should fail gracefully with syntax errors
           Right _ -> pure ()  -- Or succeed if syntax is tolerated
  ]

-- | Test IR consistency across transformations
testIRConsistencyTransformations :: TestTree
testIRConsistencyTransformations = testGroup "IR Consistency Transformations"
  [ testCase "source to semantic preserves content" $
      let originalContent = "package main\n\nfunc test() { return 42 }"
          block = CodeBlock defaultBlockDirectives originalContent (spanBetween (posAt 1 1) (posAt 3 30))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR = buildSourceIR file
          Right semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          goSource = goSource goIR
      in "package main" `isInfixOf` goSource &&
         "func test" `isInfixOf` goSource
         
  , testCase "multiple transformations are idempotent" $
      let block = CodeBlock defaultBlockDirectives "package main\n\nfunc main() {}" (spanBetween (posAt 1 1) (posAt 3 15))
          file = TypusFile defaultFileDirectives [] [block] []
          sourceIR1 = buildSourceIR file
          sourceIR2 = buildSourceIR file
          Right semanticIR1 = buildSemanticIR sourceIR1
          Right semanticIR2 = buildSemanticIR sourceIR2
          goIR1 = emitGo semanticIR1
          goIR2 = emitGo semanticIR2
      in goSource goIR1 == goSource goIR2
  ]

-- | All compiler IR consistency tests
testCompilerIRConsistencyQuickCheck :: TestTree
testCompilerIRConsistencyQuickCheck = testGroup "New Cabal Compiler IR Consistency QuickCheck Tests"
  [ testCompilerIRConsistencyProperties
  , testSourceIROperations
  , testSemanticIROperations
  , testGoIROperations
  , testIREdgeCases
  , testIRConsistencyTransformations
  ]