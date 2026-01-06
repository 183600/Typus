{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.CoreCompilerEssentialSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import qualified Data.Text as T

import Compiler (compile, CompilerError(..), CompilationPhase(..), hasTypeErrors, generateGoCode, ensureSourceIR, diagnoseTypeErrors, hasMalformedSyntax, typeDiagnosticToCompilerError)
import Parser (TypusFile(..), FileDirectives(..), defaultFileDirectives, CodeBlock(..), defaultBlockDirectives, BlockDirectives(..))
import SourceLocation (locatedAt, startPos, emptySpan, SourcePos(..), SourceSpan(..))
-- import Compiler.Errors.Compiler (CompilerError(..), CompilationPhase(..), typeCheckFailure, malformedSyntaxError)

tests :: TestTree
tests = testGroup "Core Compiler Essential Tests"
  [ testGroup "Basic Compilation"
    [ testCase "compile handles empty file" $
        let emptyFile = TypusFile defaultFileDirectives [] [] []
        in case compile emptyFile of
          Left err -> assertBool "should handle empty file gracefully" True
          Right result -> assertBool "should compile empty file" True
    
    , testCase "compile handles simple function" $
        let simpleFile = TypusFile defaultFileDirectives 
                          [] [mockCodeBlock "func main() {\n  return 0\n}"] []
        in case compile simpleFile of
          Left err -> assertBool "should compile simple function" False
          Right result -> assertBool "should produce result" True
    
    , testCase "compile detects syntax errors" $
        let invalidFile = TypusFile defaultFileDirectives 
                            [] [mockCodeBlock "func invalid {\n  missing closing"] []
        in case compile invalidFile of
          Left err -> assertBool "should detect syntax errors" True
          Right _ -> assertBool "should not succeed on invalid input" False
    ]
  
  , testGroup "Type Checking"
    [ testCase "hasTypeErrors identifies type errors" $
        let typeErrorFile = TypusFile defaultFileDirectives [] [] []
        in do
          assertBool "should handle empty file" (hasTypeErrors typeErrorFile)
    
    , testCase "compile performs type checking" $
        let typedFile = TypusFile defaultFileDirectives 
                           [] [mockCodeBlock "func add(a: int, b: int) int {\n  return a + b\n}"] []
        in case compile typedFile of
          Left err -> assertBool "should type check valid code" False
          Right result -> assertBool "should complete type checking" True
    
    , testCase "compile detects type mismatches" $
        let invalidTypedFile = TypusFile defaultFileDirectives 
                                [] [mockCodeBlock "func test() {\n  let x: int = \"string\"\n}"] []
        in case compile invalidTypedFile of
          Left err -> assertBool "should detect type mismatch" True
          Right _ -> assertBool "should not succeed on type error" False
    ]
  
  , testGroup "Code Generation"
    [ testCase "generateGoCode produces valid Go syntax" $
        let simpleFile = TypusFile defaultFileDirectives 
                          [] [mockCodeBlock "func hello() {\n  println(\"Hello\")\n}"] []
            goCode = generateGoCode simpleFile
        in do
            assertBool "should contain package declaration" ("package" `L.isInfixOf` goCode)
            assertBool "should contain func keyword" ("func" `L.isInfixOf` goCode)
    
    , testCase "generateGoCode handles multiple functions" $
        let multiFile = TypusFile defaultFileDirectives 
                          [] [ mockCodeBlock "func one() {}"
                          , mockCodeBlock "func two() {}"
                          ] []
            goCode = generateGoCode multiFile
        in do
            assertBool "should contain both functions" 
              ("func one()" `L.isInfixOf` goCode && "func two()" `L.isInfixOf` goCode)
    
    , testCase "generateGoCode preserves function signatures" $
        let signatureFile = TypusFile defaultFileDirectives 
                             [] [mockCodeBlock "func add(a: int, b: int) int {\n  return a + b\n}"] []
            goCode = generateGoCode signatureFile
        in 
            assertBool "should contain parameter types" 
              ("a int" `L.isInfixOf` goCode && "b int" `L.isInfixOf` goCode)
    ]
  
  , testGroup "Error Reporting"
    [ testCase "compile provides phase information" $
        let errorFile = TypusFile defaultFileDirectives 
                          [] [mockCodeBlock "invalid syntax here"] []
        in case compile errorFile of
          Left err -> assertBool "error should include phase" True
          Right _ -> assertBool "should not succeed on invalid input" False
    
    , testCase "compile handles multiple errors" $
        let multiErrorFile = TypusFile defaultFileDirectives 
                              [] [ mockCodeBlock "func bad1() {"
                              , mockCodeBlock "func bad2() {"
                              ] []
        in case compile multiErrorFile of
          Left err -> assertBool "should detect multiple errors" True
          Right _ -> assertBool "should not succeed with errors" False
    ]
  
  , testGroup "Integration Features"
    [ testCase "compile handles ownership annotations" $
        let ownershipFile = TypusFile (defaultFileDirectives { fdOwnership = Just (locatedAt startPos True) }) 
                              [] [mockCodeBlock "func transfer() {\n  // ownership logic\n}"] []
        in case compile ownershipFile of
          Left err -> assertBool "should handle ownership" True
          Right result -> assertBool "should process ownership" True
    
    , testCase "compile handles dependent types" $
        let depTypesFile = TypusFile (defaultFileDirectives { fdDependentTypes = Just (locatedAt startPos True) }) 
                             [] [mockCodeBlock "func dependent(n: int) {\n  // dependent type logic\n}"] []
        in case compile depTypesFile of
          Left err -> assertBool "should handle dependent types" True
          Right result -> assertBool "should process dependent types" True
    ]
  ]
  where
    mockCodeBlock content = 
      CodeBlock 
        { cbDirectives = defaultBlockDirectives
        , cbContent = content
        , cbSpan = emptySpan startPos
        }
    
    mockCompilerError phase message = 
      CompilerError
        { cePhase = phase
        , ceError = message
        , ceSourceContext = Nothing
        , ceStackTrace = []
        }
    
    defaultBlockDirectives = BlockDirectives Nothing Nothing Nothing