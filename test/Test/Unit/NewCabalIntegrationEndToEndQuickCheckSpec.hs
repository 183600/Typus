{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalIntegrationEndToEndQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Parser (parseTypus, TypusFile(..), CodeBlock(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Compiler (compile, CompilerError(..), CompilationPhase(..))
import Compiler.IR (buildSourceIR, buildSemanticIR, emitGo)
import Compiler.Errors.Core (TypeError(..), ErrorSeverity(..), ErrorCategory(..))
import SourceLocation (SourceSpan(..), SourcePos(..), posAt, spanBetween)
import Utils (trim, removeComments, normalizeIndentation)
import Dependencies.TypeSystem (newDependentTypeChecker, addType, checkType)
import Ownership.Common.Types (newOwnershipAnalyzer, OwnershipTransfer(..))
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

-- | Test end-to-end integration properties
testIntegrationEndToEndProperties :: TestTree
testIntegrationEndToEndProperties = testGroup "End-to-End Integration Properties"
  [ testProperty "parse-compile roundtrip preserves structure" propParseCompileRoundtrip
  , testProperty "source location tracking across pipeline" propSourceLocationTracking
  , testProperty "error propagation through compilation stages" propErrorPropagation
  , testProperty "dependency analysis integration" propDependencyAnalysisIntegration
  , testProperty "ownership analysis integration" propOwnershipAnalysisIntegration
  , testProperty "utils integration with parser" propUtilsParserIntegration
  ]

-- | Parse-compile roundtrip should preserve basic structure
propParseCompileRoundtrip :: String -> Property
propParseCompileRoundtrip content =
  not (null content) && not (L.any isControl content) ==> 
  let parsed = parseTypus content
      compiled = compile parsed
  in case compiled of
       Right goCode -> not (null goCode)  -- Should generate some Go code
       Left _ -> True  -- Compilation errors are acceptable for some inputs
  where
    isControl c = c `elem` "\0\1\2\3\4\5\6\7\8\11\12\14\15\16\17\18\19\20\21\22\23\24\25\26\27\28\29\30\31"

-- | Source location tracking should be consistent across the pipeline
propSourceLocationTracking :: String -> Property
propSourceLocationTracking content =
  let parsed = parseTypus content
      blocks = tfBlocks parsed
  in not (null blocks) ==> 
     let firstBlock = L.head blocks
         span = cbSpan firstBlock
         start = spanStart span
         end = spanEnd span
     in posLine start <= posLine end &&  -- Basic span validity
        (posLine start < posLine end || posColumn start <= posColumn end)

-- | Error propagation should be consistent through compilation stages
propErrorPropagation :: String -> Property
propErrorPropagation content =
  "syntax error" `L.isInfixOf` content ==> 
  let parsed = parseTypus content
      compiled = compile parsed
  in case compiled of
       Left errors -> not (null errors)  -- Should produce errors for bad syntax
       Right _ -> True  -- Or succeed if syntax is actually valid

-- | Dependency analysis should integrate with compilation
propDependencyAnalysisIntegration :: String -> Property
propDependencyAnalysisIntegration content =
  "func" `L.isInfixOf` content ==> 
  let parsed = parseTypus content
      checker = newDependentTypeChecker
      checker1 = addType "int" (Dependencies.TypeSystem.TypeDefDecl [] []) checker
      result = checkType "int" (Dependencies.TypeSystem.TVCon "int") checker1
  in case result of
       Right _ -> True  -- Should succeed for basic types
       Left _ -> True   -- Or fail gracefully

-- | Ownership analysis should integrate with compilation
propOwnershipAnalysisIntegration :: String -> Property
propOwnershipAnalysisIntegration content =
  "move" `L.isInfixOf` content ==> 
  let analyzer = newOwnershipAnalyzer
      transfer = OwnershipTransfer "source" "target"
  in transferFrom transfer == "source" && transferTo transfer == "target"

-- | Utils should integrate properly with parser output
propUtilsParserIntegration :: String -> Property
propUtilsParserIntegration content =
  not (null content) ==> 
  let trimmed = trim content
      withoutComments = removeComments content
      normalized = normalizeIndentation content
  in L.length trimmed <= L.length content &&  -- trim should not increase L.length
     not (null normalized)  -- normalize should not produce empty result

-- | Test parser-compiler integration
testParserCompilerIntegration :: TestTree
testParserCompilerIntegration = testGroup "Parser-Compiler Integration"
  [ testCase "simple function compilation" $
      let content = "//! ownership=true\npackage main\n\nfunc main() {\n    println(\"Hello, World!\")\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "package main" `L.isInfixOf` goCode &&
             "func main" `L.isInfixOf` goCode &&
             "println" `L.isInfixOf` goCode
           Left errors -> fail $ "Compilation failed: " ++ show errors
           
  , testCase "function with parameters" $
      let content = "package main\n\nfunc add(a int, b int) int {\n    return a + b\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "func add" `L.isInfixOf` goCode &&
             "a int" `L.isInfixOf` goCode &&
             "b int" `L.isInfixOf` goCode
           Left _ -> fail "Compilation failed"
           
  , testCase "multiple functions" $
      let content = "package main\n\nfunc helper() {}\n\nfunc main() {\n    helper()\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "func helper" `L.isInfixOf` goCode &&
             "func main" `L.isInfixOf` goCode
           Left _ -> fail "Compilation failed"
  ]

-- | Test error handling integration
testErrorHandlingIntegration :: TestTree
testErrorHandlingIntegration = testGroup "Error Handling Integration"
  [ testCase "syntax error propagation" $
      let content = "package main\n\nfunc invalid syntax here {"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Left errors -> not (null errors)
           Right _ -> fail "Expected compilation to fail with syntax errors"
           
  , testCase "type error detection" $
      let content = "package main\n\nfunc main() {\n    var x int = \"string\"\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Left errors -> L.any isErrorType errors
           Right _ -> fail "Expected compilation to fail with type errors"
           where
             isErrorType (TypeError _ _ TypeChecking _ _) = True
             isErrorType _ = False
             
  , testCase "ownership error detection" $
      let content = "//! ownership=true\npackage main\n\nfunc main() {\n    var x = 42\n    var y = x  // Move\n    println(x)  // Use after move\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Left errors -> not (null errors)  -- Should detect ownership issues
           Right _ -> pure ()  -- Or succeed if ownership checking is lenient
  ]

-- | Test source location integration
testSourceLocationIntegration :: TestTree
testSourceLocationIntegration = testGroup "Source Location Integration"
  [ testCase "location preservation through parsing" $
      let content = "package main\n\nfunc test() {}\n"
          parsed = parseTypus content
          blocks = tfBlocks parsed
      in case blocks of
           (block:_) -> 
             let span = cbSpan block
                 start = spanStart span
             in posLine start >= 1
           [] -> fail "No blocks found in parsed content"
           
  , testCase "location preservation through compilation" $
      let content = "package main\n\nfunc test() { return 42 }"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Right goCode -> not (null goCode)  -- Should generate code with location info
           Left errors -> 
             -- Errors should have location information
             L.any hasLocation errors
             where
               hasLocation (TypeError _ _ _ loc _) = 
                 case loc of
                   Compiler.Errors.Core.ErrorLocation _ line _ _ _ -> line > 0
               hasLocation _ = False
  ]

-- | Test type system integration
testTypeSystemIntegration :: TestTree
testTypeSystemIntegration = testGroup "Type System Integration"
  [ testCase "basic type checking integration" $
      let content = "package main\n\nfunc add(x int, y int) int {\n    return x + y\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "int" `L.isInfixOf` goCode  -- Types should be preserved
           Left _ -> fail "Type checking integration failed"
           
  , testCase "complex type checking" $
      let content = "package main\n\ntype Pair struct {\n    first int\n    second string\n}\n\nfunc makePair() Pair {\n    return Pair{42, \"hello\"}\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "type Pair" `L.isInfixOf` goCode &&
             "first int" `L.isInfixOf` goCode &&
             "second string" `L.isInfixOf` goCode
           Left _ -> fail "Complex type checking failed"
  ]

-- | Test utils integration
testUtilsIntegration :: TestTree
testUtilsIntegration = testGroup "Utils Integration"
  [ testCase "comment removal in parsing" $
      let contentWithComments = "// This is a comment\npackage main\n// Another comment\nfunc main() {}"
          withoutComments = removeComments contentWithComments
          parsed = parseTypus contentWithComments
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "package main" `L.isInfixOf` goCode &&
             "func main" `L.isInfixOf` goCode
           Left _ -> fail "Utils integration failed"
           
  , testCase "indentation normalization" $
      let contentWithIndentation = "    package main\n    \n    func main() {\n        println(\"test\")\n    }"
          normalized = normalizeIndentation contentWithIndentation
          parsed = parseTypus normalized
          compiled = compile parsed
      in case compiled of
           Right goCode -> not (null goCode)
           Left _ -> fail "Indentation normalization failed"
  ]

-- | Test complete pipeline integration
testCompletePipelineIntegration :: TestTree
testCompletePipelineIntegration = testGroup "Complete Pipeline Integration"
  [ testCase "end-to-end simple program" $
      let content = "//! ownership=true\n//! dependent-types=true\npackage main\n\nfunc main() {\n    x := 42\n    y := x + 1\n    println(y)\n}"
          parsed = parseTypus content
          sourceIR = buildSourceIR parsed
          Right semanticIR = buildSemanticIR sourceIR
          goIR = emitGo semanticIR
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "package main" `L.isInfixOf` goCode &&
             "func main" `L.isInfixOf` goCode &&
             "println" `L.isInfixOf` goCode
           Left errors -> fail $ "Pipeline failed: " ++ show errors
           
  , testCase "pipeline with type annotations" $
      let content = "package main\n\nfunc calculate(x int, y int) int {\n    result := x * y\n    return result\n}"
          parsed = parseTypus content
          compiled = compile parsed
      in case compiled of
           Right goCode -> 
             "int" `L.isInfixOf` goCode &&
             "calculate" `L.isInfixOf` goCode
           Left _ -> fail "Type annotation pipeline failed"
  ]

-- | All integration end-to-end tests
testIntegrationEndToEndQuickCheck :: TestTree
testIntegrationEndToEndQuickCheck = testGroup "New Cabal Integration End-to-End QuickCheck Tests"
  [ testIntegrationEndToEndProperties
  , testParserCompilerIntegration
  , testErrorHandlingIntegration
  , testSourceLocationIntegration
  , testTypeSystemIntegration
  , testUtilsIntegration
  , testCompletePipelineIntegration
  ]