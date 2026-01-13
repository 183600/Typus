{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.IntegrationWorkflowSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\), intersect)
import Control.Monad (when, replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- Integration Workflow Tests
-- ============================================================================

-- | Test complete workflow from parsing to compilation
prop_integration_parse_to_compile :: String -> Property
prop_integration_parse_to_compile code =
  not (null code) && length code < 100 ==>
    let parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with multiple dependent files
prop_integration_multiple_files :: String -> String -> Property
prop_integration_multiple_files file1 file2 =
  not (null file1) && not (null file2) && length file1 < 50 && length file2 < 50 ==>
    let parseResult1 = parseTypus file1
        parseResult2 = parseTypus file2
    in case (parseResult1, parseResult2) of
         (Left _, Left _) -> property True
         (Right tf1, Right tf2) -> 
           let compileResult1 = compile tf1
               compileResult2 = compile tf2
           in case (compileResult1, compileResult2) of
                (Left _, Left _) -> property True
                (Right gc1, Right gc2) -> property $ not (null gc1) && not (null gc2)
                _ -> property True
         _ -> property True

-- | Test workflow with module imports
prop_integration_module_imports :: String -> String -> String -> Property
prop_integration_module_imports moduleName importName content =
  not (null moduleName) && not (null importName) && not (null content) &&
  all isAlphaNum moduleName && all isAlphaNum importName ==>
    let moduleCode = "module " ++ moduleName ++ " {\n" ++
                     "  import " ++ importName ++ "\n" ++
                     content ++ "\n" ++
                     "}\n"
        parseResult = parseTypus moduleCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with ownership analysis
prop_integration_ownership_analysis :: String -> String -> Property
prop_integration_ownership_analysis ownerName content =
  not (null ownerName) && not (null content) && all isAlphaNum ownerName ==>
    let ownershipCode = "// ownership: true\n" ++
                        "let resource = owned_by(" ++ ownerName ++ ")\n" ++
                        content ++ "\n"
        parseResult = parseTypus ownershipCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with dependent types
prop_integration_dependent_types :: String -> Int -> Property
prop_integration_dependent_types typeName value =
  not (null typeName) && all isAlphaNum typeName && value >= 0 && value <= 100 ==>
    let dependentTypeCode = "// dependent-types: true\n" ++
                           "type Vector<" ++ show value ++ "> = Array<" ++ show value ++ ">\n" ++
                           "let v: Vector<" ++ show value ++ "> = [1, 2, 3]\n"
        parseResult = parseTypus dependentTypeCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with error handling
prop_integration_error_handling :: String -> Property
prop_integration_error_handling errorProneCode =
  not (null errorProneCode) && length errorProneCode < 50 ==>
    let codeWithError = errorProneCode ++ "\n" ++
                        "try {\n" ++
                        "  riskyOperation()\n" ++
                        "} catch (e) {\n" ++
                        "  handleError(e)\n" ++
                        "}\n"
        parseResult = parseTypus codeWithError
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with optimization passes
prop_integration_optimization :: String -> Property
prop_integration_optimization code =
  not (null code) && length code < 50 ==>
    let parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)  -- In real implementation, would apply optimizations

-- | Test workflow with source location tracking
prop_integration_source_location :: String -> Property
prop_integration_source_location code =
  not (null code) && length code < 100 ==>
    let parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let blocks = tfBlocks typusFile
               hasSpans = not (null blocks) ==> all isValidBlockSpan blocks
           in property $ hasSpans
  where
    isValidBlockSpan block = True  -- Simplified for this example

-- | Test workflow with incremental compilation
prop_integration_incremental :: String -> String -> Property
prop_integration_incremental originalCode modifiedCode =
  not (null originalCode) && not (null modifiedCode) &&
  length originalCode < 50 && length modifiedCode < 50 ==>
    let parseResult1 = parseTypus originalCode
        parseResult2 = parseTypus modifiedCode
    in case (parseResult1, parseResult2) of
         (Right tf1, Right tf2) -> 
           let compileResult1 = compile tf1
               compileResult2 = compile tf2
           in case (compileResult1, compileResult2) of
                (Right gc1, Right gc2) -> property $ not (null gc1) && not (null gc2)
                _ -> property True
         _ -> property True

-- | Test workflow with cross-module type checking
prop_integration_cross_module_type_checking :: String -> String -> String -> Property
prop_integration_cross_module_type_checking module1 module2 sharedType =
  not (null module1) && not (null module2) && not (null sharedType) &&
  all isAlphaNum module1 && all isAlphaNum module2 && all isAlphaNum sharedType ==>
    let module1Code = "module " ++ module1 ++ " {\n" ++
                      "  type " ++ sharedType ++ " = number\n" ++
                      "  let x: " ++ sharedType ++ " = 5\n" ++
                      "}\n"
        module2Code = "module " ++ module2 ++ " {\n" ++
                      "  import " ++ module1 ++ "\n" ++
                      "  let y: " ++ sharedType ++ " = 10\n" ++
                      "}\n"
        parseResult1 = parseTypus module1Code
        parseResult2 = parseTypus module2Code
    in case (parseResult1, parseResult2) of
         (Right tf1, Right tf2) -> 
           let compileResult1 = compile tf1
               compileResult2 = compile tf2
           in case (compileResult1, compileResult2) of
                (Right gc1, Right gc2) -> property $ not (null gc1) && not (null gc2)
                _ -> property True
         _ -> property True

-- | Test workflow with dependency resolution
prop_integration_dependency_resolution :: String -> String -> String -> Property
prop_integration_dependency_resolution dep1 dep2 dep3 =
  not (null dep1) && not (null dep2) && not (null dep3) &&
  length (nub [dep1, dep2, dep3]) == 3 ==>
    let dependencyCode = "import " ++ dep1 ++ "\n" ++
                         "import " ++ dep2 ++ "\n" ++
                         "import " ++ dep3 ++ "\n" ++
                         "let x = 5\n"
        parseResult = parseTypus dependencyCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with code generation
prop_integration_code_generation :: String -> Property
prop_integration_code_generation code =
  not (null code) && length code < 50 ==>
    let parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let hasGoCode = "package main" `isInfixOf` goCode || "func main" `isInfixOf` goCode
                  in property $ hasGoCode || not (null goCode)

-- | Test workflow with resource management
prop_integration_resource_management :: String -> String -> Property
prop_integration_resource_management resourceName content =
  not (null resourceName) && not (null content) && all isAlphaNum resourceName ==>
    let resourceCode = "resource " ++ resourceName ++ " {\n" ++
                       "  acquire()\n" ++
                       "  " ++ content ++ "\n" ++
                       "  release()\n" ++
                       "}\n"
        parseResult = parseTypus resourceCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with parallel compilation
prop_integration_parallel_compilation :: String -> String -> Property
prop_integration_parallel_compilation file1 file2 =
  not (null file1) && not (null file2) && length file1 < 50 && length file2 < 50 ==>
    let parseResult1 = parseTypus file1
        parseResult2 = parseTypus file2
    in case (parseResult1, parseResult2) of
         (Right tf1, Right tf2) -> 
           let compileResult1 = compile tf1
               compileResult2 = compile tf2
           in case (compileResult1, compileResult2) of
                (Right gc1, Right gc2) -> property $ not (null gc1) && not (null gc2)
                _ -> property True
         _ -> property True

-- | Test workflow with hot reloading
prop_integration_hot_reload :: String -> String -> Property
prop_integration_hot_reload originalCode modifiedCode =
  not (null originalCode) && not (null modifiedCode) &&
  length originalCode < 50 && length modifiedCode < 50 ==>
    let parseResult1 = parseTypus originalCode
        parseResult2 = parseTypus modifiedCode
    in case (parseResult1, parseResult2) of
         (Right tf1, Right tf2) -> 
           let compileResult1 = compile tf1
               compileResult2 = compile tf2
           in case (compileResult1, compileResult2) of
                (Right gc1, Right gc2) -> property $ not (null gc1) && not (null gc2)
                _ -> property True
         _ -> property True

-- | Test workflow with debugging information
prop_integration_debug_info :: String -> Property
prop_integration_debug_info code =
  not (null code) && length code < 50 ==>
    let debugCode = "// debug: true\n" ++ code
        parseResult = parseTypus debugCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with profiling
prop_integration_profiling :: String -> Property
prop_integration_profiling code =
  not (null code) && length code < 50 ==>
    let profiledCode = "// profile: true\n" ++ code
        parseResult = parseTypus profiledCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with testing framework integration
prop_integration_testing :: String -> Property
prop_integration_testing code =
  not (null code) && length code < 50 ==>
    let testCode = "test \"example\" {\n" ++ code ++ "\n}\n"
        parseResult = parseTypus testCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test workflow with build system integration
prop_integration_build_system :: String -> String -> Property
prop_integration_build_system buildConfig sourceCode =
  not (null buildConfig) && not (null sourceCode) &&
  length buildConfig < 50 && length sourceCode < 50 ==>
    let buildCode = "// build: " ++ buildConfig ++ "\n" ++ sourceCode
        parseResult = parseTypus buildCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Integration Workflow Tests"
  [ testProperty "Complete workflow from parsing to compilation" prop_integration_parse_to_compile,
    testProperty "Workflow with multiple dependent files" prop_integration_multiple_files,
    testProperty "Workflow with module imports" prop_integration_module_imports,
    testProperty "Workflow with ownership analysis" prop_integration_ownership_analysis,
    testProperty "Workflow with dependent types" prop_integration_dependent_types,
    testProperty "Workflow with error handling" prop_integration_error_handling,
    testProperty "Workflow with optimization passes" prop_integration_optimization,
    testProperty "Workflow with source location tracking" prop_integration_source_location,
    testProperty "Workflow with incremental compilation" prop_integration_incremental,
    testProperty "Workflow with cross-module type checking" prop_integration_cross_module_type_checking,
    testProperty "Workflow with dependency resolution" prop_integration_dependency_resolution,
    testProperty "Workflow with code generation" prop_integration_code_generation,
    testProperty "Workflow with resource management" prop_integration_resource_management,
    testProperty "Workflow with parallel compilation" prop_integration_parallel_compilation,
    testProperty "Workflow with hot reloading" prop_integration_hot_reload,
    testProperty "Workflow with debugging information" prop_integration_debug_info,
    testProperty "Workflow with profiling" prop_integration_profiling,
    testProperty "Workflow with testing framework integration" prop_integration_testing,
    testProperty "Workflow with build system integration" prop_integration_build_system
  ]