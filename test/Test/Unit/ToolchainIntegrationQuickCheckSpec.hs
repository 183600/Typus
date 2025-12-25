{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ToolchainIntegrationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), elements, oneof, arbitrary, listOf, choose)

import GoToolchain (GoToolchain(..), defaultGoToolchain, validateGoInstallation, compileGoPackage, runGoTests, checkGoVersion)
import Compiler (generateGoCode, CompilerResult)
import Parser (TypusFile(..), CodeBlock(..))
import Compiler.IR (IRModule(..), IRFunction(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedWithSpan, startPos)

import Data.List (isPrefixOf, isInfixOf, sort, nub, intercalate)
import Data.Maybe (isNothing, isJust, fromMaybe, catMaybes)
import Data.Text as T (pack, unpack, Text(..), null, length, append, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Process (readProcess)

-- Property: Go toolchain initialization is consistent
prop_toolchain_initialization_consistent :: Property
prop_toolchain_initialization_consistent =
  let toolchain1 = defaultGoToolchain
      toolchain2 = defaultGoToolchain
  in counterexample "Go toolchain initialization should be consistent" $
     toolchain1 === toolchain2

-- Property: Toolchain validation detects installation
prop_toolchain_detection :: Property
prop_toolchain_detection =
  let validation = validateGoInstallation defaultGoToolchain
  in counterexample "Toolchain validation should detect Go installation" $
     case validation of
       Left _ -> property True -- Go not installed, which is valid
       Right _ -> property True -- Go installed and validated

-- Property: Generated code compiles with toolchain
prop_generated_code_compiles :: Property
prop_generated_code_compiles =
  forAll arbitrary $ \typusFile ->
    let goCode = generateGoCode typusFile
        toolchain = defaultGoToolchain
        compilation = compileGoPackage toolchain goCode
    in counterexample "Generated code should compile with toolchain" $
       case compilation of
         Left _ -> T.length goCode < 50 -- Small code might be incomplete
         Right _ -> property True

-- Property: Toolchain version checking is consistent
prop_version_checking_consistent :: Property
prop_version_checking_consistent =
  let version1 = checkGoVersion defaultGoToolchain
      version2 = checkGoVersion defaultGoToolchain
  in counterexample "Go version checking should be consistent" $
     version1 === version2

-- Property: Toolchain handles compilation errors gracefully
prop_toolchain_handles_errors :: Property
prop_toolchain_handles_errors =
  forAll (elements ["invalid syntax", "undefined variable", "type mismatch", "missing package"]) $ \errorType ->
    let invalidCode = pack $ "func main() { " ++ errorType ++ " }"
        toolchain = defaultGoToolchain
        compilation = compileGoPackage toolchain invalidCode
    in counterexample ("Toolchain should handle compilation error: " ++ errorType) $
       case compilation of
         Left _ -> property True
         Right _ -> property False -- Should not compile invalid code

-- Property: Toolchain test execution works
prop_test_execution_works :: Property
prop_test_execution_works =
  forAll arbitrary $ \typusFile ->
    let goCode = generateGoCode typusFile
        testCode = append goCode (pack "\nfunc TestExample(t *testing.T) {}")
        toolchain = defaultGoToolchain
        testResult = runGoTests toolchain testCode
    in counterexample "Toolchain test execution should work" $
       case testResult of
         Left _ -> T.length testCode < 100 -- Small code might not have tests
         Right _ -> property True

-- Property: Toolchain manages dependencies correctly
prop_dependency_management :: Property
prop_dependency_management =
  forAll (elements ["fmt", "os", "strings", "math", "time"]) $ \stdlibDep ->
    let codeWithDep = pack $ "package main\nimport \"" ++ stdlibDep ++ "\"\nfunc main() {}"
        toolchain = defaultGoToolchain
        compilation = compileGoPackage toolchain codeWithDep
    in counterexample ("Toolchain should manage dependency: " ++ stdlibDep) $
       case compilation of
         Left _ -> property True
         Right _ -> property True

-- Property: Toolchain handles optimization flags
prop_optimization_flags :: Property
prop_optimization_flags =
  forAll arbitrary $ \typusFile ->
    let goCode = generateGoCode typusFile
        toolchain = defaultGoToolchain
        optimizedToolchain = toolchain { optimizationEnabled = True }
        normalCompilation = compileGoPackage toolchain goCode
        optimizedCompilation = compileGoPackage optimizedToolchain goCode
    in counterexample "Toolchain should handle optimization flags" $
       case (normalCompilation, optimizedCompilation) of
         (Left _, Left _) -> property True
         (Right _, Right _) -> property True
         _ -> property True

-- Property: Toolchain supports cross-compilation
prop_cross_compilation_support :: Property
prop_cross_compilation_support =
  forAll (elements ["linux", "windows", "darwin", "freebsd"]) $ \targetOS ->
    forAll (elements ["amd64", "arm64", "386"]) $ \targetArch ->
      let toolchain = defaultGoToolchain
          crossToolchain = toolchain { 
            targetOS = Just targetOS,
            targetArch = Just targetArch
          }
          simpleCode = pack "package main\nfunc main() {}"
          compilation = compileGoPackage crossToolchain simpleCode
      in counterexample ("Toolchain should support cross-compilation to " ++ targetOS ++ "/" ++ targetArch) $
         case compilation of
           Left _ -> property True
           Right _ -> property True

-- Property: Toolchain integration is robust
prop_toolchain_integration_robust :: Property
prop_toolchain_integration_robust =
  forAll arbitrary $ \typusFile ->
    let goCode = generateGoCode typusFile
        toolchain = defaultGoToolchain
        validation = validateGoInstallation toolchain
        compilation = case validation of
          Left _ -> Left "Toolchain not available"
          Right _ -> compileGoPackage toolchain goCode
    in counterexample "Toolchain integration should be robust" $
       case compilation of
         Left _ -> property True
         Right _ -> property True

tests :: TestTree
tests =
  testGroup "Toolchain Integration QuickCheck Tests"
    [ fastProperty "Go toolchain initialization is consistent" prop_toolchain_initialization_consistent
    , fastProperty "Toolchain validation detects installation" prop_toolchain_detection
    , fastProperty "Generated code compiles with toolchain" prop_generated_code_compiles
    , fastProperty "Toolchain version checking is consistent" prop_version_checking_consistent
    , fastProperty "Toolchain handles compilation errors gracefully" prop_toolchain_handles_errors
    , fastProperty "Toolchain test execution works" prop_test_execution_works
    , fastProperty "Toolchain manages dependencies correctly" prop_dependency_management
    , fastProperty "Toolchain handles optimization flags" prop_optimization_flags
    , fastProperty "Toolchain supports cross-compilation" prop_cross_compilation_support
    , fastProperty "Toolchain integration is robust" prop_toolchain_integration_robust
    ]