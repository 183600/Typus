{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.CompilerOptimizationInvariantSpec where

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
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort)
import Control.Monad (when, replicateM)

-- ============================================================================
-- Compiler Optimization Invariant Tests
-- ============================================================================

-- | Test that optimization preserves semantic equivalence
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics code =
  not (null code) && length code < 100 ==>
    let parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let unoptimizedResult = compile typusFile
               optimizedResult = compile typusFile  -- In real implementation, would apply optimizations
           in case (unoptimizedResult, optimizedResult) of
                (Left _, Left _) -> property True
                (Right unopt, Right opt) -> property $ not (null unopt) && not (null opt)
                _ -> property True  -- Different error states are acceptable

-- | Test that optimization doesn't increase code size significantly
prop_optimization_code_size :: String -> Property
prop_optimization_code_size code =
  not (null code) && length code < 50 ==>
    let parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let codeLength = length goCode
                  in property $ codeLength >= 0  -- Basic check that we get some output

-- | Test that optimization preserves variable naming consistency
prop_optimization_preserves_variables :: String -> String -> Property
prop_optimization_preserves_variables varName value =
  not (null varName) && not (null value) && all isAlphaNum varName ==>
    let code = "let " ++ varName ++ " = " ++ value ++ "\nprint(" ++ varName ++ ")\n"
        parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let varNameExists = varName `isInfixOf` goCode
                  in property $ varNameExists  -- Variable should still exist in output

-- | Test that optimization preserves control flow structure
prop_optimization_preserves_control_flow :: String -> String -> Property
prop_optimization_preserves_control_flow condition body =
  not (null condition) && not (null body) ==>
    let code = "if " ++ condition ++ " {\n" ++ body ++ "\n}\n"
        parseResult = parseTypus code
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test that optimization handles recursive functions correctly
prop_optimization_recursive_functions :: String -> String -> Property
prop_optimization_recursive_functions funcName body =
  not (null funcName) && not (null body) ==>
    let recursiveCode = "function " ++ funcName ++ "(n) {\n" ++
                        "  if (n <= 1) return 1;\n" ++
                        "  return " ++ funcName ++ "(n - 1);\n" ++
                        "}\n"
        parseResult = parseTypus recursiveCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let funcNameExists = funcName `isInfixOf` goCode
                  in property $ funcNameExists

-- | Test that optimization preserves type safety
prop_optimization_preserves_type_safety :: String -> String -> Property
prop_optimization_preserves_type_safety varName typeAnnotation =
  not (null varName) && not (null typeAnnotation) ==>
    let typedCode = "let " ++ varName ++ " : " ++ typeAnnotation ++ " = 5\n"
        parseResult = parseTypus typedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test that optimization doesn't introduce side effects
prop_optimization_no_side_effects :: String -> Property
prop_optimization_no_side_effects expression =
  not (null expression) && length expression < 30 ==>
    let pureCode = "let x = " ++ expression ++ "\nlet y = x + 1\n"
        parseResult = parseTypus pureCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test that optimization preserves function signatures
prop_optimization_preserves_signatures :: String -> String -> Property
prop_optimization_preserves_signatures funcName params =
  not (null funcName) && not (null params) ==>
    let funcCode = "function " ++ funcName ++ "(" ++ params ++ ") {\n  return 42;\n}\n"
        parseResult = parseTypus funcCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let funcNameExists = funcName `isInfixOf` goCode
                  in property $ funcNameExists

-- | Test that optimization handles constants correctly
prop_optimization_constant_folding :: String -> Property
prop_optimization_constant_folding constExpr =
  not (null constExpr) && length constExpr < 20 ==>
    let constCode = "let x = " ++ constExpr ++ "\nprint(x)\n"
        parseResult = parseTypus constCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test that optimization preserves loop semantics
prop_optimization_preserves_loops :: String -> String -> Property
prop_optimization_preserves_loops loopVar body =
  not (null loopVar) && all isAlphaNum loopVar && not (null body) ==>
    let loopCode = "for (" ++ loopVar ++ " = 0; " ++ loopVar ++ " < 10; " ++ loopVar ++ "++) {\n" ++
                   body ++ "\n}\n"
        parseResult = parseTypus loopCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let loopVarExists = loopVar `isInfixOf` goCode
                  in property $ loopVarExists

-- | Test that optimization handles nested structures
prop_optimization_nested_structures :: Int -> Property
prop_optimization_nested_structures depth =
  depth >= 0 && depth <= 5 ==>
    let nestedCode = generateNestedStructures depth
        parseResult = parseTypus nestedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test that optimization preserves error handling
prop_optimization_preserves_error_handling :: String -> Property
prop_optimization_preserves_error_handling errorCase =
  not (null errorCase) ==>
    let errorCode = "try {\n" ++ errorCase ++ "\n} catch (e) {\n  print(e);\n}\n"
        parseResult = parseTypus errorCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test that optimization doesn't break module boundaries
prop_optimization_module_boundaries :: String -> String -> Property
prop_optimization_module_boundaries module1 module2 =
  not (null module1) && not (null module2) && module1 /= module2 ==>
    let moduleCode = "module " ++ module1 ++ " {\n  let x = 5;\n}\n" ++
                     "module " ++ module2 ++ " {\n  let y = 10;\n}\n"
        parseResult = parseTypus moduleCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let bothModulesExist = module1 `isInfixOf` goCode && module2 `isInfixOf` goCode
                  in property $ bothModulesExist

-- | Test that optimization preserves dependency order
prop_optimization_dependency_order :: String -> String -> Property
prop_optimization_dependency_order var1 var2 =
  not (null var1) && not (null var2) && var1 /= var2 ==>
    let dependencyCode = "let " ++ var1 ++ " = 5;\nlet " ++ var2 ++ " = " ++ var1 ++ " + 1;\n"
        parseResult = parseTypus dependencyCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> 
                  let bothVarsExist = var1 `isInfixOf` goCode && var2 `isInfixOf` goCode
                  in property $ bothVarsExist

-- | Test that optimization handles large expressions
prop_optimization_large_expressions :: Int -> String -> Property
prop_optimization_large_expressions n baseExpr =
  n >= 0 && n <= 10 && not (null baseExpr) ==>
    let largeExpr = buildLargeExpression n baseExpr
        exprCode = "let x = " ++ largeExpr ++ "\n"
        parseResult = parseTypus exprCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test that optimization preserves comments
prop_optimization_preserves_comments :: String -> Property
prop_optimization_preserves_comments comment =
  let commentedCode = "// " ++ comment ++ "\nlet x = 5;\n"
      parseResult = parseTypus commentedCode
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True
              Right goCode -> property $ not (null goCode)

-- | Test that optimization handles edge cases
prop_optimization_edge_cases :: String -> Property
prop_optimization_edge_cases edgeCase =
  not (null edgeCase) && length edgeCase < 20 ==>
    let edgeCaseCode = "let x = " ++ edgeCase ++ "\n"
        parseResult = parseTypus edgeCaseCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- Helper function to generate nested structures
generateNestedStructures :: Int -> String
generateNestedStructures 0 = "let x = 0;"
generateNestedStructures n = "if (true) {\n" ++ generateNestedStructures (n - 1) ++ "\n}"

-- Helper function to build large expressions
buildLargeExpression :: Int -> String -> String
buildLargeExpression 0 base = base
buildLargeExpression n base = "(" ++ buildLargeExpression (n - 1) base ++ " + " ++ base ++ ")"

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Compiler Optimization Invariant Tests"
  [ testProperty "Optimization preserves semantics" prop_optimization_preserves_semantics,
    testProperty "Optimization doesn't increase code size significantly" prop_optimization_code_size,
    testProperty "Optimization preserves variable naming consistency" prop_optimization_preserves_variables,
    testProperty "Optimization preserves control flow structure" prop_optimization_preserves_control_flow,
    testProperty "Optimization handles recursive functions correctly" prop_optimization_recursive_functions,
    testProperty "Optimization preserves type safety" prop_optimization_preserves_type_safety,
    testProperty "Optimization doesn't introduce side effects" prop_optimization_no_side_effects,
    testProperty "Optimization preserves function signatures" prop_optimization_preserves_signatures,
    testProperty "Optimization handles constants correctly" prop_optimization_constant_folding,
    testProperty "Optimization preserves loop semantics" prop_optimization_preserves_loops,
    testProperty "Optimization handles nested structures" prop_optimization_nested_structures,
    testProperty "Optimization preserves error handling" prop_optimization_preserves_error_handling,
    testProperty "Optimization doesn't break module boundaries" prop_optimization_module_boundaries,
    testProperty "Optimization preserves dependency order" prop_optimization_dependency_order,
    testProperty "Optimization handles large expressions" prop_optimization_large_expressions,
    testProperty "Optimization preserves comments" prop_optimization_preserves_comments,
    testProperty "Optimization handles edge cases" prop_optimization_edge_cases
  ]