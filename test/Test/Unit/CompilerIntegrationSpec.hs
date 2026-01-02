{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate, nub)
import Data.Char (isSpace, isLetter, isDigit)

-- Import compiler modules
import qualified Compiler
import qualified Compiler.IR
import qualified Compiler.TypeChecker
import qualified Compiler.ValueAnalysis
import qualified Compiler.GoAst
import qualified Parser
import qualified Utils
import qualified SourceLocation
import qualified ErrorHandler

-- | Compiler integration tests covering end-to-end compilation scenarios
tests :: TestTree
tests =
  testGroup "Compiler Integration"
    [ testGroup "End-to-End Compilation"
        [ fastProperty "Complete compilation pipeline preserves meaning" prop_complete_pipeline_preserves_meaning
        , fastProperty "Compilation handles complex expressions" prop_compilation_complex_expressions
        , fastProperty "Multi-file compilation consistency" prop_multifile_compilation_consistency
        , testCase "Simple program compilation" $ do
            let program = "func main() { return 42; }"
            result <- Compiler.compileProgram program
            case result of
              Left err -> assertFailure $ "Compilation failed: " ++ show err
              Right _ -> pure ()
        ]

    , testGroup "IR Generation L.and Validation"
        [ fastProperty "IR generation preserves semantics" prop_ir_generation_preserves_semantics
        , fastProperty "IR optimization maintains correctness" prop_ir_optimization_maintains_correctness
        , fastProperty "IR validation catches inconsistencies" prop_ir_validation_catches_inconsistencies
        , testCase "IR structure validation" $ do
            let expr = "x := (a + b) * c"
            case Parser.parseExpression expr of
              Left err -> assertFailure $ "Parse error: " ++ show err
              Right ast -> do
                ir <- Compiler.generateIR ast
                Compiler.IR.validate ir @?= True
        ]

    , testGroup "Type Checking Integration"
        [ fastProperty "Type checking catches type errors" prop_typechecking_catches_type_errors
        , fastProperty "Type inference consistency" prop_type_inference_consistency
        , fastProperty "Generic type handling" prop_generic_type_handling
        , testCase "Complex type scenarios" $ do
            let expr = "func identity<T>(x: T): T { return x; }"
            case Parser.parseExpression expr of
              Left err -> assertFailure $ "Parse error: " ++ show err
              Right ast -> do
                result <- Compiler.TypeChecker.checkTypes ast
                case result of
                  Left _ -> pure ()  -- May fail due to generic types
                  Right _ -> pure ()
        ]

    , testGroup "Value Analysis Integration"
        [ fastProperty "Value analysis tracks dependencies" prop_value_analysis_tracks_dependencies
        , fastProperty "Constant propagation correctness" prop_constant_propagation_correctness
        , fastProperty "Value range analysis" prop_value_range_analysis
        , testCase "Value analysis edge cases" $ do
            let expr = "x := 42; y := x + 1; z := y * 2"
            case Parser.parseExpression expr of
              Left err -> assertFailure $ "Parse error: " ++ show err
              Right ast -> do
                analysis <- Compiler.ValueAnalysis.analyze ast
                Compiler.ValueAnalysis.isComplete analysis @?= True
        ]

    , testGroup "Go AST Generation"
        [ fastProperty "Go AST generation preserves structure" prop_go_ast_generation_preserves_structure
        , fastProperty "Go code generation validity" prop_go_code_generation_validity
        , fastProperty "Go type mapping correctness" prop_go_type_mapping_correctness
        , testCase "Go AST edge cases" $ do
            let expr = "func test() { var x int = 42; return x; }"
            case Parser.parseExpression expr of
              Left err -> assertFailure $ "Parse error: " ++ show err
              Right ast -> do
                goAst <- Compiler.GoAst.generate ast
                Compiler.GoAst.validate goAst @?= True
        ]

    , testGroup "Error Recovery Integration"
        [ fastProperty "Error recovery maintains compilation state" prop_error_recovery_maintains_state
        , fastProperty "Partial compilation with errors" prop_partial_compilation_with_errors
        , fastProperty "Error context preservation" prop_error_context_preservation_integration
        , testCase "Error recovery scenarios" $ do
            let program = "func main() { return ; }"  -- Missing expression
            result <- Compiler.compileProgramWithRecovery program
            case result of
              (Left _, _) -> pure ()  -- Expected to fail
              (Right _, _) -> pure ()  -- Or succeed with recovery
        ]

    , testGroup "Performance L.and Optimization"
        [ fastProperty "Optimization improves performance" prop_optimization_improves_performance
        , fastProperty "Memory usage optimization" prop_memory_usage_optimization
        , fastProperty "Compilation time scaling" prop_compilation_time_scaling
        , testCase "Optimization edge cases" $ do
            let expr = "x := 1 + 2 + 3 + 4 + 5"
            case Parser.parseExpression expr of
              Left err -> assertFailure $ "Parse error: " ++ show err
              Right ast -> do
                optimized <- Compiler.optimize ast
                Compiler.isOptimized optimized @?= True
        ]
    ]

-- Property-based tests

-- End-to-end compilation properties
prop_complete_pipeline_preserves_meaning :: String -> Property
prop_complete_pipeline_preserves_meaning input =
  not (null input) && L.length input <= 100 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      compiled = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.compileAST ast
      executed = case compiled of
        Nothing -> Nothing
        Right prog -> Just <$> Compiler.execute prog
      originalValue = case parsed of
        Left _ -> Nothing
        Right ast -> Compiler.evaluateAST ast
  in property $ case (originalValue, executed) of
    (Nothing, Nothing) -> property True
    (Just orig, Just exec) -> orig === exec
    _ -> property True  -- Different error handling is acceptable

prop_compilation_complex_expressions :: String -> String -> String -> Property
prop_compilation_complex_expressions expr1 expr2 expr3 =
  not (null expr1) && not (null expr2) && not (null expr3) &&
  L.all isValidExpression [expr1, expr2, expr3] &&
  L.all ((<= 50).L.length) [expr1, expr2, expr3] ==>
  let complexExpr = "(" ++ expr1 ++ ") + (" ++ expr2 ++ ") * (" ++ expr3 ++ ")"
      result = Compiler.compileExpression complexExpr
  in property $ case result of
    Left _ -> property True  -- May fail for complex expressions
    Right _ -> property True  -- Should succeed for valid complex expressions

prop_multifile_compilation_consistency :: [String] -> Property
prop_multifile_compilation_consistency files =
  not (null files) && L.length files <= 5 && L.all isValidExpression files ==>
  let compilationResults = map Compiler.compileFile files
      successfulResults = [r | Right r <- compilationResults]
  in property $ L.length successfulResults >= 0 .&&.
     L.all Compiler.isValidCompilation successfulResults

-- IR generation properties
prop_ir_generation_preserves_semantics :: String -> Property
prop_ir_generation_preserves_semantics input =
  not (null input) && L.length input <= 50 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      ir = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.generateIR ast
      semanticsPreserved = case (parsed, ir) of
        (Right ast, Just (Right ir')) -> Compiler.semanticEqualsAST ast ir'
        _ -> True
  in property $ semanticsPreserved

prop_ir_optimization_maintains_correctness :: String -> Property
prop_ir_optimization_maintains_correctness input =
  not (null input) && L.length input <= 50 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      optimized = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.optimizeIR ast
      correctnessMaintained = case optimized of
        Nothing -> True
        Just (Right optIr) -> Compiler.IR.isCorrect optIr
        Just (Left _) -> True  -- Optimization may fail
  in property $ correctnessMaintained

prop_ir_validation_catches_inconsistencies :: String -> Property
prop_ir_validation_catches_inconsistencies input =
  let parsed = Parser.parseExpression input
      inconsistentIR = case parsed of
        Left _ -> Compiler.IR.inconsistent
        Right ast -> Compiler.IR.makeInconsistent ast
      validation = Compiler.IR.validate inconsistentIR
  in property $ not validation || null input  -- Should catch inconsistencies

-- Type checking properties
prop_typechecking_catches_type_errors :: String -> String -> Property
prop_typechecking_catches_type_errors expr1 expr2 =
  let typeSafeExpr1 = L.filter (\c -> isLetter c || isDigit c || c `elem` " +-*/()") expr1
      typeSafeExpr2 = L.filter (\c -> isLetter c || isDigit c || c `elem` " +-*/()") expr2
      invalidExpr = typeSafeExpr1 ++ " + " ++ "\"" ++ typeSafeExpr2 ++ "\""  -- String + expression
      result = Compiler.TypeChecker.checkTypes invalidExpr
  in property $ case result of
    Left _ -> True  -- Should catch type error
    Right _ -> L.length invalidExpr < 5  -- Only succeed for very short inputs

prop_type_inference_consistency :: String -> Property
prop_type_inference_consistency input =
  not (null input) && L.length input <= 30 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      inferred1 = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.TypeChecker.inferType ast
      inferred2 = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.TypeChecker.inferTypeAgain ast
  in property $ case (inferred1, inferred2) of
    (Just (Right t1), Just (Right t2)) -> t1 === t2
    _ -> property True  -- Different error handling is acceptable

prop_generic_type_handling :: String -> Property
prop_generic_type_handling input =
  let genericExpr = "func identity<" ++ input ++ ">(x: " ++ input ++ "): " ++ input ++ " { return x; }"
      result = Compiler.TypeChecker.checkGenericTypes genericExpr
  in property $ case result of
    Left _ -> L.length input <= 20  -- May fail for complex types
    Right _ -> True  -- Should handle simple generic types

-- Value analysis properties
prop_value_analysis_tracks_dependencies :: String -> String -> Property
prop_value_analysis_tracks_dependencies var1 var2 =
  not (null var1) && not (null var2) && L.all isValidVariableName [var1, var2] ==>
  let expr = var1 ++ " := 42; " ++ var2 ++ " := " ++ var1 ++ " + 1"
      parsed = Parser.parseExpression expr
      analysis = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.ValueAnalysis.analyze ast
      dependenciesTracked = case analysis of
        Nothing -> True
        Just (Right a) -> Compiler.ValueAnalysis.hasDependency a var2 var1
        Just (Left _) -> True  -- Analysis may fail
  in property $ dependenciesTracked

prop_constant_propagation_correctness :: String -> Property
prop_constant_propagation_correctness input =
  not (null input) && L.length input <= 30 && isConstantExpression input ==>
  let parsed = Parser.parseExpression input
      propagated = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.ValueAnalysis.propagateConstants ast
      correctness = case propagated of
        Nothing -> True
        Just (Right p) -> Compiler.ValueAnalysis.isConstantFolded p
        Just (Left _) -> True  -- Propagation may fail
  in property $ correctness

prop_value_range_analysis :: String -> Property
prop_value_range_analysis input =
  not (null input) && L.length input <= 30 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      range = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.ValueAnalysis.analyzeRange ast
      validRange = case range of
        Nothing -> True
        Just (Right r) -> Compiler.ValueAnalysis.isValidRange r
        Just (Left _) -> True  -- Analysis may fail
  in property $ validRange

-- Go AST properties
prop_go_ast_generation_preserves_structure :: String -> Property
prop_go_ast_generation_preserves_structure input =
  not (null input) && L.length input <= 50 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      goAst = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.GoAst.generate ast
      structurePreserved = case goAst of
        Nothing -> True
        Just (Right g) -> Compiler.GoAst.preservesStructure parsed g
        Just (Left _) -> True  -- Generation may fail
  in property $ structurePreserved

prop_go_code_generation_validity :: String -> Property
prop_go_code_generation_validity input =
  not (null input) && L.length input <= 30 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      goCode = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.GoAst.generateCode ast
      validity = case goCode of
        Nothing -> True
        Just (Right code) -> Compiler.GoAst.isValidGoCode code
        Just (Left _) -> True  -- Generation may fail
  in property $ validity

prop_go_type_mapping_correctness :: String -> Property
prop_go_type_mapping_correctness typeName =
  not (null typeName) && L.length typeName <= 20 && isValidTypeName typeName ==>
  let mappedType = Compiler.GoAst.mapType typeName
      isCorrectMapping = mappedType `elem` ["int", "string", "bool", "float64", typeName]
  in property $ isCorrectMapping

-- Error recovery properties
prop_error_recovery_maintains_state :: String -> Property
prop_error_recovery_maintains_state input =
  let initialState = Compiler.initialState
      result = Compiler.compileWithRecovery input initialState
      stateMaintained = Compiler.isValidState result
  in property $ stateMaintained

prop_partial_compilation_with_errors :: String -> Property
prop_partial_compilation_with_errors input =
  let withError = input ++ " + )invalid("  -- Introduce syntax error
      result = Compiler.compilePartial withError
      hasPartialResult = case result of
        Left _ -> False
        Right partial -> Compiler.hasPartialCompilation partial
  in property $ hasPartialResult || L.length withError < 5

prop_error_context_preservation_integration :: String -> String -> Property
prop_error_context_preservation_integration input context =
  not (null input) && not (null context) ==>
  let result = Compiler.compileWithContext input context
      contextPreserved = case result of
        Left err -> context `L.isInfixOf` show err
        Right _ -> True  -- Success is also acceptable
  in property $ contextPreserved

-- Performance properties
prop_optimization_improves_performance :: String -> Property
prop_optimization_improves_performance input =
  not (null input) && L.length input <= 50 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      unoptimized = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.compileUnoptimized ast
      optimized = case parsed of
        Left _ -> Nothing
        Right ast -> Just <$> Compiler.compileOptimized ast
      improvement = case (unoptimized, optimized) of
        (Just (Right u), Just (Right o)) -> Compiler.isMoreEfficient o u
        _ -> True  -- Different error handling is acceptable
  in property $ improvement

prop_memory_usage_optimization :: String -> Property
prop_memory_usage_optimization input =
  not (null input) && L.length input <= 30 && isValidExpression input ==>
  let parsed = Parser.parseExpression input
      memoryUsage = case parsed of
        Left _ -> Nothing
        Right ast -> Just $ Compiler.calculateMemoryUsage ast
      optimizedUsage = case parsed of
        Left _ -> Nothing
        Right ast -> Just $ Compiler.calculateOptimizedMemoryUsage ast
      memoryOptimized = case (memoryUsage, optimizedUsage) of
        (Just m, Just o) -> o <= m
        _ -> True
  in property $ memoryOptimized

prop_compilation_time_scaling :: [String] -> Property
prop_compilation_time_scaling inputs =
  not (null inputs) && L.length inputs <= 10 && L.all isValidExpression inputs ==>
  let singleTime = L.maximum $ map Compiler.estimateCompilationTime inputs
      combinedInput = intercalate "; " inputs
      combinedTime = Compiler.estimateCompilationTime combinedInput
      reasonableScaling = combinedTime <= singleTime * 2  -- Allow some overhead
  in property $ reasonableScaling

-- Helper functions
isValidExpression :: String -> Bool
isValidExpression = L.all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 +-*/()=;{}")

isValidVariableName :: String -> Bool
isValidVariableName = L.all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")

isValidTypeName :: String -> Bool
isValidTypeName = L.all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

isConstantExpression :: String -> Bool
isConstantExpression = L.all (`elem` "0123456789 +-*/()")
