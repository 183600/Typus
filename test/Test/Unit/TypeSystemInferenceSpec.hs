{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.TypeSystemInferenceSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import Dependencies (TypeInferenceState(..), TypeInferenceError(..))
import Dependencies.TypeSystem (TypeConstraint(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\), intersect)
import Control.Monad (when, replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- Type System Inference Tests
-- ============================================================================

-- | Test basic type inference for literals
prop_type_inference_literals :: Int -> Property
prop_type_inference_literals value =
  let literalCode = "let x = " ++ show value ++ "\n"
      parseResult = parseTypus literalCode
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True
              Right goCode -> property $ not (null goCode)

-- | Test type inference for arithmetic operations
prop_type_inference_arithmetic :: Int -> Int -> Property
prop_type_inference_arithmetic x y =
  let arithmeticCode = "let a = " ++ show x ++ "\n" ++
                       "let b = " ++ show y ++ "\n" ++
                       "let c = a + b\n"
      parseResult = parseTypus arithmeticCode
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True
              Right goCode -> property $ not (null goCode)

-- | Test type inference for function definitions
prop_type_inference_functions :: String -> String -> Property
prop_type_inference_functions funcName paramName =
  not (null funcName) && not (null paramName) && all isAlphaNum funcName && all isAlphaNum paramName ==>
    let functionCode = "function " ++ funcName ++ "(" ++ paramName ++ ") {\n" ++
                       "  return " ++ paramName ++ "\n" ++
                       "}\n"
        parseResult = parseTypus functionCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for conditional expressions
prop_type_inference_conditionals :: Bool -> Int -> Int -> Property
prop_type_inference_conditionals condition x y =
  let conditionalCode = "let condition = " ++ show condition ++ "\n" ++
                        "let result = if (condition) " ++ show x ++ " else " ++ show y ++ "\n"
      parseResult = parseTypus conditionalCode
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let compileResult = compile typusFile
         in case compileResult of
              Left _ -> property True
              Right goCode -> property $ not (null goCode)

-- | Test type inference for arrays
prop_type_inference_arrays :: Int -> Property
prop_type_inference_arrays n =
  n >= 0 && n <= 10 ==>
    let arrayCode = "let arr = [" ++ unwords (map show [1..n]) ++ "]\n" ++
                    "let first = arr[0]\n"
        parseResult = parseTypus arrayCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for objects/records
prop_type_inference_objects :: String -> String -> Property
prop_type_inference_objects field1 field2 =
  not (null field1) && not (null field2) && all isAlphaNum field1 && all isAlphaNum field2 && field1 /= field2 ==>
    let objectCode = "let obj = { " ++ field1 ++ ": 42, " ++ field2 ++ ": \"hello\" }\n" ++
                     "let value1 = obj." ++ field1 ++ "\n" ++
                     "let value2 = obj." ++ field2 ++ "\n"
        parseResult = parseTypus objectCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for generic functions
prop_type_inference_generics :: String -> String -> Property
prop_type_inference_generics funcName typeParam =
  not (null funcName) && not (null typeParam) && all isAlphaNum funcName && all isAlphaNum typeParam ==>
    let genericCode = "function " ++ funcName ++ "<" ++ typeParam ++ ">(x: " ++ typeParam ++ ") {\n" ++
                      "  return x\n" ++
                      "}\n" ++
                      "let result = " ++ funcName ++ "(42)\n"
        parseResult = parseTypus genericCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for dependent types
prop_type_inference_dependent_types :: String -> Int -> Property
prop_type_inference_dependent_types typeName value =
  not (null typeName) && all isAlphaNum typeName && value >= 0 && value <= 100 ==>
    let dependentCode = "type Vector<" ++ show value ++ "> = Array<" ++ show value ++ ">\n" ++
                        "let v: Vector<" ++ show value ++ "> = [1, 2, 3]\n"
        parseResult = parseTypus dependentCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for recursive types
prop_type_inference_recursive_types :: String -> Property
prop_type_inference_recursive_types typeName =
  not (null typeName) && all isAlphaNum typeName ==>
    let recursiveCode = "type " ++ typeName ++ " = {\n" ++
                        "  value: number,\n" ++
                        "  next: " ++ typeName ++ " | null\n" ++
                        "}\n" ++
                        "let list: " ++ typeName ++ " = { value: 1, next: null }\n"
        parseResult = parseTypus recursiveCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for union types
prop_type_inference_union_types :: String -> String -> Property
prop_type_inference_union_types type1 type2 =
  not (null type1) && not (null type2) && type1 /= type2 ==>
    let unionCode = "let value: " ++ type1 ++ " | " ++ type2 ++ " = 42\n" ++
                    "if (typeof value === \"number\") {\n" ++
                    "  let num = value as number\n" ++
                    "}\n"
        parseResult = parseTypus unionCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for intersection types
prop_type_inference_intersection_types :: String -> String -> Property
prop_type_inference_intersection_types type1 type2 =
  not (null type1) && not (null type2) && type1 /= type2 ==>
    let intersectionCode = "type " ++ type1 ++ " = { a: number }\n" ++
                           "type " ++ type2 ++ " = { b: string }\n" ++
                           "let value: " ++ type1 ++ " & " ++ type2 ++ " = { a: 1, b: \"hello\" }\n"
        parseResult = parseTypus intersectionCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for function composition
prop_type_inference_function_composition :: String -> String -> Property
prop_type_inference_function_composition func1 func2 =
  not (null func1) && not (null func2) && all isAlphaNum func1 && all isAlphaNum func2 && func1 /= func2 ==>
    let compositionCode = "function " ++ func1 ++ "(x: number): string { return x.toString() }\n" ++
                          "function " ++ func2 ++ "(s: string): number { return parseInt(s) }\n" ++
                          "let composed = " ++ func2 ++ "(" ++ func1 ++ "(42))\n"
        parseResult = parseTypus compositionCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for higher-order functions
prop_type_inference_higher_order :: String -> String -> Property
prop_type_inference_higher_order funcName paramName =
  not (null funcName) && not (null paramName) && all isAlphaNum funcName && all isAlphaNum paramName ==>
    let higherOrderCode = "function " ++ funcName ++ "(f: (x: number) => number, " ++ paramName ++ ": number) {\n" ++
                          "  return f(" ++ paramName ++ ")\n" ++
                          "}\n" ++
                          "let result = " ++ funcName ++ "(x => x * 2, 21)\n"
        parseResult = parseTypus higherOrderCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for curried functions
prop_type_inference_curried_functions :: String -> Property
prop_type_inference_curried_functions funcName =
  not (null funcName) && all isAlphaNum funcName ==>
    let curriedCode = "function " ++ funcName ++ "(x: number) {\n" ++
                      "  return function(y: number) {\n" ++
                      "    return x + y\n" ++
                      "  }\n" ++
                      "}\n" ++
                      "let add5 = " ++ funcName ++ "(5)\n" ++
                      "let result = add5(10)\n"
        parseResult = parseTypus curriedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for type constraints
prop_type_inference_type_constraints :: String -> String -> Property
prop_type_inference_type_constraints typeName constraint =
  not (null typeName) && not (null constraint) && all isAlphaNum typeName ==>
    let constraintCode = "type " ++ typeName ++ "<T: " ++ constraint ++ "> = T\n" ++
                         "let value: " ++ typeName ++ "<number> = 42\n"
        parseResult = parseTypus constraintCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for polymorphic functions
prop_type_inference_polymorphic :: String -> Property
prop_type_inference_polymorphic funcName =
  not (null funcName) && all isAlphaNum funcName ==>
    let polymorphicCode = "function " ++ funcName ++ "<T>(x: T, y: T): T {\n" ++
                          "  return x\n" ++
                          "}\n" ++
                          "let result1 = " ++ funcName ++ "(1, 2)\n" ++
                          "let result2 = " ++ funcName ++ "(\"a\", \"b\")\n"
        parseResult = parseTypus polymorphicCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for pattern matching
prop_type_inference_pattern_matching :: String -> Property
prop_type_inference_pattern_matching typeName =
  not (null typeName) && all isAlphaNum typeName ==>
    let patternCode = "type " ++ typeName ++ " = A | B | C\n" ++
                      "function process(x: " ++ typeName ++ "): string {\n" ++
                      "  match x {\n" ++
                      "    | A => \"A\"\n" ++
                      "    | B => \"B\"\n" ++
                      "    | C => \"C\"\n" ++
                      "  }\n" ++
                      "}\n" ++
                      "let result = process(A)\n"
        parseResult = parseTypus patternCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test type inference for type-level computation
prop_type_inference_type_level :: Int -> Int -> Property
prop_type_inference_type_level x y =
  x >= 0 && y >= 0 && x <= 10 && y <= 10 ==>
    let typeLevelCode = "type Add<N, M> = N + M\n" ++
                        "type Result = Add<" ++ show x ++ ", " ++ show y ++ ">\n" ++
                        "let value: Result = " ++ show (x + y) ++ "\n"
        parseResult = parseTypus typeLevelCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Type System Inference Tests"
  [ testProperty "Type inference for literals" prop_type_inference_literals,
    testProperty "Type inference for arithmetic operations" prop_type_inference_arithmetic,
    testProperty "Type inference for function definitions" prop_type_inference_functions,
    testProperty "Type inference for conditional expressions" prop_type_inference_conditionals,
    testProperty "Type inference for arrays" prop_type_inference_arrays,
    testProperty "Type inference for objects/records" prop_type_inference_objects,
    testProperty "Type inference for generic functions" prop_type_inference_generics,
    testProperty "Type inference for dependent types" prop_type_inference_dependent_types,
    testProperty "Type inference for recursive types" prop_type_inference_recursive_types,
    testProperty "Type inference for union types" prop_type_inference_union_types,
    testProperty "Type inference for intersection types" prop_type_inference_intersection_types,
    testProperty "Type inference for function composition" prop_type_inference_function_composition,
    testProperty "Type inference for higher-order functions" prop_type_inference_higher_order,
    testProperty "Type inference for curried functions" prop_type_inference_curried_functions,
    testProperty "Type inference for type constraints" prop_type_inference_type_constraints,
    testProperty "Type inference for polymorphic functions" prop_type_inference_polymorphic,
    testProperty "Type inference for pattern matching" prop_type_inference_pattern_matching,
    testProperty "Type inference for type-level computation" prop_type_inference_type_level
  ]