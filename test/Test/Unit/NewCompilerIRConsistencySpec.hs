{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.NewCompilerIRConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, elements, listOf, oneof, sized, Positive(..))

import Parser (parseTypus, TypusFile(..), CodeBlock(..))
import Compiler.IR (IRModule(..), IRFunction(..), IRStatement(..), IRExpression(..), IRType(..))
import Compiler (compileToIR, compileToIR')
import Compiler.TypeChecker (TypeCheckResult(..))
import Data.List (nub, sort, length)
import Control.DeepSeq (force)
import Data.Either (isLeft, isRight)

tests :: TestTree
tests = testGroup "New Compiler IR Consistency Tests"
    [ testCase "generates consistent IR for simple functions" $ do
        let source = unlines
              [ "package main"
              , "func add(a: int, b: int) -> int {"
              , "  return a + b"
              , "}"
              , "func main() {"
              , "  let result = add(5, 3)"
              , "  return result"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result1 <- compileToIR typusFile
            result2 <- compileToIR typusFile
            case (result1, result2) of
              (Right ir1, Right ir2) -> do
                assertBool "IR modules should be equal" $ irModulesEqual ir1 ir2
                assertBool "Should have correct number of functions" $ 
                  length (irFunctions ir1) == 2
              (Left err1, Left err2) -> 
                assertBool "Error messages should be consistent" $ err1 == err2
              _ -> assertFailure "Compilation results should be consistent"
              
    , testCase "maintains type consistency in IR" $ do
        let source = unlines
              [ "package main"
              , "func typed_function(x: int, y: string) -> string {"
              , "  let result = x.toString() + y"
              , "  return result"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- compileToIR typusFile
            case result of
              Left err -> assertFailure $ "Compilation failed: " ++ err
              Right irModule -> do
                assertBool "Should have function" $ not $ null (irFunctions irModule)
                let func = head (irFunctions irModule)
                assertBool "Function should have correct parameter types" $ 
                  parameterTypesConsistent func ["int", "string"]
                assertBool "Function should have correct return type" $ 
                  returnTypeConsistent func "string"
                
    , testCase "preserves control flow structure in IR" $ do
        let source = unlines
              [ "package main"
              , "func control_flow(x: int) -> int {"
              , "  if x > 0 {"
              , "    return x * 2"
              , "  } else {"
              , "    return x * 3"
              , "  }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "Parse failed: " ++ err
          Right typusFile -> do
            result <- compileToIR typusFile
            case result of
              Left err -> assertFailure $ "Compilation failed: " ++ err
              Right irModule -> do
                assertBool "Should have function" $ not $ null (irFunctions irModule)
                let func = head (irFunctions irModule)
                let statements = irFunctionBody func
                assertBool "Should have conditional statement" $ 
                  any isConditionalStatement statements
                assertBool "Should have branch statements" $ 
                  any isBranchStatement statements
    ]

-- QuickCheck properties for IR consistency

-- Property: IR generation should be deterministic
prop_ir_generation_deterministic :: String -> Property
prop_ir_generation_deterministic source =
  case parseTypus source of
    Left _ -> property $ True  -- Invalid source, skip property test
    Right typusFile -> do
      result1 <- compileToIR typusFile
      result2 <- compileToIR typusFile
      property $ case (result1, result2) of
        (Right ir1, Right ir2) -> irModulesEqual ir1 ir2
        (Left err1, Left err2) -> err1 === err2
        _ -> property False

-- Helper functions for IR consistency checking

irModulesEqual :: IRModule -> IRModule -> Bool
irModulesEqual ir1 ir2 = 
  length (irFunctions ir1) == length (irFunctions ir2) &&
  all (uncurry functionsEqual) (zip (irFunctions ir1) (irFunctions ir2))

functionsEqual :: IRFunction -> IRFunction -> Bool
functionsEqual f1 f2 = 
  irFunctionName f1 == irFunctionName f2 &&
  length (irFunctionBody f1) == length (irFunctionBody f2)

parameterTypesConsistent :: IRFunction -> [String] -> Bool
parameterTypesConsistent func expectedTypes = 
  let actualTypes = map irTypeToString (irFunctionParameters func)
  in length actualTypes == length expectedTypes &&
     all (uncurry (==)) (zip actualTypes expectedTypes)

returnTypeConsistent :: IRFunction -> String -> Bool
returnTypeConsistent func expectedType = 
  irTypeToString (irFunctionReturnType func) == expectedType

isConditionalStatement :: IRStatement -> Bool
isConditionalStatement (IRConditional _ _ _) = True
isConditionalStatement _ = False

isBranchStatement :: IRStatement -> Bool
isBranchStatement (IRBranch _) = True
isBranchStatement _ = False

irTypeToString :: IRType -> String
irTypeToString irType = 
  case irType of
    IRIntType -> "int"
    IRStringType -> "string"
    IRBoolType -> "bool"
    IRVoidType -> "void"
    IRFunctionType paramTypes returnType -> 
      "(" ++ unwords (map irTypeToString paramTypes) ++ ") -> " ++ irTypeToString returnType
    _ -> "unknown"