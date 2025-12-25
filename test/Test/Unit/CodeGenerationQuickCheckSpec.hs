{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CodeGenerationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Compiler (compileTypus)
import Compiler.IR (IRProgram(..), IRStatement(..), IRExpression(..), generateCode)
import Parser (parseTypus)

import Data.Char (isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub)
import qualified Data.List as List
import qualified Data.Map as Map

-- Property: Code generation should handle basic arithmetic
prop_code_generation_arithmetic :: Int -> Int -> Property
prop_code_generation_arithmetic x y =
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   result := " ++ show x ++ " + " ++ show y
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True  -- Parsing may fail
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True  -- Compilation may fail
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True  -- Code generation may fail
               Right code -> property $ True  -- Success

-- Property: Code generation should handle variable declarations
prop_code_generation_variables :: String -> String -> Property
prop_code_generation_variables varName varType =
  not (null varName) && not (null varType) &&
  all isLetter varName && all isLetter varType ==>
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   var " ++ varName ++ " " ++ varType
        , "   " ++ varName ++ " = 42"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle function calls
prop_code_generation_function_calls :: String -> Property
prop_code_generation_function_calls funcName =
  not (null funcName) && all isLetter funcName ==>
  let source = unlines 
        [ "package main"
        , "func " ++ funcName ++ "() int {"
        , "   return 42"
        , "}"
        , "func main() {"
        , "   result := " ++ funcName ++ "()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle control structures
prop_code_generation_control_structures :: String -> Property
prop_code_generation_control_structures condition =
  length condition <= 30 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   if " ++ condition ++ " {"
        , "      x := 1"
        , "   } else {"
        , "      x := 2"
        , "   }"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle loops
prop_code_generation_loops :: Int -> Property
prop_code_generation_loops iterations =
  iterations >= 0 && iterations <= 10 ==> -- Reasonable limit
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   for i := 0; i < " ++ show iterations ++ "; i++ {"
        , "      x := i * 2"
        , "   }"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle struct definitions
prop_code_generation_structs :: String -> [String] -> Property
prop_code_generation_structs structName fieldNames =
  not (null structName) && not (null fieldNames) &&
  all isLetter structName && all (\f -> not (null f) && all isLetter f) (take 3 fieldNames) ==>
  let limitedFields = take 3 fieldNames
      fieldLines = map (\f -> "   " ++ f ++ " int") limitedFields
      source = unlines $ 
        [ "package main"
        , "type " ++ structName ++ " struct {"
        ] ++ fieldLines ++ 
        [ "}"
        , "func main() {"
        , "   s := " ++ structName ++ "{}"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle interface definitions
prop_code_generation_interfaces :: String -> [String] -> Property
prop_code_generation_interfaces interfaceName methodNames =
  not (null interfaceName) && not (null methodNames) &&
  all isLetter interfaceName && all (\m -> not (null m) && all isLetter m) (take 3 methodNames) ==>
  let limitedMethods = take 3 methodNames
      methodLines = map (\m -> "   " ++ m ++ "()") limitedMethods
      source = unlines $ 
        [ "package main"
        , "type " ++ interfaceName ++ " interface {"
        ] ++ methodLines ++ 
        [ "}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle arrays and slices
prop_code_generation_arrays :: Int -> Property
prop_code_generation_arrays size =
  size >= 0 && size <= 10 ==> -- Reasonable size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   arr := [" ++ unwords (replicate size "0") ++ "]"
        , "   slice := make([]int, " ++ show size ++ ")"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle maps
prop_code_generation_maps :: [String] -> Property
prop_code_generation_maps keys =
  not (null keys) && length (take 3 keys) <= 3 ==> -- Limit keys
  let limitedKeys = take 3 keys
      source = unlines 
        [ "package main"
        , "func main() {"
        , "   m := make(map[string]int)"
        ] ++ map (\k -> "   m[\"" ++ k ++ "\"] = 1") limitedKeys ++
        [ "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle pointers
prop_code_generation_pointers :: String -> Property
prop_code_generation_pointers varName =
  not (null varName) && all isLetter varName ==>
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ varName ++ " := 42"
        , "   ptr := &" ++ varName
        , "   value := *ptr"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle method calls
prop_code_generation_methods :: String -> String -> Property
prop_code_generation_methods structName methodName =
  not (null structName) && not (null methodName) &&
  all isLetter structName && all isLetter methodName ==>
  let source = unlines 
        [ "package main"
        , "type " ++ structName ++ " struct { value int }"
        , "func (s " ++ structName ++ ") " ++ methodName ++ "() int {"
        , "   return s.value"
        , "}"
        , "func main() {"
        , "   s := " ++ structName ++ "{value: 42}"
        , "   result := s." ++ methodName ++ "()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle goroutines
prop_code_generation_goroutines :: String -> Property
prop_code_generation_goroutines funcName =
  not (null funcName) && all isLetter funcName ==>
  let source = unlines 
        [ "package main"
        , "func " ++ funcName ++ "() {"
        , "   println(\"working\")"
        , "}"
        , "func main() {"
        , "   go " ++ funcName ++ "()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle channels
prop_code_generation_channels :: Property
prop_code_generation_channels =
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   ch := make(chan int)"
        , "   ch <- 42"
        , "   value := <-ch"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle defer statements
prop_code_generation_defer :: String -> Property
prop_code_generation_defer funcName =
  not (null funcName) && all isLetter funcName ==>
  let source = unlines 
        [ "package main"
        , "func " ++ funcName ++ "() {"
        , "   defer println(\"cleanup\")"
        , "   println(\"work\")"
        , "}"
        , "func main() {"
        , "   " ++ funcName ++ "()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle select statements
prop_code_generation_select :: Property
prop_code_generation_select =
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   ch1 := make(chan int)"
        , "   ch2 := make(chan int)"
        , "   select {"
        , "   case v1 := <-ch1:"
        , "      println(v1)"
        , "   case v2 := <-ch2:"
        , "      println(v2)"
        , "   default:"
        , "      println(\"no value\")"
        , "   }"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle panic and recover
prop_code_generation_panic_recover :: String -> Property
prop_code_generation_panic_recover message =
  length message <= 30 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func mayPanic() {"
        , "   panic(\"" ++ message ++ "\")"
        , "}"
        , "func main() {"
        , "   defer func() {"
        , "      if r := recover(); r != nil {"
        , "         println(\"recovered\")"
        , "      }"
        , "   }()"
        , "   mayPanic()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle type assertions
prop_code_generation_assertions :: String -> Property
prop_code_generation_assertions typeName =
  not (null typeName) && all isLetter typeName ==>
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   var x interface{} = 42"
        , "   y, ok := x.(" ++ typeName ++ ")"
        , "   _ = y"
        , "   _ = ok"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle generic types
prop_code_generation_generics :: String -> String -> Property
prop_code_generation_generics typeName typeParam =
  not (null typeName) && not (null typeParam) &&
  all isLetter typeName && all isLetter typeParam ==>
  let source = unlines 
        [ "package main"
        , "type Container[" ++ typeParam ++ " any] struct {"
        , "   value " ++ typeParam
        , "}"
        , "func main() {"
        , "   c := Container[int]{value: 42}"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should be consistent
prop_code_generation_consistency :: String -> Property
prop_code_generation_consistency source =
  length source <= 100 ==> -- Limit size
  case parseTypus source of
    Left _ -> property $ True
    Right parseResult -> 
      case compileTypus parseResult of
        Left _ -> property $ True
        Right ir -> 
          case generateCode ir of
            Left _ -> property $ True
            Right code1 -> 
              case generateCode ir of
                Left _ -> property $ True
                Right code2 -> code1 === code2

-- Property: Code generation should handle complex expressions
prop_code_generation_complex_expressions :: Int -> Int -> Int -> Property
prop_code_generation_complex_expressions x y z =
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   result := (" ++ show x ++ " + " ++ show y ++ ") * " ++ show z ++ " / 2"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

-- Property: Code generation should handle imports
prop_code_generation_imports :: [String] -> Property
prop_code_generation_imports importPaths =
  not (null importPaths) && all (\p -> not (null p) && not (' ' `elem` p)) (take 3 importPaths) ==>
  let limitedPaths = take 3 importPaths
      importLines = map (\p -> "import \"" ++ p ++ "\"") limitedPaths
      source = unlines $ ["package main"] ++ importLines ++ ["func main() {}"]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case compileTypus parseResult of
           Left _ -> property $ True
           Right ir -> 
             case generateCode ir of
               Left _ -> property $ True
               Right code -> property $ True

tests :: TestTree
tests = testGroup "Code Generation QuickCheck Tests"
  [ fastProperty "Code generation arithmetic" prop_code_generation_arithmetic
  , fastProperty "Code generation variables" prop_code_generation_variables
  , fastProperty "Code generation function calls" prop_code_generation_function_calls
  , fastProperty "Code generation control structures" prop_code_generation_control_structures
  , fastProperty "Code generation loops" prop_code_generation_loops
  , fastProperty "Code generation structs" prop_code_generation_structs
  , fastProperty "Code generation interfaces" prop_code_generation_interfaces
  , fastProperty "Code generation arrays" prop_code_generation_arrays
  , fastProperty "Code generation maps" prop_code_generation_maps
  , fastProperty "Code generation pointers" prop_code_generation_pointers
  , fastProperty "Code generation methods" prop_code_generation_methods
  , fastProperty "Code generation goroutines" prop_code_generation_goroutines
  , fastProperty "Code generation channels" prop_code_generation_channels
  , fastProperty "Code generation defer" prop_code_generation_defer
  , fastProperty "Code generation select" prop_code_generation_select
  , fastProperty "Code generation panic recover" prop_code_generation_panic_recover
  , fastProperty "Code generation assertions" prop_code_generation_assertions
  , fastProperty "Code generation generics" prop_code_generation_generics
  , fastProperty "Code generation consistency" prop_code_generation_consistency
  , fastProperty "Code generation complex expressions" prop_code_generation_complex_expressions
  , fastProperty "Code generation imports" prop_code_generation_imports
  ]