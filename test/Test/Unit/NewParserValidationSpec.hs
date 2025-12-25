{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewParserValidationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Parser (parseTypus)
import SourceLocation (SourceSpan(..), startPos)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlpha, isAlphaNum)

-- Property: Parser handles balanced parentheses correctly
prop_balanced_parentheses :: String -> Property
prop_balanced_parentheses content =
  not (null content) && not ('(' `elem` content) && not (')' `elem` content) ==>
  let wrapped = "package main\nfunc main() {\n  println(" ++ content ++ ")\n}"
      result = parseTypus wrapped
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles nested structures correctly
prop_nested_structures :: Int -> Property
prop_nested_structures depth =
  depth >= 0 && depth <= 5 ==>
  let nestedFuncs = concat $ replicate depth "func inner() {\n  "
      closingBraces = concat $ replicate depth "}\n"
      content = "package main\n" ++ nestedFuncs ++ "println(\"test\")\n" ++ closingBraces
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles multiple imports correctly
prop_multiple_imports :: [String] -> Property
prop_multiple_imports imports =
  not (null imports) && length imports <= 5 ==>
  let validImports = filter (\imp -> not (null imp) && not ('"' `elem` imp) && not ('/' `elem` imp)) imports
      importLines = map (\imp -> "import \"" ++ imp ++ "\"") validImports
      content = "package main\n" ++ unlines importLines ++ "\nfunc main() {}\n"
      result = parseTypus content
  in case result of
    Left _ -> not (null validImports) ==> property False
    Right _ -> property True

-- Property: Parser handles array/slice syntax correctly
prop_array_syntax :: String -> Property
prop_array_syntax elemType =
  not (null elemType) && all isAlpha elemType ==>
  let content = "package main\nfunc main() {\n  var arr []" ++ elemType ++ "\n  arr = append(arr, " ++ elemType ++ "(0))\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles interface definitions correctly
prop_interface_definitions :: String -> Property
prop_interface_definitions ifaceName =
  not (null ifaceName) && isAlpha (head ifaceName) && all isAlphaNum ifaceName ==>
  let content = "package main\ntype " ++ ifaceName ++ " interface {\n  Method() error\n}\nfunc main() {}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles struct methods correctly
prop_struct_methods :: String -> String -> Property
prop_struct_methods structName methodName =
  not (null structName) && not (null methodName) &&
  isAlpha (head structName) && isAlpha (head methodName) &&
  all isAlphaNum structName && all isAlphaNum methodName ==>
  let content = "package main\ntype " ++ structName ++ " struct{}\nfunc (s *" ++ structName ++ ") " ++ methodName ++ "() {}\nfunc main() {}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles goroutine syntax correctly
prop_goroutine_syntax :: String -> Property
prop_goroutine_syntax funcName =
  not (null funcName) && isAlpha (head funcName) && all isAlphaNum funcName ==>
  let content = "package main\nfunc " ++ funcName ++ "() {}\nfunc main() {\n  go " ++ funcName ++ "()\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles channel operations correctly
prop_channel_operations :: String -> Property
prop_channel_operations chanType =
  not (null chanType) && all isAlpha chanType ==>
  let content = "package main\nfunc main() {\n  ch := make(chan " ++ chanType ++ ")\n  ch <- " ++ chanType ++ "(0)\n  <-ch\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles select statements correctly
prop_select_statements :: Property
prop_select_statements =
  let content = "package main\nfunc main() {\n  select {\n  case <-time.After(time.Second):\n    break\n  default:\n    break\n  }\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles defer statements correctly
prop_defer_statements :: String -> Property
prop_defer_statements funcName =
  not (null funcName) && isAlpha (head funcName) && all isAlphaNum funcName ==>
  let content = "package main\nfunc " ++ funcName ++ "() {}\nfunc main() {\n  defer " ++ funcName ++ "()\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles panic and recover correctly
prop_panic_recover :: Property
prop_panic_recover =
  let content = "package main\nfunc main() {\n  defer func() {\n    if r := recover(); r != nil {\n      println(\"recovered\")\n    }\n  }()\n  panic(\"test\")\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles type assertions correctly
prop_type_assertions :: String -> Property
prop_type_assertions typeName =
  not (null typeName) && isAlpha (head typeName) && all isAlphaNum typeName ==>
  let content = "package main\nfunc main() {\n  var x interface{} = " ++ typeName ++ "(0)\n  if val, ok := x.(" ++ typeName ++ "); ok {\n    println(val)\n  }\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles switch statements correctly
prop_switch_statements :: String -> Property
prop_switch_statements varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let content = "package main\nfunc main() {\n  " ++ varName ++ " := 1\n  switch " ++ varName ++ " {\n  case 1:\n    break\n  case 2:\n    break\n  default:\n    break\n  }\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles range loops correctly
prop_range_loops :: String -> Property
prop_range_loops arrayName =
  not (null arrayName) && isAlpha (head arrayName) && all isAlphaNum arrayName ==>
  let content = "package main\nfunc main() {\n  " ++ arrayName ++ " := []int{1, 2, 3}\n  for _, v := range " ++ arrayName ++ " {\n    println(v)\n  }\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles function literals/closures correctly
prop_function_literals :: String -> Property
prop_function_literals paramName =
  not (null paramName) && isAlpha (head paramName) && all isAlphaNum paramName ==>
  let content = "package main\nfunc main() {\n  fn := func(" ++ paramName ++ " int) int {\n    return " ++ paramName ++ " * 2\n  }\n  result := fn(5)\n  println(result)\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

-- Property: Parser handles complex expressions correctly
prop_complex_expressions :: Int -> Int -> Int -> Property
prop_complex_expressions a b c =
  a >= 0 && b >= 0 && c >= 0 && a <= 100 && b <= 100 && c <= 100 ==>
  let content = "package main\nfunc main() {\n  result := (" ++ show a ++ " + " ++ show b ++ ") * " ++ show c ++ " - (" ++ show a ++ " / (" ++ show b ++ " + 1))\n  println(result)\n}\n"
      result = parseTypus content
  in case result of
    Left _ -> property False
    Right _ -> property True

tests :: TestTree
tests = testGroup "New Parser Validation tests"
  [ fastProperty "Parser handles balanced parentheses correctly" prop_balanced_parentheses
  , fastProperty "Parser handles nested structures correctly" prop_nested_structures
  , fastProperty "Parser handles multiple imports correctly" prop_multiple_imports
  , fastProperty "Parser handles array/slice syntax correctly" prop_array_syntax
  , fastProperty "Parser handles interface definitions correctly" prop_interface_definitions
  , fastProperty "Parser handles struct methods correctly" prop_struct_methods
  , fastProperty "Parser handles goroutine syntax correctly" prop_goroutine_syntax
  , fastProperty "Parser handles channel operations correctly" prop_channel_operations
  , fastProperty "Parser handles select statements correctly" prop_select_statements
  , fastProperty "Parser handles defer statements correctly" prop_defer_statements
  , fastProperty "Parser handles panic and recover correctly" prop_panic_recover
  , fastProperty "Parser handles type assertions correctly" prop_type_assertions
  , fastProperty "Parser handles switch statements correctly" prop_switch_statements
  , fastProperty "Parser handles range loops correctly" prop_range_loops
  , fastProperty "Parser handles function literals/closures correctly" prop_function_literals
  , fastProperty "Parser handles complex expressions correctly" prop_complex_expressions
  ]