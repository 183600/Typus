{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewOwnershipTransferSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Ownership (OwnershipError(..), analyzeOwnership)
import SourceLocation (SourceSpan(..), startPos)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import Data.Char (isSpace, isAlpha, isAlphaNum)

-- Property: Ownership analysis handles move semantics correctly
prop_move_semantics :: String -> Property
prop_move_semantics varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc consume(x string) {}\nfunc main() {\n  " ++ varName ++ " := \"value\"\n  consume(" ++ varName ++ ")\n  println(" ++ varName ++ ")\n}"
      errors = analyzeOwnership source
  in case errors of
    [UseAfterMove v] -> v === varName
    _ -> property False

-- Property: Ownership analysis handles borrow checker correctly
prop_borrow_checker :: String -> Property
prop_borrow_checker varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := \"value\"\n  ref1 := &" ++ varName ++ "\n  ref2 := &mut " ++ varName ++ "\n}"
      errors = analyzeOwnership source
  in case errors of
    [MutBorrowWhileBorrowed v] -> v === varName
    _ -> property False

-- Property: Ownership analysis handles lifetime inference correctly
prop_lifetime_inference :: String -> Property
prop_lifetime_inference varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc getRef() *string {\n  " ++ varName ++ " := \"value\"\n  return &" ++ varName ++ "\n}\nfunc main() {\n  ref := getRef()\n  println(*ref)\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False -- May detect lifetime issues

-- Property: Ownership analysis handles ownership transfer in function returns
prop_ownership_transfer_return :: String -> Property
prop_ownership_transfer_return varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc producer() string {\n  " ++ varName ++ " := \"value\"\n  return " ++ varName ++ "\n}\nfunc main() {\n  value := producer()\n  println(value)\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles shared ownership correctly
prop_shared_ownership :: String -> String -> Property
prop_shared_ownership var1 var2 =
  not (null var1) && not (null var2) &&
  isAlpha (head var1) && isAlpha (head var2) &&
  all isAlphaNum var1 && all isAlphaNum var2 &&
  var1 /= var2 ==>
  let source = "package main\nfunc main() {\n  " ++ var1 ++ " := \"value1\"\n  " ++ var2 ++ " := \"value2\"\n  ref1 := &" ++ var1 ++ "\n  ref2 := &" ++ var2 ++ "\n  println(*ref1)\n  println(*ref2)\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in struct fields
prop_struct_field_ownership :: String -> String -> Property
prop_struct_field_ownership structName fieldName =
  not (null structName) && not (null fieldName) &&
  isAlpha (head structName) && isAlpha (head fieldName) &&
  all isAlphaNum structName && all isAlphaNum fieldName ==>
  let source = "package main\ntype " ++ structName ++ " struct {\n  " ++ fieldName ++ " string\n}\nfunc main() {\n  data := " ++ structName ++ "{" ++ fieldName ++ ": \"value\"}\n  consumed := data." ++ fieldName ++ "\n  println(consumed)\n  println(data." ++ fieldName ++ ")\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True -- Struct fields may be copied
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in slices
prop_slice_ownership :: String -> Property
prop_slice_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := []string{\"a\", \"b\", \"c\"}\n  first := " ++ varName ++ "[0]\n  println(first)\n  println(" ++ varName ++ "[0])\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True -- Slice indexing creates copies
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in maps
prop_map_ownership :: String -> Property
prop_map_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := make(map[string]string)\n  " ++ varName ++ "[\"key\"] = \"value\"\n  value := " ++ varName ++ "[\"key\"]\n  println(value)\n  println(" ++ varName ++ "[\"key\"]\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True -- Map access creates copies
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in channels
prop_channel_ownership :: String -> Property
prop_channel_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := make(chan string)\n  go func() {\n    " ++ varName ++ " <- \"value\"\n  }()\n  value := <-" ++ varName ++ "\n  println(value)\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in closures
prop_closure_ownership :: String -> Property
prop_closure_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := \"value\"\n  fn := func() {\n    println(" ++ varName ++ ")\n  }\n  fn()\n  println(" ++ varName ++ ")\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in goroutines
prop_goroutine_ownership :: String -> Property
prop_goroutine_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := \"value\"\n  go func() {\n    println(" ++ varName ++ ")\n  }()\n  println(" ++ varName ++ ")\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in defer statements
prop_defer_ownership :: String -> Property
prop_defer_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc cleanup(x string) {}\nfunc main() {\n  " ++ varName ++ " := \"value\"\n  defer cleanup(" ++ varName ++ ")\n  println(" ++ varName ++ ")\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in panic/recover
prop_panic_recover_ownership :: String -> Property
prop_panic_recover_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc mayPanic(x string) {\n  if x == \"panic\" {\n    panic(\"error\")\n  }\n}\nfunc main() {\n  " ++ varName ++ " := \"value\"\n  defer func() {\n    if r := recover(); r != nil {\n      println(" ++ varName ++ ")\n    }\n  }()\n  mayPanic(" ++ varName ++ ")\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in interface assignments
prop_interface_ownership :: String -> Property
prop_interface_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\ntype Writer interface {\n  Write()\n}\ntype MyWriter struct {\n  data string\n}\nfunc (w *MyWriter) Write() {}\nfunc main() {\n  " ++ varName ++ " := &MyWriter{data: \"value\"}\n  var w Writer = " ++ varName ++ "\n  w.Write()\n  println(" ++ varName ++ ".data)\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in type assertions
prop_type_assertion_ownership :: String -> Property
prop_type_assertion_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := interface{}(\"value\")\n  if str, ok := " ++ varName ++ ".(string); ok {\n    println(str)\n  }\n  println(" ++ varName ++ ")\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

-- Property: Ownership analysis handles ownership transfer in select statements
prop_select_ownership :: String -> Property
prop_select_ownership varName =
  not (null varName) && isAlpha (head varName) && all isAlphaNum varName ==>
  let source = "package main\nfunc main() {\n  " ++ varName ++ " := make(chan string)\n  go func() {\n    " ++ varName ++ " <- \"value\"\n  }()\n  select {\n  case value := <-" ++ varName ++ ":\n    println(value)\n  }\n}"
      errors = analyzeOwnership source
  in case errors of
    [] -> property True
    _ -> property False

tests :: TestTree
tests = testGroup "New Ownership Transfer tests"
  [ fastProperty "Ownership analysis handles move semantics correctly" prop_move_semantics
  , fastProperty "Ownership analysis handles borrow checker correctly" prop_borrow_checker
  , fastProperty "Ownership analysis handles lifetime inference correctly" prop_lifetime_inference
  , fastProperty "Ownership analysis handles ownership transfer in function returns" prop_ownership_transfer_return
  , fastProperty "Ownership analysis handles shared ownership correctly" prop_shared_ownership
  , fastProperty "Ownership analysis handles ownership transfer in struct fields" prop_struct_field_ownership
  , fastProperty "Ownership analysis handles ownership transfer in slices" prop_slice_ownership
  , fastProperty "Ownership analysis handles ownership transfer in maps" prop_map_ownership
  , fastProperty "Ownership analysis handles ownership transfer in channels" prop_channel_ownership
  , fastProperty "Ownership analysis handles ownership transfer in closures" prop_closure_ownership
  , fastProperty "Ownership analysis handles ownership transfer in goroutines" prop_goroutine_ownership
  , fastProperty "Ownership analysis handles ownership transfer in defer statements" prop_defer_ownership
  , fastProperty "Ownership analysis handles ownership transfer in panic/recover" prop_panic_recover_ownership
  , fastProperty "Ownership analysis handles ownership transfer in interface assignments" prop_interface_ownership
  , fastProperty "Ownership analysis handles ownership transfer in type assertions" prop_type_assertion_ownership
  , fastProperty "Ownership analysis handles ownership transfer in select statements" prop_select_ownership
  ]