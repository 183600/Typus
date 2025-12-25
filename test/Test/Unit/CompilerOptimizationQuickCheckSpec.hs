{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerOptimizationQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (generateGoCode)
import Parser (TypusFile(..), parseTypus, defaultFileDirectives)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort)
import Data.Char (isSpace, isDigit)

-- Property: generateGoCode is total (always returns something)
prop_generateGoCode_total :: String -> Property
prop_generateGoCode_total input =
  let result = parseTypus input
  in case result of
    Left _ -> property True  -- Parsing may fail
    Right typusFile -> 
      let goCode = generateGoCode typusFile
      in property (not (null goCode))

-- Property: generateGoCode preserves package declarations
prop_generateGoCode_preserves_package :: String -> Property
prop_generateGoCode_preserves_package packageName =
  not (null packageName) && isAlphaNum packageName ==>
  let code = "package " ++ packageName ++ "\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right typusFile ->
      let goCode = generateGoCode typusFile
      in property (("package " ++ packageName) `isInfixOf` goCode)

-- Property: generateGoCode handles empty input
prop_generateGoCode_empty_input :: Property
prop_generateGoCode_empty_input =
  let result = parseTypus ""
  in case result of
    Left _ -> property True
    Right typusFile ->
      let goCode = generateGoCode typusFile
      in property (not (null goCode))

-- Property: generateGoCode handles simple functions
prop_generateGoCode_simple_functions :: String -> Property
prop_generateGoCode_simple_functions funcName =
  not (null funcName) && isAlphaNum funcName ==>
  let code = "package main\nfunc " ++ funcName ++ "() {}\nfunc main() {}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right typusFile ->
      let goCode = generateGoCode typusFile
      in property (funcName `isInfixOf` goCode)

-- Property: generateGoCode handles variable declarations
prop_generateGoCode_variables :: String -> String -> Property
prop_generateGoCode_variables varName varValue =
  not (null varName) && not (null varValue) && isAlphaNum varName ==>
  let code = "package main\nfunc main() {\n  " ++ varName ++ " := " ++ varValue ++ "\n}\n"
      result = parseTypus code
  in case result of
    Left _ -> property True
    Right typusFile ->
      let goCode = generateGoCode typusFile
      in property (varName `isInfixOf` goCode)

-- Helper function to check if a string contains only alphanumeric characters
isAlphaNum :: String -> Bool
isAlphaNum = all (\c -> isDigit c || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

tests :: TestTree
tests = testGroup "Compiler Optimization QuickCheck tests"
  [ fastProperty "generateGoCode is total (always returns something)" prop_generateGoCode_total
  , fastProperty "generateGoCode preserves package declarations" prop_generateGoCode_preserves_package
  , fastProperty "generateGoCode handles empty input" prop_generateGoCode_empty_input
  , fastProperty "generateGoCode handles simple functions" prop_generateGoCode_simple_functions
  , fastProperty "generateGoCode handles variable declarations" prop_generateGoCode_variables
  ]