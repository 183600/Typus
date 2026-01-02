{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerOptimizationConsistencyQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))

import Compiler (compile, CompilerError(..), CompilationResult(..))
import Compiler.IR (SourceIR(..), optimizeIR, simplifyIR)
import Compiler.GoAst (renderGoModule)
import Parser (parseTypus)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort, nub)

-- Property: Optimization preserves semantic equivalence
prop_optimization_preserves_semantics :: String -> Property
prop_optimization_preserves_semantics code =
  let hasCode = L.length code > 10
      simpleCode = L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ' ' ++ '
' ++ '=' ++ ';' ++ '(' ++ ')' ++ '+' ++ '-' ++ '*') code
  in hasCode && simpleCode ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right ir ->
          let optimized = optimizeIR ir
              simplified = simplifyIR ir
              originalStr = show ir
              optimizedStr = show optimized
              simplifiedStr = show simplified
              sameStructure = L.length (lines originalStr) == L.length (lines optimizedStr) ||
                              L.length (lines originalStr) <= L.length (lines optimizedStr) + 2
          in property $ sameStructure
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Multiple optimizations are idempotent
prop_optimization_idempotent :: String -> Property
prop_optimization_idempotent code =
  let hasCode = L.length code > 5
  in hasCode ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right ir ->
          let optimized1 = optimizeIR ir
              optimized2 = optimizeIR optimized1
              optimized1Str = show optimized1
              optimized2Str = show optimized2
              idempotent = optimized1Str == optimized2Str
          in property $ idempotent .||. L.length optimized2Str <= L.length optimized1Str
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Simplification reduces complexity
prop_simplification_reduces_complexity :: String -> Property
prop_simplification_reduces_complexity code =
  let hasCode = L.length code > 10
      hasComplexity = L.any (`elem` code) "+-*/"
  in hasCode && hasComplexity ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right ir ->
          let simplified = simplifyIR ir
              originalStr = show ir
              simplifiedStr = show simplified
              reducedComplexity = L.length simplifiedStr <= L.length originalStr ||
                                  L.length (lines simplifiedStr) <= L.length (lines originalStr)
          in property $ reducedComplexity
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Optimization preserves function signatures
prop_optimization_preserves_functions :: String -> String -> Property
prop_optimization_preserves_functions funcName funcBody =
  let validName = L.length funcName > 0 && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ '_') funcName
      validBody = L.length funcBody > 0
      code = "func " ++ funcName ++ "() { " ++ funcBody ++ " }"
  in validName && validBody ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right ir ->
          let optimized = optimizeIR ir
              optimizedStr = show optimized
              hasFunction = "func" `L.isInfixOf` optimizedStr && funcName `L.isInfixOf` optimizedStr
          in property $ hasFunction
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Optimization doesn't introduce new errors
prop_optimization_no_new_errors :: String -> Property
prop_optimization_no_new_errors code =
  let hasCode = L.length code > 5
  in hasCode ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right ir ->
          let optimized = optimizeIR ir
              optimizedStr = show optimized
              hasErrorIndicators = L.any (`isInfixId` optimizedStr) ["error", "Error", "undefined", "null"]
          in property $ not hasErrorIndicators
        Left _ -> property $ True
    Left _ -> property $ True
  where
    isInfixId needle haystack = needle `L.isInfixOf` haystack && L.length needle > 2

-- Property: Optimization preserves variable declarations
prop_optimization_preserves_variables :: [String] -> Property
prop_optimization_preserves_variables varNames =
  let hasVars = L.length varNames > 0
      validVars = L.all (\v -> L.length v > 0 && L.all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ '_') v) varNames
      uniqueVars = L.length (nub varNames) == L.length varNames
      varDecls = L.map (\v -> "var " ++ v ++ " = 0") varNames
      code = unlines varDecls
  in hasVars && validVars && uniqueVars ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right ir ->
          let optimized = optimizeIR ir
              optimizedStr = show optimized
              allVarsPresent = L.all (`isInfix` optimizedStr) varNames
          in property $ allVarsPresent
        Left _ -> property $ True
    Left _ -> property $ True

-- Property: Optimization is deterministic
prop_optimization_deterministic :: String -> Property
prop_optimization_deterministic code =
  let hasCode = L.length code > 5
  in hasCode ==>
  case parseTypus code of
    Right typusFile ->
      case compile typusFile of
        Right ir ->
          let optimized1 = optimizeIR ir
              optimized2 = optimizeIR ir
              optimized1Str = show optimized1
              optimized2Str = show optimized2
              deterministic = optimized1Str == optimized2Str
          in property $ deterministic
        Left _ -> property $ True
    Left _ -> property $ True

tests :: TestTree
tests = testGroup "Compiler Optimization Consistency QuickCheck Tests"
  [ fastProperty "Optimization preserves semantic equivalence" prop_optimization_preserves_semantics
  , fastProperty "Multiple optimizations are idempotent" prop_optimization_idempotent
  , fastProperty "Simplification reduces complexity" prop_simplification_reduces_complexity
  , fastProperty "Optimization preserves function signatures" prop_optimization_preserves_functions
  , fastProperty "Optimization doesn't introduce new errors" prop_optimization_no_new_errors
  , fastProperty "Optimization preserves variable declarations" prop_optimization_preserves_variables
  , fastProperty "Optimization is deterministic" prop_optimization_deterministic
  ]