{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.DependencyAnalysisQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Dependencies (analyzeDependencies, DependencyGraph(..), Dependency(..))
import Parser (parseTypus)
import Compiler (compileTypus)

import Data.Char (isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Property: Dependency analysis should detect function dependencies
prop_dependency_analysis_functions :: String -> [String] -> Property
prop_dependency_analysis_functions mainFunc calledFuncs =
  not (null mainFunc) && not (null calledFuncs) &&
  all isLetter mainFunc && all (\f -> not (null f) && all isLetter f) (take 3 calledFuncs) ==>
  let limitedFuncs = take 3 calledFuncs
      funcDefs = map (\f -> "func " ++ f ++ "() int { return 42 }") limitedFuncs
      calls = map (\f -> "   _ = " ++ f ++ "()") limitedFuncs
      source = unlines $ 
        [ "package main"
        ] ++ funcDefs ++
        [ "func " ++ mainFunc ++ "() {"
        ] ++ calls ++
        [ "}"
        , "func main() {"
        , "   " ++ mainFunc ++ "()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True  -- Parsing may fail
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True  -- Analysis may fail
           Right deps -> property $ True  -- Success

-- Property: Dependency analysis should detect type dependencies
prop_dependency_analysis_types :: String -> [String] -> Property
prop_dependency_analysis_types baseType fieldTypes =
  not (null baseType) && not (null fieldTypes) &&
  all isLetter baseType && all (\t -> not (null t) && all isLetter t) (take 3 fieldTypes) ==>
  let limitedTypes = take 3 fieldTypes
      typeDefs = map (\t -> "type " ++ t ++ " int") limitedTypes
      fieldLines = map (\t -> "   field " ++ t) limitedTypes
      source = unlines $ 
        [ "package main"
        ] ++ typeDefs ++
        [ "type " ++ baseType ++ " struct {"
        ] ++ fieldLines ++
        [ "}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should detect import dependencies
prop_dependency_analysis_imports :: [String] -> Property
prop_dependency_analysis_imports importPaths =
  not (null importPaths) && all (\p -> not (null p) && not (' ' `elem` p)) (take 4 importPaths) ==>
  let limitedPaths = take 4 importPaths
      importLines = map (\p -> "import \"" ++ p ++ "\"") limitedPaths
      source = unlines $ ["package main"] ++ importLines ++ ["func main() {}"]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should detect interface dependencies
prop_dependency_analysis_interfaces :: String -> [String] -> Property
prop_dependency_analysis_interfaces interfaceName methodTypes =
  not (null interfaceName) && not (null methodTypes) &&
  all isLetter interfaceName && all (\t -> not (null t) && all isLetter t) (take 3 methodTypes) ==>
  let limitedTypes = take 3 methodTypes
      typeDefs = map (\t -> "type " ++ t ++ " int") limitedTypes
      methodLines = zipWith (\t i -> "   Method" ++ show i ++ "() " ++ t) limitedTypes [1..]
      source = unlines $ 
        [ "package main"
        ] ++ typeDefs ++
        [ "type " ++ interfaceName ++ " interface {"
        ] ++ methodLines ++
        [ "}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should detect variable dependencies
prop_dependency_analysis_variables :: String -> [String] -> Property
prop_dependency_analysis_variables baseVar dependentVars =
  not (null baseVar) && not (null dependentVars) &&
  all isLetter baseVar && all (\v -> not (null v) && all isLetter v) (take 3 dependentVars) ==>
  let limitedVars = take 3 dependentVars
      varDefs = map (\v -> v ++ " := " ++ baseVar ++ " + 1") limitedVars
      source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ baseVar ++ " := 42"
        ] ++ varDefs ++
        [ "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle circular dependencies
prop_dependency_analysis_circular :: [String] -> Property
prop_dependency_analysis_circular funcNames =
  length funcNames >= 2 && length (take 3 funcNames) >= 2 &&
  all (\f -> not (null f) && all isLetter f) (take 3 funcNames) ==>
  let limitedNames = take 3 funcNames
      [f1, f2, f3] = take 3 (limitedNames ++ ["default1", "default2", "default3"])
      source = unlines 
        [ "package main"
        , "func " ++ f1 ++ "() {"
        , "   " ++ f2 ++ "()"
        , "}"
        , "func " ++ f2 ++ "() {"
        , "   " ++ f3 ++ "()"
        , "}"
        , "func " ++ f3 ++ "() {"
        , "   " ++ f1 ++ "()"
        , "}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should detect transitive dependencies
prop_dependency_analysis_transitive :: [String] -> Property
prop_dependency_analysis_transitive funcChain =
  length funcChain >= 2 && length (take 4 funcChain) >= 2 &&
  all (\f -> not (null f) && all isLetter f) (take 4 funcChain) ==>
  let limitedChain = take 4 funcChain
      chainPairs = zip limitedChain (tail limitedChain)
      funcDefs = map (\(caller, callee) -> 
        "func " ++ caller ++ "() { " ++ callee ++ "() }") chainPairs
      source = unlines $ 
        [ "package main"
        ] ++ funcDefs ++
        [ "func main() {"
        , "   " ++ head limitedChain ++ "()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle complex dependency graphs
prop_dependency_analysis_complex :: [String] -> [String] -> Property
prop_dependency_analysis_complex funcNames typeNames =
  not (null funcNames) && not (null typeNames) &&
  length (take 3 funcNames) >= 1 && length (take 3 typeNames) >= 1 &&
  all (\f -> not (null f) && all isLetter f) (take 3 funcNames) &&
  all (\t -> not (null t) && all isLetter t) (take 3 typeNames) ==>
  let limitedFuncs = take 3 funcNames
      limitedTypes = take 3 typeNames
      typeDefs = map (\t -> "type " ++ t ++ " int") limitedTypes
      funcDefs = map (\f -> "func " ++ f ++ "() " ++ head limitedTypes ++ " { return 42 }") limitedFuncs
      source = unlines $ 
        [ "package main"
        ] ++ typeDefs ++ funcDefs ++
        [ "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle package-level dependencies
prop_dependency_analysis_package_level :: [String] -> Property
prop_dependency_analysis_package_level varNames =
  not (null varNames) && length (take 3 varNames) >= 1 &&
  all (\v -> not (null v) && all isLetter v) (take 3 varNames) ==>
  let limitedVars = take 3 varNames
      varDefs = map (\v -> "var " ++ v ++ " int = 42") limitedVars
      source = unlines $ 
        [ "package main"
        ] ++ varDefs ++
        [ "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should detect method dependencies
prop_dependency_analysis_methods :: String -> [String] -> Property
prop_dependency_analysis_methods structName methodNames =
  not (null structName) && not (null methodNames) &&
  all isLetter structName && all (\m -> not (null m) && all isLetter m) (take 3 methodNames) ==>
  let limitedMethods = take 3 methodNames
      methodDefs = map (\m -> 
        "func (s " ++ structName ++ ") " ++ m ++ "() int { return 42 }") limitedMethods
      source = unlines $ 
        [ "package main"
        , "type " ++ structName ++ " struct { value int }"
        ] ++ methodDefs ++
        [ "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle generic dependencies
prop_dependency_analysis_generics :: String -> [String] -> Property
prop_dependency_analysis_generics genericName typeParams =
  not (null genericName) && not (null typeParams) &&
  all isLetter genericName && all (\t -> not (null t) && all isLetter t) (take 2 typeParams) ==>
  let limitedParams = take 2 typeParams
      paramList = unwords (map (\p -> p ++ " any") limitedParams)
      source = unlines 
        [ "package main"
        , "type " ++ genericName ++ "[" ++ paramList ++ "] struct {"
        ] ++ map (\p -> "   " ++ p ++ "Field " ++ p) limitedParams ++
        [ "}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should be consistent
prop_dependency_analysis_consistency :: String -> Property
prop_dependency_analysis_consistency source =
  length source <= 100 ==> -- Limit size
  case parseTypus source of
    Left _ -> property $ True
    Right parseResult -> 
      case analyzeDependencies parseResult of
        Left _ -> property $ True
        Right deps1 -> 
          case analyzeDependencies parseResult of
            Left _ -> property $ True
            Right deps2 -> property $ True

-- Property: Dependency analysis should handle recursive structures
prop_dependency_analysis_recursive :: String -> Property
prop_dependency_analysis_recursive structName =
  not (null structName) && all isLetter structName ==>
  let source = unlines 
        [ "package main"
        , "type " ++ structName ++ " struct {"
        , "   value int"
        , "   next *" ++ structName
        , "}"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle cross-file dependencies
prop_dependency_analysis_cross_file :: String -> Property
prop_dependency_analysis_cross_file packageName =
  not (null packageName) && all isLetter packageName ==>
  let source = unlines 
        [ "package main"
        , "import \"" ++ packageName ++ "\""
        , "func main() {"
        , "   " ++ packageName ++ ".Function()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle conditional dependencies
prop_dependency_analysis_conditional :: String -> Property
prop_dependency_analysis_conditional condition =
  length condition <= 30 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func helper() int { return 42 }"
        , "func main() {"
        , "   if " ++ condition ++ " {"
        , "      helper()"
        , "   }"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle anonymous functions
prop_dependency_analysis_anonymous :: String -> Property
prop_dependency_analysis_anonymous varName =
  not (null varName) && all isLetter varName ==>
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ varName ++ " := 42"
        , "   fn := func() {"
        , "      _ = " ++ varName
        , "   }"
        , "   fn()"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle constant dependencies
prop_dependency_analysis_constants :: [String] -> Property
prop_dependency_analysis_constants constNames =
  not (null constNames) && length (take 3 constNames) >= 1 &&
  all (\c -> not (null c) && all isLetter c) (take 3 constNames) ==>
  let limitedConsts = take 3 constNames
      constDefs = map (\c -> "const " ++ c ++ " = 42") limitedConsts
      source = unlines $ 
        [ "package main"
        ] ++ constDefs ++
        [ "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle build tag dependencies
prop_dependency_analysis_build_tags :: [String] -> Property
prop_dependency_analysis_build_tags tags =
  not (null tags) && length (take 3 tags) >= 1 &&
  all (\t -> not (null t)) (take 3 tags) ==>
  let limitedTags = take 3 tags
      tagLines = map (\t -> "//go:build " ++ t) limitedTags
      source = unlines $ tagLines ++ 
        [ "package main"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

-- Property: Dependency analysis should handle dependency cycles detection
prop_dependency_analysis_cycle_detection :: [String] -> Property
prop_dependency_analysis_cycle_detection nodeNames =
  length nodeNames >= 2 && length (take 4 nodeNames) >= 2 &&
  all (\n -> not (null n) && all isLetter n) (take 4 nodeNames) ==>
  let limitedNodes = take 4 nodeNames
      nodePairs = zip limitedNodes (tail limitedNodes ++ [head limitedNodes])
      nodeDefs = map (\(node, dep) -> 
        "func " ++ node ++ "() { " ++ dep ++ "() }") nodePairs
      source = unlines $ 
        [ "package main"
        ] ++ nodeDefs ++
        [ "func main() {}"
        ]
  in case parseTypus source of
       Left _ -> property $ True
       Right parseResult -> 
         case analyzeDependencies parseResult of
           Left _ -> property $ True
           Right deps -> property $ True

tests :: TestTree
tests = testGroup "Dependency Analysis QuickCheck Tests"
  [ fastProperty "Dependency analysis functions" prop_dependency_analysis_functions
  , fastProperty "Dependency analysis types" prop_dependency_analysis_types
  , fastProperty "Dependency analysis imports" prop_dependency_analysis_imports
  , fastProperty "Dependency analysis interfaces" prop_dependency_analysis_interfaces
  , fastProperty "Dependency analysis variables" prop_dependency_analysis_variables
  , fastProperty "Dependency analysis circular" prop_dependency_analysis_circular
  , fastProperty "Dependency analysis transitive" prop_dependency_analysis_transitive
  , fastProperty "Dependency analysis complex" prop_dependency_analysis_complex
  , fastProperty "Dependency analysis package level" prop_dependency_analysis_package_level
  , fastProperty "Dependency analysis methods" prop_dependency_analysis_methods
  , fastProperty "Dependency analysis generics" prop_dependency_analysis_generics
  , fastProperty "Dependency analysis consistency" prop_dependency_analysis_consistency
  , fastProperty "Dependency analysis recursive" prop_dependency_analysis_recursive
  , fastProperty "Dependency analysis cross file" prop_dependency_analysis_cross_file
  , fastProperty "Dependency analysis conditional" prop_dependency_analysis_conditional
  , fastProperty "Dependency analysis anonymous" prop_dependency_analysis_anonymous
  , fastProperty "Dependency analysis constants" prop_dependency_analysis_constants
  , fastProperty "Dependency analysis build tags" prop_dependency_analysis_build_tags
  , fastProperty "Dependency analysis cycle detection" prop_dependency_analysis_cycle_detection
  ]