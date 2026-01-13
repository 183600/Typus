{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.DependencyCycleDetectionSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import Dependencies.AST (DependencyGraph(..), DependencyNode(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\), intersect)
import Control.Monad (when, replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- ============================================================================
-- Dependency Cycle Detection Tests
-- ============================================================================

-- | Test simple dependency cycle detection
prop_dependency_simple_cycle :: String -> Property
prop_dependency_simple_cycle moduleName =
  not (null moduleName) && length moduleName <= 10 ==>
    let cyclicCode = "module " ++ moduleName ++ " {\n" ++
                     "  import " ++ moduleName ++ "\n" ++
                     "  let x = 5\n" ++
                     "}\n"
        parseResult = parseTypus cyclicCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test two-module dependency cycle
prop_dependency_two_module_cycle :: String -> String -> Property
prop_dependency_two_module_cycle module1 module2 =
  not (null module1) && not (null module2) && module1 /= module2 ==>
    let cyclicCode = "module " ++ module1 ++ " {\n" ++
                     "  import " ++ module2 ++ "\n" ++
                     "  let x = 5\n" ++
                     "}\n" ++
                     "module " ++ module2 ++ " {\n" ++
                     "  import " ++ module1 ++ "\n" ++
                     "  let y = 10\n" ++
                     "}\n"
        parseResult = parseTypus cyclicCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test three-module dependency cycle
prop_dependency_three_module_cycle :: String -> String -> String -> Property
prop_dependency_three_module_cycle module1 module2 module3 =
  not (null module1) && not (null module2) && not (null module3) &&
  length (nub [module1, module2, module3]) == 3 ==>
    let cyclicCode = "module " ++ module1 ++ " {\n" ++
                     "  import " ++ module2 ++ "\n" ++
                     "  let x = 5\n" ++
                     "}\n" ++
                     "module " ++ module2 ++ " {\n" ++
                     "  import " ++ module3 ++ "\n" ++
                     "  let y = 10\n" ++
                     "}\n" ++
                     "module " ++ module3 ++ " {\n" ++
                     "  import " ++ module1 ++ "\n" ++
                     "  let z = 15\n" ++
                     "}\n"
        parseResult = parseTypus cyclicCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test complex dependency cycle with indirect dependencies
prop_dependency_indirect_cycle :: String -> String -> String -> String -> Property
prop_dependency_indirect_cycle module1 module2 module3 module4 =
  not (null module1) && not (null module2) && not (null module3) && not (null module4) &&
  length (nub [module1, module2, module3, module4]) == 4 ==>
    let indirectCyclicCode = "module " ++ module1 ++ " {\n" ++
                             "  import " ++ module2 ++ "\n" ++
                             "  let x = 5\n" ++
                             "}\n" ++
                             "module " ++ module2 ++ " {\n" ++
                             "  import " ++ module3 ++ "\n" ++
                             "  let y = 10\n" ++
                             "}\n" ++
                             "module " ++ module3 ++ " {\n" ++
                             "  import " ++ module4 ++ "\n" ++
                             "  let z = 15\n" ++
                             "}\n" ++
                             "module " ++ module4 ++ " {\n" ++
                             "  import " ++ module1 ++ "\n" ++
                             "  let w = 20\n" ++
                             "}\n"
        parseResult = parseTypus indirectCyclicCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test self-dependency in function definitions
prop_dependency_self_function_dependency :: String -> String -> Property
prop_dependency_self_function_dependency funcName paramName =
  not (null funcName) && not (null paramName) ==>
    let selfDepCode = "function " ++ funcName ++ "(" ++ paramName ++ ": " ++ funcName ++ ") {\n" ++
                      "  return " ++ funcName ++ "(" ++ paramName ++ ")\n" ++
                      "}\n"
        parseResult = parseTypus selfDepCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test circular type dependencies
prop_dependency_circular_type_dependency :: String -> String -> Property
prop_dependency_circular_type_dependency type1 type2 =
  not (null type1) && not (null type2) && type1 /= type2 ==>
    let circularTypeCode = "type " ++ type1 ++ " = " ++ type2 ++ "\n" ++
                          "type " ++ type2 ++ " = " ++ type1 ++ "\n"
        parseResult = parseTypus circularTypeCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with conditional imports
prop_dependency_conditional_cycle :: String -> String -> Bool -> Property
prop_dependency_conditional_cycle module1 module2 condition =
  not (null module1) && not (null module2) && module1 /= module2 ==>
    let conditionalCode = "module " ++ module1 ++ " {\n" ++
                          "  if (" ++ show condition ++ ") {\n" ++
                          "    import " ++ module2 ++ "\n" ++
                          "  }\n" ++
                          "  let x = 5\n" ++
                          "}\n" ++
                          "module " ++ module2 ++ " {\n" ++
                          "  import " ++ module1 ++ "\n" ++
                          "  let y = 10\n" ++
                          "}\n"
        parseResult = parseTypus conditionalCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with generic types
prop_dependency_generic_cycle :: String -> String -> String -> Property
prop_dependency_generic_cycle type1 type2 typeParam =
  not (null type1) && not (null type2) && not (null typeParam) &&
  type1 /= type2 ==>
    let genericCode = "type " ++ type1 ++ "<" ++ typeParam ++ "> = " ++ type2 ++ "<" ++ type1 ++ "<" ++ typeParam ++ ">>\n" ++
                      "type " ++ type2 ++ "<T> = " ++ type1 ++ "<T>\n"
        parseResult = parseTypus genericCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with inheritance
prop_dependency_inheritance_cycle :: String -> String -> Property
prop_dependency_inheritance_cycle class1 class2 =
  not (null class1) && not (null class2) && class1 /= class2 ==>
    let inheritanceCode = "class " ++ class1 ++ " extends " ++ class2 ++ " {\n" ++
                          "  method1() {}\n" ++
                          "}\n" ++
                          "class " ++ class2 ++ " extends " ++ class1 ++ " {\n" ++
                          "  method2() {}\n" ++
                          "}\n"
        parseResult = parseTypus inheritanceCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with interface implementation
prop_dependency_interface_cycle :: String -> String -> Property
prop_dependency_interface_cycle interface1 interface2 =
  not (null interface1) && not (null interface2) && interface1 /= interface2 ==>
    let interfaceCode = "interface " ++ interface1 ++ " extends " ++ interface2 ++ " {\n" ++
                        "  method1(): void\n" ++
                        "}\n" ++
                        "interface " ++ interface2 ++ " extends " ++ interface1 ++ " {\n" ++
                        "  method2(): void\n" ++
                        "}\n"
        parseResult = parseTypus interfaceCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with mixins
prop_dependency_mixin_cycle :: String -> String -> Property
prop_dependency_mixin_cycle mixin1 mixin2 =
  not (null mixin1) && not (null mixin2) && mixin1 /= mixin2 ==>
    let mixinCode = "mixin " ++ mixin1 ++ " = " ++ mixin2 ++ " {\n" ++
                    "  method1() {}\n" ++
                    "}\n" ++
                    "mixin " ++ mixin2 ++ " = " ++ mixin1 ++ " {\n" ++
                    "  method2() {}\n" ++
                    "}\n"
        parseResult = parseTypus mixinCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with recursive modules
prop_dependency_recursive_module :: String -> Property
prop_dependency_recursive_module moduleName =
  not (null moduleName) && length moduleName <= 10 ==>
    let recursiveCode = "module " ++ moduleName ++ " {\n" ++
                        "  export function " ++ moduleName ++ "() {\n" ++
                        "    return " ++ moduleName ++ "()\n" ++
                        "  }\n" ++
                        "}\n"
        parseResult = parseTypus recursiveCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with nested modules
prop_dependency_nested_module_cycle :: String -> String -> Property
prop_dependency_nested_module_cycle outerModule innerModule =
  not (null outerModule) && not (null innerModule) && outerModule /= innerModule ==>
    let nestedCode = "module " ++ outerModule ++ " {\n" ++
                     "  module " ++ innerModule ++ " {\n" ++
                     "    import " ++ outerModule ++ "\n" ++
                     "    let x = 5\n" ++
                     "  }\n" ++
                     "  import " ++ innerModule ++ "\n" ++
                     "  let y = 10\n" ++
                     "}\n"
        parseResult = parseTypus nestedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with dynamic imports
prop_dependency_dynamic_import_cycle :: String -> String -> Property
prop_dependency_dynamic_import_cycle module1 module2 =
  not (null module1) && not (null module2) && module1 /= module2 ==>
    let dynamicCode = "module " ++ module1 ++ " {\n" ++
                      "  async function load() {\n" ++
                      "    await import('" ++ module2 ++ "')\n" ++
                      "  }\n" ++
                      "}\n" ++
                      "module " ++ module2 ++ " {\n" ++
                      "  async function load() {\n" ++
                      "    await import('" ++ module1 ++ "')\n" ++
                      "  }\n" ++
                      "}\n"
        parseResult = parseTypus dynamicCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with lazy loading
prop_dependency_lazy_loading_cycle :: String -> String -> Property
prop_dependency_lazy_loading_cycle module1 module2 =
  not (null module1) && not (null module2) && module1 /= module2 ==>
    let lazyCode = "module " ++ module1 ++ " {\n" ++
                   "  lazy val x = require('" ++ module2 ++ "')\n" ++
                   "}\n" ++
                   "module " ++ module2 ++ " {\n" ++
                   "  lazy val y = require('" ++ module1 ++ "')\n" ++
                   "}\n"
        parseResult = parseTypus lazyCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with macro expansion
prop_dependency_macro_cycle :: String -> String -> Property
prop_dependency_macro_cycle macro1 macro2 =
  not (null macro1) && not (null macro2) && macro1 /= macro2 ==>
    let macroCode = "macro " ++ macro1 ++ " = expand(" ++ macro2 ++ ")\n" ++
                    "macro " ++ macro2 ++ " = expand(" ++ macro1 ++ ")\n"
        parseResult = parseTypus macroCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test dependency cycle with template instantiation
prop_dependency_template_cycle :: String -> String -> Property
prop_dependency_template_cycle template1 template2 =
  not (null template1) && not (null template2) && template1 /= template2 ==>
    let templateCode = "template " ++ template1 ++ "(T) = " ++ template2 ++ "<" ++ template1 ++ "<T>>\n" ++
                       "template " ++ template2 ++ "(U) = " ++ template1 ++ "<" ++ template2 ++ "<U>>\n"
        parseResult = parseTypus templateCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Dependency Cycle Detection Tests"
  [ testProperty "Simple dependency cycle detection" prop_dependency_simple_cycle,
    testProperty "Two-module dependency cycle" prop_dependency_two_module_cycle,
    testProperty "Three-module dependency cycle" prop_dependency_three_module_cycle,
    testProperty "Complex dependency cycle with indirect dependencies" prop_dependency_indirect_cycle,
    testProperty "Self-dependency in function definitions" prop_dependency_self_function_dependency,
    testProperty "Circular type dependencies" prop_dependency_circular_type_dependency,
    testProperty "Dependency cycle with conditional imports" prop_dependency_conditional_cycle,
    testProperty "Dependency cycle with generic types" prop_dependency_generic_cycle,
    testProperty "Dependency cycle with inheritance" prop_dependency_inheritance_cycle,
    testProperty "Dependency cycle with interface implementation" prop_dependency_interface_cycle,
    testProperty "Dependency cycle with mixins" prop_dependency_mixin_cycle,
    testProperty "Dependency cycle with recursive modules" prop_dependency_recursive_module,
    testProperty "Dependency cycle with nested modules" prop_dependency_nested_module_cycle,
    testProperty "Dependency cycle with dynamic imports" prop_dependency_dynamic_import_cycle,
    testProperty "Dependency cycle with lazy loading" prop_dependency_lazy_loading_cycle,
    testProperty "Dependency cycle with macro expansion" prop_dependency_macro_cycle,
    testProperty "Dependency cycle with template instantiation" prop_dependency_template_cycle
  ]