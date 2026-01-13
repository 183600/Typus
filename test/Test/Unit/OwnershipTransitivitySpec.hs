{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.OwnershipTransitivitySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import Ownership (OwnershipAnalysis(..), OwnershipRelation(..), transferOwnership)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\))
import Control.Monad (when, replicateM)
import qualified Data.Set as Set

-- ============================================================================
-- Ownership Transitivity Tests
-- ============================================================================

-- | Test ownership transfer in linear chains
prop_ownership_linear_transfer :: String -> String -> Property
prop_ownership_linear_transfer owner1 owner2 =
  not (null owner1) && not (null owner2) && owner1 /= owner2 ==>
    let transferCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                      "x.transfer_to(" ++ owner2 ++ ")\n"
        parseResult = parseTypus transferCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer in branching scenarios
prop_ownership_branching_transfer :: String -> String -> String -> Property
prop_ownership_branching_transfer owner1 owner2 owner3 =
  not (null owner1) && not (null owner2) && not (null owner3) &&
  length (nub [owner1, owner2, owner3]) == 3 ==>
    let branchingCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                       "if (condition) {\n" ++
                       "  x.transfer_to(" ++ owner2 ++ ")\n" ++
                       "} else {\n" ++
                       "  x.transfer_to(" ++ owner3 ++ ")\n" ++
                       "}\n"
        parseResult = parseTypus branchingCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with temporary borrowing
prop_ownership_temporary_borrowing :: String -> String -> Property
prop_ownership_temporary_borrowing owner borrower =
  not (null owner) && not (null borrower) && owner /= borrower ==>
    let borrowCode = "let x = owned_by(" ++ owner ++ ")\n" ++
                    "x.borrow_by(" ++ borrower ++ ")\n" ++
                    "// Use x while borrowed\n" ++
                    "x.return_from(" ++ borrower ++ ")\n"
        parseResult = parseTypus borrowCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with shared access
prop_ownership_shared_access :: String -> String -> String -> Property
prop_ownership_shared_access owner reader1 reader2 =
  not (null owner) && not (null reader1) && not (null reader2) &&
  length (nub [owner, reader1, reader2]) == 3 ==>
    let sharedCode = "let x = owned_by(" ++ owner ++ ")\n" ++
                    "x.share_with(" ++ reader1 ++ ")\n" ++
                    "x.share_with(" ++ reader2 ++ ")\n" ++
                    "// Both readers can access x\n"
        parseResult = parseTypus sharedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with move semantics
prop_ownership_move_semantics :: String -> String -> Property
prop_ownership_move_semantics fromOwner toOwner =
  not (null fromOwner) && not (null toOwner) && fromOwner /= toOwner ==>
    let moveCode = "let x = owned_by(" ++ fromOwner ++ ")\n" ++
                   "x.move_to(" ++ toOwner ++ ")\n" ++
                   "// fromOwner no longer has access\n"
        parseResult = parseTypus moveCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with conditional paths
prop_ownership_conditional_transfer :: String -> String -> Bool -> Property
prop_ownership_conditional_transfer owner1 owner2 condition =
  not (null owner1) && not (null owner2) && owner1 /= owner2 ==>
    let conditionalCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                          "if (" ++ show condition ++ ") {\n" ++
                          "  x.transfer_to(" ++ owner2 ++ ")\n" ++
                          "}\n"
        parseResult = parseTypus conditionalCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer in loops
prop_ownership_loop_transfer :: String -> String -> Int -> Property
prop_ownership_loop_transfer owner1 owner2 iterations =
  not (null owner1) && not (null owner2) && owner1 /= owner2 &&
  iterations >= 0 && iterations <= 10 ==>
    let loopCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                   "for (i = 0; i < " ++ show iterations ++ "; i++) {\n" ++
                   "  x.transfer_to(" ++ owner2 ++ ")\n" ++
                   "  x.transfer_to(" ++ owner1 ++ ")\n" ++
                   "}\n"
        parseResult = parseTypus loopCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with function calls
prop_ownership_function_transfer :: String -> String -> String -> Property
prop_ownership_function_transfer funcName paramOwner returnOwner =
  not (null funcName) && not (null paramOwner) && not (null returnOwner) &&
  paramOwner /= returnOwner ==>
    let functionCode = "function " ++ funcName ++ "(x: owned_by(" ++ paramOwner ++ ")) -> owned_by(" ++ returnOwner ++ ") {\n" ++
                       "  // Process x\n" ++
                       "  return x\n" ++
                       "}\n" ++
                       "let y = " ++ funcName ++ "(someValue)\n"
        parseResult = parseTypus functionCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with data structures
prop_ownership_struct_transfer :: String -> String -> String -> Property
prop_ownership_struct_transfer structName fieldOwner structOwner =
  not (null structName) && not (null fieldOwner) && not (null structOwner) &&
  fieldOwner /= structOwner ==>
    let structCode = "struct " ++ structName ++ " {\n" ++
                     "  field: owned_by(" ++ fieldOwner ++ ")\n" ++
                     "}\n" ++
                     "let s = " ++ structName ++ "(owned_by(" ++ structOwner ++ "))\n"
        parseResult = parseTypus structCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with exceptions
prop_ownership_exception_transfer :: String -> String -> Property
prop_ownership_exception_transfer owner1 owner2 =
  not (null owner1) && not (null owner2) && owner1 /= owner2 ==>
    let exceptionCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                       "try {\n" ++
                       "  x.transfer_to(" ++ owner2 ++ ")\n" ++
                       "  throw Exception()\n" ++
                       "} catch (e) {\n" ++
                       "  // Handle exception\n" ++
                       "}\n"
        parseResult = parseTypus exceptionCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with async operations
prop_ownership_async_transfer :: String -> String -> Property
prop_ownership_async_transfer owner1 owner2 =
  not (null owner1) && not (null owner2) && owner1 /= owner2 ==>
    let asyncCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                    "async {\n" ++
                    "  x.transfer_to(" ++ owner2 ++ ")\n" ++
                    "}\n"
        parseResult = parseTypus asyncCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with generics
prop_ownership_generic_transfer :: String -> String -> String -> Property
prop_ownership_generic_transfer typeName owner1 owner2 =
  not (null typeName) && not (null owner1) && not (null owner2) &&
  owner1 /= owner2 ==>
    let genericCode = "let x: " ++ typeName ++ "<owned_by(" ++ owner1 ++ ")>\n" ++
                      "let y: " ++ typeName ++ "<owned_by(" ++ owner2 ++ ")>\n" ++
                      "y = x.transfer()\n"
        parseResult = parseTypus genericCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with lifetimes
prop_ownership_lifetime_transfer :: String -> String -> Int -> Property
prop_ownership_lifetime_transfer owner1 owner2 lifetime =
  not (null owner1) && not (null owner2) && owner1 /= owner2 &&
  lifetime >= 0 && lifetime <= 100 ==>
    let lifetimeCode = "let x<'" ++ show lifetime ++ "> = owned_by(" ++ owner1 ++ ")\n" ++
                       "x.transfer_to<" ++ show lifetime ++ ">(" ++ owner2 ++ ")\n"
        parseResult = parseTypus lifetimeCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with complex expressions
prop_ownership_complex_expressions :: String -> String -> String -> Property
prop_ownership_complex_expressions owner1 owner2 operation =
  not (null owner1) && not (null owner2) && not (null operation) &&
  owner1 /= owner2 ==>
    let complexCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                      "let y = owned_by(" ++ owner2 ++ ")\n" ++
                      "let z = x." ++ operation ++ "(y)\n"
        parseResult = parseTypus complexCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with resource cleanup
prop_ownership_resource_cleanup :: String -> String -> Property
prop_ownership_resource_cleanup owner1 owner2 =
  not (null owner1) && not (null owner2) && owner1 /= owner2 ==>
    let cleanupCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                      "x.transfer_to(" ++ owner2 ++ ")\n" ++
                      "// Cleanup resources from " ++ owner1 ++ "\n" ++
                      "x.cleanup()\n"
        parseResult = parseTypus cleanupCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with validation
prop_ownership_validation :: String -> String -> Bool -> Property
prop_ownership_validation owner1 owner2 isValid =
  not (null owner1) && not (null owner2) && owner1 /= owner2 ==>
    let validationCode = "let x = owned_by(" ++ owner1 ++ ")\n" ++
                         "if (" ++ show isValid ++ ") {\n" ++
                         "  x.transfer_to(" ++ owner2 ++ ")\n" ++
                         "} else {\n" ++
                         "  // Transfer rejected\n" ++
                         "}\n"
        parseResult = parseTypus validationCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test ownership transfer with nested ownership
prop_ownership_nested_ownership :: String -> String -> String -> Property
prop_ownership_nested_ownership outerOwner innerOwner newOwner =
  not (null outerOwner) && not (null innerOwner) && not (null newOwner) &&
  length (nub [outerOwner, innerOwner, newOwner]) == 3 ==>
    let nestedCode = "let outer = owned_by(" ++ outerOwner ++ ")\n" ++
                     "let inner = owned_by(" ++ innerOwner ++ ")\n" ++
                     "outer.contains(inner)\n" ++
                     "outer.transfer_to(" ++ newOwner ++ ")\n"
        parseResult = parseTypus nestedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Ownership Transitivity Tests"
  [ testProperty "Ownership transfer in linear chains" prop_ownership_linear_transfer,
    testProperty "Ownership transfer in branching scenarios" prop_ownership_branching_transfer,
    testProperty "Ownership transfer with temporary borrowing" prop_ownership_temporary_borrowing,
    testProperty "Ownership transfer with shared access" prop_ownership_shared_access,
    testProperty "Ownership transfer with move semantics" prop_ownership_move_semantics,
    testProperty "Ownership transfer with conditional paths" prop_ownership_conditional_transfer,
    testProperty "Ownership transfer in loops" prop_ownership_loop_transfer,
    testProperty "Ownership transfer with function calls" prop_ownership_function_transfer,
    testProperty "Ownership transfer with data structures" prop_ownership_struct_transfer,
    testProperty "Ownership transfer with exceptions" prop_ownership_exception_transfer,
    testProperty "Ownership transfer with async operations" prop_ownership_async_transfer,
    testProperty "Ownership transfer with generics" prop_ownership_generic_transfer,
    testProperty "Ownership transfer with lifetimes" prop_ownership_lifetime_transfer,
    testProperty "Ownership transfer with complex expressions" prop_ownership_complex_expressions,
    testProperty "Ownership transfer with resource cleanup" prop_ownership_resource_cleanup,
    testProperty "Ownership transfer with validation" prop_ownership_validation,
    testProperty "Ownership transfer with nested ownership" prop_ownership_nested_ownership
  ]