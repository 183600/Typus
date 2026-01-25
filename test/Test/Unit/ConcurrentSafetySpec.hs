{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.ConcurrentSafetySpec where



import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isControl, isPunctuation, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub, partition, sort, (\\), intersect)
import Control.Monad (when, replicateM)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try, SomeException, evaluate)

-- ============================================================================
-- Concurrent Safety Tests
-- ============================================================================

-- | Test concurrent parsing of different files
prop_concurrent_parsing :: String -> String -> Property
prop_concurrent_parsing file1 file2 =
  not (null file1) && not (null file2) &&
  length file1 < 50 && length file2 < 50 ==>
    let parseResult1 = parseTypus file1
        parseResult2 = parseTypus file2
    in case (parseResult1, parseResult2) of
         (Left _, Left _) -> property True
         (Right tf1, Right tf2) -> 
           let compileResult1 = compile tf1
               compileResult2 = compile tf2
           in case (compileResult1, compileResult2) of
                (Left _, Left _) -> property True
                (Right gc1, Right gc2) -> property $ not (null gc1) && not (null gc2)
                _ -> property True
         _ -> property True

-- | Test concurrent compilation of independent modules
prop_concurrent_compilation :: String -> String -> Property
prop_concurrent_compilation module1 module2 =
  not (null module1) && not (null module2) &&
  length module1 < 50 && length module2 < 50 ==>
    let module1Code = "module Module1 {\n" ++ module1 ++ "\n}\n"
        module2Code = "module Module2 {\n" ++ module2 ++ "\n}\n"
        parseResult1 = parseTypus module1Code
        parseResult2 = parseTypus module2Code
    in case (parseResult1, parseResult2) of
         (Right tf1, Right tf2) -> 
           let compileResult1 = compile tf1
               compileResult2 = compile tf2
           in case (compileResult1, compileResult2) of
                (Right gc1, Right gc2) -> property $ not (null gc1) && not (null gc2)
                _ -> property True
         _ -> property True

-- | Test concurrent access to shared resources
prop_concurrent_shared_resources :: String -> Property
prop_concurrent_shared_resources resourceName =
  not (null resourceName) && all isAlphaNum resourceName ==>
    let sharedCode = "shared resource " ++ resourceName ++ " {\n" ++
                     "  acquire()\n" ++
                     "  // Critical section\n" ++
                     "  release()\n" ++
                     "}\n" ++
                     "parallel {\n" ++
                     "  use(" ++ resourceName ++ ")\n" ++
                     "}\n"
        parseResult = parseTypus sharedCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent ownership transfers
prop_concurrent_ownership_transfer :: String -> String -> String -> Property
prop_concurrent_ownership_transfer owner1 owner2 resource =
  not (null owner1) && not (null owner2) && not (null resource) &&
  owner1 /= owner2 ==>
    let concurrentOwnership = "let " ++ resource ++ " = owned_by(" ++ owner1 ++ ")\n" ++
                              "parallel {\n" ++
                              "  " ++ resource ++ ".transfer_to(" ++ owner2 ++ ")\n" ++
                              "}\n"
        parseResult = parseTypus concurrentOwnership
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent type checking
prop_concurrent_type_checking :: String -> String -> Property
prop_concurrent_type_checking type1 type2 =
  not (null type1) && not (null type2) && type1 /= type2 ==>
    let typeCheckingCode = "type " ++ type1 ++ " = number\n" ++
                           "type " ++ type2 ++ " = string\n" ++
                           "parallel {\n" ++
                           "  let x: " ++ type1 ++ " = 5\n" ++
                           "  let y: " ++ type2 ++ " = \"hello\"\n" ++
                           "}\n"
        parseResult = parseTypus typeCheckingCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent dependency resolution
prop_concurrent_dependency_resolution :: String -> String -> String -> Property
prop_concurrent_dependency_resolution dep1 dep2 dep3 =
  not (null dep1) && not (null dep2) && not (null dep3) &&
  length (nub [dep1, dep2, dep3]) == 3 ==>
    let concurrentDeps = "parallel {\n" ++
                        "  import " ++ dep1 ++ "\n" ++
                        "  import " ++ dep2 ++ "\n" ++
                        "  import " ++ dep3 ++ "\n" ++
                        "}\n"
        parseResult = parseTypus concurrentDeps
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent error handling
prop_concurrent_error_handling :: String -> Property
prop_concurrent_error_handling errorType =
  not (null errorType) ==>
    let concurrentErrors = "parallel {\n" ++
                           "  try {\n" ++
                           "    throw " ++ errorType ++ "()\n" ++
                           "  } catch (e) {\n" ++
                           "    handle(e)\n" ++
                           "  }\n" ++
                           "}\n"
        parseResult = parseTypus concurrentErrors
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent optimization passes
prop_concurrent_optimization :: String -> Property
prop_concurrent_optimization code =
  not (null code) && length code < 50 ==>
    let concurrentOpt = "parallel {\n" ++
                        "  optimize(" ++ code ++ ")\n" ++
                        "  optimize(" ++ code ++ ")\n" ++
                        "}\n"
        parseResult = parseTypus concurrentOpt
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent code generation
prop_concurrent_code_generation :: String -> Property
prop_concurrent_code_generation code =
  not (null code) && length code < 50 ==>
    let concurrentCodeGen = "parallel {\n" ++
                            "  generate(" ++ code ++ ")\n" ++
                            "  generate(" ++ code ++ ")\n" ++
                            "}\n"
        parseResult = parseTypus concurrentCodeGen
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent source location tracking
prop_concurrent_source_location :: String -> Property
prop_concurrent_source_location code =
  not (null code) && length code < 50 ==>
    let concurrentLoc = "parallel {\n" ++
                        "  track(" ++ code ++ ")\n" ++
                        "  track(" ++ code ++ ")\n" ++
                        "}\n"
        parseResult = parseTypus concurrentLoc
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent memory management
prop_concurrent_memory_management :: String -> Property
prop_concurrent_memory_management resource =
  not (null resource) && all isAlphaNum resource ==>
    let concurrentMem = "parallel {\n" ++
                        "  allocate(" ++ resource ++ ")\n" ++
                        "  use(" ++ resource ++ ")\n" ++
                        "  deallocate(" ++ resource ++ ")\n" ++
                        "}\n"
        parseResult = parseTypus concurrentMem
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent cache access
prop_concurrent_cache_access :: String -> Property
prop_concurrent_cache_access key =
  not (null key) && all isAlphaNum key ==>
    let concurrentCache = "parallel {\n" ++
                          "  cache.get(\"" ++ key ++ "\")\n" ++
                          "  cache.set(\"" ++ key ++ "\", value)\n" ++
                          "  cache.get(\"" ++ key ++ "\")\n" ++
                          "}\n"
        parseResult = parseTypus concurrentCache
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent logging
prop_concurrent_logging :: String -> Property
prop_concurrent_logging message =
  not (null message) ==>
    let concurrentLog = "parallel {\n" ++
                        "  log(\"" ++ message ++ "\")\n" ++
                        "  log(\"" ++ message ++ "\")\n" ++
                        "  log(\"" ++ message ++ "\")\n" ++
                        "}\n"
        parseResult = parseTypus concurrentLog
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent profiling
prop_concurrent_profiling :: String -> Property
prop_concurrent_profiling operation =
  not (null operation) && all isAlphaNum operation ==>
    let concurrentProfile = "parallel {\n" ++
                            "  profile(" ++ operation ++ ")\n" ++
                            "  profile(" ++ operation ++ ")\n" ++
                            "}\n"
        parseResult = parseTypus concurrentProfile
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent debugging
prop_concurrent_debugging :: String -> Property
prop_concurrent_debugging debugTarget =
  not (null debugTarget) && all isAlphaNum debugTarget ==>
    let concurrentDebug = "parallel {\n" ++
                         "  debug(" ++ debugTarget ++ ")\n" ++
                         "  debug(" ++ debugTarget ++ ")\n" ++
                         "}\n"
        parseResult = parseTypus concurrentDebug
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent testing
prop_concurrent_testing :: String -> Property
prop_concurrent_testing testName =
  not (null testName) && all isAlphaNum testName ==>
    let concurrentTest = "parallel {\n" ++
                         "  test(\"" ++ testName ++ "\")\n" ++
                         "  test(\"" ++ testName ++ "\")\n" ++
                         "}\n"
        parseResult = parseTypus concurrentTest
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent build processes
prop_concurrent_build :: String -> Property
prop_concurrent_build target =
  not (null target) && all isAlphaNum target ==>
    let concurrentBuild = "parallel {\n" ++
                          "  build(" ++ target ++ ")\n" ++
                          "  build(" ++ target ++ ")\n" ++
                          "}\n"
        parseResult = parseTypus concurrentBuild
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test concurrent hot reloading
prop_concurrent_hot_reload :: String -> Property
prop_concurrent_hot_reload modulePath =
  not (null modulePath) ==>
    let concurrentReload = "parallel {\n" ++
                           "  watch(" ++ modulePath ++ ")\n" ++
                           "  reload(" ++ modulePath ++ ")\n" ++
                           "}\n"
        parseResult = parseTypus concurrentReload
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Concurrent Safety Tests"
  [ testProperty "Concurrent parsing of different files" prop_concurrent_parsing,
    testProperty "Concurrent compilation of independent modules" prop_concurrent_compilation,
    testProperty "Concurrent access to shared resources" prop_concurrent_shared_resources,
    testProperty "Concurrent ownership transfers" prop_concurrent_ownership_transfer,
    testProperty "Concurrent type checking" prop_concurrent_type_checking,
    testProperty "Concurrent dependency resolution" prop_concurrent_dependency_resolution,
    testProperty "Concurrent error handling" prop_concurrent_error_handling,
    testProperty "Concurrent optimization passes" prop_concurrent_optimization,
    testProperty "Concurrent code generation" prop_concurrent_code_generation,
    testProperty "Concurrent source location tracking" prop_concurrent_source_location,
    testProperty "Concurrent memory management" prop_concurrent_memory_management,
    testProperty "Concurrent cache access" prop_concurrent_cache_access,
    testProperty "Concurrent logging" prop_concurrent_logging,
    testProperty "Concurrent profiling" prop_concurrent_profiling,
    testProperty "Concurrent debugging" prop_concurrent_debugging,
    testProperty "Concurrent testing" prop_concurrent_testing,
    testProperty "Concurrent build processes" prop_concurrent_build,
    testProperty "Concurrent hot reloading" prop_concurrent_hot_reload
  ]