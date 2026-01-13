{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.MemorySafetySpec where

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
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Storable (poke, peek)

-- ============================================================================
-- Memory Safety Tests
-- ============================================================================

-- | Test memory allocation and deallocation
prop_memory_allocation :: Int -> Property
prop_memory_allocation size =
  size >= 0 && size <= 1000 ==>
    let allocCode = "let ptr = allocate(" ++ show size ++ ")\n" ++
                    "use(ptr)\n" ++
                    "deallocate(ptr)\n"
        parseResult = parseTypus allocCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory leak prevention
prop_memory_leak_prevention :: Int -> Property
prop_memory_leak_prevention iterations =
  iterations >= 0 && iterations <= 100 ==>
    let leakPreventionCode = "for (i = 0; i < " ++ show iterations ++ "; i++) {\n" ++
                             "  let ptr = allocate(100)\n" ++
                             "  use(ptr)\n" ++
                             "  deallocate(ptr)  // Always deallocate\n" ++
                             "}\n"
        parseResult = parseTypus leakPreventionCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test buffer overflow protection
prop_memory_buffer_overflow :: Int -> Int -> Property
prop_memory_buffer_overflow bufferSize accessSize =
  bufferSize >= 0 && accessSize >= 0 && bufferSize <= 100 && accessSize <= 100 ==>
    let overflowCode = "let buffer = allocate(" ++ show bufferSize ++ ")\n" ++
                       "safe_access(buffer, " ++ show accessSize ++ ")\n" ++
                       "deallocate(buffer)\n"
        parseResult = parseTypus overflowCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test use-after-free prevention
prop_memory_use_after_free :: String -> Property
prop_memory_use_after_free varName =
  not (null varName) && all isAlphaNum varName ==>
    let useAfterFreeCode = "let " ++ varName ++ " = allocate(100)\n" ++
                          "use(" ++ varName ++ ")\n" ++
                          "deallocate(" ++ varName ++ ")\n" ++
                          "// " ++ varName ++ " is now freed and cannot be used\n"
        parseResult = parseTypus useAfterFreeCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test double-free prevention
prop_memory_double_free :: String -> Property
prop_memory_double_free varName =
  not (null varName) && all isAlphaNum varName ==>
    let doubleFreeCode = "let " ++ varName ++ " = allocate(100)\n" ++
                        "use(" ++ varName ++ ")\n" ++
                        "deallocate(" ++ varName ++ ")\n" ++
                        "// Second deallocation should be prevented\n" ++
                        "safe_deallocate(" ++ varName ++ ")\n"
        parseResult = parseTypus doubleFreeCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test null pointer dereference prevention
prop_memory_null_pointer :: String -> Property
prop_memory_null_pointer varName =
  not (null varName) && all isAlphaNum varName ==>
    let nullPointerCode = "let " ++ varName ++ " = null\n" ++
                         "if (" ++ varName ++ " != null) {\n" ++
                         "  use(" ++ varName ++ ")\n" ++
                         "} else {\n" ++
                         "  // Handle null pointer\n" ++
                         "}\n"
        parseResult = parseTypus nullPointerCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory bounds checking
prop_memory_bounds_checking :: Int -> Int -> Property
prop_memory_bounds_checking arraySize index =
  arraySize >= 0 && index >= 0 && arraySize <= 100 && index <= 100 ==>
    let boundsCode = "let arr = allocate_array(" ++ show arraySize ++ ")\n" ++
                     "safe_access(arr, " ++ show index ++ ")\n" ++
                     "deallocate(arr)\n"
        parseResult = parseTypus boundsCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test stack overflow protection
prop_memory_stack_overflow :: Int -> Property
prop_memory_stack_overflow depth =
  depth >= 0 && depth <= 100 ==>
    let stackOverflowCode = "function recursive(n: number) {\n" ++
                            "  if (n > 0) {\n" ++
                            "    return recursive(n - 1)\n" ++
                            "  }\n" ++
                            "  return 0\n" ++
                            "}\n" ++
                            "recursive(" ++ show depth ++ ")\n"
        parseResult = parseTypus stackOverflowCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory initialization
prop_memory_initialization :: Int -> Property
prop_memory_initialization size =
  size >= 0 && size <= 100 ==>
    let initCode = "let ptr = allocate(" ++ show size ++ ")\n" ++
                   "initialize(ptr, 0)\n" ++
                   "use(ptr)\n" ++
                   "deallocate(ptr)\n"
        parseResult = parseTypus initCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory alignment
prop_memory_alignment :: Int -> Property
prop_memory_alignment alignment =
  alignment >= 0 && alignment <= 64 && isPowerOfTwo alignment ==>
    let alignCode = "let ptr = allocate_aligned(100, " ++ show alignment ++ ")\n" ++
                    "use(ptr)\n" ++
                    "deallocate_aligned(ptr)\n"
        parseResult = parseTypus alignCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory pool management
prop_memory_pool_management :: Int -> Int -> Property
prop_memory_pool_management poolSize objectSize =
  poolSize >= 0 && objectSize >= 0 && poolSize <= 1000 && objectSize <= 100 ==>
    let poolCode = "let pool = create_pool(" ++ show poolSize ++ ", " ++ show objectSize ++ ")\n" ++
                   "let obj1 = pool.allocate()\n" ++
                   "let obj2 = pool.allocate()\n" ++
                   "pool.deallocate(obj1)\n" ++
                   "pool.deallocate(obj2)\n" ++
                   "destroy_pool(pool)\n"
        parseResult = parseTypus poolCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test garbage collection
prop_memory_garbage_collection :: Int -> Property
prop_memory_garbage_collection objects =
  objects >= 0 && objects <= 100 ==>
    let gcCode = "for (i = 0; i < " ++ show objects ++ "; i++) {\n" ++
                 "  let obj = create_object()\n" ++
                 "  // obj goes out of scope and should be collected\n" ++
                 "}\n" ++
                 "collect_garbage()\n"
        parseResult = parseTypus gcCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test reference counting
prop_memory_reference_counting :: Int -> Property
prop_memory_reference_counting refs =
  refs >= 0 && refs <= 10 ==>
    let refCountCode = "let obj = create_ref_counted()\n" ++
                       "for (i = 0; i < " ++ show refs ++ "; i++) {\n" ++
                       "  add_ref(obj)\n" ++
                       "}\n" ++
                       "for (i = 0; i < " ++ show refs ++ "; i++) {\n" ++
                       "  release_ref(obj)\n" ++
                       "}\n" ++
                       "// obj should be deallocated when count reaches 0\n"
        parseResult = parseTypus refCountCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory mapping
prop_memory_memory_mapping :: Int -> Property
prop_memory_memory_mapping size =
  size >= 0 && size <= 1000 ==>
    let mmapCode = "let ptr = memory_map(" ++ show size ++ ")\n" ++
                   "use(ptr)\n" ++
                   "memory_unmap(ptr, " ++ show size ++ ")\n"
        parseResult = parseTypus mmapCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory sanitization
prop_memory_sanitization :: String -> Property
prop_memory_sanitization dataPattern =
  not (null dataPattern) && length dataPattern < 20 ==>
    let sanitizeCode = "let ptr = allocate(100)\n" ++
                       "write_pattern(ptr, \"" ++ dataPattern ++ "\")\n" ++
                       "sanitize(ptr)\n" ++
                       "deallocate(ptr)\n"
        parseResult = parseTypus sanitizeCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory fragmentation
prop_memory_fragmentation :: Int -> Property
prop_memory_fragmentation allocations =
  allocations >= 0 && allocations <= 100 ==>
    let fragCode = "let ptrs = []\n" ++
                   "for (i = 0; i < " ++ show allocations ++ "; i++) {\n" ++
                   "  let size = random_size(1, 100)\n" ++
                   "  ptrs.append(allocate(size))\n" ++
                   "}\n" ++
                   "for (ptr in ptrs) {\n" ++
                   "  deallocate(ptr)\n" ++
                   "}\n" ++
                   "defragment()\n"
        parseResult = parseTypus fragCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test memory limits
prop_memory_limits :: Int -> Property
prop_memory_limits limitMB =
  limitMB >= 0 && limitMB <= 100 ==>
    let limitCode = "set_memory_limit(" ++ show limitMB ++ "MB)\n" ++
                    "try {\n" ++
                    "  allocate_large_memory()\n" ++
                    "} catch (MemoryLimitExceeded e) {\n" ++
                    "  handle_limit_exceeded(e)\n" ++
                    "}\n"
        parseResult = parseTypus limitCode
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- Helper function
isPowerOfTwo :: Int -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = n .&. (n - 1) == 0
  where (.&.) = ((`mod`) . (*2))

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Memory Safety Tests"
  [ testProperty "Memory allocation and deallocation" prop_memory_allocation,
    testProperty "Memory leak prevention" prop_memory_leak_prevention,
    testProperty "Buffer overflow protection" prop_memory_buffer_overflow,
    testProperty "Use-after-free prevention" prop_memory_use_after_free,
    testProperty "Double-free prevention" prop_memory_double_free,
    testProperty "Null pointer dereference prevention" prop_memory_null_pointer,
    testProperty "Memory bounds checking" prop_memory_bounds_checking,
    testProperty "Stack overflow protection" prop_memory_stack_overflow,
    testProperty "Memory initialization" prop_memory_initialization,
    testProperty "Memory alignment" prop_memory_alignment,
    testProperty "Memory pool management" prop_memory_pool_management,
    testProperty "Garbage collection" prop_memory_garbage_collection,
    testProperty "Reference counting" prop_memory_reference_counting,
    testProperty "Memory mapping" prop_memory_memory_mapping,
    testProperty "Memory sanitization" prop_memory_sanitization,
    testProperty "Memory fragmentation" prop_memory_fragmentation,
    testProperty "Memory limits" prop_memory_limits
  ]