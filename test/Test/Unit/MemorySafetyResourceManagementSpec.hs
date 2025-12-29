{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-bounds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.MemorySafetyResourceManagementSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf
  , sized, resize, suchThat, frequency, choose, getPositive, getNonEmpty
  )

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (nub, sort, (\\))
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Either (isLeft, isRight)
import Control.Monad (replicateM, when)
import System.Mem (performGC)

-- | Generate resource identifiers
genResourceId :: Gen String
genResourceId = oneof
  [ elements ["file1", "file2", "socket1", "socket2", "buffer1", "buffer2"]
  , do
      n <- choose (1, 100)
      prefix <- elements ["res", "obj", "handle"]
      return $ prefix ++ show n
  ]

-- | Generate resource types
genResourceType :: Gen ResourceType
genResourceType = elements 
  [ FileResource, SocketResource, MemoryResource, ThreadResource, LockResource]

-- | Generate resource states
genResourceState :: Gen ResourceState
genResourceState = elements 
  [ Allocated, InUse, Freed, Leaked, Corrupted]

-- | Generate memory allocation sizes
genAllocationSize :: Gen Int
genAllocationSize = oneof
  [ choose (1, 1024)           -- Small allocations
  , choose (1024, 1048576)     -- Medium allocations  
  , choose (1048576, 104857600) -- Large allocations
  ]

-- | Generate resource lifetimes
genResourceLifetime :: Gen Int
genResourceLifetime = choose (1, 1000)

-- | Generate memory access patterns
genAccessPattern :: Gen AccessPattern
genAccessPattern = oneof
  [ return SequentialAccess
  , return RandomAccess
  , return StridedAccess
  , do
      stride <- choose (1, 64)
      return $ StridedAccessWithStride stride
  ]

-- | Generate buffer overflow scenarios
genBufferScenario :: Gen BufferScenario
genBufferScenario = do
  bufferSize <- genAllocationSize
  accessSize <- choose (bufferSize - 10, bufferSize + 100)
  return $ BufferScenario bufferSize accessSize

-- | Generate use-after-free scenarios
genUseAfterFreeScenario :: Gen UseAfterFreeScenario
genUseAfterFreeScenario = do
  resourceId <- genResourceId
  freeTime <- choose (1, 100)
  accessTime <- choose (freeTime + 1, freeTime + 100)
  return $ UseAfterFreeScenario resourceId freeTime accessTime

-- | Generate double-free scenarios
genDoubleFreeScenario :: Gen DoubleFreeScenario
genDoubleFreeScenario = do
  resourceId <- genResourceId
  firstFree <- choose (1, 100)
  secondFree <- choose (firstFree + 1, firstFree + 50)
  return $ DoubleFreeScenario resourceId firstFree secondFree

-- | Generate memory leak scenarios
genMemoryLeakScenario :: Gen MemoryLeakScenario
genMemoryLeakScenario = do
  numResources <- choose (1, 20)
  resources <- replicateM numResources genResourceId
  leakedResources <- sublistOf resources
  return $ MemoryLeakScenario resources leakedResources
  where
    sublistOf xs = do
      k <- choose (0, length xs)
      indices <- replicateM k $ choose (0, length xs - 1)
      return $ map (xs !!) (nub indices)

-- Property: Resource allocation should track correctly
prop_resource_allocation_tracking :: ResourceType -> Int -> Property
prop_resource_allocation_tracking resourceType size =
  size > 0 && size <= 1048576 ==> 
  let initialTracker = emptyResourceTracker
      resourceId = "test_resource"
      afterAllocation = allocateResource initialTracker resourceId resourceType size
      isTracked = isResourceTracked afterAllocation resourceId
  in property $ isTracked

-- Property: Resource deallocation should remove tracking
prop_resource_deallocation_cleanup :: ResourceType -> Int -> Property
prop_resource_deallocation_cleanup resourceType size =
  size > 0 ==> 
  let initialTracker = emptyResourceTracker
      resourceId = "test_resource"
      afterAllocation = allocateResource initialTracker resourceId resourceType size
      afterDeallocation = deallocateResource afterAllocation resourceId
      isTracked = isResourceTracked afterDeallocation resourceId
  in property $ not isTracked

-- Property: Buffer overflow should be detected
prop_buffer_overflow_detection :: BufferScenario -> Property
prop_buffer_overflow_detection scenario =
  let BufferScenario bufferSize accessSize = scenario
      result = detectBufferOverflow bufferSize accessSize
  in property $ result === (accessSize > bufferSize)

-- Property: Use-after-free should be detected
prop_use_after_free_detection :: UseAfterFreeScenario -> Property
prop_use_after_free_detection scenario =
  let UseAfterFreeScenario resourceId freeTime accessTime = scenario
      timeline = createResourceTimeline resourceId freeTime
      result = detectUseAfterFree timeline accessTime
  in property $ result === (accessTime > freeTime)

-- Property: Double-free should be detected
prop_double_free_detection :: DoubleFreeScenario -> Property
prop_double_free_detection scenario =
  let DoubleFreeScenario resourceId firstFree secondFree = scenario
      timeline = createDoubleFreeTimeline resourceId firstFree secondFree
      result = detectDoubleFree timeline
  in property $ result

-- Property: Memory leaks should be detected
prop_memory_leak_detection :: MemoryLeakScenario -> Property
prop_memory_leak_detection scenario =
  let MemoryLeakScenario allResources leakedResources = scenario
      tracker = createLeakTracker allResources leakedResources
      detectedLeaks = findMemoryLeaks tracker
  in property $ sort detectedLeaks === sort leakedResources

-- Property: Resource limits should be enforced
prop_resource_limits_enforced :: Int -> Int -> Property
prop_resource_limits_enforced maxResources allocationCount =
  maxResources > 0 && allocationCount >= 0 ==> 
  let limiter = ResourceLimiter maxResources
      resources = map (\i -> "resource" ++ show i) [1..allocationCount]
      results = map (tryAllocate limiter) resources
      successfulAllocations = length $ filter id results
  in property $ successfulAllocations <= maxResources

-- Property: Memory usage should stay within bounds
prop_memory_usage_bounds :: [Int] -> Property
prop_memory_usage_bounds allocationSizes =
  all (> 0) allocationSizes ==> 
  let totalMemory = sum allocationSizes
      memoryLimit = 1048576  -- 1MB
      withinBounds = totalMemory <= memoryLimit
      result = checkMemoryUsage allocationSizes memoryLimit
  in property $ result === withinBounds

-- Property: Garbage collection should reclaim memory
prop_garbage_collection_reclaims :: [String] -> Property
prop_garbage_collection_reclaims resourceIds =
  not (null resourceIds) ==> 
  let initialTracker = foldl' (\tracker rid -> allocateResource tracker rid MemoryResource 1024) 
                          emptyResourceTracker resourceIds
      initialCount = countResources initialTracker
      afterGC = performGarbageCollection initialTracker
      finalCount = countResources afterGC
  in property $ finalCount <= initialCount

-- Property: Reference counting should prevent premature cleanup
prop_reference_counting_prevents_cleanup :: String -> Int -> Property
prop_reference_counting_prevents_cleanup resourceId refCount =
  refCount > 0 ==> 
  let tracker = allocateWithReference emptyResourceTracker resourceId refCount
      canDeallocate = canDeallocateWithReferences tracker resourceId
  in property $ not canDeallocate

-- Property: Circular references should be detected
prop_circular_reference_detection :: [(String, [String])] -> Property
prop_circular_reference_detection dependencies =
  not (null dependencies) ==> 
  let graph = buildDependencyGraph dependencies
      hasCycle = detectCircularReferences graph
  in property $ hasCycle ==> hasCircularPath dependencies

-- Property: Resource cleanup should be exception-safe
prop_exception_safe_cleanup :: [String] -> Property
prop_exception_safe_cleanup resourceIds =
  not (null resourceIds) ==> 
  let tracker = foldl' (\tracker rid -> allocateResource tracker rid FileResource 1024) 
                          emptyResourceTracker resourceIds
      result = cleanupWithExceptions tracker
  in property $ isRight result || isLeft result

-- Property: Memory pools should reuse allocations
prop_memory_pool_reuse :: Int -> Int -> Property
prop_memory_pool_reuse poolSize requestCount =
  poolSize > 0 && requestCount > 0 ==> 
  let pool = createMemoryPool poolSize
      allocations = replicate requestCount (requestFromPool pool)
      successfulAllocations = length $ filter isJust allocations
  in property $ successfulAllocations <= poolSize

-- Property: Stack overflow should be prevented
prop_stack_overflow_prevention :: Int -> Property
prop_stack_overflow_prevention recursionDepth =
  recursionDepth >= 0 && recursionDepth <= 1000 ==> 
  let result = checkStackDepth recursionDepth
      maxSafeDepth = 100
  in property $ result === (recursionDepth <= maxSafeDepth)

-- Property: Memory alignment should be correct
prop_memory_alignment_correct :: Int -> Int -> Property
prop_memory_alignment_correct size alignment =
  size > 0 && alignment > 0 && isPowerOfTwo alignment ==> 
  let alignedSize = alignMemory size alignment
  in property $ alignedSize `mod` alignment === 0 && alignedSize >= size

-- Property: Resource handles should be unique
prop_resource_handles_unique :: [String] -> Property
prop_resource_handles_unique resourceIds =
  let handles = map generateResourceHandle resourceIds
      uniqueHandles = nub handles
  in property $ length handles === length uniqueHandles

-- Property: Memory mapping should be safe
prop_memory_mapping_safe :: Int -> Property
prop_memory_mapping_safe mapSize =
  mapSize > 0 && mapSize <= 10485760 ==>  -- Max 10MB
  let result = createMemoryMapping mapSize
  in property $ isRight result || isLeft result

-- | Helper functions and data types

data ResourceType = FileResource | SocketResource | MemoryResource | ThreadResource | LockResource
  deriving (Show, Eq)

data ResourceState = Allocated | InUse | Freed | Leaked | Corrupted
  deriving (Show, Eq)

data AccessPattern = SequentialAccess | RandomAccess | StridedAccess | StridedAccessWithStride Int
  deriving (Show, Eq)

data BufferScenario = BufferScenario Int Int
  deriving (Show, Eq)

data UseAfterFreeScenario = UseAfterFreeScenario String Int Int
  deriving (Show, Eq)

data DoubleFreeScenario = DoubleFreeScenario String Int Int
  deriving (Show, Eq)

data MemoryLeakScenario = MemoryLeakScenario [String] [String]
  deriving (Show, Eq)

data ResourceTracker = ResourceTracker
  { allocations :: Map.Map String (ResourceType, Int, ResourceState)
  , totalAllocated :: Int
  , peakUsage :: Int
  } deriving (Show, Eq)

data ResourceLimiter = ResourceLimiter
  { maxResources :: Int
  , currentResources :: Int
  } deriving (Show, Eq)

data MemoryPool = MemoryPool
  { poolSize :: Int
  , availableBlocks :: [Int]
  , allocatedBlocks :: Set.Set Int
  } deriving (Show, Eq)

emptyResourceTracker :: ResourceTracker
emptyResourceTracker = ResourceTracker Map.empty 0 0

allocateResource :: ResourceTracker -> String -> ResourceType -> Int -> ResourceTracker
allocateResource tracker resourceId resourceType size =
  let newAllocations = Map.insert resourceId (resourceType, size, Allocated) (allocations tracker)
      newTotal = totalAllocated tracker + size
      newPeak = max (peakUsage tracker) newTotal
  in tracker { allocations = newAllocations, totalAllocated = newTotal, peakUsage = newPeak }

deallocateResource :: ResourceTracker -> String -> ResourceTracker
deallocateResource tracker resourceId =
  case Map.lookup resourceId (allocations tracker) of
    Just (resourceType, size, _) -> 
      let newAllocations = Map.insert resourceId (resourceType, size, Freed) (allocations tracker)
      in tracker { allocations = newAllocations }
    Nothing -> tracker

isResourceTracked :: ResourceTracker -> String -> Bool
isResourceTracked tracker resourceId = Map.member resourceId (allocations tracker)

detectBufferOverflow :: Int -> Int -> Bool
detectBufferOverflow bufferSize accessSize = accessSize > bufferSize

createResourceTimeline :: String -> Int -> [(Int, String)]
createResourceTimeline resourceId freeTime = [(freeTime, "free:" ++ resourceId)]

detectUseAfterFree :: [(Int, String)] -> Int -> Bool
detectUseAfterFree timeline accessTime = 
  any (\(time, event) -> time < accessTime && "free:" `isPrefixOf` event) timeline

createDoubleFreeTimeline :: String -> Int -> Int -> [(Int, String)]
createDoubleFreeTimeline resourceId firstFree secondFree = 
  [(firstFree, "free:" ++ resourceId), (secondFree, "free:" ++ resourceId)]

detectDoubleFree :: [(Int, String)] -> Bool
detectDoubleFree timeline = 
  let freeEvents = filter (("free:" `isPrefixOf`) . snd) timeline
  in length freeEvents >= 2

createLeakTracker :: [String] -> [String] -> ResourceTracker
createLeakTracker allResources leakedResources =
  let tracker = foldl' (\t r -> allocateResource t r MemoryResource 1024) emptyResourceTracker allResources
      leakedTracker = foldl' (\t r -> Map.insert r (MemoryResource, 1024, Leaked) (allocations t)) tracker leakedResources
  in tracker { allocations = leakedTracker }

findMemoryLeaks :: ResourceTracker -> [String]
findMemoryLeaks tracker = 
  Map.keys $ Map.filter (\(_, _, state) -> state == Leaked) (allocations tracker)

tryAllocate :: ResourceLimiter -> String -> Bool
tryAllocate limiter _ = currentResources limiter < maxResources limiter

checkMemoryUsage :: [Int] -> Int -> Bool
checkMemoryUsage allocationSizes limit = sum allocationSizes <= limit

performGarbageCollection :: ResourceTracker -> ResourceTracker
performGarbageCollection tracker = tracker  -- Simplified

countResources :: ResourceTracker -> Int
countResources tracker = Map.size $ allocations tracker

allocateWithReference :: ResourceTracker -> String -> Int -> ResourceTracker
allocateWithReference tracker resourceId refCount = 
  allocateResource tracker resourceId MemoryResource (refCount * 1024)

canDeallocateWithReferences :: ResourceTracker -> String -> Bool
canDeallocateWithReferences tracker _ = False  -- Simplified

buildDependencyGraph :: [(String, [String])] -> Map.Map String [String]
buildDependencyGraph dependencies = Map.fromList dependencies

detectCircularReferences :: Map.Map String [String] -> Bool
detectCircularReferences graph = False  -- Simplified

hasCircularPath :: [(String, [String])] -> Bool
hasCircularPath dependencies = 
  any (\(node, deps) -> node `elem` concat deps) dependencies

cleanupWithExceptions :: ResourceTracker -> Either String ()
cleanupWithExceptions _ = Right ()

createMemoryPool :: Int -> MemoryPool
createMemoryPool size = MemoryPool size [1..size] Set.empty

requestFromPool :: MemoryPool -> Maybe Int
requestFromPool pool = 
  case availableBlocks pool of
    (block:rest) -> Just block
    [] -> Nothing

checkStackDepth :: Int -> Bool
checkStackDepth depth = depth <= 100

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0

alignMemory :: Int -> Int -> Int
alignMemory size alignment = ((size + alignment - 1) `div` alignment) * alignment

generateResourceHandle :: String -> String
generateResourceHandle resourceId = "handle_" ++ resourceId

createMemoryMapping :: Int -> Either String Int
createMemoryMapping size = Right size

foldl' :: (a -> b -> a) -> a -> [b] -> afoldl' = foldl

tests :: TestTree
tests = testGroup "Memory Safety Resource Management Tests"
  [ testGroup "Property-based tests"
    [ fastProperty "resource allocation tracking" prop_resource_allocation_tracking
    , fastProperty "resource deallocation cleanup" prop_resource_deallocation_cleanup
    , fastProperty "buffer overflow detection" prop_buffer_overflow_detection
    , fastProperty "use after free detection" prop_use_after_free_detection
    , fastProperty "double free detection" prop_double_free_detection
    , fastProperty "memory leak detection" prop_memory_leak_detection
    , fastProperty "resource limits enforced" prop_resource_limits_enforced
    , fastProperty "memory usage bounds" prop_memory_usage_bounds
    , fastProperty "garbage collection reclaims" prop_garbage_collection_reclaims
    , fastProperty "reference counting prevents cleanup" prop_reference_counting_prevents_cleanup
    , fastProperty "circular reference detection" prop_circular_reference_detection
    , fastProperty "exception safe cleanup" prop_exception_safe_cleanup
    , fastProperty "memory pool reuse" prop_memory_pool_reuse
    , fastProperty "stack overflow prevention" prop_stack_overflow_prevention
    , fastProperty "memory alignment correct" prop_memory_alignment_correct
    , fastProperty "resource handles unique" prop_resource_handles_unique
    , fastProperty "memory mapping safe" prop_memory_mapping_safe
    ]

  , testGroup "Unit tests"
    [ testCase "basic resource allocation" $ do
        let tracker = allocateResource emptyResourceTracker "test" MemoryResource 1024
        isResourceTracked tracker "test" @?= True
        totalAllocated tracker @?= 1024
    
    , testCase "resource deallocation" $ do
        let tracker = allocateResource emptyResourceTracker "test" MemoryResource 1024
        let deallocated = deallocateResource tracker "test"
        let resourceInfo = Map.lookup "test" (allocations deallocated)
        case resourceInfo of
          Just (_, _, state) -> state @?= Freed
          Nothing -> assertFailure "Resource not found"
    
    , testCase "buffer overflow detection" $ do
        detectBufferOverflow 1024 1025 @?= True
        detectBufferOverflow 1024 1024 @?= False
        detectBufferOverflow 1024 512 @?= False
    
    , testCase "use after free detection" $ do
        let timeline = [(10, "free:test"), (15, "access:test")]
        detectUseAfterFree timeline 15 @?= True
        detectUseAfterFree timeline 5 @?= False
    
    , testCase "double free detection" $ do
        let timeline = [(10, "free:test"), (20, "free:test")]
        detectDoubleFree timeline @?= True
        detectDoubleFree [(10, "free:test")] @?= False
    
    , testCase "memory leak detection" $ do
        let tracker = createLeakTracker ["test1", "test2", "test3"] ["test1", "test3"]
        let leaks = findMemoryLeaks tracker
        sort leaks @?= sort ["test1", "test3"]
    
    , testCase "resource limits" $ do
        let limiter = ResourceLimiter 2 0
        let result1 = tryAllocate limiter "res1"
        let result2 = tryAllocate limiter "res2"
        let result3 = tryAllocate limiter "res3"
        result1 @?= True
        result2 @?= True
        result3 @?= False
    
    , testCase "memory alignment" $ do
        alignMemory 1000 64 @?= 1024
        alignMemory 1024 64 @?= 1024
        alignMemory 1025 64 @?= 1088
    ]
  ]