-- Memory Optimization Template for QuickCheck Tests
-- Add these imports and configurations to any QuickCheck test file to reduce memory usage

-- 1. Add these imports after your existing imports:
import TestSupport.UniversalMemoryOptimization 
  ( optimizeTestSuite
  , optimizeProperty
  , emergencyLevel
  , lowLevel
  , moderateLevel
  , genSafeString
  , genSafeList
  , genSafeInt
  , limitString
  , limitList
  , limitInt
  , quickOptimize
  , autoLimitStringProperty
  , autoLimitListProperty
  , autoLimitIntProperty
  )

-- 2. Replace your test properties with memory-limited versions:
-- Instead of: prop_my_test :: String -> Property
-- Use:       prop_my_test :: String -> Property
--            prop_my_test s = let limited = limitString 5 s in ...your property logic...

-- 3. Replace your test suite definition:
-- Instead of: tests = testGroup "My Tests" [testProperty "test1" prop1, ...]
-- Use:       tests = optimizeTestSuite lowLevel $ testGroup "My Tests (Memory Optimized)" [testProperty "test1" prop1, ...]

-- 4. Or use the quick optimization wrapper:
-- tests = quickOptimize $ testGroup "My Tests" [testProperty "test1" prop1, ...]

-- 5. For string properties, use automatic limiting:
-- tests = testGroup "My Tests" [autoLimitStringProperty "test1" prop1, ...]

-- 6. For list properties, use automatic limiting:
-- tests = testGroup "My Tests" [autoLimitListProperty "test1" prop1, ...]

-- 7. For int properties, use automatic limiting:
-- tests = testGroup "My Tests" [autoLimitIntProperty "test1" prop1, ...]

-- Example optimized test file:
--
-- module Test.MyOptimizedSpec where
--
-- import Test.Tasty
-- import Test.Tasty.QuickCheck
-- import TestSupport.UniversalMemoryOptimization
--
-- prop_trim_test :: String -> Property
-- prop_trim_test s = 
--   let limited = limitString 5 s
--   in property $ length (trim limited) <= length limited
--
-- tests :: TestTree
-- tests = optimizeTestSuite lowLevel $ testGroup "My Optimized Tests"
--   [ testProperty "trim test" prop_trim_test
--   , autoLimitStringProperty "another test" prop_another
--   ]

-- Memory levels (choose based on available memory):
-- - emergencyLevel: 1MB limit (most restrictive)
-- - lowLevel: 8MB limit (good for CI/CD)
-- - moderateLevel: 16MB limit (good for development)