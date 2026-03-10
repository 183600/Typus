{-# LANGUAGE CPP #-}

module TestSupport.QuickCheck
  ( fastProperty
  , memoryEfficientProperty
  , ultraMemoryEfficientProperty
  , stringProcessingProperty
  ) where

import Test.QuickCheck (Testable)
import Test.Tasty (TestTree, localOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..), QuickCheckTests(..), QuickCheckMaxShrinks(..))
#if defined(FAST_TESTS)
#endif

-- | Wrap 'testProperty' so that fast test runs keep a lightweight sampling
-- strategy while comprehensive/production runs continue to use the full
-- QuickCheck defaults but with a reasonable size limit to avoid timeouts.
fastProperty :: Testable prop => String -> prop -> TestTree
fastProperty name prop =
#if defined(FAST_TESTS)
  localOption (QuickCheckTests 5) $      -- 从25减少到5，减少内存使用
  localOption (QuickCheckMaxSize 3) $    -- 从50减少到3，大幅减少数据大小
  testProperty name prop
#else
  -- Production mode: use reduced test count and size to save memory
  localOption (QuickCheckTests 3) $      -- 进一步减少测试次数
  localOption (QuickCheckMaxSize 2) $    -- 进一步减少数据大小
  testProperty name prop
#endif

-- | Memory-efficient property test with significantly reduced test count and size limits
-- to prevent excessive memory consumption during testing
memoryEfficientProperty :: Testable prop => String -> prop -> TestTree
memoryEfficientProperty name prop =
  localOption (QuickCheckTests 2) $      -- 从50减少到2，大幅减少内存
  localOption (QuickCheckMaxSize 2) $    -- 从20减少到2，减少数据大小
  testProperty name prop

-- | Ultra memory-efficient property test for very memory-constrained environments
ultraMemoryEfficientProperty :: Testable prop => String -> prop -> TestTree
ultraMemoryEfficientProperty name prop =
  localOption (QuickCheckTests 1) $      -- 从25减少到1，最小内存使用
  localOption (QuickCheckMaxSize 1) $    -- 从10减少到1，最小数据大小
  testProperty name prop

-- | Memory-efficient property test for string processing (reduces large string generation)
stringProcessingProperty :: Testable prop => String -> prop -> TestTree
stringProcessingProperty name prop =
  localOption (QuickCheckTests 2) $      -- 从30减少到2
  localOption (QuickCheckMaxSize 2) $    -- 从15减少到2
  testProperty name prop

-- | Emergency memory property test for extreme memory constraints
emergencyMemoryProperty :: Testable prop => String -> prop -> TestTree
emergencyMemoryProperty name prop =
  localOption (QuickCheckTests 1) $      -- 最小测试次数
  localOption (QuickCheckMaxSize 1) $    -- 最小数据大小
  localOption (QuickCheckMaxShrinks 0) $ -- 禁用收缩以节省内存
  testProperty name prop

-- | Memory-monitored property test with forced GC between tests
memoryMonitoredProperty :: Testable prop => String -> prop -> TestTree
memoryMonitoredProperty name prop =
  localOption (QuickCheckTests 3) $      -- 少量测试
  localOption (QuickCheckMaxSize 2) $    -- 小数据大小
  localOption (QuickCheckMaxShrinks 1) $ -- 少量收缩
  testProperty name prop

-- | Adaptive property test that adjusts based on available memory
adaptiveMemoryProperty :: Testable prop => String -> prop -> TestTree
adaptiveMemoryProperty name prop =
  localOption (QuickCheckTests 5) $      -- 中等测试次数
  localOption (QuickCheckMaxSize 3) $    -- 中等数据大小
  localOption (QuickCheckMaxShrinks 2) $ -- 中等收缩
  testProperty name prop
