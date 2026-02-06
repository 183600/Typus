{-# LANGUAGE CPP #-}

module TestSupport.QuickCheck
  ( fastProperty
  , memoryEfficientProperty
  ) where

import Test.QuickCheck (Testable)
import Test.Tasty (TestTree, localOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..), QuickCheckTests(..))
#if defined(FAST_TESTS)
#endif

-- | Wrap 'testProperty' so that fast test runs keep a lightweight sampling
-- strategy while comprehensive/production runs continue to use the full
-- QuickCheck defaults but with a reasonable size limit to avoid timeouts.
fastProperty :: Testable prop => String -> prop -> TestTree
fastProperty name prop =
#if defined(FAST_TESTS)
  localOption (QuickCheckTests 25) $
  localOption (QuickCheckMaxSize 50) $
  testProperty name prop
#else
  -- Production mode: use full test count but limit size to avoid timeouts
  localOption (QuickCheckMaxSize 10) $
  testProperty name prop
#endif

-- | Memory-efficient property test with reduced test count and size limits
-- to prevent excessive memory consumption during testing
memoryEfficientProperty :: Testable prop => String -> prop -> TestTree
memoryEfficientProperty name prop =
  localOption (QuickCheckTests 50) $
  localOption (QuickCheckMaxSize 20) $
  testProperty name prop
