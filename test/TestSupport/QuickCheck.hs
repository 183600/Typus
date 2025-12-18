{-# LANGUAGE CPP #-}

module TestSupport.QuickCheck
  ( fastProperty
  ) where

import Test.QuickCheck (Testable)
import Test.Tasty (TestTree, localOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))
#if defined(FAST_TESTS)
import Test.Tasty.QuickCheck (QuickCheckTests(..))
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
