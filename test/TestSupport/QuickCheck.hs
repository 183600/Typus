{-# LANGUAGE CPP #-}

module TestSupport.QuickCheck
  ( fastProperty
  ) where

import Test.QuickCheck (Testable)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
#if defined(FAST_TESTS)
import Test.Tasty (localOption)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), QuickCheckTests(..))
#endif

-- | Wrap 'testProperty' so that fast test runs keep a lightweight sampling
-- strategy while comprehensive/production runs continue to use the full
-- QuickCheck defaults.
fastProperty :: Testable prop => String -> prop -> TestTree
fastProperty name prop =
#if defined(FAST_TESTS)
  localOption (QuickCheckTests 25) $
  localOption (QuickCheckMaxSize 50) $
  testProperty name prop
#else
  testProperty name prop
#endif
