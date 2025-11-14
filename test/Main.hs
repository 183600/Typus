{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Dependencies.Arbitrary ()
import qualified Test.Unit

#if defined(FULL_TESTS) || defined(PRODUCTION_TESTS)
import qualified Test.Golden
import qualified Test.Integration
#endif

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup "Typus"
    (  [Test.Unit.tests]
    ++ integrationSuites
    ++ goldenSuites
    )
  where
    integrationSuites =
#if defined(FAST_TESTS)
      []
#elif defined(FULL_TESTS) || defined(PRODUCTION_TESTS)
      [Test.Integration.tests]
#else
      []
#endif
    goldenSuites =
#if defined(FAST_TESTS)
      []
#elif defined(FULL_TESTS) || defined(PRODUCTION_TESTS)
      [Test.Golden.tests]
#else
      []
#endif
