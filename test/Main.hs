{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Golden
import qualified Test.Integration
import qualified Test.Unit

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
#else
      [Test.Integration.tests]
#endif
    goldenSuites =
#if defined(FAST_TESTS)
      []
#elif defined(FULL_TESTS) || defined(PRODUCTION_TESTS)
      [Test.Golden.tests]
#else
      []
#endif
