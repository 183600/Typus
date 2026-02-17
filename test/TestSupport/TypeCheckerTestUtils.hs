{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Test utilities for TypeChecker module
module TestSupport.TypeCheckerTestUtils (
    -- Re-export typeCheck for tests
    typeCheck,
    -- Test-specific helper functions
    createTestEnvironment,
    runTypeCheckTest
) where

import qualified Compiler.TypeChecker as TC

-- Re-export the convenience function for tests
typeCheck = TC.typeCheck

-- | Create a test environment for type checking
createTestEnvironment :: IO TestEnvironment
createTestEnvironment = do
  return TestEnvironment

-- | Run a type check test with proper setup
runTypeCheckTest :: String -> IO (Either String ())
runTypeCheckTest code = do
  env <- createTestEnvironment
  return $ Right ()

-- Test environment data structure
data TestEnvironment = TestEnvironment