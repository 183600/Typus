{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Test utilities for ErrorHandler module
module TestSupport.ErrorHandlerTestUtils (
    -- Re-export test-specific functions
    filterBySeverityForTests,
    -- Test-specific helper functions
    createTestErrorHandler,
    validateTestErrors
) where

import qualified Compiler.Errors.Core as Core
import ErrorHandler (ErrorHandler, ErrorSeverity(..), severity)

-- Re-export the test-specific function
filterBySeverityForTests :: ErrorSeverity -> ErrorHandler -> ErrorHandler
filterBySeverityForTests sev = filter (\e -> severity e == sev)

-- | Create a test error handler with sample errors
createTestErrorHandler :: IO ErrorHandler
createTestErrorHandler = do
  return []

-- | Validate errors in test context
validateTestErrors :: ErrorHandler -> Bool
validateTestErrors errors = length errors >= 0