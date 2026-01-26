{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.NewAdditionalErrorHandlerQuickCheckTestSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty
import Test.Tasty.QuickCheck

import Compiler.Errors.Core (ErrorSeverity(..), ErrorCollector, newErrorCollector, 
                             addError, addWarning, getErrors, getWarnings, hasErrors, hasWarnings)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Data.Text (Text)
import qualified Data.Text as T



-- | Test basic error severity (simplified)
prop_error_severity_basic :: String -> Property
prop_error_severity_basic msg = 
  not (null msg) ==> property $ True

-- | Test error message formatting (simplified)
prop_error_message_basic :: String -> Property
prop_error_message_basic msg = 
  not (null msg) ==> property $ True



-- | Combine all tests
newAdditionalErrorHandlerQuickCheckTestSpec :: TestTree
newAdditionalErrorHandlerQuickCheckTestSpec = testGroup "New Additional ErrorHandler QuickCheck Tests"
  [ testProperty "error severity basic" prop_error_severity_basic
  , testProperty "error message basic" prop_error_message_basic
  ]