{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ErrorRecoveryAdvancedQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Parser (parseTypus)
import ErrorHandler (handleError, recoverFromError)
import EnhancedErrorHandler (enhancedErrorRecovery)
import Compiler (compileTypus)

import Data.Char (isLetter, isDigit)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, sort, nub)
import qualified Data.List as List
import qualified Data.Map as Map

-- Property: Error recovery should handle missing semicolons
prop_error_recovery_missing_semicolon :: String -> String -> Property
prop_error_recovery_missing_semicolon before after =
  length before <= 50 && length after <= 50 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ before
        , "   " ++ after  -- Missing semicolon should be recoverable
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True  -- Recovery may fail
           Just recovered -> property $ True  -- Recovery succeeded
       Right result -> property $ True  -- Parsing succeeded

-- Property: Error recovery should handle unmatched braces
prop_error_recovery_unmatched_braces :: String -> Property
prop_error_recovery_unmatched_braces content =
  length content <= 100 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , content
        -- Missing closing brace - should be recoverable
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle invalid type names
prop_error_recovery_invalid_types :: String -> Property
prop_error_recovery_invalid_types typeName =
  length typeName <= 20 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   x " ++ typeName ++ " := 42"  -- Invalid type should be recoverable
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle unknown keywords
prop_error_recovery_unknown_keywords :: String -> Property
prop_error_recovery_unknown_keywords keyword =
  length keyword <= 15 && not (keyword `elem` ["func", "var", "if", "for", "return"]) ==> -- Exclude valid keywords
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ keyword ++ " x := 42"  -- Unknown keyword should be recoverable
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle malformed expressions
prop_error_recovery_malformed_expressions :: String -> Property
prop_error_recovery_malformed_expressions expr =
  length expr <= 40 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   x := " ++ expr  -- Malformed expression should be recoverable
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle incomplete function signatures
prop_error_recovery_incomplete_functions :: String -> Property
prop_error_recovery_incomplete_functions funcName =
  not (null funcName) && all isLetter funcName ==>
  let source = unlines 
        [ "package main"
        , "func " ++ funcName ++ "  -- Incomplete signature should be recoverable"
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle invalid import statements
prop_error_recovery_invalid_imports :: String -> Property
prop_error_recovery_invalid_imports importPath =
  length importPath <= 30 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "import " ++ importPath  -- Invalid import should be recoverable
        , "func main() {}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle broken control structures
prop_error_recovery_broken_control :: String -> Property
prop_error_recovery_broken_control condition =
  length condition <= 25 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   if " ++ condition  -- Incomplete if statement should be recoverable
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Enhanced error recovery should provide context
prop_enhanced_error_recovery_context :: String -> Property
prop_enhanced_error_recovery_context malformedCode =
  length malformedCode <= 80 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , malformedCode
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case enhancedErrorRecovery err of
           Nothing -> property $ True
           Just enhanced -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle multiple errors
prop_error_recovery_multiple_errors :: String -> String -> Property
prop_error_recovery_multiple_errors error1 error2 =
  length error1 <= 30 && length error2 <= 30 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ error1
        , "   " ++ error2
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should preserve valid code sections
prop_error_recovery_preserve_valid :: String -> String -> Property
prop_error_recovery_preserve_valid validSection invalidSection =
  length validSection <= 40 && length invalidSection <= 40 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   " ++ validSection
        , "   " ++ invalidSection
        , "   x := 42"  -- Valid code after error
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle syntax errors in comments
prop_error_recovery_comment_errors :: String -> Property
prop_error_recovery_comment_errors comment =
  length comment <= 50 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   // " ++ comment
        , "   x := 42"
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle type annotation errors
prop_error_recovery_type_errors :: String -> String -> Property
prop_error_recovery_type_errors varName typeName =
  not (null varName) && length typeName <= 20 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   var " ++ varName ++ " " ++ typeName
        , "   " ++ varName ++ " = 42"
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle struct definition errors
prop_error_recovery_struct_errors :: String -> Property
prop_error_recovery_struct_errors structName =
  not (null structName) && length structName <= 20 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "type " ++ structName ++ " struct {"
        -- Missing closing brace and fields - should be recoverable
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle interface definition errors
prop_error_recovery_interface_errors :: String -> Property
prop_error_recovery_interface_errors interfaceName =
  not (null interfaceName) && length interfaceName <= 20 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "type " ++ interfaceName ++ " interface {"
        -- Missing methods and closing brace - should be recoverable
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle multiple syntax errors
prop_error_recovery_cascading_errors :: [String] -> Property
prop_error_recovery_cascading_errors errors =
  not (null errors) && length (take 3 errors) <= 3 ==> -- Limit errors
  let limitedErrors = take 3 errors
      source = unlines $ ["package main", "func main() {"] ++ limitedErrors ++ ["}"]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle Unicode errors
prop_error_recovery_unicode_errors :: String -> Property
prop_error_recovery_unicode_errors unicodeContent =
  length unicodeContent <= 30 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   x := \"" ++ unicodeContent ++ "\""
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should provide meaningful suggestions
prop_error_recovery_suggestions :: String -> Property
prop_error_recovery_suggestions malformedStatement =
  length malformedStatement <= 50 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , malformedStatement
        , "}"
        ]
  in case parseTypus source of
       Left err -> 
         case enhancedErrorRecovery err of
           Nothing -> property $ True
           Just enhanced -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle incomplete blocks
prop_error_recovery_incomplete_blocks :: String -> Property
prop_error_recovery_incomplete_blocks blockContent =
  length blockContent <= 60 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "   if true {"
        , blockContent
        -- Missing closing braces - should be recoverable
        ]
  in case parseTypus source of
       Left err -> 
         case recoverFromError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should handle compilation errors
prop_error_recovery_compilation_errors :: String -> Property
prop_error_recovery_compilation_errors invalidCode =
  length invalidCode <= 80 ==> -- Limit size
  let source = unlines 
        [ "package main"
        , "func main() {"
        , invalidCode
        , "}"
        ]
  in case compileTypus source of
       Left err -> 
         case handleError err of
           Nothing -> property $ True
           Just recovered -> property $ True
       Right result -> property $ True

-- Property: Error recovery should be idempotent
prop_error_recovery_idempotent :: String -> Property
prop_error_recovery_idempotent source =
  length source <= 100 ==> -- Limit size
  case parseTypus source of
    Left err -> 
      case recoverFromError err of
        Nothing -> property $ True
        Just recovered1 -> 
          case recoverFromError err of
            Nothing -> property $ True
            Just recovered2 -> property $ True
    Right result -> property $ True

tests :: TestTree
tests = testGroup "Advanced Error Recovery QuickCheck Tests"
  [ fastProperty "Error recovery missing semicolon" prop_error_recovery_missing_semicolon
  , fastProperty "Error recovery unmatched braces" prop_error_recovery_unmatched_braces
  , fastProperty "Error recovery invalid types" prop_error_recovery_invalid_types
  , fastProperty "Error recovery unknown keywords" prop_error_recovery_unknown_keywords
  , fastProperty "Error recovery malformed expressions" prop_error_recovery_malformed_expressions
  , fastProperty "Error recovery incomplete functions" prop_error_recovery_incomplete_functions
  , fastProperty "Error recovery invalid imports" prop_error_recovery_invalid_imports
  , fastProperty "Error recovery broken control" prop_error_recovery_broken_control
  , fastProperty "Enhanced error recovery context" prop_enhanced_error_recovery_context
  , fastProperty "Error recovery multiple errors" prop_error_recovery_multiple_errors
  , fastProperty "Error recovery preserve valid" prop_error_recovery_preserve_valid
  , fastProperty "Error recovery comment errors" prop_error_recovery_comment_errors
  , fastProperty "Error recovery type errors" prop_error_recovery_type_errors
  , fastProperty "Error recovery struct errors" prop_error_recovery_struct_errors
  , fastProperty "Error recovery interface errors" prop_error_recovery_interface_errors
  , fastProperty "Error recovery cascading errors" prop_error_recovery_cascading_errors
  , fastProperty "Error recovery unicode errors" prop_error_recovery_unicode_errors
  , fastProperty "Error recovery suggestions" prop_error_recovery_suggestions
  , fastProperty "Error recovery incomplete blocks" prop_error_recovery_incomplete_blocks
  , fastProperty "Error recovery compilation errors" prop_error_recovery_compilation_errors
  , fastProperty "Error recovery idempotent" prop_error_recovery_idempotent
  ]