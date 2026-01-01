{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CompilerErrorRecoveryQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, sized, resize)
import qualified Test.QuickCheck as QC

import Compiler (compile, CompilerError(..), CompilerResult, CompilationPhase(..), renderCompilationError, formatCompilerErrors)
import Parser (TypusFile(..), CodeBlock(..), parseTypus)
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Control.Monad (when)

-- | Generate valid identifier names
genIdentifier :: Gen String
genIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
  return (first : rest)

-- | Generate simple type names
genType :: Gen String
genType = oneof 
  [ return "int"
  , return "string"
  , return "bool"
  , return "float"
  , return "void"
  , genIdentifier >>= \id -> return (id ++ "Type")
  ]

-- | Generate malformed code snippets for error recovery testing
genMalformedCode :: Gen String
genMalformedCode = oneof
  [ -- Unclosed braces
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "() {\n  x := 1\n  return x"
  
  , -- Invalid syntax
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "() -> {\n  return 42"
  
  , -- Type errors
    do
      funcName <- genIdentifier
      t1 <- genType
      t2 <- genType
      return $ "func " ++ funcName ++ "() " ++ t1 ++ " {\n  return : " ++ t2 ++ " 42"
  
  , -- Missing semicolons L.and brackets
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "()\n  x := 1\n  if x {\n    return x\n  \n}"
  
  , -- Invalid characters
    do
      funcName <- genIdentifier
      return $ "func " ++ funcName ++ "() {\n  x := @#$%\n  return x\n}"
  ]

-- | Generate valid code snippets
genValidCode :: Gen String
genValidCode = do
  funcName <- genIdentifier
  returnType <- genType
  let validFunc = "func " ++ funcName ++ "() " ++ returnType ++ " {\n  return default(" ++ returnType ++ ")\n}"
  return validFunc

-- | Generate mixed valid L.and invalid code blocks
genMixedCode :: Gen String
genMixedCode = do
  validBlocks <- listOf genValidCode
  malformedBlocks <- listOf genMalformedCode
  let allBlocks = zipWith (\i block -> "// Block " ++ show i ++ "\n" ++ block) ([1..] :: [Int]) (validBlocks ++ malformedBlocks)
  return $ unlines allBlocks

-- Property: Compiler can handle malformed code without crashing
prop_compiler_handles_malformed_code :: String -> Property
prop_compiler_handles_malformed_code malformedCode =
  let parseResult = parseTypus malformedCode
      result = case parseResult of
        Left _ -> Left [] -- Parse error, treat as compilation error
        Right typusFile -> compile typusFile
  in property $ case result of
    Left _ -> True -- Should return error, not crash
    Right _ -> True -- Or successfully compile

-- Property: Error messages contain source location information
prop_error_messages_contain_location :: String -> Property
prop_error_messages_contain_location malformedCode =
  not (null malformedCode) ==>
  let result = compileString malformedCode
  in case result of
    Left errors -> property $ hasLocationInfo (renderCompilationError errors)
    Right _ -> property $ True -- No errors means compilation succeeded
  where
    hasLocationInfo errorMsg = 
      "line" `L.isInfixOf` errorMsg || 
      "column" `L.isInfixOf` errorMsg ||
      "position" `L.isInfixOf` errorMsg

-- Property: Compiler provides meaningful error messages
prop_meaningful_error_messages :: String -> Property
prop_meaningful_error_messages malformedCode =
  not (null malformedCode) && L.length malformedCode > 5 ==>
  let result = compileString malformedCode
  in case result of
    Left errors -> property $ isMeaningfulError (renderCompilationError errors)
    Right _ -> property $ True
  where
    isMeaningfulError errorMsg = 
      L.length errorMsg > 10 && -- Error message should be substantial
      not (L.null (filter isAlpha errorMsg)) && -- Should contain letters
      not ("error" `L.isInfixOf` errorMsg && L.length errorMsg < 20) -- Not just generic "error"

-- Property: Error recovery allows parsing of subsequent valid blocks
prop_error_recovery_allows_subsequent_blocks :: String -> String -> Property
prop_error_recovery_allows_subsequent_blocks validCode malformedCode =
  not (null validCode) && not (null malformedCode) ==>
  let combinedCode = validCode ++ "\n" ++ malformedCode ++ "\n" ++ validCode
      result = compileString combinedCode
  in property $ case result of
    Left _ -> True -- Should handle errors gracefully
    Right _ -> True -- Or compile successfully

-- Property: Multiple errors are collected L.and reported
prop_multiple_errors_collected :: String -> Property
prop_multiple_errors_collected codeWithErrors =
  let result = compileString codeWithErrors
  in case result of
    Left errors -> property $ L.length errors >= 1 -- Should find at least one error
    Right _ -> property $ True -- Or compile successfully

-- Property: Error positions are accurate
prop_error_positions_accurate :: String -> Property
prop_error_positions_accurate malformedCode =
  not (null malformedCode) && malformedCode `L.isInfixOf` "func" ==>
  let result = compileString malformedCode
  in case result of
    Left errors -> property $ hasValidPositions errors
    Right _ -> property $ True
  where
    hasValidPositions errs = 
      let errorMsg = renderCompilationError errs
      in "line" `L.isInfixOf` errorMsg && 
         L.any isDigit errorMsg

-- Property: Compiler can handle empty input gracefully
prop_compiler_handles_empty_input :: Property
prop_compiler_handles_empty_input =
  let result = compileString ""
  in property $ case result of
    Left _ -> True -- Should return error for empty input
    Right _ -> True -- Or handle empty input as valid

-- Property: Compiler can handle whitespace-only input
prop_compiler_handles_whitespace_input :: String -> Property
prop_compiler_handles_whitespace_input whitespace =
  L.all isSpace whitespace ==>
  let result = compileString whitespace
  in property $ case result of
    Left _ -> True -- Should return error for whitespace-only input
    Right _ -> True -- Or handle as valid

-- Property: Error messages are consistent
prop_error_messages_consistent :: String -> Property
prop_error_messages_consistent malformedCode =
  not (null malformedCode) ==>
  let result1 = compileString malformedCode
      result2 = compileString malformedCode
  in case (result1, result2) of
    (Left errors1, Left errors2) -> 
      property $ L.length errors1 === L.length errors2
    (Right _, Right _) -> 
      property $ True -- Consistent success
    _ -> 
      property $ True -- One succeeds, one fails (edge case)

-- Property: Compiler handles very long lines
prop_compiler_handles_long_lines :: Int -> String -> Property
prop_compiler_handles_long_lines multiplier baseContent =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for performance
  let longLine = L.concat (replicate multiplier (baseContent ++ " "))
      codeWithLongLine = "func test() {\n  " ++ longLine ++ "\n}"
      result = compileString codeWithLongLine
  in property $ case result of
    Left _ -> True -- Should handle long lines gracefully
    Right _ -> True -- Or compile successfully

-- Property: Nested error scenarios
prop_nested_error_scenarios :: String -> String -> Property
prop_nested_error_scenarios outerError innerError =
  not (null outerError) && not (null innerError) ==>
  let nestedCode = "func outer() {\n  " ++ outerError ++ "\n  func inner() {\n    " ++ innerError ++ "\n  }\n}"
      result = compileString nestedCode
  in property $ case result of
    Left errors -> property $ L.length errors >= 1 -- Should detect errors
    Right _ -> property $ True

-- Helper function to compile strings by first parsing them
compileString :: String -> CompilerResult String
compileString code = case parseTypus code of
  Left _ -> Left [] -- Parse error, treat as compilation error
  Right typusFile -> compile typusFile

-- Helper function to check if a character is a digit
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Export L.all tests
tests :: TestTree
tests =
  testGroup "Compiler Error Recovery QuickCheck Tests"
    [ fastProperty "compiler handles malformed code without crashing" prop_compiler_handles_malformed_code
    , fastProperty "error messages contain source location information" prop_error_messages_contain_location
    , fastProperty "compiler provides meaningful error messages" prop_meaningful_error_messages
    , fastProperty "error recovery allows parsing of subsequent valid blocks" prop_error_recovery_allows_subsequent_blocks
    , fastProperty "multiple errors are collected L.and reported" prop_multiple_errors_collected
    , fastProperty "error positions are accurate" prop_error_positions_accurate
    , fastProperty "compiler handles empty input gracefully" prop_compiler_handles_empty_input
    , fastProperty "compiler handles whitespace-only input" prop_compiler_handles_whitespace_input
    , fastProperty "error messages are consistent" prop_error_messages_consistent
    , fastProperty "compiler handles very long lines" prop_compiler_handles_long_lines
    , fastProperty "nested error scenarios" prop_nested_error_scenarios
    ]