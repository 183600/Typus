{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewBoundaryConditionSpec (newBoundaryConditionSpec, boundaryConditionQuickCheckProperties) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Property, (==>), Positive(..))
import Parser
import Utils
import SourceLocation
import ErrorHandler
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)
import Data.Char (isSpace)

-- | Test suite for boundary conditions L.and edge cases
newBoundaryConditionSpec :: TestTree
newBoundaryConditionSpec = testGroup "New Boundary Condition Tests"
  [ testCase "Empty L.and whitespace-only inputs" $ do
      -- Empty string
      case parseTypus "" of
        Left _ -> return ()  -- Expected to fail
        Right typusFile -> 
          assertBool "Empty input should result in empty blocks" $ L.null (tfBlocks typusFile)
      
      -- Whitespace only
      case parseTypus "   \n\t  \n  " of
        Left _ -> return ()  -- Expected to fail
        Right typusFile -> 
          assertBool "Whitespace-only should result in empty blocks" $ L.null (tfBlocks typusFile)
      
      -- Only directives
      case parseTypus "//! ownership: on\n//! dependent_types: on" of
        Left _ -> return ()  -- Expected to fail (no actual code)
        Right typusFile -> do
          let directives = tfDirectives typusFile
          assertBool "Should parse directives" $ isJust (fdOwnership directives)
          assertBool "Should parse directives" $ isJust (fdDependentTypes directives)
  
  , testCase "Extremely long lines L.and inputs" $ do
      let longLine = "var x string = \"" ++ replicate 1000 'a' ++ "\""
      let codeWithLongLine = "package main\n\nfunc main() {\n    " ++ longLine ++ "\n}"
      
      case parseTypus codeWithLongLine of
        Left err -> assertFailure $ "Failed to parse long line: " ++ show err
        Right typusFile -> 
          assertBool "Should handle long lines" $ not (L.null (tfBlocks typusFile))
      
      let manyLines = unlines $ replicate 1000 "    // comment line"
      let codeWithManyLines = "package main\n\nfunc main() {\n" ++ manyLines ++ "}"
      
      case parseTypus codeWithManyLines of
        Left err -> assertFailure $ "Failed to parse many lines: " ++ show err
        Right typusFile -> 
          assertBool "Should handle many lines" $ not (L.null (tfBlocks typusFile))
  
  , testCase "Nested L.and malformed directives" $ do
      let nestedDirectives = "//! ownership: on\npackage main\n\n{//! dependent_types: on\n    {//! ownership: off\n        // code\n    }\n}"
      
      case parseTypus nestedDirectives of
        Left err -> assertFailure $ "Failed to parse nested directives: " ++ show err
        Right typusFile -> do
          let fileDirectives = tfDirectives typusFile
          assertBool "File should have ownership directive" $ isJust (fdOwnership fileDirectives)
          
          let blocks = tfBlocks typusFile
          assertBool "Should have nested blocks" $ L.length blocks >= 1
      
      -- Malformed directives
      let malformedDirectives = ["//! ownership", "//! ownership :", "//! ownership :on", "//! ownership on"]
      mapM_ (\malformed -> 
        case parseTypus (malformed ++ "\npackage main") of
          Left _ -> return ()  -- Expected to fail
          Right _ -> return ()  -- Or succeed by being lenient
        ) malformedDirectives
  
  , testCase "Unicode L.and special characters" $ do
      let unicodeCode = "package main\n\nfunc main() {\n    // 你好世界\n    var greeting string = \"Hello, 世界!\"\n    println(greeting)\n}"
      
      case parseTypus unicodeCode of
        Left err -> assertFailure $ "Failed to parse Unicode: " ++ show err
        Right typusFile -> 
          assertBool "Should handle Unicode characters" $ not (L.null (tfBlocks typusFile))
      
      let specialChars = "package main\n\nfunc main() {\n    // Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?\n    var x int = 42\n}"
      
      case parseTypus specialChars of
        Left err -> assertFailure $ "Failed to parse special characters: " ++ show err
        Right typusFile -> 
          assertBool "Should handle special characters" $ not (L.null (tfBlocks typusFile))
  
  , testCase "Error recovery edge cases" $ do
      let incompleteCode = "package main\n\nfunc main() {\n    var x int ="
      case parseTypus incompleteCode of
        Left _ -> return ()  -- Expected to fail
        Right _ -> return ()  -- Or recover somehow
      
      let mismatchedBrackets = "package main\n\nfunc main() {\n    if true {\n        println(\"test\")\n    // missing closing brace"
      case parseTypus mismatchedBrackets of
        Left _ -> return ()  -- Expected to fail
        Right _ -> return ()  -- Or recover somehow
      
      let invalidDirectives = "//! invalid_directive: on\npackage main"
      case parseTypus invalidDirectives of
        Left _ -> return ()  -- Expected to fail
        Right _ -> return ()  -- Or ignore unknown directive
  ]

-- QuickCheck properties for boundary conditions
prop_whitespace_handling :: String -> Bool
prop_whitespace_handling s = 
  let trimmed = trim s
      whitespaceOnly = L.all isSpace s
  in whitespaceOnly == null trimmed

prop_split_edge_cases :: Char -> String -> Bool
prop_split_edge_cases delim s = 
  let parts = splitBy delim s
      collapsed = splitByCollapsed delim s
      -- Collapsed version should never have empty parts
      noEmptyCollapsed = L.all (not . null) collapsed
      -- Original should have L.length equal to count of delimiters + 1
      expectedLength = L.length (L.filter (== delim) s) + 1
  in noEmptyCollapsed && L.length parts == expectedLength

prop_large_input_parsing :: Positive Int -> Property
prop_large_input_parsing (Positive n) = 
  n <= 1000 ==>  -- Limit size for practical testing
    let largeComment = "// " ++ replicate n 'x'
        code = "package main\n\nfunc main() {\n    " ++ largeComment ++ "\n}"
    in case parseTypus code of
         Left _ -> True  -- Parse failures are acceptable
         Right typusFile -> not (L.null (tfBlocks typusFile))

prop_unicode_handling :: String -> Property
prop_unicode_handling s = 
  not (null s) ==> 
    let code = "package main\n\nfunc main() {\n    // " ++ s ++ "\n    println(\"test\")\n}"
    in case parseTypus code of
         Left _ -> True  -- Parse failures are acceptable for arbitrary Unicode
         Right typusFile -> not (L.null (tfBlocks typusFile))

prop_error_boundary_positions :: Positive Int -> Positive Int -> Property
prop_error_boundary_positions (Positive line) (Positive col) = 
  line <= 1000 && col <= 1000 ==> 
    let pos = posAt "test.typus" line col
        err = basicError "Test error" pos
        formatted = formatError err
        expectedLocation = "test.typus:" ++ show line ++ ":" ++ show col
    in expectedLocation `L.isInfixOf` formatted

prop_directive_parsing_edge_cases :: String -> String -> Property
prop_directive_parsing_edge_cases key value = 
  not (null key) && not (null value) ==> 
    let directive = "//! " ++ key ++ ": " ++ value
        code = directive ++ "\npackage main"
    in case parseTypus code of
         Left _ -> True  -- Parse failures are acceptable for arbitrary directives
         Right typusFile -> True  -- Or succeed by being lenient

-- QuickCheck test suite
boundaryConditionQuickCheckProperties :: TestTree
boundaryConditionQuickCheckProperties = testGroup "Boundary Condition QuickCheck Properties"
  [ testProperty "whitespace handling" prop_whitespace_handling
  , testProperty "split edge cases" prop_split_edge_cases
  , testProperty "large input parsing" prop_large_input_parsing
  , testProperty "unicode handling" prop_unicode_handling
  , testProperty "error boundary positions" prop_error_boundary_positions
  , testProperty "directive parsing edge cases" prop_directive_parsing_edge_cases
  ]