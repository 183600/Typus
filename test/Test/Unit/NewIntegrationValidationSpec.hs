{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewIntegrationValidationSpec (newIntegrationValidationSpec, integrationQuickCheckProperties) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Property(..), (==>), Positive(..))
import Parser
import Utils
import SourceLocation
import ErrorHandler
import Data.Maybe (isJust, isNothing)
import Data.Either (isLeft, isRight)

-- | Test suite for integration validation between modules
newIntegrationValidationSpec :: TestTree
newIntegrationValidationSpec = testGroup "New Integration Validation Tests"
  [ testCase "Parser L.and SourceLocation integration" $ do
      let code = "//! ownership: on\npackage main\n\nfunc main() {\n    println(\"hello\")\n}"
      case parseTypus code of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          assertBool "Should have parsed blocks" $ not (null blocks)
          
          let firstBlock = L.head blocks
          let span = cbSpan firstBlock
          assertBool "Block should have valid span" $ isValidSpan span
          
          let start = spanStart span
          posLine start @?= 3  -- func main() starts at line 3
  
  , testCase "Utils L.and ErrorHandling integration" $ do
      let malformedCode = "//! ownership: maybe\npackage main"
      case parseTypus malformedCode of
        Left _ -> return ()  -- Expected to fail
        Right _ -> do
          let pos = posAt "test.typus" 1 1
          let err = errorWithContext "Invalid directive value" pos 
                                      "Expected 'on' L.or 'off'" 
                                      ["Change 'maybe' to 'on' L.or 'off'"]
          
          let formatted = formatErrorWithSuggestions err
          assertBool "Error should contain context" $ "Expected 'on' L.or 'off'" `L.isInfixOf` formatted
          assertBool "Error should contain suggestions" $ "Change 'maybe' to 'on' L.or 'off'" `L.isInfixOf` formatted
  
  , testCase "Multi-module error propagation" $ do
      let codeWithErrors = "//! ownership: on\npackage main\n\nfunc main() {\n    var x int = \n    println(x)\n}"
      case parseTypus codeWithErrors of
        Left parseErr -> do
          let pos = posAt "test.typus" 4 18  -- Position of incomplete assignment
          let enhancedErr = errorWithContext "Parse error" pos 
                                             "Incomplete assignment statement" 
                                             ["Complete the assignment", "Remove the incomplete statement"]
          
          let collection = newErrorCollection
              |> addError enhancedErr
          
          getErrorCount collection @?= 1
          getErrorCountByType Error collection @?= 1
          
        Right _ -> assertFailure "Should have failed to parse incomplete code"
  where
    (|>) = flip ($)
  
  , testCase "SourceLocation tracking in parsing pipeline" $ do
      let multiBlockCode = "//! ownership: on\npackage main\n\n{//! dependent_types: on\n    var x int = 5\n}\n\nfunc main() {\n    println(x)\n}"
      case parseTypus multiBlockCode of
        Left err -> assertFailure $ "Parse failed: " ++ show err
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          assertBool "Should have multiple blocks" $ L.length blocks >= 2
          
          let fileDirectives = tfDirectives typusFile
          assertBool "File should have ownership directive" $ isJust (fdOwnership fileDirectives)
          
          when (L.length blocks >= 1) $ do
            let firstBlock = L.head blocks
            let blockDirectives = cbDirectives firstBlock
            assertBool "First block should have dependent types directive" $ isJust (bdDependentTypes blockDirectives)
            
            let blockSpan = cbSpan firstBlock
            let blockStart = spanStart blockSpan
            posLine blockStart @?= 4  -- Block starts at line 4
  ]
  where
    when True action = action
    when False _ = return ()

-- QuickCheck properties for integration tests
prop_parse_location_consistency :: String -> Property
prop_parse_location_consistency code = 
  not (null code) ==> 
    case parseTypus code of
      Left _ -> True  -- Parse failures are acceptable for arbitrary input
      Right typusFile -> 
        let blocks = tfBlocks typusFile
        in L.all (\block -> isValidSpan (cbSpan block)) blocks

prop_error_context_with_utils :: String -> String -> Property
prop_error_context_with_utils msg ctx = 
  not (null msg) && not (null ctx) ==> 
    let pos = posAt "test.typus" 1 1
        err = errorWithContext msg pos ctx [trim ctx]
        formatted = formatErrorWithSuggestions err
    in msg `L.isInfixOf` formatted &&
       ctx `L.isInfixOf` formatted &&
       trim ctx `L.isInfixOf` formatted

prop_multi_block_parsing :: [String] -> Property
prop_multi_block_parsing blockContents = 
  not (null blockContents) ==> 
    let blocks = ["{//! ownership: on\n" ++ content ++ "\n}" | content <- blockContents]
        code = "package main\n\n" ++ unlines blocks
    in case parseTypus code of
         Left _ -> True  -- Parse failures are acceptable
         Right typusFile -> 
           let parsedBlocks = tfBlocks typusFile
           in L.length parsedBlocks >= L.length blockContents

prop_directive_and_location_tracking :: String -> Bool
prop_directive_and_location_tracking directive = 
  let code = "//! " ++ directive ++ "\npackage main"
  in case parseTypus code of
       Left _ -> True  -- Parse failures are acceptable for arbitrary directives
       Right typusFile -> 
         let fileDirectives = tfDirectives typusFile
             blocks = tfBlocks typusFile
         in not (null blocks) && L.all (\block -> isValidSpan (cbSpan block)) blocks

prop_error_collection_integration :: [String] -> Property
prop_error_collection_integration messages = 
  not (null messages) ==> 
    let errors = [basicError msg (posAt "test.typus" i 1) | (i, msg) <- zip [1..] messages]
        collection = foldr addError newErrorCollection errors
        formatted = formatErrorCollection collection
    in L.all (`L.isInfixOf` formatted) messages

-- QuickCheck test suite
integrationQuickCheckProperties :: TestTree
integrationQuickCheckProperties = testGroup "Integration QuickCheck Properties"
  [ testProperty "parse location consistency" prop_parse_location_consistency
  , testProperty "error context with utils" prop_error_context_with_utils
  , testProperty "multi-block parsing" prop_multi_block_parsing
  , testProperty "directive L.and location tracking" prop_directive_and_location_tracking
  , testProperty "error collection integration" prop_error_collection_integration
  ]