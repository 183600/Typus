{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewParserValidationSpec (newParserValidationSpec, parserQuickCheckProperties) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Property, (==>), Positive(..))
import Parser
import SourceLocation
import Data.Maybe (isJust, isNothing)
import qualified Data.List as L
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- | Test suite for Parser validation functions
newParserValidationSpec :: TestTree
newParserValidationSpec = testGroup "New Parser Validation Tests"
  [ testCase "File directives parsing" $ do
      let empty = defaultFileDirectives
      fdOwnership empty @?= Nothing
      fdDependentTypes empty @?= Nothing
      fdConstraints empty @?= Nothing
  
  , testCase "Block directives parsing" $ do
      let empty = defaultBlockDirectives
      bdOwnership empty @?= Nothing
      bdDependentTypes empty @?= Nothing
      bdConstraints empty @?= Nothing
  
  , testCase "Directive parsing in various contexts" $ do
      -- Test file-level directive
      let fileWithOwnership = "//! ownership: on\npackage main"
      case parseTypus fileWithOwnership of
        Left _ -> assertFailure "Should parse file directive successfully"
        Right typusFile -> do
          let directives = tfDirectives typusFile
          assertBool "Ownership directive should be present" $ isJust (fdOwnership directives)
      
      -- Test block-level directive
      let blockWithDependentTypes = "func main() {\n    //! dependent_types: on\n    var x int = 5\n}"
      case parseTypus blockWithDependentTypes of
        Left _ -> assertFailure "Should parse block directive successfully"
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          assertBool "Should have at least one block" $ not (null blocks)
          let firstBlock = L.head blocks
          let directives = cbDirectives firstBlock
          assertBool "Dependent types directive should be present" $ isJust (bdDependentTypes directives)
  
  , testCase "Code block structure validation" $ do
      let simpleCode = "package main\n\nfunc main() {\n    println(\"hello\")\n}"
      case parseTypus simpleCode of
        Left _ -> assertFailure "Should parse simple code successfully"
        Right typusFile -> do
          let blocks = tfBlocks typusFile
          assertBool "Should have at least one code block" $ not (null blocks)
          let firstBlock = L.head blocks
          assertBool "Code block should not be empty" $ not (L.null (cbContent firstBlock))
  
  , testCase "Mixed directives handling" $ do
      let mixedDirectives = "//! ownership: on\n//! dependent_types: on\npackage main\n\nfunc test() {\n    //! constraints: on\n    // code here\n}"
      case parseTypus mixedDirectives of
        Left _ -> assertFailure "Should parse mixed directives successfully"
        Right typusFile -> do
          let fileDirectives = tfDirectives typusFile
          assertBool "File should have ownership directive" $ isJust (fdOwnership fileDirectives)
          assertBool "File should have dependent types directive" $ isJust (fdDependentTypes fileDirectives)
          
          let blocks = tfBlocks typusFile
          assertBool "Should have at least one block" $ L.length blocks >= 1
          
          when (L.length blocks >= 1) $ do
            let firstBlock = L.head blocks
            let blockDirectives = cbDirectives firstBlock
            assertBool "Block should have constraints directive" $ isJust (bdConstraints blockDirectives)
  
  , testCase "Error handling for malformed directives" $ do
      let malformedDirective = "//! ownership: maybe\npackage main"
      case parseTypus malformedDirective of
        Left _ -> return ()  -- Expected to fail
        Right _ -> assertFailure "Should fail on malformed directive"
      
      let unknownDirective = "//! unknown_feature: on\npackage main"
      case parseTypus unknownDirective of
        Left _ -> return ()  -- Expected to fail L.or ignore unknown directive
        Right _ -> return ()  -- Or succeed by ignoring it
  ]
  where
    when True action = action
    when False _ = return ()

-- QuickCheck properties for Parser functions
prop_parse_empty_string :: Bool
prop_parse_empty_string = 
  case parseTypus "" of
    Left _ -> True  -- Empty string should fail to parse meaningfully
    Right typusFile -> L.null (tfBlocks typusFile)  -- Or parse to empty blocks

prop_parse_simple_package :: String -> Property
prop_parse_simple_package name = 
  not (null name) && L.all (not . isSpace) name ==>
    let code = "package " ++ name
    in case parseTypus code of
         Left _ -> False
         Right typusFile -> not (L.null (tfBlocks typusFile))

prop_directive_parsing_consistency :: String -> Bool
prop_directive_parsing_consistency content = 
  let withDirective = "//! ownership: on\n" ++ content
      withoutDirective = content
  in case (parseTypus withDirective, parseTypus withoutDirective) of
       (Right withFile, Right withoutFile) -> 
         -- Both should parse, but withFile should have ownership directive
         isJust (fdOwnership (tfDirectives withFile)) &&
         isNothing (fdOwnership (tfDirectives withoutFile))
       _ -> True  -- If either fails, that's acceptable for arbitrary content

prop_block_directive_scope :: String -> Property
prop_block_directive_scope content = 
  not (null content) ==> 
    let code = "func main() {\n    //! ownership: on\n    " ++ content ++ "\n}"
    in case parseTypus code of
         Left _ -> False
         Right typusFile -> 
           let blocks = tfBlocks typusFile
           in not (null blocks) && 
              let firstBlock = L.head blocks
                  directives = cbDirectives firstBlock
              in isJust (bdOwnership directives)

-- QuickCheck test suite
parserQuickCheckProperties :: TestTree
parserQuickCheckProperties = testGroup "Parser QuickCheck Properties"
  [ testProperty "parse empty string" prop_parse_empty_string
  , testProperty "parse simple package declaration" prop_parse_simple_package
  , testProperty "directive parsing consistency" prop_directive_parsing_consistency
  , testProperty "block directive scope" prop_block_directive_scope
  ]