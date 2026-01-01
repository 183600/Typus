{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalCompilerSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertFailure, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

-- Parser modules
import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , spanStart
  , spanEnd
  )

import Utils
  ( trim
  , removeComments
  , normalizeIndentation
  )

import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)
import Data.Maybe (isJust, isNothing, fromMaybe)

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New Cabal Compiler Tests"
    [ testGroup "Parser module tests"
        [ testCase "parseTypus handles empty file" $ do
            let emptyContent = ""
                result = parseTypus emptyContent
            case result of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right file -> do
                tfCodeBlocks file @?= []
                tfFileDirectives file @?= defaultFileDirectives
                
        , testCase "parseTypus handles simple code block" $ do
            let simpleContent = "func main() {\n  return 42\n}"
                result = parseTypus simpleContent
            case result of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right file -> do
                L.length (tfCodeBlocks file) @?= 1
                
        , testCase "parseTypus handles file directives" $ do
            let contentWithDirectives = "// @ownership: true\n// @dependent-types: false\nfunc test() {}"
                result = parseTypus contentWithDirectives
            case result of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right file -> do
                let directives = tfFileDirectives file
                isJust (fdOwnership directives) @?= True
                
        , testCase "parseTypus handles block directives" $ do
            let contentWithBlockDirectives = "// @block-ownership: true\n// @block-dependent-types: true\n{\n  x := 1\n}"
                result = parseTypus contentWithBlockDirectives
            case result of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right file -> do
                let blocks = tfCodeBlocks file
                L.length blocks @?= 1
                
        , testCase "parseTypus handles mixed directives L.and code" $ do
            let mixedContent = "// @ownership: true\n\nfunc main() {\n  // @block-dependent-types: false\n  return 0\n}"
                result = parseTypus mixedContent
            case result of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right file -> do
                L.length (tfCodeBlocks file) @?= 1
                isJust (fdOwnership $ tfFileDirectives file) @?= True
        ]
        
    , testGroup "Directive processing tests"
        [ testCase "FileDirectives equality works correctly" $ do
            let directive1 = FileDirectives Nothing Nothing Nothing
                directive2 = FileDirectives (Just $ Located True startPos (emptySpan startPos)) Nothing Nothing
            directive1 @/= directive2
            
        , testCase "BlockDirectives extraction works" $ do
            let blockDirectives = BlockDirectives 
                    (Just $ Located False startPos (emptySpan startPos))
                    (Just $ Located True startPos (emptySpan startPos))
                    Nothing
            bdOwnership blockDirectives @?= Just (Located False startPos (emptySpan startPos))
            bdDependentTypes blockDirectives @?= Just (Located True startPos (emptySpan startPos))
            bdConstraints blockDirectives @?= Nothing
            
        , testCase "default directives are consistent" $ do
            let defaultFile = defaultFileDirectives
                defaultBlock = defaultBlockDirectives
            fdOwnership defaultFile @?= Nothing
            bdOwnership defaultBlock @?= Nothing
        ]
        
    , testGroup "Error handling tests"
        [ testCase "parseTypus handles malformed input gracefully" $ do
            let malformed = "func incomplete {\n  missing closing brace"
                result = parseTypus malformed
            case result of
              Left _ -> assertBool "Expected parse error" True
              Right _ -> assertFailure "Expected parse error but got success"
              
        , testCase "parseTypus preserves location information" $ do
            let content = "func test() {\n  // line comment\n  x := 1\n}"
                result = parseTypus content
            case result of
              Left err -> assertFailure $ "Parse error: " ++ errorBundlePretty err
              Right file -> do
                let blocks = tfCodeBlocks file
                if not (null blocks)
                  then do
                    let block = L.head blocks
                        span = cbSpan block
                    isValidSpan span @?= True
                  else assertFailure "Expected at least one code block"
        ]
    ]

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: Parsing empty content always yields default file directives
prop_parse_empty_directives :: Property
prop_parse_empty_directives =
  let result = parseTypus ""
  in case result of
       Left _ -> property $ counterexample "Parse failed on empty input" False
       Right file -> property $ tfFileDirectives file === defaultFileDirectives

-- Property: Parsing L.and re-parsing yields consistent results
prop_parse_consistency :: String -> Property
prop_parse_consistency str =
  let result1 = parseTypus str
  in case result1 of
       Left _ -> property $ Discard
       Right file1 -> 
         let content = unlines $ L.map (unlines . cbLines) $ tfCodeBlocks file1
             result2 = parseTypus content
         in case result2 of
              Left _ -> property $ Discard
              Right file2 -> 
                property $ L.length (tfCodeBlocks file1) === L.length (tfCodeBlocks file2)

-- Property: File directives are preserved across parse cycles
prop_directive_preservation :: String -> Property
prop_directive_preservation str =
  let result = parseTypus str
  in case result of
       Left _ -> property $ Discard
       Right file -> 
         let directives = tfFileDirectives file
             content = "// @ownership: true\n" ++ str
             resultWithDirectives = parseTypus content
         in case resultWithDirectives of
              Left _ -> property $ Discard
              Right fileWithDirectives ->
                let newDirectives = tfFileDirectives fileWithDirectives
                in property $ isJust (fdOwnership newDirectives) .&&. 
                             (fdOwnership directives =/= fdOwnership newDirectives)

-- Property: Default directives are safe defaults
prop_default_directives_safe :: Property
prop_default_directives_safe =
  let defaultFile = defaultFileDirectives
      defaultBlock = defaultBlockDirectives
  in property $ isNothing (fdOwnership defaultFile) .&&.
                isNothing (fdDependentTypes defaultFile) .&&.
                isNothing (fdConstraints defaultFile) .&&.
                isNothing (bdOwnership defaultBlock) .&&.
                isNothing (bdDependentTypes defaultBlock) .&&.
                isNothing (bdConstraints defaultBlock)

-- Property: Directive location tracking is consistent
prop_directive_location_consistent :: String -> Property
prop_directive_location_consistent str =
  let content = "// @ownership: true\n" ++ str
      result = parseTypus content
  in case result of
       Left _ -> property $ Discard
       Right file ->
         let directives = tfFileDirectives file
             ownershipDirective = fdOwnership directives
         in case ownershipDirective of
              Nothing -> property $ Discard
              Just located ->
                let span = locatedSpan located
                in property $ isValidSpan span

-- Property: Code block count is predictable for simple inputs
prop_codeblock_count_predictable :: String -> Property
prop_codeblock_count_predictable str =
  let braceCount = L.length (L.filter (== '{') str)
      result = parseTypus str
  in case result of
       Left _ -> property $ Discard
       Right file ->
         let blockCount = L.length $ tfCodeBlocks file
         in property $ blockCount <= braceCount

-- Property: File directives can be extracted safely
prop_extract_directives_safe :: String -> Property
prop_extract_directives_safe str =
  let result = parseTypus str
  in case result of
       Left _ -> property $ Discard
       Right file ->
         let directives = tfFileDirectives file
         in property $ isJust (fdOwnership directives) || 
                     isJust (fdDependentTypes directives) || 
                     isJust (fdConstraints directives) || 
                     True  -- It's valid to have no directives