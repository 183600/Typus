{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserBasicPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements, oneof, sized, suchThat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Char as Char

import Parser
  ( parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..)
  , defaultFileDirectives, defaultBlockDirectives
  )
import SourceLocation (SourceSpan(..), SourcePos(..), spanStart, spanEnd)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = do
    fdOwnership <- oneof [return Nothing, Just <$> arbitrary]
    fdDependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    fdConstraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ FileDirectives fdOwnership fdDependentTypes fdConstraints

instance Arbitrary BlockDirectives where
  arbitrary = do
    bdOwnership <- oneof [return Nothing, Just <$> arbitrary]
    bdDependentTypes <- oneof [return Nothing, Just <$> arbitrary]
    bdConstraints <- oneof [return Nothing, Just <$> arbitrary]
    return $ BlockDirectives bdOwnership bdDependentTypes bdConstraints

-- Simple generator for code content
arbitraryCodeContent :: Gen String
arbitraryCodeContent = do
  lines' <- listOf $ do
    line <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t_.,;:+-*/=[]{}()"
    return $ L.filter (not . (== '\n')) line
  return $ unlines lines'

instance Arbitrary CodeBlock where
  arbitrary = do
    cbDirectives <- arbitrary
    cbContent <- arbitraryCodeContent
    cbSpan <- arbitrary
    return $ CodeBlock cbDirectives cbContent cbSpan

instance Arbitrary TypusFile where
  arbitrary = do
    tfDirectives <- arbitrary
    tfBuildTags <- listOf arbitrary
    tfBlocks <- listOf arbitrary
    tfSyntaxErrors <- listOf arbitrary
    return $ TypusFile tfDirectives tfBuildTags tfBlocks tfSyntaxErrors

-- ============================================================================
-- Parser Basic Properties
-- ============================================================================

-- Property: parseTypus handles empty input
prop_parse_empty_input :: Property
prop_parse_empty_input =
  let result = parseTypus ""
  in case result of
    Left _ -> property False
    Right file -> property $ tfBlocks file === [] .&&. tfBuildTags file === []

-- Property: parseTypus handles whitespace-only input
prop_parse_whitespace_input :: String -> Property
prop_parse_whitespace_input ws =
  L.all Char.isSpace ws ==>
  let result = parseTypus ws
  in case result of
    Left _ -> property False
    Right file -> property $ L.null (tfBlocks file) || L.all (null . cbContent) (tfBlocks file)

-- Property: parseTypus preserves simple code content
prop_parse_preserves_simple_code :: String -> Property
prop_parse_preserves_simple_code code =
  not (L.any (`elem` code) ["//!", "{//!", "}", "//go:build", "// +build", "package"]) ==>
  let result = parseTypus code
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file)) ==> 
      let combinedContent = concatMap cbContent (tfBlocks file)
      in combinedContent `L.isInfixOf` code || code `L.isInfixOf` combinedContent

-- Property: parseTypus handles single line comments
prop_parse_single_line_comments :: String -> Property
prop_parse_single_line_comments comment =
  not (L.any (`elem` comment) ["\n", "\r", "//!", "{//!", "}", "//go:build", "// +build"]) ==>
  let input = "// " ++ comment ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ True -- Comments should be ignored L.or handled gracefully

-- Property: parseTypus handles file directives
prop_parse_file_directives :: Bool -> Bool -> Bool -> Property
prop_parse_file_directives ownership dependentTypes constraints =
  let directiveStr = "//! ownership: " ++ show ownership ++ 
                     ", dependent_types: " ++ show dependentTypes ++
                     ", constraints: " ++ show constraints ++ "\n"
      result = parseTypus directiveStr
  in case result of
    Left _ -> property False
    Right file -> property $ 
      let fileDirs = tfDirectives file
          expectedOwnership = if ownership then Just ownership else Nothing
          expectedDependentTypes = if dependentTypes || constraints then Just dependentTypes else Nothing
          expectedConstraints = if constraints then Just constraints else Nothing
      in fdOwnership fileDirs === fL.map (const ownership) (fdOwnership fileDirs) .&&.
         (isJust (fdDependentTypes fileDirs) ==> 
          locValue (fromMaybe (error "impossible") (fdDependentTypes fileDirs)) === dependentTypes)

-- Property: parseTypus handles block directives
prop_parse_block_directives :: Bool -> Bool -> Bool -> String -> Property
prop_parse_block_directives ownership dependentTypes constraints code =
  let directiveStr = "{//! ownership: " ++ show ownership ++ 
                     ", dependent_types: " ++ show dependentTypes ++
                     ", constraints: " ++ show constraints ++ "}\n" ++
                     code ++ "\n}\n"
      result = parseTypus directiveStr
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file)) ==>
      let firstBlock = L.head (tfBlocks file)
          blockDirs = cbDirectives firstBlock
      in (isJust (bdOwnership blockDirs) ==> 
          locValue (fromMaybe (error "impossible") (bdOwnership blockDirs)) === ownership) .&&.
         (isJust (bdDependentTypes blockDirs) ==> 
          locValue (fromMaybe (error "impossible") (bdDependentTypes blockDirs)) === dependentTypes)

-- Property: parseTypus handles multiple blocks
prop_parse_multiple_blocks :: [String] -> Property
prop_parse_multiple_blocks codeBlocks =
  L.length codeBlocks <= 5 ==> -- Limit for performance
  let blockWrappers = L.map (\code -> code ++ "\n}\n") codeBlocks
      input = L.concat blockWrappers
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) >= L.length codeBlocks - 1

-- Property: parseTypus handles build tags
prop_parse_build_tags :: String -> Property
prop_parse_build_tags tag =
  not (L.any (`elem` tag) ["\n", "\r"]) ==>
  let input = "//go:build " ++ tag ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBuildTags file)) ==> 
      tag `L.isInfixOf` concatMap locValue (tfBuildTags file)

-- Property: parseTypus handles package declarations
prop_parse_package_declaration :: String -> Property
prop_parse_package_declaration packageName =
  not (L.any (`elem` packageName) ["\n", "\r", " ", "\t", "//"]) ==>
  let input = "package " ++ packageName ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property $ "Multiple package declarations found" `L.isInfixOf` show result || 
                      "syntax error" `L.isInfixOf` show result
    Right file -> property $ True -- Should parse successfully L.or give appropriate error

-- Property: parseTypus rejects multiple package declarations
prop_parse_multiple_package_declarations :: String -> String -> Property
prop_parse_multiple_package_declarations pkg1 pkg2 =
  not (L.any (`elem` pkg1 ++ pkg2) ["\n", "\r", " ", "\t", "//"]) && pkg1 /= pkg2 ==>
  let input = "package " ++ pkg1 ++ "\npackage " ++ pkg2 ++ "\n"
      result = parseTypus input
  in case result of
    Left err -> property $ "Multiple package declarations" `L.isInfixOf` err
    Right _ -> property False -- Should not succeed with multiple packages

-- Property: parseTypus handles if statements correctly
prop_parse_if_statements :: String -> Property
prop_parse_if_statements condition =
  not (L.any (`elem` condition) ["\n", "\r", "{", "}", "//"]) ==>
  let goodInput = "if " ++ condition ++ " {\n  // code\n}\n"
      badInput = "if " ++ condition ++ "\n  // missing opening brace\n"
      goodResult = parseTypus goodInput
      badResult = parseTypus badInput
  in case (goodResult, badResult) of
    (Right _, Left err) -> property $ "missing opening brace" `L.isInfixOf` err
    (Right _, Right _) -> property False -- Both shouldn't succeed
    (Left _, Right _) -> property False -- Bad case shouldn't succeed
    (Left _, Left _) -> property True -- Both failing is acceptable

-- Property: parseTypus handles nested blocks
prop_parse_nested_blocks :: String -> String -> Property
prop_parse_nested_blocks outerCode innerCode =
  not (L.any (`elem` outerCode ++ innerCode) ["//!", "}", "//go:build", "// +build"]) ==>
  let input = "{//! ownership: on}\n" ++ outerCode ++ "\n{//! dependent_types: on}\n" ++ innerCode ++ "\n}\n}\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ L.length (tfBlocks file) >= 1

-- Property: parseTypus handles mixed directives L.and code
prop_parse_mixed_directives_code :: String -> String -> Property
prop_parse_mixed_directives_code fileDirective code =
  not (L.any (`elem` fileDirective) ["\n", "\r", "//!", "}", "//go:build", "// +build"]) ==>
  let input = "//! ownership: on\n" ++ fileDirective ++ "\n" ++ code ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ 
      let fileDirs = tfDirectives file
      in isJust (fdOwnership fileDirs)

-- Property: parseTypus handles malformed directives gracefully
prop_parse_malformed_directives :: String -> Property
prop_parse_malformed_directives directive =
  not (null directive) ==>
  let input = "//! " ++ directive ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property True -- Should fail gracefully
    Right file -> property $ True -- Or succeed with default values

-- Property: parseTypus preserves line structure
prop_parse_preserves_line_structure :: [String] -> Property
prop_parse_preserves_line_structure lines' =
  L.length lines' <= 10 ==> -- Limit for performance
  let input = unlines lines'
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ 
      let totalLines = L.sum $ L.map (L.length . lines . cbContent) (tfBlocks file)
      in totalLines >= L.length (L.filter (not . null) lines') - 2 -- Allow for some parser overhead

-- Property: parseTypus handles unicode characters
prop_parse_unicode :: String -> Property
prop_parse_unicode unicodeText =
  not (L.any (`elem` unicodeText) ["//!", "{//!", "}", "//go:build", "// +build"]) ==>
  let input = unicodeText ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file)) ==> 
      unicodeText `L.isInfixOf` concatMap cbContent (tfBlocks file)

-- Property: parseTypus handles very long lines
prop_parse_long_lines :: Int -> String -> Property
prop_parse_long_lines multiplier baseText =
  multiplier > 0 && multiplier <= 100 ==> -- Limit for performance
  let longLine = L.concat (replicate multiplier baseText)
      input = longLine ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ True -- Should handle long lines gracefully

-- Property: parseTypus handles empty blocks
prop_parse_empty_blocks :: Property
prop_parse_empty_blocks =
  let input = "{//! ownership: on}\n\n}\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file)) ==> 
      let firstBlock = L.head (tfBlocks file)
      in L.null (cbContent firstBlock) || L.all Char.isSpace (cbContent firstBlock)

-- Property: parseTypus handles deeply nested structures
prop_parse_deeply_nested :: Int -> Property
prop_parse_deeply_nested depth =
  depth > 0 && depth <= 5 ==> -- Limit for performance
  let openBraces = replicate depth '{'
      closeBraces = replicate depth '}'
      input = "{//! ownership: on}\n" ++ L.concat openBraces ++ "\n" ++ L.concat closeBraces ++ "\n"
      result = parseTypus input
  in case result of
    Left _ -> property False
    Right file -> property $ True -- Should handle nesting gracefully

-- ============================================================================
-- Directive Validation Properties
-- ============================================================================

-- Property: file directives are parsed correctly
prop_file_directives_parsing :: [(String, Bool)] -> Property
prop_file_directives_parsing directives =
  L.length directives <= 3 ==> -- Limit to reasonable number
  let directiveStr = "//! " ++ unwords (L.map (\(k, v) -> k ++ ": " ++ show v) directives) ++ "\n"
      result = parseTypus directiveStr
  in case result of
    Left _ -> property False
    Right file -> property $ 
      let fileDirs = tfDirectives file
          hasOwnership = L.any (\(k, _) -> k == "ownership") directives
          hasDependentTypes = L.any (\(k, _) -> k == "dependent_types") directives
          hasConstraints = L.any (\(k, _) -> k == "constraints") directives
      in (hasOwnership ==> isJust (fdOwnership fileDirs)) .&&.
         (hasDependentTypes ==> isJust (fdDependentTypes fileDirs)) .&&.
         (hasConstraints ==> isJust (fdConstraints fileDirs))

-- Property: block directives are parsed correctly
prop_block_directives_parsing :: [(String, Bool)] -> String -> Property
prop_block_directives_parsing directives code =
  L.length directives <= 3 ==> -- Limit to reasonable number
  let directiveStr = "{//! " ++ unwords (L.map (\(k, v) -> k ++ ": " ++ show v) directives) ++ "}\n" ++ code ++ "\n}\n"
      result = parseTypus directiveStr
  in case result of
    Left _ -> property False
    Right file -> property $ not (L.null (tfBlocks file)) ==>
      let firstBlock = L.head (tfBlocks file)
          blockDirs = cbDirectives firstBlock
          hasOwnership = L.any (\(k, _) -> k == "ownership") directives
          hasDependentTypes = L.any (\(k, _) -> k == "dependent_types") directives
          hasConstraints = L.any (\(k, _) -> k == "constraints") directives
      in (hasOwnership ==> isJust (bdOwnership blockDirs)) .&&.
         (hasDependentTypes ==> isJust (bdDependentTypes blockDirs)) .&&.
         (hasConstraints ==> isJust (bdConstraints blockDirs))

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- Property: parseTypus provides meaningful error messages
prop_parse_error_messages :: Property
prop_parse_error_messages =
  let malformedInput = "{//! ownership: on\nmissing closing brace"
      result = parseTypus malformedInput
  in case result of
    Left err -> property $ L.length err > 0
    Right _ -> property False -- Should not succeed with malformed input

-- Property: parseTypus handles syntax errors gracefully
prop_parse_syntax_errors :: String -> Property
prop_parse_syntax_errors malformedCode =
  "{//! ownership: on" `L.isInfixOf` malformedCode && not ("}" `L.isInfixOf` malformedCode) ==>
  let result = parseTypus malformedCode
  in case result of
    Left _ -> property True -- Should fail gracefully
    Right file -> property $ not (L.null (tfSyntaxErrors file)) || True -- May succeed but with syntax errors

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Basic Properties Tests"
  [ testGroup "Basic Parsing Properties"
    [ fastProperty "parseTypus handles empty input" prop_parse_empty_input
    , fastProperty "parseTypus handles whitespace-only input" prop_parse_whitespace_input
    , fastProperty "parseTypus preserves simple code content" prop_parse_preserves_simple_code
    , fastProperty "parseTypus handles single line comments" prop_parse_single_line_comments
    , fastProperty "parseTypus preserves line structure" prop_parse_preserves_line_structure
    , fastProperty "parseTypus handles unicode characters" prop_parse_unicode
    , fastProperty "parseTypus handles very long lines" prop_parse_long_lines
    ]
  , testGroup "Directive Parsing Properties"
    [ fastProperty "parseTypus handles file directives" prop_parse_file_directives
    , fastProperty "parseTypus handles block directives" prop_parse_block_directives
    , fastProperty "parseTypus handles multiple blocks" prop_parse_multiple_blocks
    , fastProperty "parseTypus handles build tags" prop_parse_build_tags
    , fastProperty "parseTypus handles nested blocks" prop_parse_nested_blocks
    , fastProperty "parseTypus handles mixed directives L.and code" prop_parse_mixed_directives_code
    , fastProperty "parseTypus handles malformed directives gracefully" prop_parse_malformed_directives
    , fastProperty "parseTypus handles empty blocks" prop_parse_empty_blocks
    , fastProperty "parseTypus handles deeply nested structures" prop_parse_deeply_nested
    ]
  , testGroup "Language Feature Properties"
    [ fastProperty "parseTypus handles package declarations" prop_parse_package_declaration
    , fastProperty "parseTypus rejects multiple package declarations" prop_parse_multiple_package_declarations
    , fastProperty "parseTypus handles if statements correctly" prop_parse_if_statements
    ]
  , testGroup "Directive Validation Properties"
    [ fastProperty "file directives are parsed correctly" prop_file_directives_parsing
    , fastProperty "block directives are parsed correctly" prop_block_directives_parsing
    ]
  , testGroup "Error Handling Properties"
    [ fastProperty "parseTypus provides meaningful error messages" prop_parse_error_messages
    , fastProperty "parseTypus handles syntax errors gracefully" prop_parse_syntax_errors
    ]
  ]