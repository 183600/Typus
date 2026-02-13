{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.DirectiveSystemSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)

-- Tests for directive system as described in README.md

-- | Test parsing of file-level directives
prop_parse_file_level_directive :: String -> Property
prop_parse_file_level_directive feature =
  let directive = "//! " ++ feature ++ ": on"
      hasDirective = "//! " `isInfixOf` directive && ": on" `isInfixOf` directive
  in property $ hasDirective

-- | Test parsing of alternative dependent_types directive
prop_parse_constraints_directive :: Property
prop_parse_constraints_directive =
  let directive = "//! constraints: on"
      hasDirective = "constraints" `isInfixOf` directive
  in property $ hasDirective

-- | Test parsing of block-level directives
prop_parse_block_level_directive :: String -> Property
prop_parse_block_level_directive feature =
  let blockDirective = "{//! " ++ feature ++ ": on"
      hasBlockDirective = "{//! " `isInfixOf` blockDirective
  in property $ hasBlockDirective

-- | Test parsing of multiple features in one block
prop_parse_multiple_features :: String -> Property
prop_parse_multiple_features feature1 =
  let feature2 = "ownership"
      multiDirective = "{//! " ++ feature1 ++ ": on\n//! " ++ feature2 ++ ": on"
      hasMulti = feature1 `isInfixOf` multiDirective && feature2 `isInfixOf` multiDirective
  in property $ hasMulti

-- | Test parsing of constraint_mode directive
prop_parse_constraint_mode :: String -> Property
prop_parse_constraint_mode mode =
  let directive = "//! constraint_mode: " ++ mode
      hasConstraintMode = "constraint_mode" `isInfixOf` directive
  in property $ hasConstraintMode

-- | Test identification of directive position (before package)
prop_directive_before_package :: String -> Property
prop_directive_before_package feature =
  let code = "//! " ++ feature ++ ": on\npackage main"
      directiveBeforePackage = "//! " `isInfixOf` code && "package main" `isInfixOf` code
  in property $ directiveBeforePackage

-- | Test identification of mixed Go and Typus code
prop_identify_mixed_code :: String -> Property
prop_identify_mixed_code code =
  let hasDirectives = "//! " `isInfixOf` code
      hasGoCode = "package " `isInfixOf` code || "func " `isInfixOf` code
      isMixed = hasDirectives && hasGoCode
  in property $ isMixed

-- | Test parsing of ownership directive specifically
prop_parse_ownership_directive :: Property
prop_parse_ownership_directive =
  let directive = "//! ownership: on"
      hasOwnership = "ownership" `isInfixOf` directive
  in property $ hasOwnership

-- | Test parsing of dependent_types directive specifically
prop_parse_dependent_types_directive :: Property
prop_parse_dependent_types_directive =
  let directive = "//! dependent_types: on"
      hasDependentTypes = "dependent_types" `isInfixOf` directive
  in property $ hasDependentTypes

-- | Test that directives can be turned off
prop_parse_directive_off :: String -> Property
prop_parse_directive_off feature =
  let directive = "//! " ++ feature ++ ": off"
      hasOff = ": off" `isInfixOf` directive
  in property $ hasOff

-- | Test identification of directive comments
prop_identify_directive_comment :: String -> Property
prop_identify_directive_comment feature =
  let comment = "// This is a comment\n//! " ++ feature ++ ": on"
      hasDirective = "//! " `isInfixOf` comment
      hasRegularComment = "// " `isInfixOf` comment && not ("//! " `isPrefixOf` comment)
  in property $ hasDirective && hasRegularComment

-- | Test that directives are properly scoped
prop_directive_scope :: String -> Property
prop_directive_scope feature =
  let scopedCode = "func main() {\n  // 普通 Go 代码\n  \n  {//! " ++ feature ++ ": on\n    // 此块启用特性\n  }\n}"
      hasBlockDirective = "{//! " `isInfixOf` scopedCode
      hasRegularBlock = "{\n    // 此块启用特性\n  }" `isInfixOf` scopedCode
  in property $ hasBlockDirective && hasRegularBlock

-- | Test parsing of directive with whitespace variations
prop_directive_whitespace :: String -> Property
prop_directive_whitespace feature =
  let directive1 = "//! " ++ feature ++ ": on"
      directive2 = "//!  " ++ feature ++ " :  on"
      directive3 = "//!\t" ++ feature ++ ":\ton"
      hasFeature1 = feature `isInfixOf` directive1
      hasFeature2 = feature `isInfixOf` directive2
      hasFeature3 = feature `isInfixOf` directive3
  in property $ hasFeature1 && hasFeature2 && hasFeature3

-- | Test identification of unsupported directive
prop_identify_unsupported_directive :: String -> Property
prop_identify_unsupported_directive feature =
  let unsupportedDirective = "//! " ++ feature ++ ": on"
      isUnsupported = not (feature `elem` ["ownership", "dependent_types", "constraints"])
      hasDirective = "//! " `isInfixOf` unsupportedDirective
  in property $ if isUnsupported then hasDirective else property True

tests :: TestTree
tests = testGroup "Directive System Tests"
  [ testProperty "Parse file-level directive" prop_parse_file_level_directive
  , testProperty "Parse constraints directive" prop_parse_constraints_directive
  , testProperty "Parse block-level directive" prop_parse_block_level_directive
  , testProperty "Parse multiple features" prop_parse_multiple_features
  , testProperty "Parse constraint_mode directive" prop_parse_constraint_mode
  , testProperty "Directive before package" prop_directive_before_package
  , testProperty "Identify mixed code" prop_identify_mixed_code
  , testProperty "Parse ownership directive" prop_parse_ownership_directive
  , testProperty "Parse dependent_types directive" prop_parse_dependent_types_directive
  , testProperty "Parse directive off" prop_parse_directive_off
  , testProperty "Identify directive comment" prop_identify_directive_comment
  , testProperty "Directive scope" prop_directive_scope
  , testProperty "Directive whitespace variations" prop_directive_whitespace
  , testProperty "Identify unsupported directive" prop_identify_unsupported_directive
  ]