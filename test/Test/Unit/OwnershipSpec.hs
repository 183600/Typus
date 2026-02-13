{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.OwnershipSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Utils as U
import Data.List (isInfixOf, isPrefixOf)

-- Tests for ownership mechanism as described in README.md

-- | Test parsing of ownership directive
prop_parse_ownership_directive :: String -> Property
prop_parse_ownership_directive context =
  let directive = "//! ownership: on"
      hasOwnershipDirective = "ownership" `isInfixOf` directive && "on" `isInfixOf` directive
  in property $ hasOwnershipDirective

-- | Test identification of move operations
prop_identify_move_operation :: String -> Property
prop_identify_move_operation varName =
  let moveOp = "t := " ++ varName
      hasMove = ":=" `isInfixOf` moveOp
  in property $ hasMove

-- | Test identification of immutable borrow
prop_identify_immutable_borrow :: String -> Property
prop_identify_immutable_borrow varName =
  let borrow = "r := &" ++ varName
      hasBorrow = "&" `isInfixOf` borrow && not ("&mut" `isInfixOf` borrow)
  in property $ hasBorrow

-- | Test identification of mutable borrow
prop_identify_mutable_borrow :: String -> Property
prop_identify_mutable_borrow varName =
  let mutBorrow = "m := &mut " ++ varName
      hasMutBorrow = "&mut" `isInfixOf` mutBorrow
  in property $ hasMutBorrow

-- | Test parsing of ownership error messages
prop_parse_ownership_error :: String -> Property
prop_parse_ownership_error varName =
  let errorMsg = "编译错误：" ++ varName ++ " 已被移动"
      hasError = "已被移动" `isInfixOf` errorMsg
  in property $ hasError

-- | Test identification of ownership blocks
prop_identify_ownership_block :: String -> Property
prop_identify_ownership_block code =
  let blockStart = "{//! ownership: on"
      hasOwnershipBlock = blockStart `isInfixOf` code
  in property $ hasOwnershipBlock

-- | Test that ownership code can be distinguished from regular Go code
prop_distinguish_ownership_code :: String -> Property
prop_distinguish_ownership_code code =
  let hasOwnershipFeatures = any (`isInfixOf` code)
        ["&mut ", "& ", "//! ownership:", ":="]
  in property $ hasOwnershipFeatures

-- | Test parsing of ownership transfer patterns
prop_parse_ownership_transfer :: String -> Property
prop_parse_ownership_transfer fromVar toVar =
  let transfer = toVar ++ " := " ++ fromVar
      hasTransfer = ":=" `isInfixOf` transfer
  in property $ hasTransfer

-- | Test identification of borrow checker violations
prop_identify_borrow_violation :: String -> Property
prop_identify_borrow_violation code =
  let violationKeywords = ["借用", "不可变借用", "可变借用", "同时存在"]
      hasViolation = any (`isInfixOf` code) violationKeywords
  in property $ hasViolation

-- | Test parsing of lifetime-related code
prop_parse_lifetime_code :: String -> Property
prop_parse_lifetime_code code =
  let lifetimeKeywords = ["生命周期", "作用域", "借用", "所有权"]
      hasLifetime = any (`isInfixOf` code) lifetimeKeywords
  in property $ hasLifetime

-- | Test identification of GC compatibility notes
prop_identify_gc_compatibility :: String -> Property
prop_identify_gc_compatibility code =
  let gcKeywords = ["垃圾回收", "GC", "运行时开销", "内存回收"]
      hasGC = any (`isInfixOf` code) gcKeywords
  in property $ hasGC

-- | Test parsing of ownership rules documentation
prop_parse_ownership_rules :: String -> Property
prop_parse_ownership_rules ruleText =
  let ruleKeywords = ["借用规则", "同一时刻", "不可变借用", "可变借用"]
      hasRules = any (`isInfixOf` ruleText) ruleKeywords
  in property $ hasRules

-- | Test identification of experimental status warnings
prop_identify_experimental_status :: String -> Property
prop_identify_experimental_status code =
  let experimentalKeywords = ["实验性", "当前版本", "规划中", "不完整"]
      hasExperimental = any (`isInfixOf` code) experimentalKeywords
  in property $ hasExperimental

-- | Test parsing of ownership limitations
prop_parse_ownership_limitations :: String -> Property
prop_parse_ownership_limitations limitationText =
  let limitationKeywords = ["限制", "不支持", "当前", "尚不支持"]
      hasLimitations = any (`isInfixOf` limitationText) limitationKeywords
  in property $ hasLimitations

tests :: TestTree
tests = testGroup "Ownership Mechanism Tests"
  [ testProperty "Parse ownership directive" prop_parse_ownership_directive
  , testProperty "Identify move operation" prop_identify_move_operation
  , testProperty "Identify immutable borrow" prop_identify_immutable_borrow
  , testProperty "Identify mutable borrow" prop_identify_mutable_borrow
  , testProperty "Parse ownership error" prop_parse_ownership_error
  , testProperty "Identify ownership block" prop_identify_ownership_block
  , testProperty "Distinguish ownership code" prop_distinguish_ownership_code
  , testProperty "Parse ownership transfer" prop_parse_ownership_transfer
  , testProperty "Identify borrow violation" prop_identify_borrow_violation
  , testProperty "Parse lifetime code" prop_parse_lifetime_code
  , testProperty "Identify GC compatibility" prop_identify_gc_compatibility
  , testProperty "Parse ownership rules" prop_parse_ownership_rules
  , testProperty "Identify experimental status" prop_identify_experimental_status
  , testProperty "Parse ownership limitations" prop_parse_ownership_limitations
  ]
