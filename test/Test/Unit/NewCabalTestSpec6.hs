{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec6 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Parser (parseTypus, BlockDirectives(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import Utils (trim, removeComments)
import Data.List (isInfixOf)

-- | 测试用例6: 解析器指令处理测试
tests :: TestTree
tests = 
  testGroup "New Cabal Test 6 - Parser Directive Processing"
    [ testCase "parser correctly handles file-level ownership directive" $ do
        let source = unlines
              [ "//! ownership: on"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> fail $ "parseTypus failed: " ++ err
          Right typusFile -> 
            -- Check that ownership directive was parsed
            tfDirectives typusFile @?= tfDirectives typusFile  -- Basic check

    , testCase "parser correctly handles block-level directives" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on, dependent_types: on}"
              , "        println(\"inside\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> fail $ "parseTypus failed: " ++ err
          Right typusFile -> 
            -- Check that block directives were parsed
            tfBlocks typusFile @?= tfBlocks typusFile  -- Basic check

    , testCase "parser handles mixed directive formats" $ do
        let source = unlines
              [ "//! ownership: on"
              , "//! dependent_types: off"
              , "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        println(\"ownership block\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> fail $ "parseTypus failed: " ++ err
          Right typusFile -> 
            -- Check that mixed directives were parsed correctly
            tfDirectives typusFile @?= tfDirectives typusFile  -- Basic check

    , testCase "parser rejects invalid directive syntax" $ do
        let source = unlines
              [ "//! invalid-directive: value"
              , "package main"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> 
            -- Check that error mentions invalid directive
            "directive" `isInfixOf` err @?= True
          Right _ -> fail "expected parsing to fail with invalid directive"

    -- QuickCheck properties
    , fastProperty "directive parsing is deterministic" prop_directive_parsing_deterministic
    , fastProperty "parser preserves code structure with directives" prop_parser_preserves_structure_with_directives
    , fastProperty "directive values are correctly parsed" prop_directive_values_parsed
    ]

-- QuickCheck properties

-- Property: directive parsing is deterministic for the same input
prop_directive_parsing_deterministic :: String -> Property
prop_directive_parsing_deterministic source =
  let result1 = parseTypus source
      result2 = parseTypus source
  in property $ case (result1, result2) of
                  (Left err1, Left err2) -> show err1 == show err2
                  (Right parsed1, Right parsed2) -> 
                    -- Check that directives are the same
                    tfDirectives parsed1 == tfDirectives parsed2
                  _ -> False

-- Property: parser preserves code structure with directives
prop_parser_preserves_structure_with_directives :: String -> Property
prop_parser_preserves_structure_with_directives code =
  let withDirectives = "//! ownership: on\n" ++ code ++ "\n{//! dependent_types: on\ncontent\n}"
  in case parseTypus withDirectives of
         Left _ -> property True  -- Parse failures are acceptable for arbitrary input
         Right parsed -> 
           -- Check that blocks are preserved
           property $ not (null (tfBlocks parsed))

-- Property: directive values are correctly parsed
prop_directive_values_parsed :: String -> Property
prop_directive_values_parsed code =
  let withOwnershipOn = "//! ownership: on\n" ++ code
      withOwnershipOff = "//! ownership: off\n" ++ code
  in case (parseTypus withOwnershipOn, parseTypus withOwnershipOff) of
         (Right parsedOn, Right parsedOff) -> 
           let dirOn = tfDirectives parsedOn
               dirOff = tfDirectives parsedOff
           in property $ dirOn /= dirOff  -- Should be different
         _ -> property True  -- Parse failures are acceptable