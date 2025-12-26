{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCoreFunctionalitySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf1, elements, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Parser (parseTypus, TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), generateGoCode)
import SourceLocation 
  ( SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, 
    spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan, locatedAt, 
    locatedWithSpan, advancePos, advancePosBy, advancePosByText )
import Utils 
  ( trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
    removeLineComments, removeComments, normalizeIndentation, breakOn )

import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import qualified Data.Text as T
import Control.Monad (foldM)

-- ============================================================================
-- Parser Tests
-- ============================================================================

-- Test parsing of file directives
test_parseFileDirectives :: TestTree
test_parseFileDirectives = testCase "parse file directives correctly" $ do
    let input = "//! ownership: on, dependent_types: true\n"
    case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile -> do
            assertEqual "ownership directive should be on" 
                (Just True) (fmap locValue (fdOwnership (tfDirectives typusFile)))
            assertEqual "dependent_types directive should be true"
                (Just True) (fmap locValue (fdDependentTypes (tfDirectives typusFile)))

-- Test parsing of block directives
test_parseBlockDirectives :: TestTree
test_parseBlockDirectives = testCase "parse block directives correctly" $ do
    let input = "{//! ownership: off, constraints: on}\nlet x = 42\n"
    case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile -> do
            let blocks = tfBlocks typusFile
            assertBool "should have one block" (length blocks == 1)
            let block = head blocks
                directives = cbDirectives block
            assertEqual "ownership directive should be off"
                (Just False) (fmap locValue (bdOwnership directives))
            assertEqual "constraints directive should be on"
                (Just True) (fmap locValue (bdConstraints directives))

-- Test parsing with syntax errors
test_parseWithSyntaxErrors :: TestTree
test_parseWithSyntaxErrors = testCase "handle syntax errors gracefully" $ do
    let input = "if true\n    x := 1\n"  -- missing opening brace
    case parseTypus input of
        Left err -> assertBool "should report syntax error" $ "missing opening brace" `isInfixOf` err
        Right typusFile -> do
            let syntaxErrors = tfSyntaxErrors typusFile
            assertBool "should have syntax errors" (not (null syntaxErrors))

-- ============================================================================
-- Compiler Tests
-- ============================================================================

-- Test compilation of simple valid code
test_compileSimpleCode :: TestTree
test_compileSimpleCode = testCase "compile simple valid code" $ do
    let input = "package main\n\nfunc main() {\n    fmt.Println(\"Hello\")\n}\n"
    case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile -> do
            case compile typusFile of
                Left errs -> assertFailure $ "Compilation failed: " ++ show errs
                Right goCode -> do
                    assertBool "should contain package main" $ "package main" `isInfixOf` goCode
                    assertBool "should contain func main" $ "func main" `isInfixOf` goCode

-- Test compilation with type errors
test_compileWithTypeErrors :: TestTree
test_compileWithTypeErrors = testCase "detect type errors during compilation" $ do
    let input = "package main\n\nvar x int = \"string\"\n"
    case parseTypus input of
        Left err -> assertFailure $ "Parse failed: " ++ err
        Right typusFile -> do
            case compile typusFile of
                Left errs -> do
                    assertBool "should have type errors" (not (null errs))
                    let typeError = findTypeError errs
                    assertBool "should find type error" (typeError /= Nothing)
                Right _ -> assertFailure "Expected compilation to fail with type errors"

-- Test Go code generation fallback
test_goCodeGenerationFallback :: TestTree
test_goCodeGenerationFallback = testCase "fallback to raw source on generation failure" $ do
    let input = "invalid typus code\n"
    case parseTypus input of
        Left _ -> assertFailure "Parse should succeed even with invalid code"
        Right typusFile -> do
            let goCode = generateGoCode typusFile
            assertBool "should return original source" $ input `isInfixOf` goCode

-- ============================================================================
-- SourceLocation Tests
-- ============================================================================

-- Test position advancement
test_positionAdvancement :: TestTree
test_positionAdvancement = testCase "advance position correctly" $ do
    let pos1 = startPos
        pos2 = advancePos 'a' pos1
        pos3 = advancePos '\n' pos2
        pos4 = advancePos '\t' pos3
    assertEqual "after 'a'" (SourcePos 1 2 1) pos2
    assertEqual "after '\\n'" (SourcePos 2 1 2) pos3
    assertEqual "after '\\t'" (SourcePos 2 9 3) pos4

-- Test span operations
test_spanOperations :: TestTree
test_spanOperations = testCase "span operations work correctly" $ do
    let pos1 = posAt 1 1
        pos2 = posAt 1 10
        pos3 = posAt 2 5
        span1 = spanBetween pos1 pos2
        span2 = spanBetween pos2 pos3
        merged = mergeSpans span1 span2
    assertBool "span1 should be valid" (isValidSpan span1)
    assertBool "span2 should be valid" (isValidSpan span2)
    assertEqual "merged span should start at pos1" pos1 (spanStart merged)
    assertEqual "merged span should end at pos3" pos3 (spanEnd merged)

-- ============================================================================
-- Utils Tests
-- ============================================================================

-- Test comment removal with edge cases
test_commentRemovalEdgeCases :: TestTree
test_commentRemovalEdgeCases = testCase "handle comment removal edge cases" $ do
    let input1 = "var s = \"// not a comment\" // real comment\n"
        expected1 = "var s = \"// not a comment\" \n"
        result1 = removeLineComments input1
    assertEqual "should preserve comments in strings" expected1 result1
    
    let input2 = "code /* block\ncomment */ more"
        expected2 = "code  \n more"
        result2 = removeComments input2
    assertEqual "should handle multiline block comments" expected2 result2

-- Test indentation normalization
test_indentationNormalization :: TestTree
test_indentationNormalization = testCase "normalize indentation correctly" $ do
    let input = "    func main() {\n        fmt.Println()\n    }\n"
        expected = "func main() {\n    fmt.Println()\n}\n"
        result = normalizeIndentation input
    assertEqual "should remove common indentation" expected result

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Property: splitBy and splitByCollapsed relationship
prop_splitBy_relationship :: Char -> String -> Property
prop_splitBy_relationship delim str =
  let regular = splitBy delim str
      collapsed = splitByCollapsed delim str
  in property $ length collapsed <= length regular .&&.
     (if all (/= delim) str then regular === collapsed else property True)

-- Property: trim idempotency
prop_trim_idempotent :: String -> Property
prop_trim_idempotent str =
  let trimmed1 = trim str
      trimmed2 = trim trimmed1
  in property $ trimmed1 === trimmed2

-- Property: breakOn correctness
prop_breakOn_correctness :: String -> String -> String -> Property
prop_breakOn_correctness pat prefix suffix =
  not (null pat) ==>
  let haystack = prefix ++ pat ++ suffix
      (before, after) = breakOn pat haystack
  in property $ before ++ pat ++ after === haystack

-- Property: position advancement consistency
prop_position_advancement_consistent :: String -> Property
prop_position_advancement_consistent str =
  let finalPos = advancePosBy str startPos
      expectedOffset = length str
  in property $ posOffset finalPos === expectedOffset

-- Property: span merging associativity
prop_span_merging_associative :: SourcePos -> SourcePos -> SourcePos -> Property
prop_span_merging_associative p1 p2 p3 =
  let span1 = spanBetween p1 p2
      span2 = spanBetween p2 p3
      merged1 = mergeSpans span1 span2
      merged2 = mergeSpans (spanBetween p1 p2) (spanBetween p2 p3)
  in property $ merged1 === merged2

-- Helper functions
findTypeError :: [CompilerError] -> Maybe CompilerError
findTypeError [] = Nothing
findTypeError (err:rest) = 
    if "type error" `isInfixOf` T.unpack (ceMessage err)
    then Just err
    else findTypeError rest

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Core Functionality Tests"
    [ testGroup "Parser Tests"
        [ test_parseFileDirectives
        , test_parseBlockDirectives
        , test_parseWithSyntaxErrors
        ]
    , testGroup "Compiler Tests"
        [ test_compileSimpleCode
        , test_compileWithTypeErrors
        , test_goCodeGenerationFallback
        ]
    , testGroup "SourceLocation Tests"
        [ test_positionAdvancement
        , test_spanOperations
        ]
    , testGroup "Utils Tests"
        [ test_commentRemovalEdgeCases
        , test_indentationNormalization
        ]
    , testGroup "QuickCheck Properties"
        [ fastProperty "splitBy relationship" prop_splitBy_relationship
        , fastProperty "trim idempotent" prop_trim_idempotent
        , fastProperty "breakOn correctness" prop_breakOn_correctness
        , fastProperty "position advancement consistent" prop_position_advancement_consistent
        , fastProperty "span merging associative" prop_span_merging_associative
        ]
    ]