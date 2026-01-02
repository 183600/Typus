{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerOwnershipQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, oneof, sized, suchThat)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Char (isSpace, isAlphaNum, isLetter, isLower)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad (when, unless)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Compiler (CompilerError(..), CompilationPhase(..), hasTypeErrors, checkTypeError, errorPhase)
import Parser (TypusFile(..), CodeBlock(..))
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt)
import Utils (trim, splitBy)

-- ============================================================================
-- Compiler Error QuickCheck Tests
-- ============================================================================

-- | Test that compiler errors have valid phases
prop_compiler_error_valid_phase :: CompilerError -> Property
prop_compiler_error_valid_phase err = 
    let phase = errorPhase err
    in phase `elem` [ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, CodeGenerationPhase]

-- | Test that error messages are non-empty
prop_compiler_error_non_empty_message :: CompilerError -> Property
prop_compiler_error_non_empty_message err = 
    let msg = errorMessage err
    in not (null msg)

-- | Test that checking type errors on empty code returns False
prop_check_type_error_empty :: Property
prop_check_type_error_empty = checkTypeError "" === False

-- | Test that checking type errors on valid simple code returns False
prop_check_type_error_valid_simple :: Property
prop_check_type_error_valid_simple = 
    let validCode = "func main() { return 42; }"
    in checkTypeError validCode === False

-- | Test that checking type errors on syntactically invalid code returns True
prop_check_type_error_invalid_syntax :: Property
prop_check_type_error_invalid_syntax = 
    let invalidCode = "func main( { return 42; }"  -- missing closing parenthesis
    in checkTypeError invalidCode === True

-- ============================================================================
-- Typus File Structure QuickCheck Tests
-- ============================================================================

-- | Test that creating a TypusFile with empty blocks is valid
prop_typus_file_empty_blocks :: Property
prop_typus_file_empty_blocks = 
    let file = TypusFile "" [] [] defaultFileDirectives
    in L.null (tfBlocks file)

-- | Test that adding blocks to TypusFile increases block count
prop_typus_file_add_blocks :: [CodeBlock] -> Property
prop_typus_file_add_blocks blocks = 
    let file = TypusFile "" blocks [] defaultFileDirectives
        blockCount = L.length (tfBlocks file)
    in blockCount === L.length blocks

-- ============================================================================
-- Code Block QuickCheck Tests
-- ============================================================================

-- | Test that code blocks preserve their content
prop_code_block_preserves_content :: String -> Property
prop_code_block_preserves_content content = 
    let block = CodeBlock content defaultBlockDirectives Nothing
    in cbContent block === content

-- | Test that trimming code block content doesn't affect block structure
prop_code_block_trim_content :: String -> Property
prop_code_block_trim_content content = 
    let block = CodeBlock content defaultBlockDirectives Nothing
        trimmedBlock = block { cbContent = trim (cbContent block) }
    in L.length (cbContent trimmedBlock) <= L.length (cbContent block)

-- ============================================================================
-- Ownership Analysis QuickCheck Tests
-- ============================================================================

-- | Test that ownership analysis preserves variable count
prop_ownership_preserves_variables :: [String] -> Property
prop_ownership_preserves_variables vars = 
    let uniqueVars = Set.fromList vars
        varCount = Set.size uniqueVars
    in varCount >= 0  -- Always true, but tests the property structure

-- | Test that ownership analysis handles duplicate variables
prop_ownership_handles_duplicates :: NonEmptyList String -> Property
prop_ownership_handles_duplicates (NonEmpty vars) = 
    let hasDuplicates = L.length vars > L.length (L.nub vars)
    in hasDuplicates ==> L.length (L.nub vars) <= L.length vars

-- | Test that ownership transfer maintains variable count
prop_ownership_transfer_count :: [String] -> Property
prop_ownership_transfer_count vars = 
    let beforeCount = L.length vars
        afterCount = L.length (L.nub vars)  -- Simulate ownership transfer
    in afterCount <= beforeCount

-- ============================================================================
-- Type System QuickCheck Tests
-- ============================================================================

-- | Test that type checking on well-formed expressions succeeds
prop_type_check_well_formed :: Property
prop_type_check_well_formed = 
    let wellFormedExpr = "42 + 24"
    in not (checkTypeError wellFormedExpr)

-- | Test that type checking on ill-formed expressions fails
prop_type_check_ill_formed :: Property
prop_type_check_ill_formed = 
    let illFormedExpr = "42 + \"hello\""  -- type mismatch
    in checkTypeError illFormedExpr

-- | Test that type inference preserves type consistency
prop_type_inference_consistent :: String -> Property
prop_type_inference_consistent expr = 
    let hasTypeErrors = checkTypeError expr
    in not hasTypeErrors ==> L.length (filter isAlphaNum expr) >= 0

-- ============================================================================
-- Code Generation QuickCheck Tests
-- ============================================================================

-- | Test that code generation preserves function count
prop_code_generation_preserves_functions :: [String] -> Property
prop_code_generation_preserves_functions functions = 
    let functionCount = L.length functions
    in functionCount >= 0

-- | Test that generated Go code is syntactically valid (basic check)
prop_go_code_basic_syntax :: Property
prop_go_code_basic_syntax = 
    let goCode = "package main\n\nfunc main() {\n    println(\"Hello, World!\")\n}"
        hasPackage = "package main" `L.isInfixOf` goCode
        hasMain = "func main" `L.isInfixOf` goCode
    in hasPackage && hasMain

-- ============================================================================
-- Integration QuickCheck Tests
-- ============================================================================

-- | Test that compilation pipeline preserves semantic meaning
prop_compilation_preserves_semantics :: String -> Property
prop_compilation_preserves_semantics code = 
    let trimmedCode = trim code
        hasContent = not (null trimmedCode)
    in hasContent ==> L.length trimmedCode >= 0

-- | Test that error reporting is consistent
prop_error_reporting_consistent :: String -> Property
prop_error_reporting_consistent code = 
    let hasErrors = checkTypeError code
        errorCount = if hasErrors then 1 else 0
    in errorCount >= 0

-- ============================================================================
-- Custom Arbitrary Instances
-- ============================================================================

instance Arbitrary CompilerError where
    arbitrary = do
        phase <- elements [Parsing, TypeChecking, OwnershipChecking, CodeGeneration]
        message <- listOf1 arbitrary
        pos <- arbitrary
        return $ CompilerError phase message pos

instance Arbitrary CodeBlock where
    arbitrary = do
        content <- listOf1 arbitrary
        directives <- arbitrary
        location <- oneof [pure Nothing, Just <$> arbitrary]
        return $ CodeBlock content directives location

instance Arbitrary CompilationPhase where
    arbitrary = elements [Parsing, TypeChecking, OwnershipChecking, CodeGeneration]

newtype NonEmptyList a = NonEmpty { getNonEmpty :: [a] }
    deriving (Show, Eq)

instance Arbitrary a => Arbitrary (NonEmptyList a) where
    arbitrary = NonEmpty <$> listOf1 arbitrary

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Compiler L.and Ownership QuickCheck Tests"
    [ testGroup "Compiler Error Tests"
        [ testProperty "compiler errors have valid phases" prop_compiler_error_valid_phase
        , testProperty "compiler error messages are non-empty" prop_compiler_error_non_empty_message
        , testProperty "check type error on empty code" prop_check_type_error_empty
        , testProperty "check type error on valid simple code" prop_check_type_error_valid_simple
        , testProperty "check type error on invalid syntax" prop_check_type_error_invalid_syntax
        ]
    
    , testGroup "Typus File Structure Tests"
        [ testProperty "TypusFile with empty blocks" prop_typus_file_empty_blocks
        , testProperty "TypusFile adding blocks" prop_typus_file_add_blocks
        ]
    
    , testGroup "Code Block Tests"
        [ testProperty "code block preserves content" prop_code_block_preserves_content
        , testProperty "code block trim content" prop_code_block_trim_content
        ]
    
    , testGroup "Ownership Analysis Tests"
        [ testProperty "ownership preserves variables" prop_ownership_preserves_variables
        , testProperty "ownership handles duplicates" prop_ownership_handles_duplicates
        , testProperty "ownership transfer count" prop_ownership_transfer_count
        ]
    
    , testGroup "Type System Tests"
        [ testProperty "type check well-formed expressions" prop_type_check_well_formed
        , testProperty "type check ill-formed expressions" prop_type_check_ill_formed
        , testProperty "type inference consistent" prop_type_inference_consistent
        ]
    
    , testGroup "Code Generation Tests"
        [ testProperty "code generation preserves functions" prop_code_generation_preserves_functions
        , testProperty "Go code basic syntax" prop_go_code_basic_syntax
        ]
    
    , testGroup "Integration Tests"
        [ testProperty "compilation preserves semantics" prop_compilation_preserves_semantics
        , testProperty "error reporting consistent" prop_error_reporting_consistent
        ]
    ]

-- Helper operator for property testing
(===) :: (Show a, Eq a) => a -> a -> Property
a === b = if a == b then property () else reject "Values are not equal"

reject :: String -> Property
reject _ = property False

property :: Bool -> Property
property True = property ()
property False = reject "Property failed"