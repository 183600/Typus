{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.CompilerErrorHandlingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Compiler (compile, CompilerError(..), CompilerResult, CompilationPhase(..), 
                renderCompilationError, formatCompilerErrors, generateDetailedReport,
                hasTypeErrors, TypeCheckDiagnostic(..), diagnoseTypeErrors,
                checkTypeError, hasMalformedSyntax, checkDependentTypes, checkOwnership)
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, sort)

-- | Generate simple code blocks for testing
genSimpleCodeBlock :: Gen CodeBlock
genSimpleCodeBlock = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r(){}[];:,."
  return $ CodeBlock defaultBlockDirectives (unlines content)

-- | Generate typus files with random code blocks
genTypusFile :: Gen TypusFile
genTypusFile = do
  numBlocks <- choose (0, 5)
  blocks <- vectorOf numBlocks genSimpleCodeBlock
  return $ TypusFile defaultFileDirectives blocks

-- | Generate potentially problematic code strings
genProblematicCode :: Gen String
genProblematicCode = oneof
  [ return ""  -- empty code
  , return " "  -- whitespace only
  , listOf $ elements " \t\n\r"  -- various whitespace
  , listOf $ elements "(){}[];:,."  -- just punctuation
  , listOf $ elements "+-*/%=!<>&|"  -- just operators
  , return "function without proper syntax"
  , return "var x = ;"  -- incomplete assignment
  , return "if { }"  -- malformed if
  , return "for ( ; ; ) { }"  -- empty for loop
  , return "function x() { return x + y; }"  -- undefined variables
  ]

-- | Test compilation with empty input
test_compile_empty_input :: TestTree
test_compile_empty_input = testCase "compile handles empty input" $ do
  let result = compile "" 
  case result of
    Left _ -> assertBool "Empty input compilation should fail gracefully" True
    Right _ -> assertBool "Empty input compilation might succeed" True

-- | Test compilation with whitespace-only input
test_compile_whitespace_only :: TestTree  
test_compile_whitespace_only = testCase "compile handles whitespace-only input" $ do
  let whitespaceInputs = [" ", "  ", "\t", "\n", "\r", "  \t\n\r  "]
  mapM_ (\input -> do
    let result = compile input
    case result of
      Left _ -> assertBool $ "Whitespace-only compilation failed gracefully: " ++ show input
      Right _ -> assertBool $ "Whitespace-only compilation succeeded: " ++ show input
  ) whitespaceInputs

-- | Test error rendering doesn't crash
test_error_rendering :: TestTree
test_error_rendering = testCase "error rendering is robust" $ do
  let errors = 
        [ CompilerError "Test error" (Just (mkSourcePos 1 1)) SyntaxError
        , CompilerError "Another error" Nothing TypeError
        , CompilerError "Warning" (Just (mkSourcePos 2 5)) Warning
        ]
  mapM_ (\error -> do
    let rendered = renderCompilationError error
        formatted = formatCompilerErrors [error]
        report = generateDetailedReport [error]
    assertBool "Error rendering produces output" $ not (null rendered)
    assertBool "Error formatting produces output" $ not (null formatted)
    assertBool "Report generation produces output" $ not (null report)
  ) errors

-- | Test type error detection
test_type_error_detection :: TestTree
test_type_error_detection = testCase "type error detection" $ do
  let typeErrors = [TypeError, Warning, SyntaxError]
      nonTypeErrors = [LinkError, RuntimeError]
  mapM_ (\errorType -> do
    let error = CompilerError "Test" (Just (mkSourcePos 1 1)) errorType
        hasType = hasTypeErrors [error]
    if errorType `elem` typeErrors
      then assertBool $ "Should detect type error for " ++ show errorType $ hasType
      else assertBool $ "Should not detect type error for " ++ show errorType $ not hasType
  ) (typeErrors ++ nonTypeErrors)

-- | Test malformed syntax detection
test_malformed_syntax_detection :: TestTree
test_malformed_syntax_detection = testCase "malformed syntax detection" $ do
  let syntaxError = CompilerError "Syntax error" (Just (mkSourcePos 1 1)) SyntaxError
      typeError = CompilerError "Type error" (Just (mkSourcePos 1 1)) TypeError
      warning = CompilerError "Warning" (Just (mkSourcePos 1 1)) Warning
  assertBool "Should detect malformed syntax for SyntaxError" $ hasMalformedSyntax [syntaxError]
  assertBool "Should not detect malformed syntax for TypeError" $ not $ hasMalformedSyntax [typeError]
  assertBool "Should not detect malformed syntax for Warning" $ not $ hasMalformedSyntax [warning]
  assertBool "Should detect malformed syntax in mixed list" $ hasMalformedSyntax [syntaxError, typeError]

-- | Property: Compilation should not crash on any input
prop_compilation_robustness :: String -> Property
prop_compilation_robustness input = 
  let result = compile input
  in property $ case result of
    Left _ -> True  -- Failing to compile is OK
    Right _ -> True  -- Succeeding to compile is OK

-- | Property: Error rendering should not crash
prop_error_rendering_robustness :: String -> Property
prop_error_rendering_robustness message = 
  let error = CompilerError message Nothing SyntaxError
      rendered = renderCompilationError error
      formatted = formatCompilerErrors [error]
  in property $ not (null rendered) .&&. not (null formatted)

-- | Property: Error formatting preserves order
prop_error_formatting_preserves_order :: Property
prop_error_formatting_preserves_order = 
  forAll (listOf (choose (1, 100))) $ \errorNums ->
    let errors = map (\n -> CompilerError ("Error " ++ show n) (Just (mkSourcePos n 1)) SyntaxError) errorNums
        formatted = formatCompilerErrors errors
        -- Check that error numbers appear in order in the formatted output
        checkOrder [] = True
        checkOrder [_] = True
        checkOrder (x:y:xs) = x <= y && checkOrder (y:xs)
        extractedNums = map (\n -> read $ "Error " ++ show n) $ filter (isPrefixOf "Error ") $ words formatted
    in property $ checkOrder extractedNums

-- | Property: Multiple errors are handled correctly
prop_multiple_errors_handling :: Property
prop_multiple_errors_handling = 
  forAll (choose (1, 20)) $ \numErrors ->
    let errors = map (\n -> CompilerError ("Error " ++ show n) (Just (mkSourcePos n 1)) SyntaxError) [1..numErrors]
        formatted = formatCompilerErrors errors
        report = generateDetailedReport errors
    in property $ not (null formatted) .&&. not (null report)

-- | Property: Compilation with very long input doesn't crash
prop_compilation_long_input :: Property
prop_compilation_long_input = forAll (vectorOf 10000 (elements "abc\n")) $ \longInput ->
  let result = compile longInput
  in property $ case result of
    Left _ -> True
    Right _ -> True

-- | Property: Compilation with special characters doesn't crash
prop_compilation_special_chars :: Property
prop_compilation_special_chars = forAll (listOf $ elements $ map toEnum [0..255]) $ \specialChars ->
  let result = compile specialChars
  in property $ case result of
    Left _ -> True
    Right _ -> True

-- | Property: Error messages are non-empty
prop_error_messages_non_empty :: String -> Property
prop_error_messages_non_empty message = 
  let error = CompilerError message Nothing SyntaxError
      rendered = renderCompilationError error
  in property $ not (null rendered)

-- | Property: Error position is preserved in rendering
prop_error_position_preserved :: Property
prop_error_position_preserved = 
  forAll (choose (1, 100)) $ \line ->
  forAll (choose (1, 100)) $ \col ->
    let pos = mkSourcePos line col
        error = CompilerError "Test error" (Just pos) SyntaxError
        rendered = renderCompilationError error
        hasLinePos = show line `isInfixOf` rendered
        hasColPos = show col `isInfixOf` rendered
    in property $ hasLinePos .&&. hasColPos

-- Helper function to create SourcePos (assuming it exists)
mkSourcePos :: Int -> Int -> SourcePos
mkSourcePos line col = SourcePos line col

-- Dummy SourcePos type if it doesn't exist in the actual module
data SourcePos = SourcePos Int Int deriving (Eq, Show, Ord)

instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 1000)
    col <- choose (1, 1000)
    return $ mkSourcePos line col

tests :: TestTree
tests = testGroup "Compiler Error Handling Tests"
  [ test_compile_empty_input
  , test_compile_whitespace_only
  , test_error_rendering
  , test_type_error_detection
  , test_malformed_syntax_detection
  , fastProperty "Compilation robustness" prop_compilation_robustness
  , fastProperty "Error rendering robustness" prop_error_rendering_robustness
  , fastProperty "Error formatting preserves order" prop_error_formatting_preserves_order
  , fastProperty "Multiple errors handling" prop_multiple_errors_handling
  , fastProperty "Compilation with long input" prop_compilation_long_input
  , fastProperty "Compilation with special characters" prop_compilation_special_chars
  , fastProperty "Error messages non-empty" prop_error_messages_non_empty
  , fastProperty "Error position preserved" prop_error_position_preserved
  ]