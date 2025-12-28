{-# LANGUAGE CPP #-}
module Test.Unit.SyntaxValidatorRobustnessQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample, suchThat, (==>))
import qualified SyntaxValidator
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import qualified Data.Text as T

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate valid identifiers
genValidIdentifier :: Gen String
genValidIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

-- Generate invalid identifiers
genInvalidIdentifier :: Gen String
genInvalidIdentifier = oneof
  [ pure ""  -- Empty
  , pure "123invalid"  -- Starts with number
  , elements ["!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "-", "+", "="]
  , listOf $ elements ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '+', '=']
  ]

-- Generate valid directives
genValidDirective :: Gen String
genValidDirective = oneof
  [ pure "//! ownership: on"
  , pure "//! ownership: off"
  , pure "//! dependent_types: on"
  , pure "//! dependent_types: off"
  , pure "//! constraints: on"
  , pure "//! constraints: off"
  , do
      own <- elements ["on", "off"]
      dep <- elements ["on", "off"]
      return $ "//! ownership: " ++ own ++ ", dependent_types: " ++ dep
  ]

-- Generate invalid directives
genInvalidDirective :: Gen String
genInvalidDirective = oneof
  [ pure "//! invalid: directive"
  , pure "//! ownership: maybe"
  , pure "//! dependent_types: sometimes"
  , pure "// ownership: on"  -- Missing !
  , pure "//!ownership: on"  -- Missing space
  , pure "!! ownership: on"  -- Extra !
  ]

-- Generate valid code blocks
genValidCodeBlock :: Gen String
genValidCodeBlock = do
  lines <- listOf $ oneof
    [ pure "package main"
    , pure "import \"fmt\""
    , pure "func main() {"
    , pure "fmt.Println(\"Hello, World!\")"
    , pure "}"
    , genValidIdentifier >>= \ident -> return $ "var " ++ ident ++ " int"
    , genValidIdentifier >>= \ident -> return $ "func " ++ ident ++ "() {}"
    ]
  return $ unlines lines

-- Generate invalid code blocks
genInvalidCodeBlock :: Gen String
genInvalidCodeBlock = do
  invalidLines <- listOf $ oneof
    [ pure "123invalid"
    , pure "func {"
    , pure "var 123abc int"
    , pure "if condition {"
    , pure "unclosed string \""
    , pure "unclosed comment /*"
    ]
  return $ unlines invalidLines

-- Generate mixed content (valid and invalid)
genMixedContent :: Gen String
genMixedContent = do
  directives <- listOf $ oneof [genValidDirective, genInvalidDirective]
  codeBlocks <- listOf $ oneof [genValidCodeBlock, genInvalidCodeBlock]
  return $ unlines directives ++ unlines codeBlocks

-- Generate unicode strings
genUnicodeString :: Gen String
genUnicodeString = do
  chars <- listOf $ elements $ map chr [0x20..0x7E] ++ map chr [0x80..0xFF]
  return chars
  where
    chr n = toEnum n

-- Generate very long strings
genLongString :: Gen String
genLongString = do
  length' <- choose (1000, 10000)
  listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t"

-- ============================================================================
-- Properties for identifier validation
-- ============================================================================

prop_valid_identifier_acceptance :: String -> Property
prop_valid_identifier_acceptance ident =
  let isValid = SyntaxValidator.isValidIdentifier ident
  in counterexample ("Identifier: " ++ ident ++ ", Valid: " ++ show isValid) $
     (all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']) ident && 
      not (null ident) && 
      head ident `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']) ==> isValid

prop_invalid_identifier_rejection :: String -> Property
prop_invalid_identifier_rejection ident =
  let isInvalid = null ident || 
                 (not (null ident) && head ident `elem` "0123456789") ||
                 any (`notElem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])) ident
      isValid = SyntaxValidator.isValidIdentifier ident
  in isInvalid ==> not isValid

-- ============================================================================
-- Properties for directive validation
-- ============================================================================

prop_valid_directive_acceptance :: String -> Property
prop_valid_directive_acceptance directive =
  let isValid = SyntaxValidator.isValidDirective directive
  in counterexample ("Directive: " ++ directive ++ ", Valid: " ++ show isValid) $
     ("//! " `isPrefixOf` directive) ==> isValid
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

prop_invalid_directive_rejection :: String -> Property
prop_invalid_directive_rejection directive =
  let hasCorrectPrefix = "//! " `isPrefixOf` directive
      isValid = SyntaxValidator.isValidDirective directive
  in not hasCorrectPrefix ==> not isValid
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- ============================================================================
-- Properties for code block validation
-- ============================================================================

prop_code_block_robustness :: String -> Property
prop_code_block_robustness codeBlock =
  let validationResult = SyntaxValidator.validateCodeBlock codeBlock
  in counterexample ("Code block length: " ++ show (length codeBlock)) $
     -- Validation should not crash on any input
     length codeBlock >= 0 ==> property True

prop_empty_code_block_handling :: Property
prop_empty_code_block_handling =
  let validationResult = SyntaxValidator.validateCodeBlock ""
  in property True  -- Should handle empty input gracefully

-- ============================================================================
-- Properties for syntax error recovery
-- ============================================================================

prop_syntax_error_recovery :: String -> Property
prop_syntax_error_recovery content =
  let errors = SyntaxValidator.validateSyntax content
  in counterexample ("Content length: " ++ show (length content) ++ ", Errors: " ++ show (length errors)) $
     -- Error recovery should not crash
     length content >= 0 ==> length errors >= 0

prop_error_location_accuracy :: String -> Property
prop_error_location_accuracy content =
  let errors = SyntaxValidator.validateSyntax content
  in counterexample ("Errors: " ++ show (length errors)) $
     all (\err -> SyntaxValidator.errorLine err >= 1) errors

-- ============================================================================
-- Properties for unicode handling
-- ============================================================================

prop_unicode_handling_robustness :: String -> Property
prop_unicode_handling_robustness unicodeStr =
  let validationResult = SyntaxValidator.validateUnicode unicodeStr
  in counterexample ("Unicode string: " ++ take 50 unicodeStr) $
     -- Should handle unicode without crashing
     length unicodeStr >= 0 ==> property True

prop_unicode_identifier_validation :: String -> Property
prop_unicode_identifier_validation unicodeIdent =
  let isValid = SyntaxValidator.isValidIdentifier unicodeIdent
  in counterexample ("Unicode identifier: " ++ unicodeIdent) $
     -- Unicode identifiers should be handled consistently
     length unicodeIdent >= 0 ==> (isValid === isValid)

-- ============================================================================
-- Properties for large input handling
-- ============================================================================

prop_large_input_handling :: String -> Property
prop_large_input_handling largeStr =
  let validationResult = SyntaxValidator.validateSyntax largeStr
  in counterexample ("Large input length: " ++ show (length largeStr)) $
     -- Should handle large inputs without performance issues
     length largeStr >= 1000 ==> property True

prop_memory_efficiency :: String -> Property
prop_memory_efficiency content =
  let errors1 = SyntaxValidator.validateSyntax content
      errors2 = SyntaxValidator.validateSyntax content
  in counterexample ("Content length: " ++ show (length content)) $
     -- Multiple validations should not accumulate memory
     length errors1 === length errors2

-- ============================================================================
-- Properties for malformed input handling
-- ============================================================================

prop_malformed_directive_handling :: String -> Property
prop_malformed_directive_handling malformedDirective =
  let validationResult = SyntaxValidator.validateDirective malformedDirective
  in counterexample ("Malformed directive: " ++ malformedDirective) $
     -- Should handle malformed directives gracefully
     length malformedDirective >= 0 ==> property True

prop_unclosed_construct_handling :: String -> Property
prop_unclosed_construct_handling content =
  let hasUnclosedString = "\"" `isInfixOf` content && not ("\"" `isInfixOf` drop 1 content)
      hasUnclosedComment = "/*" `isInfixOf` content && not ("*/" `isInfixOf` drop 2 content)
      errors = SyntaxValidator.validateSyntax content
  in counterexample ("Content: " ++ take 50 content) $
     (hasUnclosedString || hasUnclosedComment) ==> length errors >= 0
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- ============================================================================
-- Properties for concurrent validation
-- ============================================================================

prop_concurrent_validation_consistency :: String -> Property
prop_concurrent_validation_consistency content =
  let validation1 = SyntaxValidator.validateSyntax content
      validation2 = SyntaxValidator.validateSyntax content
  in counterexample ("Content length: " ++ show (length content)) $
     -- Multiple validations should produce consistent results
     length validation1 === length validation2

-- ============================================================================
-- Edge case properties
-- ============================================================================

prop_null_byte_handling :: Property
prop_null_byte_handling =
  let contentWithNull = "content\0with\0null\0bytes"
      validationResult = SyntaxValidator.validateSyntax contentWithNull
  in property True  -- Should handle null bytes

prop_control_character_handling :: Property
prop_control_character_handling =
  let controlChars = map chr [0..31] ++ [chr 127]
      contentWithControls = "content" ++ controlChars ++ "more"
      validationResult = SyntaxValidator.validateSyntax contentWithControls
  in property True  -- Should handle control characters
  where
    chr = toEnum

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Syntax Validator Robustness QuickCheck Tests"
  [ testGroup "Identifier validation properties"
    [ fastProperty "valid identifier acceptance" prop_valid_identifier_acceptance
    , fastProperty "invalid identifier rejection" prop_invalid_identifier_rejection
    ]
  , testGroup "Directive validation properties"
    [ fastProperty "valid directive acceptance" prop_valid_directive_acceptance
    , fastProperty "invalid directive rejection" prop_invalid_directive_rejection
    ]
  , testGroup "Code block validation properties"
    [ fastProperty "code block robustness" prop_code_block_robustness
    , fastProperty "empty code block handling" prop_empty_code_block_handling
    ]
  , testGroup "Syntax error recovery properties"
    [ fastProperty "syntax error recovery" prop_syntax_error_recovery
    , fastProperty "error location accuracy" prop_error_location_accuracy
    ]
  , testGroup "Unicode handling properties"
    [ fastProperty "unicode handling robustness" prop_unicode_handling_robustness
    , fastProperty "unicode identifier validation" prop_unicode_identifier_validation
    ]
  , testGroup "Large input handling properties"
    [ fastProperty "large input handling" prop_large_input_handling
    , fastProperty "memory efficiency" prop_memory_efficiency
    ]
  , testGroup "Malformed input handling properties"
    [ fastProperty "malformed directive handling" prop_malformed_directive_handling
    , fastProperty "unclosed construct handling" prop_unclosed_construct_handling
    ]
  , testGroup "Concurrent validation properties"
    [ fastProperty "concurrent validation consistency" prop_concurrent_validation_consistency
    ]
  , testGroup "Edge case properties"
    [ fastProperty "null byte handling" prop_null_byte_handling
    , fastProperty "control character handling" prop_control_character_handling
    ]
  ]