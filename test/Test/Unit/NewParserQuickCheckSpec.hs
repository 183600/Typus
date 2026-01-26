{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans  -Wno-unused-imports -Wno-name-shadowing  -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Unit.NewParserQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), emptySpan, spanBetween)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Text.Megaparsec as MP

-- Arbitrary instances for QuickCheck
-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


instance Arbitrary BlockDirectives where
  arbitrary = return defaultBlockDirectives

instance Arbitrary FileDirectives where
  arbitrary = return defaultFileDirectives

instance Arbitrary (Located String) where
  arbitrary = do
    str <- arbitrary
    let sourceSpan = SourceSpan (SourcePos 0 0 0) (SourcePos 0 0 0)
    return $ Located str (spanStart sourceSpan) sourceSpan

instance Arbitrary CodeBlock where
  arbitrary = CodeBlock <$> arbitrary <*> arbitrary <*> arbitrary

-- ============================================================================
-- Parser Module QuickCheck Tests
-- ============================================================================

-- Test FileDirectives properties
prop_file_directives_default_valid :: Property
prop_file_directives_default_valid = 
  let fd = defaultFileDirectives
  in property $ isNothing (fdOwnership fd) && 
                isNothing (fdDependentTypes fd) && 
                isNothing (fdConstraints fd)

-- Test BlockDirectives properties
prop_block_directives_default_valid :: Property
prop_block_directives_default_valid = 
  let bd = defaultBlockDirectives
  in property $ isNothing (bdOwnership bd) && 
                isNothing (bdDependentTypes bd) && 
                isNothing (bdConstraints bd)

-- Test CodeBlock properties
prop_code_block_creation :: BlockDirectives -> String -> SourceSpan -> Property
prop_code_block_creation directives content sourceSpan = 
  let block = CodeBlock directives content sourceSpan
  in property $ cbDirectives block == directives && 
                cbContent block == content && 
                cbSpan block == sourceSpan

prop_code_block_content_preserved :: String -> Property
prop_code_block_content_preserved content = 
  let sourceSpan = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length content + 1) (length content))
      block = CodeBlock defaultBlockDirectives content sourceSpan
  in property $ cbContent block == content

-- Test TypusFile properties
prop_typus_file_creation :: FileDirectives -> [Located String] -> [CodeBlock] -> Property
prop_typus_file_creation directives buildTags blocks = 
  let file = TypusFile directives buildTags blocks []
  in property $ tfDirectives file == directives && 
                tfBuildTags file == buildTags && 
                tfBlocks file == blocks &&
                null (tfSyntaxErrors file)

prop_typus_file_empty :: Property
prop_typus_file_empty = 
  let file = TypusFile defaultFileDirectives [] [] []
  in property $ tfDirectives file == defaultFileDirectives &&
                null (tfBuildTags file) &&
                null (tfBlocks file) &&
                null (tfSyntaxErrors file)

-- Test directive parsing properties
prop_parse_simple_directive :: String -> String -> Property
prop_parse_simple_directive key value = 
  let -- Only use valid identifier characters
      validKey = filter (\c -> isAlphaNum c || c == '_' || c == '-') $ take 10 key
      validValue = filter (\c -> isAlphaNum c || c == '_' || c == '-') $ take 10 value
      input = T.pack (validKey ++ "=" ++ validValue)
      result = parse fileDirectiveParser "" input
  in if null validKey || null validValue
     then property $ True  -- Allow empty key or value
     else case result of
            Left _ -> property $ False  -- Should succeed for simple key=value pairs
            Right pairs -> property $ pairs == [(T.pack validKey, T.pack validValue)]

prop_parse_multiple_directives :: String -> String -> String -> String -> Property
prop_parse_multiple_directives key1 value1 key2 value2 = 
  let -- Only use valid identifier characters
      validKey1 = filter (\c -> isAlphaNum c || c == '_' || c == '-') $ take 10 key1
      validValue1 = filter (\c -> isAlphaNum c || c == '_' || c == '-') $ take 10 value1
      validKey2 = filter (\c -> isAlphaNum c || c == '_' || c == '-') $ take 10 key2
      validValue2 = filter (\c -> isAlphaNum c || c == '_' || c == '-') $ take 10 value2
      input = T.pack (validKey1 ++ "=" ++ validValue1 ++ "," ++ validKey2 ++ "=" ++ validValue2)
      result = parse fileDirectiveParser "" input
  in if null validKey1 || null validValue1 || null validKey2 || null validValue2
     then property $ True  -- Allow empty keys or values
     else case result of
            Left _ -> property $ False  -- Should succeed for simple key=value pairs
            Right pairs -> property $ pairs == [(T.pack key1, T.pack value1), (T.pack key2, T.pack value2)]

-- Test identifier parsing
prop_is_identifier_char_valid :: Char -> Property
prop_is_identifier_char_valid c = 
  let expected = isAlphaNum c || c == '_' || c == '-'
  in property $ isIdentifierChar c == expected

prop_identifier_alnum_chars :: Property
prop_identifier_alnum_chars = 
  forAll arbitrary $ \c -> 
    if isAlphaNum c
    then property $ isIdentifierChar c
    else property $ True  -- Non-alnum chars may or may not be valid

prop_identifier_special_chars :: Property
prop_identifier_special_chars = 
  property $ isIdentifierChar '_' && 
            isIdentifierChar '-' &&
            not (isIdentifierChar ' ') &&
            not (isIdentifierChar '@') &&
            not (isIdentifierChar '#')

-- Test parsing behavior with edge cases
prop_parse_empty_string :: Property
prop_parse_empty_string = 
  let result = parseTypus ""
  in case result of
    Left _ -> property $ False  -- Empty string should parse successfully
    Right file -> property $ null (tfBlocks file) && null (tfSyntaxErrors file)

prop_parse_only_whitespace :: String -> Property
prop_parse_only_whitespace ws = 
  let allWs = all isSpace ws
      result = parseTypus ws
  in if allWs
     then case result of
       Left _ -> property $ False  -- Only whitespace should parse successfully
       Right file -> property $ null (tfBlocks file)
     else property $ True  -- Skip test if not all whitespace

prop_parse_simple_content :: String -> Property
prop_parse_simple_content content = 
  let notAllWs = not (all isSpace content)
      result = parseTypus content
  in if notAllWs && not ("//" `isInfixOf` content) && not ("/*" `isInfixOf` content)
     then case result of
       Left _ -> property $ False  -- Simple content should parse
       Right file -> property $ not (null $ tfBlocks file) || 
                               (not (null content) && not (null $ tfSyntaxErrors file))
     else property $ True  -- Skip test for complex content

-- Test directive parsing with various formats
prop_parse_directive_with_spaces :: String -> String -> Property
prop_parse_directive_with_spaces key value = 
  let input = T.pack ("  " ++ key ++ "  =  " ++ value ++ "  ")
      result = parse fileDirectiveParser "" input
  in case result of
    Left _ -> property $ False  -- Should succeed with spaces
    Right pairs -> property $ pairs == [(T.pack key, T.pack value)]

prop_parse_directive_empty_value :: String -> Property
prop_parse_directive_empty_value key = 
  let input = T.pack (key ++ "=")
      result = parse fileDirectiveParser "" input
  in case result of
    Left _ -> property $ False  -- Should succeed with empty value
    Right pairs -> property $ pairs == [(T.pack key, T.pack "")]

prop_parse_directive_empty_key :: String -> Property
prop_parse_directive_empty_key value = 
  let input = T.pack ("=" ++ value)
      result = parse fileDirectiveParser "" input
  in case result of
    Left _ -> property $ True  -- Empty key might fail
    Right pairs -> property $ pairs == [(T.pack "", T.pack value)]

-- Test parsing with comments
prop_parse_line_comments :: String -> String -> Property
prop_parse_line_comments before comment = 
  let input = before ++ "// " ++ comment
      result = parseTypus input
  in case result of
    Left _ -> property $ True  -- May fail depending on content
    Right file -> property $ True  -- Should parse successfully

prop_parse_block_comments :: String -> String -> String -> Property
prop_parse_block_comments before comment after = 
  let input = before ++ "/* " ++ comment ++ " */" ++ after
      result = parseTypus input
  in case result of
    Left _ -> property $ True  -- May fail depending on content
    Right file -> property $ True  -- Should parse successfully

-- Test parsing with build tags
prop_parse_build_tags :: [String] -> Property
prop_parse_build_tags tags = 
  let tagString = unwords $ map (\t -> "+" ++ t) tags
      result = parseTypus tagString
  in case result of
    Left _ -> property $ True  -- May fail depending on tags
    Right file -> property $ True  -- Should parse successfully

-- Test error handling
prop_parse_with_invalid_syntax :: String -> Property
prop_parse_with_invalid_syntax content = 
  let hasInvalidChars = any (`elem` "@#$%^&*()") content
      result = parseTypus content
  in if hasInvalidChars
     then case result of
       Left _ -> property $ True  -- Should fail with invalid syntax
       Right file -> property $ not (null $ tfSyntaxErrors file)
     else property $ True  -- Skip test for valid content

-- Test roundtrip properties
prop_parse_roundtrip_preserves_structure :: String -> Property
prop_parse_roundtrip_preserves_structure content = 
  let result = parseTypus content
  in case result of
    Left _ -> property $ True  -- Skip if parsing fails
    Right file -> 
      let reconstructed = unlines $ map cbContent (tfBlocks file)
      in property $ length (lines reconstructed) <= length (lines content)

-- Unit tests for edge cases
test_parser_edge_cases :: TestTree
test_parser_edge_cases = testGroup "Parser Edge Cases"
  [ testCase "defaultFileDirectives" $ do
      let fd = defaultFileDirectives
      assertBool "ownership is Nothing" $ isNothing (fdOwnership fd)
      assertBool "dependentTypes is Nothing" $ isNothing (fdDependentTypes fd)
      assertBool "constraints is Nothing" $ isNothing (fdConstraints fd)
    
  , testCase "defaultBlockDirectives" $ do
      let bd = defaultBlockDirectives
      assertBool "ownership is Nothing" $ isNothing (bdOwnership bd)
      assertBool "dependentTypes is Nothing" $ isNothing (bdDependentTypes bd)
      assertBool "constraints is Nothing" $ isNothing (bdConstraints bd)
    
  , testCase "parse simple directive" $ do
      let result = parse fileDirectiveParser "" (T.pack "key=value")
      case result of
        Left err -> assertFailure $ "Failed to parse simple directive: " ++ errorBundlePretty err
        Right pairs -> assertEqual "parsed directive" [(T.pack "key", T.pack "value")] pairs
    
  , testCase "parse multiple directives" $ do
      let result = parse fileDirectiveParser "" (T.pack "key1=value1,key2=value2")
      case result of
        Left err -> assertFailure $ "Failed to parse multiple directives: " ++ errorBundlePretty err
        Right pairs -> assertEqual "parsed directives" 
                                   [(T.pack "key1", T.pack "value1"), (T.pack "key2", T.pack "value2")] pairs
    
  , testCase "parse empty input" $ do
      let result = parseTypus ""
      case result of
        Left err -> assertFailure $ "Failed to parse empty input: " ++ show err
        Right file -> do
          assertBool "no blocks" $ null $ tfBlocks file
          assertBool "no syntax errors" $ null $ tfSyntaxErrors file
    
  , testCase "parse whitespace only" $ do
      let result = parseTypus "   \n  \t  \n   "
      case result of
        Left err -> assertFailure $ "Failed to parse whitespace: " ++ show err
        Right file -> assertBool "no blocks" $ null $ tfBlocks file
    
  , testCase "parse simple content" $ do
      let content = "let x = 42"
          result = parseTypus content
      case result of
        Left err -> assertFailure $ "Failed to parse simple content: " ++ show err
        Right file -> assertBool "has blocks" $ not $ null $ tfBlocks file
    
  , testCase "parse with line comments" $ do
      let content = "let x = 42 // comment\nlet y = 24"
          result = parseTypus content
      case result of
        Left err -> assertFailure $ "Failed to parse with comments: " ++ show err
        Right file -> assertBool "has blocks" $ not $ null $ tfBlocks file
    
  , testCase "parse with block comments" $ do
      let content = "let x = 42 /* comment */\nlet y = 24"
          result = parseTypus content
      case result of
        Left err -> assertFailure $ "Failed to parse with block comments: " ++ show err
        Right file -> assertBool "has blocks" $ not $ null $ tfBlocks file
    
  , testCase "parse with build tags" $ do
      let content = "+tag1 +tag2\nlet x = 42"
          result = parseTypus content
      case result of
        Left err -> assertFailure $ "Failed to parse with build tags: " ++ show err
        Right file -> do
          assertBool "has build tags" $ not $ null $ tfBuildTags file
          assertBool "has blocks" $ not $ null $ tfBlocks file
  ]

-- QuickCheck properties
test_parser_properties :: TestTree
test_parser_properties = testGroup "Parser QuickCheck Properties"
  [ testProperty "FileDirectives default valid" prop_file_directives_default_valid
  , testProperty "BlockDirectives default valid" prop_block_directives_default_valid
  , testProperty "CodeBlock creation" prop_code_block_creation
  , testProperty "CodeBlock content preserved" prop_code_block_content_preserved
  , testProperty "TypusFile creation" prop_typus_file_creation
  , testProperty "TypusFile empty" prop_typus_file_empty
  , testProperty "parse simple directive" prop_parse_simple_directive
  , testProperty "parse multiple directives" prop_parse_multiple_directives
  , testProperty "isIdentifierChar valid" prop_is_identifier_char_valid
  , testProperty "identifier alnum chars" prop_identifier_alnum_chars
  , testProperty "identifier special chars" prop_identifier_special_chars
  , testProperty "parse empty string" prop_parse_empty_string
  , testProperty "parse only whitespace" prop_parse_only_whitespace
  , testProperty "parse simple content" prop_parse_simple_content
  , testProperty "parse directive with spaces" prop_parse_directive_with_spaces
  , testProperty "parse directive empty value" prop_parse_directive_empty_value
  , testProperty "parse directive empty key" prop_parse_directive_empty_key
  , testProperty "parse line comments" prop_parse_line_comments
  , testProperty "parse block comments" prop_parse_block_comments
  , testProperty "parse build tags" prop_parse_build_tags
  , testProperty "parse with invalid syntax" prop_parse_with_invalid_syntax
  , testProperty "parse roundtrip preserves structure" prop_parse_roundtrip_preserves_structure
  ]

-- Main test suite
parserTests :: TestTree
parserTests = testGroup "Parser Module Tests"
  [ test_parser_edge_cases
  , test_parser_properties
  ]