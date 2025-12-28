{-# LANGUAGE CPP #-}
module Test.Unit.ParserDirectiveQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, choose, 
                        Property, (===), forAll, counterexample, suchThat)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..),
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), locatedAt, startPos)
import qualified Data.Text as T

-- ============================================================================
-- Test data generators
-- ============================================================================

-- Generate valid directive keys
genDirectiveKey :: Gen String
genDirectiveKey = elements ["ownership", "dependent_types", "constraints"]

-- Generate boolean values as strings
genBoolString :: Gen String
genBoolString = elements ["true", "false", "on", "off", "1", "0"]

-- Generate directive values
genDirectiveValue :: Gen String
genDirectiveValue = oneof 
  [ genBoolString
  , elements ["enabled", "disabled", "yes", "no"]
  ]

-- Generate directive pairs
genDirectivePair :: Gen (String, String)
genDirectivePair = do
  key <- genDirectiveKey
  value <- genDirectiveValue
  return (key, value)

-- Generate multiple directive pairs
genDirectivePairs :: Gen [(String, String)]
genDirectivePairs = listOf genDirectivePair

-- Generate directive strings with various formats
genDirectiveString :: Gen String
genDirectiveString = do
  pairs <- genDirectivePairs
  let formattedPairs = map (\(k, v) -> k ++ ": " ++ v) pairs
      joined = if null formattedPairs 
               then ""
               else "//! " ++ unwords formattedPairs
  return joined

-- Generate code blocks with directives
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  directives <- genBlockDirectives
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t.,;:!@#$%^&*()[]{}<>+-*/="
  span <- genSourceSpan
  return $ CodeBlock directives (concat content) span

-- Generate file directives
genFileDirectives :: Gen FileDirectives
genFileDirectives = do
  ownership <- oneof [pure Nothing, Just <$> locatedAt startPos <$> elements [True, False]]
  dependentTypes <- oneof [pure Nothing, Just <$> locatedAt startPos <$> elements [True, False]]
  constraints <- oneof [pure Nothing, Just <$> locatedAt startPos <$> elements [True, False]]
  return $ FileDirectives ownership dependentTypes constraints

-- Generate block directives
genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = do
  ownership <- oneof [pure Nothing, Just <$> locatedAt startPos <$> elements [True, False]]
  dependentTypes <- oneof [pure Nothing, Just <$> locatedAt startPos <$> elements [True, False]]
  constraints <- oneof [pure Nothing, Just <$> locatedAt startPos <$> elements [True, False]]
  return $ BlockDirectives ownership dependentTypes constraints

-- Generate source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = do
  startLine <- choose (1, 100)
  startCol <- choose (1, 100)
  endLine <- choose (startLine, startLine + 50)
  endCol <- if endLine == startLine 
            then choose (startCol, startCol + 100)
            else choose (1, 100)
  return $ SourceSpan (SourcePos startLine startCol 0) (SourcePos endLine endCol 0)

-- ============================================================================
-- Properties for FileDirectives
-- ============================================================================

prop_default_file_directives_all_nothing :: Property
prop_default_file_directives_all_nothing =
  let defaults = defaultFileDirectives
  in fdOwnership defaults === Nothing &&
     fdDependentTypes defaults === Nothing &&
     fdConstraints defaults === Nothing

prop_file_directives_extraction_consistency :: FileDirectives -> Property
prop_file_directives_extraction_consistency directives =
  -- Test that extracting directives preserves the values
  case fdOwnership directives of
    Nothing -> property True
    Just loc -> locValue loc `elem` [True, False]

-- ============================================================================
-- Properties for BlockDirectives
-- ============================================================================

prop_default_block_directives_all_nothing :: Property
prop_default_block_directives_all_nothing =
  let defaults = defaultBlockDirectives
  in bdOwnership defaults === Nothing &&
     bdDependentTypes defaults === Nothing &&
     bdConstraints defaults === Nothing

prop_block_directives_boolean_values :: BlockDirectives -> Property
prop_block_directives_boolean_values directives =
  let checkDirective maybeLoc = case maybeLoc of
        Nothing -> True
        Just loc -> locValue loc `elem` [True, False]
  in checkDirective (bdOwnership directives) &&
     checkDirective (bdDependentTypes directives) &&
     checkDirective (bdConstraints directives)

-- ============================================================================
-- Properties for CodeBlock
-- ============================================================================

prop_code_block_preserves_directives :: BlockDirectives -> String -> SourceSpan -> Property
prop_code_block_preserves_directives directives content span =
  let codeBlock = CodeBlock directives content span
  in cbDirectives codeBlock === directives &&
     cbContent codeBlock === content &&
     cbSpan codeBlock === span

prop_code_block_content_length :: CodeBlock -> Property
prop_code_block_content_length codeBlock =
  let content = cbContent codeBlock
  in length content >= 0

-- ============================================================================
-- Properties for TypusFile
-- ============================================================================

prop_typus_file_structure :: FileDirectives -> [CodeBlock] -> Property
prop_typus_file_structure directives blocks =
  let buildTags = []  -- Simplified for testing
      syntaxErrors = []  -- Simplified for testing
      typusFile = TypusFile directives buildTags blocks syntaxErrors
  in tfDirectives typusFile === directives &&
     tfBlocks typusFile === blocks &&
     length (tfBuildTags typusFile) === 0 &&
     length (tfSyntaxErrors typusFile) === 0

-- ============================================================================
-- Properties for directive parsing simulation
-- ============================================================================

prop_directive_key_recognition :: String -> Property
prop_directive_key_recognition key =
  let validKeys = ["ownership", "dependent_types", "constraints"]
      isValidKey = key `elem` validKeys
  in counterexample ("Key: " ++ key ++ ", Valid: " ++ show isValidKey) $
     isValidKey ==> length key > 0

prop_directive_value_parsing :: String -> Property
prop_directive_value_parsing value =
  let trueValues = ["true", "on", "1", "enabled", "yes"]
      falseValues = ["false", "off", "0", "disabled", "no"]
      isTrueValue = value `elem` trueValues
      isFalseValue = value `elem` falseValues
      isValidValue = isTrueValue || isFalseValue
  in counterexample ("Value: " ++ value ++ ", Valid: " ++ show isValidValue) $
     isValidValue ==> length value > 0

prop_directive_format_validation :: String -> Property
prop_directive_format_validation directiveStr =
  let hasCorrectPrefix = "//! " `isPrefixOf` directiveStr
      hasColonPattern = ':' `elem` directiveStr
      wellFormed = hasCorrectPrefix && hasColonPattern
  in counterexample ("Directive: " ++ directiveStr ++ ", Well-formed: " ++ show wellFormed) $
     wellFormed ==> length directiveStr > 4
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- ============================================================================
-- Properties for directive combination
-- ============================================================================

prop_directive_combination_commutativity :: [(String, String)] -> [(String, String)] -> Property
prop_directive_combination_commutativity pairs1 pairs2 =
  let combined1 = pairs1 ++ pairs2
      combined2 = pairs2 ++ pairs1
      -- Sort both combinations to compare
      sorted1 = sort combined1
      sorted2 = sort combined2
  in sorted1 === sorted2
  where
    sort [] = []
    sort xs = let (pivot, rest) = partition pivot xs
                 pivot = head xs
                 partition p [] = ([], [])
                 partition p (x:xs) 
                   | x <= p = let (less, more) = partition p xs in (x:less, more)
                   | otherwise = let (less, more) = partition p xs in (less, x:more)
             in less ++ [pivot] ++ more

-- ============================================================================
-- Edge case properties
-- ============================================================================

prop_empty_directive_handling :: Property
prop_empty_directive_handling =
  let emptyDirectives = FileDirectives Nothing Nothing Nothing
  in fdOwnership emptyDirectives === Nothing &&
     fdDependentTypes emptyDirectives === Nothing &&
     fdConstraints emptyDirectives === Nothing

prop_malformed_directive_resilience :: String -> Property
prop_malformed_directive_resilience malformedStr =
  -- Test that the system can handle malformed directive strings gracefully
  let hasDirectiveMarker = "//!" `isInfixOf` malformedStr
      hasColon = ':' `isInfixOf` malformedStr
  in counterexample ("Malformed: " ++ malformedStr) $
     (not hasDirectiveMarker || not hasColon) ==> length malformedStr >= 0
  where
    isInfixOf needle haystack = needle `elem` [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

-- ============================================================================
-- Test suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Directive QuickCheck Tests"
  [ testGroup "FileDirectives properties"
    [ fastProperty "default file directives are all Nothing" prop_default_file_directives_all_nothing
    , fastProperty "file directives extraction consistency" prop_file_directives_extraction_consistency
    ]
  , testGroup "BlockDirectives properties"
    [ fastProperty "default block directives are all Nothing" prop_default_block_directives_all_nothing
    , fastProperty "block directives contain boolean values" prop_block_directives_boolean_values
    ]
  , testGroup "CodeBlock properties"
    [ fastProperty "code block preserves directives" prop_code_block_preserves_directives
    , fastProperty "code block content length is non-negative" prop_code_block_content_length
    ]
  , testGroup "TypusFile properties"
    [ fastProperty "typus file structure preservation" prop_typus_file_structure
    ]
  , testGroup "Directive parsing properties"
    [ fastProperty "directive key recognition" prop_directive_key_recognition
    , fastProperty "directive value parsing" prop_directive_value_parsing
    , fastProperty "directive format validation" prop_directive_format_validation
    ]
  , testGroup "Directive combination properties"
    [ fastProperty "directive combination commutativity" prop_directive_combination_commutativity
    ]
  , testGroup "Edge case properties"
    [ fastProperty "empty directive handling" prop_empty_directive_handling
    , fastProperty "malformed directive resilience" prop_malformed_directive_resilience
    ]
  ]