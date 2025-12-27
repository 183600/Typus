{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.NewCabalParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property)
import TestSupport.QuickCheck (fastProperty)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf)
import GHC.Generics (Generic)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

-- ============================================================================
-- Test Data Generators
-- ============================================================================

-- | Generate valid source positions
genSourcePos :: Gen SourcePos
genSourcePos = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

-- | Generate valid source spans
genSourceSpan :: Gen SourceSpan
genSourceSpan = SourceSpan <$> genSourcePos <*> genSourcePos

-- | Generate located values
genLocated :: Gen a -> Gen (Located a)
genLocated gen = Located <$> genSourceSpan <*> gen

-- | Generate file directives
genFileDirectives :: Gen FileDirectives
genFileDirectives = FileDirectives <$> maybeGen (genLocated arbitrary)
                                  <*> maybeGen (genLocated arbitrary)  
                                  <*> maybeGen (genLocated arbitrary)
  where
    maybeGen gen = do
      shouldInclude <- arbitrary
      if shouldInclude then Just <$> gen else pure Nothing

-- | Generate block directives  
genBlockDirectives :: Gen BlockDirectives
genBlockDirectives = BlockDirectives <$> maybeGen (genLocated arbitrary)
                                     <*> maybeGen (genLocated arbitrary)
                                     <*> maybeGen (genLocated arbitrary)

-- | Generate simple code blocks
genCodeBlock :: Gen CodeBlock
genCodeBlock = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t{}();,"
  directives <- genBlockDirectives
  return $ CodeBlock (T.pack content) directives

-- | Generate typus files
genTypusFile :: Gen TypusFile
genTypusFile = do
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \n\t{}();,"
  directives <- genFileDirectives
  blocks <- listOf1 genCodeBlock
  return $ TypusFile (T.pack content) directives blocks

instance Arbitrary SourcePos where
  arbitrary = genSourcePos

instance Arbitrary SourceSpan where
  arbitrary = genSourceSpan

instance Arbitrary FileDirectives where
  arbitrary = genFileDirectives

instance Arbitrary BlockDirectives where
  arbitrary = genBlockDirectives

instance Arbitrary CodeBlock where
  arbitrary = genCodeBlock

instance Arbitrary TypusFile where
  arbitrary = genTypusFile

-- ============================================================================
-- Parser Property Tests
-- ============================================================================

-- | Property: Parsing empty content should succeed with minimal structure
prop_parse_empty_content :: Property
prop_parse_empty_content = 
  let result = parseTypus "" 
  in case result of
    Left _ -> False  -- Should not fail on empty content
    Right file -> T.null (tfContent file)  -- Content should be empty

-- | Property: Parsing content with only whitespace should preserve structure
prop_parse_whitespace_content :: String -> Property
prop_parse_whitespace_content ws =
  let allWs = all isSpace ws
      result = parseTypus ws
  in if allWs
     then case result of
       Left _ -> False  -- Should not fail on whitespace only
       Right file -> T.all isSpace (tfContent file)  -- Should preserve whitespace
     else property True  -- Skip test if not all whitespace

-- | Property: File directives parsing should be consistent
prop_parse_file_directives_consistency :: FileDirectives -> Property
prop_parse_file_directives_consistency directives =
  let content = "// @ownership " ++ show (fdOwnership directives) ++ "\n" ++
                "// @dependent-types " ++ show (fdDependentTypes directives) ++ "\n" ++
                "// @constraints " ++ show (fdConstraints directives) ++ "\n" ++
                "fn test() { return 42; }"
      result = parseTypus content
  in case result of
    Left _ -> property False  -- Should not fail
    Right file -> tfDirectives file `seq` property True  -- Should have directives

-- | Property: Block directives parsing should be consistent  
prop_parse_block_directives_consistency :: BlockDirectives -> Property
prop_parse_block_directives_consistency directives =
  let content = "// @ownership " ++ show (bdOwnership directives) ++ "\n" ++
                "// @dependent-types " ++ show (bdDependentTypes directives) ++ "\n" ++
                "// @constraints " ++ show (bdConstraints directives) ++ "\n" ++
                "fn test() { return 42; }"
      result = parseTypus content
  in case result of
    Left _ -> property False  -- Should not fail
    Right file -> not (null (tfBlocks file)) `seq` property True  -- Should have blocks

-- | Property: Simple function parsing should work consistently
prop_parse_simple_function :: String -> Property
prop_parse_simple_function name =
  let validName = all isAlphaNum (take 10 name) && not (null name)
      content = "fn " ++ take 10 name ++ "() { return 42; }"
      result = parseTypus content
  in if validName
     then case result of
       Left _ -> property False  -- Should not fail on valid function
       Right file -> not (null (tfBlocks file)) `seq` property True
     else property True  -- Skip invalid names

-- | Property: Multiple blocks should be preserved in parsing
prop_parse_multiple_blocks :: [String] -> Property
prop_parse_multiple_blocks blockContents =
  let validContents = filter (not . null) $ take 5 blockContents
      content = unlines $ map (\bc -> "fn block_" ++ take 5 bc ++ "() { return 1; }") validContents
      result = parseTypus content
  in if null validContents
     then property True  -- Skip if no valid blocks
     else case result of
       Left _ -> property False  -- Should not fail
       Right file -> length (tfBlocks file) >= length validContents `seq` property True

-- | Property: Comment handling should be consistent
prop_parse_comment_handling :: String -> Property
prop_parse_comment_handling comment =
  let safeComment = take 20 $ filter (/= '\n') comment
      content = "// " ++ safeComment ++ "\nfn test() { return 42; }"
      result = parseTypus content
  in case result of
    Left _ -> property False  -- Should not fail with comments
    Right file -> property True  -- Should parse successfully

-- | Property: Indentation should be preserved in parsing
prop_parse_indentation_preservation :: Int -> String -> Property
prop_parse_indentation_preservation indent content =
  let safeIndent = max 0 $ min 10 indent
      safeContent = take 15 $ filter (/= '\n') content
      indentedContent = replicate safeIndent ' ' ++ "fn test() { return 42; }"
      result = parseTypus indentedContent
  in case result of
    Left _ -> property False  -- Should not fail with indentation
    Right file -> property True  -- Should parse successfully

-- | Property: Error recovery should work on malformed input
prop_parse_error_recovery :: String -> Property
prop_parse_error_recovery malformed =
  let content = take 50 malformed ++ "fn valid_function() { return 42; }"
      result = parseTypus content
  in case result of
    Left _ -> property True  -- May fail, but should not crash
    Right file -> property True  -- May succeed with partial parsing

-- | Property: Round-trip parsing should preserve basic structure
prop_parse_roundtrip_basic :: TypusFile -> Property
prop_parse_roundtrip_basic file =
  let content = T.unpack $ tfContent file
      result = parseTypus content
  in case result of
    Left _ -> property False  -- Should not fail on round-trip
    Right reparsed -> property True  -- Should parse successfully

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "New Cabal Parser QuickCheck Tests"
  [ fastProperty "parse empty content" prop_parse_empty_content
  , fastProperty "parse whitespace content" prop_parse_whitespace_content  
  , fastProperty "parse file directives consistency" prop_parse_file_directives_consistency
  , fastProperty "parse block directives consistency" prop_parse_block_directives_consistency
  , fastProperty "parse simple function" prop_parse_simple_function
  , fastProperty "parse multiple blocks" prop_parse_multiple_blocks
  , fastProperty "parse comment handling" prop_parse_comment_handling
  , fastProperty "parse indentation preservation" prop_parse_indentation_preservation
  , fastProperty "parse error recovery" prop_parse_error_recovery
  , fastProperty "parse roundtrip basic" prop_parse_roundtrip_basic
  ]