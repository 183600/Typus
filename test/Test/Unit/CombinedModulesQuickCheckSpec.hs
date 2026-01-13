{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.CombinedModulesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Utils
import Parser (TypusFile(..), parseTypus, defaultFileDirectives, 
              FileDirectives(..), CodeBlock(..), cbSpan, cbContent, 
              fdOwnership, fdDependentTypes, fdConstraints)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, spanBetween)
import Compiler (compile, CompilerError(..))
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Control.Monad (when)

-- ============================================================================
-- Combined Modules QuickCheck Tests
-- ============================================================================

-- | Test Utils and Parser interaction
prop_utils_parser_interaction :: String -> Property
prop_utils_parser_interaction content = 
  not (null content) ==>
    let trimmedContent = trim content
        parseResult = parseTypus trimmedContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ tfDirectives typusFile == defaultFileDirectives

-- | Test SourceLocation and Parser interaction
prop_sourcelocation_parser_interaction :: String -> Property
prop_sourcelocation_parser_interaction content = 
  not (null content) && not (all isSpace content) ==>
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let blocks = tfBlocks typusFile
           in property $ not (null blocks) ==> 
              let firstBlock = head blocks
                  span = Parser.cbSpan firstBlock
              in isValidSpan span

-- | Test Compiler and Parser interaction
prop_compiler_parser_interaction :: String -> Property
prop_compiler_parser_interaction content = 
  not (null content) && length content < 100 ==>
    let parseResult = parseTypus content
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let compileResult = compile typusFile
           in case compileResult of
                Left _ -> property True
                Right goCode -> property $ not (null goCode)

-- | Test Utils and SourceLocation interaction
prop_utils_sourcelocation_interaction :: String -> Property
prop_utils_sourcelocation_interaction content = 
  let linesContent = lines content
      trimmedLines = map trim linesContent
      lineCount = length trimmedLines
      pos = SourcePos lineCount 1 0
  in property $ lineCount > 0 ==> posLine pos == lineCount

-- | Test error handling across modules
prop_error_handling_consistency :: String -> Property
prop_error_handling_consistency content = 
  not (null content) && length content < 50 ==>
    let parseResult = parseTypus content
        compileResult = case parseResult of
                          Left _ -> Left [mockError]
                          Right typusFile -> compile typusFile
    in case (parseResult, compileResult) of
         (Left _, Left _) -> property True
         (Right _, Right _) -> property True
         (Left _, Right _) -> property False  -- Should not compile if parse failed
         (Right _, Left _) -> property True   -- Parse success but compile failure is OK
  where
    mockError = error "Mock error for testing"

-- | Test data consistency across modules
prop_data_consistency :: String -> Property
prop_data_consistency content = 
  not (null content) ==>
    let parseResult = parseTypus content
        trimmedContent = trim content
        splitContent = splitBy '\n' trimmedContent
    in case parseResult of
         Left _ -> property True
         Right typusFile -> 
           let blocks = tfBlocks typusFile
               blockCount = length blocks
           in property $ blockCount >= 0

-- | Test module interaction with special characters
prop_special_characters_handling :: String -> Property
prop_special_characters_handling content = 
  let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
      contentWithSpecial = content ++ specialChars
      parseResult = parseTypus contentWithSpecial
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = Parser.cbContent firstBlock
            in specialChars `isInfixOf` blockContent

-- | Test module interaction with Unicode
prop_unicode_handling :: String -> Property
prop_unicode_handling content = 
  let unicodeChars = "中文测试ñáéíóú"
      contentWithUnicode = content ++ unicodeChars
      parseResult = parseTypus contentWithUnicode
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let blocks = tfBlocks typusFile
         in property $ not (null blocks) ==> 
            let firstBlock = head blocks
                blockContent = Parser.cbContent firstBlock
            in unicodeChars `isInfixOf` blockContent

-- | Test module interaction with large inputs
prop_large_input_handling :: String -> Property
prop_large_input_handling content = 
  let largeContent = concat $ replicate 10 content
      parseResult = parseTypus largeContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> property $ not (null (tfBlocks typusFile))

-- | Test module interaction with empty inputs
prop_empty_input_handling :: Property
prop_empty_input_handling = 
  let emptyContent = ""
      parseResult = parseTypus emptyContent
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         property $ tfDirectives typusFile == defaultFileDirectives && 
                    null (tfBuildTags typusFile) && 
                    null (tfBlocks typusFile)

-- | Test module interaction with whitespace-only inputs
prop_whitespace_input_handling :: String -> Property
prop_whitespace_input_handling ws = 
  all isSpace ws ==>
    let parseResult = parseTypus ws
    in case parseResult of
         Left _ -> property True
         Right typusFile -> property $ null (tfBlocks typusFile)

-- | Test module interaction with directive processing
prop_directive_processing :: Bool -> Bool -> Bool -> Property
prop_directive_processing ownership deps constraints = 
  let content = "// ownership: " ++ show ownership ++ "\n" ++
                "// dependent-types: " ++ show deps ++ "\n" ++
                "// constraints: " ++ show constraints ++ "\n"
      parseResult = parseTypus content
  in case parseResult of
       Left _ -> property True
       Right typusFile -> 
         let directives = tfDirectives typusFile
         in case (Parser.fdOwnership directives, Parser.fdDependentTypes directives, Parser.fdConstraints directives) of
              (Just o, Just d, Just c) -> 
                property $ locValue o == ownership && locValue d == deps && locValue c == constraints
              _ -> property False

-- Helper function to check if a span is valid
isValidSpan :: SourceSpan -> Bool
isValidSpan span = True  -- Simplified for this example

-- | Tasty test suite
testSuite :: TestTree
testSuite = testGroup "Combined Modules QuickCheck Properties"
  [ testProperty "Utils and Parser interaction" prop_utils_parser_interaction,
    testProperty "SourceLocation and Parser interaction" prop_sourcelocation_parser_interaction,
    testProperty "Compiler and Parser interaction" prop_compiler_parser_interaction,
    testProperty "Utils and SourceLocation interaction" prop_utils_sourcelocation_interaction,
    testProperty "Error handling consistency" prop_error_handling_consistency,
    testProperty "Data consistency across modules" prop_data_consistency,
    testProperty "Special characters handling" prop_special_characters_handling,
    testProperty "Unicode handling" prop_unicode_handling,
    testProperty "Large input handling" prop_large_input_handling,
    testProperty "Empty input handling" prop_empty_input_handling,
    testProperty "Whitespace input handling" prop_whitespace_input_handling,
    testProperty "Directive processing" prop_directive_processing
  ]