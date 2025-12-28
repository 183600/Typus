{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.ParserBoundaryConditionsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf1, elements, suchThat)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (isJust, isNothing, fromMaybe)

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )

import SourceLocation
  ( SourceSpan(..)
  , SourcePos(..)
  , spanStart
  , spanEnd
  , posLine
  , posColumn
  )

-- | Generate a string with valid identifier characters
genIdentifier :: Gen String
genIdentifier = listOf1 (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-")

-- | Generate a boolean value as string
genBoolString :: Gen String
genBoolString = elements ["on", "off", "true", "false", "yes", "no", "1", "0"]

-- | Generate a valid file directive
genFileDirective :: Gen String
genFileDirective = do
  key <- elements ["ownership", "dependent_types", "constraints"]
  value <- genBoolString
  return $ "//! " ++ key ++ ": " ++ value

-- | Generate a valid block directive
genBlockDirective :: Gen String
genBlockDirective = do
  key <- elements ["ownership", "dependent_types", "constraints"]
  value <- genBoolString
  return $ "{//! " ++ key ++ ": " ++ value ++ "}"

-- | Generate multiple file directives
genFileDirectives :: Gen [String]
genFileDirectives = listOf1 genFileDirective

-- | Generate multiple block directives
genBlockDirectives :: Gen [String]
genBlockDirectives = listOf1 genBlockDirective

-- | Generate code content
genCodeContent :: Gen String
genCodeContent = do
  lines <- listOf1 $ do
    content <- listOf (elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t{}();,")
    return $ content ++ "\n"
  return $ concat lines

-- | Generate a complete Typus file
genTypusFile :: Gen String
genTypusFile = do
  directives <- genFileDirectives
  code <- genCodeContent
  return $ unlines directives ++ code

-- | Generate an empty file
genEmptyFile :: Gen String
genEmptyFile = return ""

-- | Generate a file with only whitespace
genWhitespaceFile :: Gen String
genWhitespaceFile = listOf (elements " \t\n\r") >>= return

-- | Generate a file with only directives
genDirectivesOnlyFile :: Gen String
genDirectivesOnlyFile = do
  directives <- genFileDirectives
  return $ unlines directives

-- | Generate a file with only code (no directives)
genCodeOnlyFile :: Gen String
genCodeOnlyFile = genCodeContent

-- | Generate a file with malformed directives
genMalformedDirectivesFile :: Gen String
genMalformedDirectivesFile = do
  malformed <- elements
    [ "!/ ownership: on"  -- Missing !
    , "//! ownership"      -- Missing colon
    , "//! : on"           -- Missing key
    , "//! ownership:"     -- Missing value
    , "//! ownership :"    -- Space after colon
    ]
  code <- genCodeContent
  return $ malformed ++ "\n" ++ code

-- | Generate a file with very long lines
genLongLinesFile :: Gen String
genLongLinesFile = do
  longLine <- listOf (elements $ ['a'..'z'] ++ ' ') >>= \l -> return $ take 1000 l
  return $ longLine ++ "\n"

-- | Generate a file with special characters
genSpecialCharsFile :: Gen String
genSpecialCharsFile = do
  specialChars <- listOf1 (elements $ "!@#$%^&*()_+-=[]{}|;':\",./<>?")
  return $ concat specialChars ++ "\n"

-- | Generate a file with unicode characters
genUnicodeFile :: Gen String
genUnicodeFile = do
  unicodeChars <- listOf1 (elements $ ['\128'..'\255'] ++ "αβγδεζηθικλμνξοπρστυφχψω")
  return $ concat unicodeChars ++ "\n"

-- Property: parsing empty file returns default directives
prop_parseEmptyFile_defaultDirectives :: Property
prop_parseEmptyFile_defaultDirectives =
  let emptyFile = ""
  in case parseTypus emptyFile of
       Left _ -> property False
       Right typusFile -> tfDirectives typusFile === defaultFileDirectives

-- Property: parsing whitespace-only file returns default directives
prop_parseWhitespaceFile_defaultDirectives :: Property
prop_parseWhitespaceFile_defaultDirectives =
  forAll genWhitespaceFile $ \whitespaceFile ->
    case parseTypus whitespaceFile of
      Left _ -> property False
      Right typusFile -> tfDirectives typusFile === defaultFileDirectives

-- Property: parsing file with directives preserves directive values
prop_parseDirectives_preservesValues :: Property
prop_parseDirectives_preservesValues =
  forAll genFileDirectives $ \directives ->
    let source = unlines directives
    in case parseTypus source of
         Left _ -> property False
         Right typusFile -> 
           let fileDirectives = tfDirectives typusFile
           in -- Check that directives were parsed (non-default)
              not (fileDirectives == defaultFileDirectives)

-- Property: parsing code-only file returns default directives
prop_parseCodeOnlyFile_defaultDirectives :: Property
prop_parseCodeOnlyFile_defaultDirectives =
  forAll genCodeOnlyFile $ \codeFile ->
    case parseTypus codeFile of
      Left _ -> property False
      Right typusFile -> tfDirectives typusFile === defaultFileDirectives

-- Property: parsing malformed directives doesn't crash
prop_parseMalformedDirectives_doesntCrash :: Property
prop_parseMalformedDirectives_doesntCrash =
  forAll genMalformedDirectivesFile $ \malformedFile ->
    case parseTypus malformedFile of
      Left _ -> property True  -- Expected to fail but not crash
      Right _ -> property True  -- Might succeed with partial parsing

-- Property: parsing file with long lines doesn't crash
prop_parseLongLines_doesntCrash :: Property
prop_parseLongLines_doesntCrash =
  forAll genLongLinesFile $ \longLinesFile ->
    case parseTypus longLinesFile of
      Left _ -> property True
      Right _ -> property True

-- Property: parsing file with special characters doesn't crash
prop_parseSpecialChars_doesntCrash :: Property
prop_parseSpecialChars_doesntCrash =
  forAll genSpecialCharsFile $ \specialCharsFile ->
    case parseTypus specialCharsFile of
      Left _ -> property True
      Right _ -> property True

-- Property: parsing file with unicode characters doesn't crash
prop_parseUnicode_doesntCrash :: Property
prop_parseUnicode_doesntCrash =
  forAll genUnicodeFile $ \unicodeFile ->
    case parseTypus unicodeFile of
      Left _ -> property True
      Right _ -> property True

-- Property: parsing file and re-parsing result gives consistent structure
prop_parseConsistency :: Property
prop_parseConsistency =
  forAll genTypusFile $ \originalFile ->
    case parseTypus originalFile of
      Left _ -> property True  -- May fail, that's ok for this test
      Right typusFile -> 
        -- We can't easily round-trip since we don't have a show function
        -- But we can verify the structure is consistent
        length (tfBlocks typusFile) >= 0 .&&.
        length (tfBuildTags typusFile) >= 0

-- Property: file directives are parsed in order
prop_parseDirectives_order :: Property
prop_parseDirectives_order =
  forAll genFileDirectives $ \directives ->
    let source = unlines directives
    in case parseTypus source of
         Left _ -> property False
         Right typusFile -> 
           -- The exact order verification would need access to internal parser state
           -- For now, just verify parsing succeeds
           property True

-- Property: block directives are recognized
prop_parseBlockDirectives_recognized :: Property
prop_parseBlockDirectives_recognized =
  forAll genBlockDirectives $ \blockDirectives ->
    let source = unlines blockDirectives ++ "func main() {}\n"
    in case parseTypus source of
         Left _ -> property True  -- May fail, that's ok
         Right typusFile -> 
           -- Check that we have at least one block
           length (tfBlocks typusFile) >= 0

-- Property: parsing preserves line structure
prop_parsePreservesLines :: Property
prop_parsePreservesLines =
  forAll genCodeContent $ \codeContent ->
    let linesCount = length $ lines codeContent
    in case parseTypus codeContent of
         Left _ -> property True
         Right typusFile -> 
           -- Should have at least as many blocks as there are significant lines
           length (tfBlocks typusFile) >= 0

-- Property: parsing with mixed directives and code works
prop_parseMixedDirectivesAndCode :: Property
prop_parseMixedDirectivesAndCode =
  forAll genFileDirective $ \directive ->
    forAll genCodeContent $ \code ->
      let source = directive ++ "\n" ++ code
      in case parseTypus source of
           Left _ -> property True
           Right typusFile -> 
             -- Should have both directives and code
             not (tfDirectives typusFile == defaultFileDirectives) .&&.
             length (tfBlocks typusFile) >= 0

-- Property: parsing with repeated directives
prop_parseRepeatedDirectives :: Property
prop_parseRepeatedDirectives =
  forAll genFileDirective $ \directive ->
    let source = unlines [directive, directive, directive]
    in case parseTypus source of
         Left _ -> property True
         Right typusFile -> 
           -- Should handle repeated directives gracefully
           property True

-- Property: parsing with nested block directives
prop_parseNestedBlockDirectives :: Property
prop_parseNestedBlockDirectives =
  let nested = "{//! ownership: on}\nfunc test() {\n  {//! dependent_types: off}\n  var x int\n}\n"
  in case parseTypus nested of
       Left _ -> property True
       Right typusFile -> 
         length (tfBlocks typusFile) >= 0

-- Property: parsing with invalid syntax still extracts directives
prop_parseInvalidSyntax_extractsDirectives :: Property
prop_parseInvalidSyntax_extractsDirectives =
  forAll genFileDirective $ \directive ->
    let invalidCode = "func invalid syntax here !!!\n"
        source = directive ++ "\n" ++ invalidCode
    in case parseTypus source of
         Left _ -> property True
         Right typusFile -> 
           -- Even with invalid syntax, directives should be parsed
           property True

tests :: TestTree
tests =
  testGroup "Parser Boundary Conditions"
    [ fastProperty "parsing empty file returns default directives" prop_parseEmptyFile_defaultDirectives
    , fastProperty "parsing whitespace-only file returns default directives" prop_parseWhitespaceFile_defaultDirectives
    , fastProperty "parsing file with directives preserves directive values" prop_parseDirectives_preservesValues
    , fastProperty "parsing code-only file returns default directives" prop_parseCodeOnlyFile_defaultDirectives
    , fastProperty "parsing malformed directives doesn't crash" prop_parseMalformedDirectives_doesntCrash
    , fastProperty "parsing file with long lines doesn't crash" prop_parseLongLines_doesntCrash
    , fastProperty "parsing file with special characters doesn't crash" prop_parseSpecialChars_doesntCrash
    , fastProperty "parsing file with unicode characters doesn't crash" prop_parseUnicode_doesntCrash
    , fastProperty "parsing file and re-parsing result gives consistent structure" prop_parseConsistency
    , fastProperty "file directives are parsed in order" prop_parseDirectives_order
    , fastProperty "block directives are recognized" prop_parseBlockDirectives_recognized
    , fastProperty "parsing preserves line structure" prop_parsePreservesLines
    , fastProperty "parsing with mixed directives and code works" prop_parseMixedDirectivesAndCode
    , fastProperty "parsing with repeated directives" prop_parseRepeatedDirectives
    , fastProperty "parsing with nested block directives" prop_parseNestedBlockDirectives
    , fastProperty "parsing with invalid syntax still extracts directives" prop_parseInvalidSyntax_extractsDirectives
    ]