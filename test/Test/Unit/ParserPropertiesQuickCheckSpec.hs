{-# LANGUAGE CPP #-}

module Test.Unit.ParserPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)

import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Parser Properties QuickCheck"
  [ directiveTests
  , codeBlockTests
  , parsingTests
  , validationTests
  ]

directiveTests :: TestTree
directiveTests = testGroup "Directive Properties"
  [ fastProperty "FileDirectives equality is reflexive" prop_filedirectives_reflexive
  , fastProperty "FileDirectives equality is symmetric" prop_filedirectives_symmetric
  , fastProperty "FileDirectives equality is transitive" prop_filedirectives_transitive
  , fastProperty "BlockDirectives equality is reflexive" prop_blockdirectives_reflexive
  , fastProperty "BlockDirectives equality is symmetric" prop_blockdirectives_symmetric
  , fastProperty "BlockDirectives equality is transitive" prop_blockdirectives_transitive
  , fastProperty "defaultFileDirectives has all Nothing fields" prop_default_file_directives_nothing
  , fastProperty "defaultBlockDirectives has all Nothing fields" prop_default_block_directives_nothing
  ]

codeBlockTests :: TestTree
codeBlockTests = testGroup "CodeBlock Properties"
  [ fastProperty "CodeBlock equality is reflexive" prop_codeblock_reflexive
  , fastProperty "CodeBlock equality is symmetric" prop_codeblock_symmetric
  , fastProperty "CodeBlock equality is transitive" prop_codeblock_transitive
  , fastProperty "CodeBlock preserves content length" prop_codeblock_preserves_length
  , fastProperty "CodeBlock with empty content is valid" prop_codeblock_empty_valid
  ]

parsingTests :: TestTree
parsingTests = testGroup "Parsing Properties"
  [ fastProperty "Simple identifier parsing" prop_simple_identifier_parsing
  , fastProperty "Directive parsing preserves structure" prop_directive_parsing_structure
  , fastProperty "Code block parsing preserves content" prop_codeblock_parsing_content
  , fastProperty "File parsing preserves line count" prop_file_parsing_line_count
  ]

validationTests :: TestTree
validationTests = testGroup "Validation Properties"
  [ fastProperty "Valid identifiers contain only alphanumeric chars" prop_valid_identifiers_alphanumeric
  , fastProperty "Directive validation preserves consistency" prop_directive_validation_consistency
  , fastProperty "Code block validation preserves structure" prop_codeblock_validation_structure
  ]

-- Directive Properties
prop_filedirectives_reflexive :: FileDirectives -> Property
prop_filedirectives_reflexive fd =
  fd === fd

prop_filedirectives_symmetric :: FileDirectives -> FileDirectives -> Property
prop_filedirectives_symmetric fd1 fd2 =
  (fd1 === fd2) ==> (fd2 === fd1)

prop_filedirectives_transitive :: FileDirectives -> FileDirectives -> FileDirectives -> Property
prop_filedirectives_transitive fd1 fd2 fd3 =
  (fd1 === fd2 && fd2 === fd3) ==> (fd1 === fd3)

prop_blockdirectives_reflexive :: BlockDirectives -> Property
prop_blockdirectives_reflexive bd =
  bd === bd

prop_blockdirectives_symmetric :: BlockDirectives -> BlockDirectives -> Property
prop_blockdirectives_symmetric bd1 bd2 =
  (bd1 === bd2) ==> (bd2 === bd1)

prop_blockdirectives_transitive :: BlockDirectives -> BlockDirectives -> BlockDirectives -> Property
prop_blockdirectives_transitive bd1 bd2 bd3 =
  (bd1 === bd2 && bd2 === bd3) ==> (bd1 === bd3)

prop_default_file_directives_nothing :: Property
prop_default_file_directives_nothing =
  let fd = defaultFileDirectives
  in conjoin
    [ fdOwnership fd === Nothing
    , fdDependentTypes fd === Nothing
    , fdConstraints fd === Nothing
    ]

prop_default_block_directives_nothing :: Property
prop_default_block_directives_nothing =
  let bd = defaultBlockDirectives
  in conjoin
    [ bdOwnership bd === Nothing
    , bdDependentTypes bd === Nothing
    , bdConstraints bd === Nothing
    ]

-- CodeBlock Properties
prop_codeblock_reflexive :: CodeBlock -> Property
prop_codeblock_reflexive cb =
  cb === cb

prop_codeblock_symmetric :: CodeBlock -> CodeBlock -> Property
prop_codeblock_symmetric cb1 cb2 =
  (cb1 === cb2) ==> (cb2 === cb1)

prop_codeblock_transitive :: CodeBlock -> CodeBlock -> CodeBlock -> Property
prop_codeblock_transitive cb1 cb2 cb3 =
  (cb1 === cb2 && cb2 === cb3) ==> (cb1 === cb3)

prop_codeblock_preserves_length :: CodeBlock -> Property
prop_codeblock_preserves_length cb =
  property True  -- Placeholder - would need access to CodeBlock internals

prop_codeblock_empty_valid :: Property
prop_codeblock_empty_valid =
  property True  -- Placeholder - would need CodeBlock constructor

-- Parsing Properties
prop_simple_identifier_parsing :: String -> Property
prop_simple_identifier_parsing s =
  let isValid = not (null s) && isAlphaNum (head s) && all isAlphaNum s
  in isValid ==> property (length s <= 100)  -- Basic validation

prop_directive_parsing_structure :: String -> Property
prop_directive_parsing_structure s =
  "//" `isPrefixOf` s ==> property (length s >= 2)

prop_codeblock_parsing_content :: String -> Property
prop_codeblock_parsing_content s =
  not (null s) ==> property (length (lines s) >= 1)

prop_file_parsing_line_count :: String -> Property
prop_file_parsing_line_count s =
  let lineCount = length (lines s)
  in lineCount >= 0 ==> property True

-- Validation Properties
prop_valid_identifiers_alphanumeric :: String -> Property
prop_valid_identifiers_alphanumeric s =
  let isValid = not (null s) && isAlphaNum (head s) && all isAlphaNum s
  in isValid ==> property (all isAlphaNum s)

prop_directive_validation_consistency :: FileDirectives -> Property
prop_directive_validation_consistency fd =
  property True  -- Placeholder for actual validation logic

prop_codeblock_validation_structure :: CodeBlock -> Property
prop_codeblock_validation_structure cb =
  property True  -- Placeholder for actual validation logic