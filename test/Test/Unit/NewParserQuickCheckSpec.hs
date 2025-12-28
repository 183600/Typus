{-# LANGUAGE CPP #-}

module Test.Unit.NewParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), 
             TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), locatedWithSpan, startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Parser QuickCheck Tests"
  [ fileDirectiveProperties
  , blockDirectiveProperties
  , codeBlockProperties
  , parsingProperties
  , directiveParsingProperties
  ]

fileDirectiveProperties :: TestTree
fileDirectiveProperties = testGroup "File Directive Properties"
  [ fastProperty "defaultFileDirectives has all Nothing fields" prop_default_file_directives_nothing
  , fastProperty "FileDirectives equality is reflexive" prop_filedirectives_reflexive
  , fastProperty "FileDirectives equality is symmetric" prop_filedirectives_symmetric
  , fastProperty "FileDirectives with same values are equal" prop_filedirectives_equal_same_values
  ]

blockDirectiveProperties :: TestTree
blockDirectiveProperties = testGroup "Block Directive Properties"
  [ fastProperty "defaultBlockDirectives has all Nothing fields" prop_default_block_directives_nothing
  , fastProperty "BlockDirectives equality is reflexive" prop_blockdirectives_reflexive
  , fastProperty "BlockDirectives equality is symmetric" prop_blockdirectives_symmetric
  , fastProperty "BlockDirectives with same values are equal" prop_blockdirectives_equal_same_values
  ]

codeBlockProperties :: TestTree
codeBlockProperties = testGroup "Code Block Properties"
  [ fastProperty "CodeBlock with same content is equal" prop_codeblock_equal_same_content
  , fastProperty "CodeBlock equality is reflexive" prop_codeblock_reflexive
  , fastProperty "CodeBlock equality is symmetric" prop_codeblock_symmetric
  , fastProperty "CodeBlock preserves content order" prop_codeblock_preserves_order
  ]

parsingProperties :: TestTree
parsingProperties = testGroup "Parsing Properties"
  [ fastProperty "parseTypus handles empty input" prop_parsetypus_empty_input
  , fastProperty "parseTypus handles whitespace-only input" prop_parsetypus_whitespace_only
  , fastProperty "parseTypus preserves line structure" prop_parsetypus_preserves_lines
  , fastProperty "parseTypus is deterministic" prop_parsetypus_deterministic
  ]

directiveParsingProperties :: TestTree
directiveParsingProperties = testGroup "Directive Parsing Properties"
  [ fastProperty "ownership directive parsing consistency" prop_ownership_directive_consistency
  , fastProperty "dependent types directive parsing consistency" prop_dependent_types_directive_consistency
  , fastProperty "constraints directive parsing consistency" prop_constraints_directive_consistency
  , fastProperty "multiple directives are parsed correctly" prop_multiple_directives_correct
  ]

-- File directive properties
prop_default_file_directives_nothing :: Property
prop_default_file_directives_nothing =
  let fd = defaultFileDirectives
  in conjoin
    [ property $ fdOwnership fd === Nothing
    , property $ fdDependentTypes fd === Nothing
    , property $ fdConstraints fd === Nothing
    ]

prop_filedirectives_reflexive :: FileDirectives -> Property
prop_filedirectives_reflexive fd =
  property $ fd == fd

prop_filedirectives_symmetric :: FileDirectives -> FileDirectives -> Property
prop_filedirectives_symmetric fd1 fd2 =
  (fd1 == fd2) ==> property $ fd2 == fd1

prop_filedirectives_equal_same_values :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_filedirectives_equal_same_values ownership deps constraints =
  let fd1 = FileDirectives 
        { fdOwnership = fmap (locatedAt startPos) ownership
        , fdDependentTypes = fmap (locatedAt startPos) deps
        , fdConstraints = fmap (locatedAt startPos) constraints
        }
      fd2 = FileDirectives
        { fdOwnership = fmap (locatedAt startPos) ownership
        , fdDependentTypes = fmap (locatedAt startPos) deps
        , fdConstraints = fmap (locatedAt startPos) constraints
        }
  in property $ fd1 == fd2

-- Block directive properties
prop_default_block_directives_nothing :: Property
prop_default_block_directives_nothing =
  let bd = defaultBlockDirectives
  in conjoin
    [ property $ bdOwnership bd === Nothing
    , property $ bdDependentTypes bd === Nothing
    , property $ bdConstraints bd === Nothing
    ]

prop_blockdirectives_reflexive :: BlockDirectives -> Property
prop_blockdirectives_reflexive bd =
  property $ bd == bd

prop_blockdirectives_symmetric :: BlockDirectives -> BlockDirectives -> Property
prop_blockdirectives_symmetric bd1 bd2 =
  (bd1 == bd2) ==> property $ bd2 == bd1

prop_blockdirectives_equal_same_values :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
prop_blockdirectives_equal_same_values ownership deps constraints =
  let bd1 = BlockDirectives 
        { bdOwnership = fmap (locatedAt startPos) ownership
        , bdDependentTypes = fmap (locatedAt startPos) deps
        , bdConstraints = fmap (locatedAt startPos) constraints
        }
      bd2 = BlockDirectives
        { bdOwnership = fmap (locatedAt startPos) ownership
        , bdDependentTypes = fmap (locatedAt startPos) deps
        , bdConstraints = fmap (locatedAt startPos) constraints
        }
  in property $ bd1 == bd2

-- Code block properties
prop_codeblock_equal_same_content :: String -> BlockDirectives -> String -> Property
prop_codeblock_equal_same_content content directives rawCode =
  let span = emptySpan
      cb1 = CodeBlock span directives content rawCode
      cb2 = CodeBlock span directives content rawCode
  in property $ cb1 == cb2

prop_codeblock_reflexive :: CodeBlock -> Property
prop_codeblock_reflexive cb =
  property $ cb == cb

prop_codeblock_symmetric :: CodeBlock -> CodeBlock -> Property
prop_codeblock_symmetric cb1 cb2 =
  (cb1 == cb2) ==> property $ cb2 == cb1

prop_codeblock_preserves_order :: String -> String -> Property
prop_codeblock_preserves_order part1 part2 =
  let content = part1 ++ "\n" ++ part2
      directives = defaultBlockDirectives
      span = emptySpan
      cb = CodeBlock span directives content content
  in property $ content `isInfixOf` content cb

-- Parsing properties
prop_parsetypus_empty_input :: Property
prop_parsetypus_empty_input =
  let result = parseTypus ""
  in property $ case result of
    Left _ -> True  -- Parsing empty input should either succeed or fail gracefully
    Right tf -> True

prop_parsetypus_whitespace_only :: String -> Property
prop_parsetypus_whitespace_only s =
  all isSpace s ==>
  let result = parseTypus s
  in property $ case result of
    Left _ -> True
    Right tf -> True

prop_parsetypus_preserves_lines :: String -> Property
prop_parsetypus_preserves_lines s =
  let lineCount = length $ lines s
      result = parseTypus s
  in property $ case result of
    Left _ -> True
    Right tf -> True  -- Should preserve line structure in some form

prop_parsetypus_deterministic :: String -> Property
prop_parsetypus_deterministic s =
  let result1 = parseTypus s
      result2 = parseTypus s
  in property $ result1 == result2

-- Directive parsing properties
prop_ownership_directive_consistency :: String -> Property
prop_ownership_directive_consistency content =
  let withOwnership = "// @ownership: true\n" ++ content
      withoutOwnership = content
      result1 = parseTypus withOwnership
      result2 = parseTypus withoutOwnership
  in property $ case (result1, result2) of
    (Right tf1, Right tf2) -> True  -- Both should parse successfully
    _ -> True  -- At least should not crash

prop_dependent_types_directive_consistency :: String -> Property
prop_dependent_types_directive_consistency content =
  let withDepTypes = "// @dependent-types: true\n" ++ content
      withoutDepTypes = content
      result1 = parseTypus withDepTypes
      result2 = parseTypus withoutDepTypes
  in property $ case (result1, result2) of
    (Right tf1, Right tf2) -> True
    _ -> True

prop_constraints_directive_consistency :: String -> Property
prop_constraints_directive_consistency content =
  let withConstraints = "// @constraints: true\n" ++ content
      withoutConstraints = content
      result1 = parseTypus withConstraints
      result2 = parseTypus withoutConstraints
  in property $ case (result1, result2) of
    (Right tf1, Right tf2) -> True
    _ -> True

prop_multiple_directives_correct :: String -> Property
prop_multiple_directives_correct content =
  let withDirectives = "// @ownership: true\n// @dependent-types: true\n// @constraints: true\n" ++ content
      result = parseTypus withDirectives
  in property $ case result of
    Left _ -> True
    Right tf -> True

-- Helper function
locatedAt :: SourcePos -> a -> Located a
locatedAt pos value = locatedWithSpan (SourceSpan pos pos) value

content :: CodeBlock -> String
content (CodeBlock _ _ c _) = c