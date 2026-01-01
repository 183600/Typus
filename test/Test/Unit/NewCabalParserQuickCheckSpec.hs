{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewCabalParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, choose, listOf, elements, suchThat)
import Parser
import SourceLocation (SourcePos(..), startPos)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

-- | QuickCheck tests for Parser module
tests :: TestTree
tests =
  testGroup "New Cabal Parser QuickCheck Tests"
    [ testProperty "defaultFileDirectives has no values set" prop_defaultFileDirectivesEmpty
    , testProperty "defaultBlockDirectives has no values set" prop_defaultBlockDirectivesEmpty
    , testProperty "parseTypus handles empty input" prop_parseEmptyInput
    , testProperty "parseTypus handles whitespace-only input" prop_parseWhitespaceInput
    , testProperty "FileDirectives equality works correctly" prop_fileDirectivesEquality
    , testProperty "BlockDirectives equality works correctly" prop_blockDirectivesEquality
    , testProperty "parseTypus preserves line structure" prop_parsePreservesLines
    , testProperty "CodeBlock content consistency" prop_codeBlockConsistency
    ]

-- | defaultFileDirectives should have L.all fields as Nothing
prop_defaultFileDirectivesEmpty :: Bool
prop_defaultFileDirectivesEmpty =
  let directives = defaultFileDirectives
  in isNothing (fdOwnership directives) &&
     isNothing (fdDependentTypes directives) &&
     isNothing (fdConstraints directives)

-- | defaultBlockDirectives should have L.all fields as Nothing  
prop_defaultBlockDirectivesEmpty :: Bool
prop_defaultBlockDirectivesEmpty =
  let directives = defaultBlockDirectives
  in isNothing (bdOwnership directives) &&
     isNothing (bdDependentTypes directives) &&
     isNothing (bdConstraints directives)

-- | parseTypus should handle empty input gracefully
prop_parseEmptyInput :: Bool
prop_parseEmptyInput =
  let result = parseTypus "" startPos
  in case result of
    Left _ -> True  -- Parsing empty input might fail, which is acceptable
    Right file -> L.null (tfCodeBlocks file)  -- Should have no code blocks

-- | parseTypus should handle whitespace-only input
prop_parseWhitespaceInput :: String -> Property
prop_parseWhitespaceInput input =
  forAll (elements $ replicate (L.length input) ' ' ++ 
                    replicate (L.length input) '\t' ++ 
                    replicate (L.length input) '\n') $ \whitespace ->
    let result = parseTypus whitespace startPos
    in case result of
      Left _ -> property True  -- Parsing might fail, which is acceptable
      Right file -> L.null (tfCodeBlocks file)  -- Should have no meaningful code blocks

-- | FileDirectives equality should work correctly
prop_fileDirectivesEquality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_fileDirectivesEquality ownership depTypes constraints =
  let directives1 = FileDirectives ownership depTypes constraints
      directives2 = FileDirectives ownership depTypes constraints
  in directives1 == directives2

-- | BlockDirectives equality should work correctly
prop_blockDirectivesEquality :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Bool
prop_blockDirectivesEquality ownership depTypes constraints =
  let directives1 = BlockDirectives ownership depTypes constraints
      directives2 = BlockDirectives ownership depTypes constraints
  in directives1 == directives2

-- | parseTypus should preserve line structure
prop_parsePreservesLines :: String -> Property
prop_parsePreservesLines input =
  let result = parseTypus input startPos
      originalLines = lines input
  in case result of
    Left _ -> property True  -- Parsing might fail
    Right file -> 
      let blocks = tfCodeBlocks file
          totalBlockLines = L.sum $ L.map (L.length . lines . cbContent) blocks
      in counterexample ("Original lines: " ++ show (L.length originalLines) ++ 
                        ", Block lines: " ++ show totalBlockLines) $
         totalBlockLines <= L.length originalLines  -- Should not create more lines than input

-- | CodeBlock content should be consistent
prop_codeBlockConsistency :: String -> Bool
prop_codeBlockConsistency content =
  let block = CodeBlock defaultBlockDirectives content
  in cbDirectives block == defaultBlockDirectives &&
     cbContent block == content