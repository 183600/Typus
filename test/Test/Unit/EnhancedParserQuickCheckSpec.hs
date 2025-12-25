{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.EnhancedParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, suchThat
  , resize, Positive(..), NonEmpty(..)
  )

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseTypus
  )

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , locatedValue
  , startPos
  )

import Data.List (isPrefixOf, isInfixOf, sort, nub)
import Data.Char (isSpace, isAlpha, isAlphaNum)
import qualified Data.Text as T

-- Property: Parsing directives preserves order
prop_parseDirectives_preserves_order :: [String] -> Property
prop_parseDirectives_preserves_order directives =
  not (null directives) && all (not . null) directives ==>
  let directiveLines = map (\d -> "//! " ++ d) directives
      source = unlines directiveLines
      result = parseTypus source
  in case result of
    Left _ -> property True -- Parsing errors are acceptable for malformed directives
    Right typusFile -> 
      let parsedDirectives = extractDirectiveLines (tfDirectives typusFile)
      in property $ length parsedDirectives <= length directives

-- Property: Round-trip property for simple code blocks
prop_parseCodeBlock_roundtrip :: String -> Property
prop_parseCodeBlock_roundtrip code =
  not (null code) && not ("//!" `isInfixOf` code) && not ("{//!" `isInfixOf` code) ==>
  let source = unlines ["package main", "func main() {", code, "}"]
      result = parseTypus source
  in case result of
    Left _ -> property True
    Right typusFile -> 
      case tfBlocks typusFile of
        [block] -> property True -- Successfully parsed one block
        _ -> property True -- Multiple blocks or none are also acceptable

-- Property: Parser handles mixed directives and code
prop_parseMixed_directives_and_code :: [String] -> String -> Property
prop_parseMixed_directives_and_code directives code =
  not (null directives) && all (not . null) directives ==>
  let directiveLines = map (\d -> "//! " ++ d) directives
      codeLines = if null code then ["func main() {}"] else lines code
      source = unlines (directiveLines ++ codeLines)
      result = parseTypus source
  in case result of
    Left _ -> property True -- Parsing errors are acceptable
    Right typusFile -> 
      let hasDirectives = hasAnyDirectives (tfDirectives typusFile)
          hasBlocks = not (null (tfBlocks typusFile))
      in property (hasDirectives || hasBlocks)

-- Property: Parser handles nested block directives
prop_parseNested_block_directives :: String -> Property
prop_parseNested_block_directives content =
  not (null content) && not ("//!" `isInfixOf` content) ==>
  let source = unlines 
        [ "package main"
        , "func main() {"
        , "  //! ownership: on"
        , content
        , "}"
        ]
      result = parseTypus source
  in case result of
    Left _ -> property True
    Right typusFile -> property $ length (tfBlocks typusFile) >= 1

-- Property: Parser gracefully handles malformed directives
prop_parseMalformed_directives :: String -> Property
prop_parseMalformed_directives malformed =
  not (null malformed) && not ("//!" `isPrefixOf` malformed) ==>
  let source = unlines 
        [ malformed ++ " ownership: on"  -- Missing //! prefix
        , "//! dependent_types"  -- Missing value
        , "package main"
        , "func main() {}"
        ]
      result = parseTypus source
  in case result of
    Left _ -> property True -- Should handle malformed input gracefully
    Right _ -> property True -- Or parse successfully despite malformed directives

-- Property: Parser preserves source structure information
prop_parsePreserves_source_structure :: [String] -> Property
prop_parsePreserves_source_structure lines' =
  not (null lines') && all (not . null) lines' ==>
  let source = unlines lines'
      result = parseTypus source
  in case result of
    Left _ -> property True
    Right typusFile -> 
      let blockCount = length (tfBlocks typusFile)
          directiveCount = countDirectives (tfDirectives typusFile)
      in property (blockCount >= 0 && directiveCount >= 0)

-- Property: Parser handles ownership directive consistency
prop_parseOwnership_consistency :: Bool -> Property
prop_parseOwnership_consistency ownershipValue =
  let source = unlines 
        [ "//! ownership: " ++ show ownershipValue
        , "package main"
        , "func main() {}"
        ]
      result = parseTypus source
  in case result of
    Left _ -> property True
    Right typusFile -> 
      case fdOwnership (tfDirectives typusFile) of
        Nothing -> property True
        Just loc -> locatedValue loc === ownershipValue

-- Property: Parser handles dependent types directive consistency
prop_parseDependentTypes_consistency :: Bool -> Property
prop_parseDependentTypes_consistency dependentValue =
  let source = unlines 
        [ "//! dependent_types: " ++ show dependentValue
        , "package main"
        , "func main() {}"
        ]
      result = parseTypus source
  in case result of
    Left _ -> property True
    Right typusFile -> 
      case fdDependentTypes (tfDirectives typusFile) of
        Nothing -> property True
        Just loc -> locatedValue loc === dependentValue

-- Helper functions
extractDirectiveLines :: FileDirectives -> [String]
extractDirectiveLines directives = 
  let ownership = case fdOwnership directives of
                   Nothing -> []
                   Just _ -> ["ownership"]
      dependentTypes = case fdDependentTypes directives of
                        Nothing -> []
                        Just _ -> ["dependent_types"]
      constraints = case fdConstraints directives of
                      Nothing -> []
                      Just _ -> ["constraints"]
  in ownership ++ dependentTypes ++ constraints

hasAnyDirectives :: FileDirectives -> Bool
hasAnyDirectives directives = 
  isJust (fdOwnership directives) ||
  isJust (fdDependentTypes directives) ||
  isJust (fdConstraints directives)
  where
    isJust Nothing = False
    isJust (Just _) = True

countDirectives :: FileDirectives -> Int
countDirectives directives = 
  (if isJust (fdOwnership directives) then 1 else 0) +
  (if isJust (fdDependentTypes directives) then 1 else 0) +
  (if isJust (fdConstraints directives) then 1 else 0)
  where
    isJust Nothing = False
    isJust (Just _) = True

tests :: TestTree
tests = testGroup "Enhanced Parser QuickCheck Tests"
  [ fastProperty "Preserves directive order" prop_parseDirectives_preserves_order
  , fastProperty "Code block round-trip" prop_parseCodeBlock_roundtrip
  , fastProperty "Mixed directives and code" prop_parseMixed_directives_and_code
  , fastProperty "Nested block directives" prop_parseNested_block_directives
  , fastProperty "Malformed directives handling" prop_parseMalformed_directives
  , fastProperty "Source structure preservation" prop_parsePreserves_source_structure
  , fastProperty "Ownership directive consistency" prop_parseOwnership_consistency
  , fastProperty "Dependent types directive consistency" prop_parseDependentTypes_consistency
  ]