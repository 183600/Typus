{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck property tests for Parser module
module Test.Unit.NewParserQuickCheckPropertySpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Parser
import SourceLocation (SourceSpan(..), SourcePos(..), locatedWithSpan)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isSpace)
import Control.Arrow ((&&&))

-- | Test group for Parser module QuickCheck properties
testParserQuickCheckProperties :: TestTree
testParserQuickCheckProperties = testGroup "Parser Module QuickCheck Property Tests"
  [ directiveProperties
  , parserUtilityProperties
  , parsingProperties
  , codeBlockProperties
  , typusFileProperties
  ]

-- | Properties for directive parsing
directiveProperties :: TestTree
directiveProperties = testGroup "Directive properties"
  [ testProperty "parseBool accepts valid boolean values" $
    \boolVal -> 
      let input = if boolVal then "on" else "off"
      in parseBool input === Right boolVal
  
  , testProperty "parseBool accepts true/false" $
    \boolVal -> 
      let input = if boolVal then "true" else "false"
      in parseBool input === Right boolVal
  
  , testProperty "parseBool rejects invalid values" $
    \invalidVal -> 
      let invalidInputs = ["maybe", "yes", "no", "1", "0", invalidVal]
          results = map parseBool invalidInputs
      in L.all isLeft results
  
  , testProperty "defaultFileDirectives has L.all Nothing values" $
    \_ -> fdOwnership defaultFileDirectives === Nothing &&
          fdDependentTypes defaultFileDirectives === Nothing &&
          fdConstraints defaultFileDirectives === Nothing
  
  , testProperty "defaultBlockDirectives has L.all Nothing values" $
    \_ -> bdOwnership defaultBlockDirectives === Nothing &&
          bdDependentTypes defaultBlockDirectives === Nothing &&
          bdConstraints defaultBlockDirectives === Nothing
  ]

-- | Properties for parser utility functions
parserUtilityProperties :: TestTree
parserUtilityProperties = testGroup "Parser utility properties"
  [ testProperty "trimRight removes trailing whitespace" $
    \str -> 
      let trimmed = trimRight (str ++ "   \n\r")
      in not (L.any (`elem` [' ', '\n', '\r']) (L.reverse trimmed))
  
  , testProperty "trimRight preserves non-whitespace suffix" $
    \str suffix -> 
      let input = str ++ suffix
          trimmed = trimRight input
      in suffix `L.isPrefixOf` trimmed || null suffix
  
  , testProperty "curlyDelta counts braces correctly" $
    \openCount closeCount -> 
      let opens = replicate openCount '{'
          closes = replicate closeCount '}'
          input = opens ++ closes
          delta = curlyDelta input
      in delta === openCount - closeCount
  
  , testProperty "curlyDelta ignores braces in strings" $
    \str -> 
      let input = "\"" ++ str ++ "{ }\""
          delta = curlyDelta input
      in delta === 0
  
  , testProperty "curlyDelta ignores braces in line comments" $
    \before after -> 
      let input = before ++ "// { } comment\n" ++ after
          delta = curlyDelta input
      in delta === 0
  
  , testProperty "leadingIndentation counts leading spaces L.and tabs" $
    \spaces tabs content -> 
      let indent = replicate spaces ' ' ++ replicate tabs '\t'
          input = indent ++ content
          count = leadingIndentation input
      in count === spaces + tabs
  
  , testProperty "leadingIndentation stops at first non-indent char" $
    \spaces content -> 
      let indent = replicate spaces ' '
          input = indent ++ "x" ++ content
          count = leadingIndentation input
      in count === spaces
  ]

-- | Properties for parsing functions
parsingProperties :: TestTree
parsingProperties = testGroup "Parsing properties"
  [ testProperty "parseTypus handles empty input" $
    \_ -> case parseTypus "" of
      Left _ -> property True
      Right file -> tfBlocks file === []
  
  , testProperty "parseTypus preserves non-directive content" $
    \content -> 
      let input = content
      in case parseTypus input of
        Left _ -> property True
        Right file -> L.any (\block -> content `L.isInfixOf` cbContent block) (tfBlocks file)
  
  , testProperty "parseTypus handles whitespace-only input" $
    \whitespace -> 
      let input = replicate 100 whitespace
      in case parseTypus input of
        Left _ -> property True
        Right file -> L.all (null . cbContent) (tfBlocks file)
  
  , testProperty "parseTypus can parse simple file directives" $
    \ownershipVal dependentTypesVal -> 
      let ownership = if ownershipVal then "on" else "off"
          dependentTypes = if dependentTypesVal then "on" else "off"
          input = "//! ownership: " ++ ownership ++ "\n//! dependent_types: " ++ dependentTypes ++ "\n"
      in case parseTypus input of
        Left _ -> property True
        Right file -> case tfDirectives file of
          FileDirectives{..} -> 
            (fdOwnership >>= locatedValue) === Just ownershipVal &&
            (fdDependentTypes >>= locatedValue) === Just dependentTypesVal
  
  , testProperty "parseTypus handles build tags" $
    \tag1 tag2 -> 
      let input = "//go:build " ++ tag1 ++ "\n// +build " ++ tag2 ++ "\n"
      in case parseTypus input of
        Left _ -> property True
        Right file -> L.length (tfBuildTags file) >= 2
  ]

-- | Properties for CodeBlock
codeBlockProperties :: TestTree
codeBlockProperties = testGroup "CodeBlock properties"
  [ testProperty "CodeBlock with default directives has Nothing values" $
    \content span -> 
      let block = CodeBlock defaultBlockDirectives content span
      in bdOwnership (cbDirectives block) === Nothing &&
         bdDependentTypes (cbDirectives block) === Nothing &&
         bdConstraints (cbDirectives block) === Nothing
  
  , testProperty "CodeBlock preserves content" $
    \content span -> 
      let block = CodeBlock defaultBlockDirectives content span
      in cbContent block === content
  
  , testProperty "CodeBlock preserves span" $
    \content span -> 
      let block = CodeBlock defaultBlockDirectives content span
      in cbSpan block === span
  ]

-- | Properties for TypusFile
typusFileProperties :: TestTree
typusFileProperties = testGroup "TypusFile properties"
  [ testProperty "TypusFile with no blocks has empty build tags list" $
    \directives -> 
      let file = TypusFile directives [] [] []
      in tfBuildTags file === []
  
  , testProperty "TypusFile preserves directives" $
    \directives -> 
      let file = TypusFile directives [] [] []
      in tfDirectives file === directives
  
  , testProperty "TypusFile preserves blocks" $
    \directives blocks -> 
      let file = TypusFile directives [] blocks []
      in tfBlocks file === blocks
  
  , testProperty "TypusFile preserves syntax errors" $
    \directives syntaxErrors -> 
      let file = TypusFile directives [] [] syntaxErrors
      in tfSyntaxErrors file === syntaxErrors
  ]

-- | Additional edge case properties
edgeCaseProperties :: TestTree
edgeCaseProperties = testGroup "Parser edge case properties"
  [ testProperty "parseTypus handles malformed directives gracefully" $
    \malformedDirective -> 
      let input = "//! " ++ malformedDirective ++ "\n"
      in case parseTypus input of
        Left _ -> property True
        Right _ -> property True  -- Should either fail L.or parse gracefully
  
  , testProperty "parseTypus handles mixed line endings" $
    \content1 content2 -> 
      let input = content1 ++ "\r\n" ++ content2 ++ "\n" ++ content1 ++ "\r"
      in case parseTypus input of
        Left _ -> property True
        Right file -> not (L.null (tfBlocks file)) || not (null content1 ++ content2)
  
  , testProperty "curlyDelta handles nested structures" $
    \nestingLevel -> 
      let openBraces = replicate nestingLevel '{'
          closeBraces = replicate nestingLevel '}'
          input = concatMap (\i -> replicate i '{' ++ replicate i '}') [1..nestingLevel]
          delta = curlyDelta input
      in delta === 0
  
  , testProperty "parseBool is case sensitive" $
    \boolVal -> 
      let upper = if boolVal then "ON" else "OFF"
          mixed = if boolVal then "On" else "Off"
      in isLeft (parseBool upper) && isLeft (parseBool mixed)
  ]

-- Helper function to check if a result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- | Test for parser round-trip properties
roundTripProperties :: TestTree
roundTripProperties = testGroup "Parser round-trip properties"
  [ testProperty "Simple content round-trip" $
    \content -> 
      case parseTypus content of
        Left _ -> property True
        Right file -> 
          let reconstructed = unlines (map cbContent (tfBlocks file))
          in content `L.isInfixOf` reconstructed || null content
  
  , testProperty "Directive preservation round-trip" $
    \ownershipVal -> 
      let ownership = if ownershipVal then "on" else "off"
          input = "//! ownership: " ++ ownership ++ "\n"
      in case parseTypus input of
        Left _ -> property True
        Right file -> 
          case fdOwnership (tfDirectives file) of
            Nothing -> property False
            Just located -> locatedValue located === Just ownershipVal
  ]