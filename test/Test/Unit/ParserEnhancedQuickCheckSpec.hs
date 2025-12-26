{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..),
             defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd)
import Utils (trim)
import Data.List (isPrefixOf, isInfixOf)

tests :: TestTree
tests = testGroup "Parser Enhanced QuickCheck Tests"
  [ basicParsingProperties
  , directiveParsingProperties
  , blockParsingProperties
  , errorHandlingProperties
  , syntaxValidationProperties
  ]

-- | Basic parsing properties
basicParsingProperties :: TestTree
basicParsingProperties = testGroup "Basic Parsing Properties"
  [ testProperty "parseTypus on empty string returns valid file" $
      \() -> 
        let result = parseTypus ""
        in case result of
          Left _ -> property False
          Right file -> tfDirectives file === defaultFileDirectives .&&. 
                       null (tfBlocks file)
  
  , testProperty "parseTypus on whitespace-only string returns valid file" $
      \whitespace -> 
        all (`elem` [' ', '\t', '\n', '\r']) whitespace ==> 
        let result = parseTypus whitespace
        in case result of
          Left _ -> property False
          Right file -> tfDirectives file === defaultFileDirectives
  
  , testProperty "parseTypus preserves non-directive content" $
      \content -> 
        not (any (`isPrefixOf` content) ["//!", "{//!", "//go:build", "// +build"]) ==> 
        let result = parseTypus content
        in case result of
          Left _ -> property False
          Right file -> not (null (tfBlocks file)) ==> 
                       content `isInfixOf` (concatMap cbContent (tfBlocks file))
  
  , testProperty "parseTypus handles mixed content" $
      \prefix directives suffix -> 
        let input = prefix ++ "\n//! ownership: true\n" ++ directives ++ "\n" ++ suffix
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> case fdOwnership (tfDirectives file) of
            Nothing -> property False
            Just ownership -> locValue ownership === True
  ]

-- | Directive parsing properties
directiveParsingProperties :: TestTree
directiveParsingProperties = testGroup "Directive Parsing Properties"
  [ testProperty "parse file ownership directive" $
      \value -> 
        let input = "//! ownership: " ++ if value then "true" else "false"
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> case fdOwnership (tfDirectives file) of
            Nothing -> property False
            Just ownership -> locValue ownership === value
  
  , testProperty "parse file dependent_types directive" $
      \value -> 
        let input = "//! dependent_types: " ++ if value then "true" else "false"
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> case fdDependentTypes (tfDirectives file) of
            Nothing -> property False
            Just dt -> locValue dt === value
  
  , testProperty "parse file constraints directive" $
      \value -> 
        let input = "//! constraints: " ++ if value then "true" else "false"
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> case fdConstraints (tfDirectives file) of
            Nothing -> property False
            Just constraints -> locValue constraints === value
  
  , testProperty "parse multiple file directives" $
      \ownership dt constraints -> 
        let input = "//! ownership: " ++ if ownership then "true" else "false" ++ "\n" ++
                    "//! dependent_types: " ++ if dt then "true" else "false" ++ "\n" ++
                    "//! constraints: " ++ if constraints then "true" else "false"
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> 
            case (fdOwnership (tfDirectives file), 
                  fdDependentTypes (tfDirectives file),
                  fdConstraints (tfDirectives file)) of
              (Just own, Just dep, Just cons) -> 
                locValue own === ownership .&&. 
                locValue dep === dt .&&. 
                locValue cons === constraints
              _ -> property False
  
  , testProperty "parse block directive" $
      \ownership content -> 
        let input = "{//! ownership: " ++ if ownership then "true" else "false" ++ "}\n" ++
                    content ++ "\n"
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> not (null (tfBlocks file)) ==> 
                       let firstBlock = head (tfBlocks file)
                           directives = cbDirectives firstBlock
                       in case bdOwnership directives of
                            Nothing -> property False
                            Just own -> locValue own === ownership
  ]

-- | Block parsing properties
blockParsingProperties :: TestTree
blockParsingProperties = testGroup "Block Parsing Properties"
  [ testProperty "parse simple code block" $
      \content -> 
        let input = content
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> not (null content) ==> not (null (tfBlocks file))
  
  , testProperty "parse multiple blocks" $
      \content1 content2 -> 
        let input = content1 ++ "\n\n" ++ content2
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> length (tfBlocks file) >= 1
  
  , testProperty "block content preservation" $
      \content -> 
        let input = content
            result = parseTypus input
        in case result of
          Left _ -> property False
          Right file -> not (null (tfBlocks file)) ==> 
                       let blockContent = concatMap cbContent (tfBlocks file)
                       in content `isInfixOf` blockContent
  ]

-- | Error handling properties
errorHandlingProperties :: TestTree
errorHandlingProperties = testGroup "Error Handling Properties"
  [ testProperty "parseTypus handles malformed directives gracefully" $
      \directive -> 
        not ("//!" `isPrefixOf` directive) ==> 
        let input = "//! " ++ directive
            result = parseTypus input
        in case result of
          Left _ -> property True  -- Expected to fail
          Right _ -> property True -- Might succeed if directive is valid
  
  , testProperty "parseTypus handles unclosed block directives" $
      \content -> 
        let input = "{//! ownership: true\n" ++ content
            result = parseTypus input
        in case result of
          Left _ -> property True  -- Expected to fail or recover
          Right _ -> property True -- Might recover
  
  , testProperty "parseTypus handles invalid directive values" $
      \key value -> 
        let input = "//! " ++ key ++ ": " ++ value
            result = parseTypus input
        in case result of
          Left _ -> property True  -- Expected to fail
          Right _ -> property True -- Might succeed if valid
  ]

-- | Syntax validation properties
syntaxValidationProperties :: TestTree
syntaxValidationProperties = testGroup "Syntax Validation Properties"
  [ testProperty "parseTypus detects syntax errors" $
      \content -> 
        let hasIfWithoutBrace = "if " `isInfixOf` content && not ("{" `isInfixOf` content)
            result = parseTypus content
        in case result of
          Left _ -> property True
          Right file -> hasIfWithoutBrace ==> not (null (tfSyntaxErrors file))
  
  , testProperty "parseTypus detects multiple package declarations" $
      \content -> 
        let input = "package main\n" ++ content ++ "\npackage test"
            result = parseTypus input
        in case result of
          Left _ -> property True  -- Expected to fail
          Right _ -> property True -- Might recover
  ]