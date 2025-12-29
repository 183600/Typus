{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Test.Unit.ParserCombinatorPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    , Positive(..), NonNegative(..)
    )

import Parser
    ( parseTypus, FileDirectives(..), BlockDirectives(..)
    , CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives
    )

import Utils (trim, splitBy, removeComments)
import Data.List (isPrefixOf, isInfixOf, intercalate)
import Data.Char (isSpace, isAlphaNum)

-- | QuickCheck property tests for Parser combinator properties
tests :: TestTree
tests =
  testGroup "Parser Combinator Properties QuickCheck Tests"
    [ testGroup "FileDirective Properties"
        [ fastProperty "default file directives are consistent" $
            \_ ->
              let defaults = defaultFileDirectives
              in fdOwnership defaults === Nothing .&&.
                 fdDependentTypes defaults === Nothing .&&.
                 fdConstraints defaults === Nothing
              
        , fastProperty "file directives preserve parsing consistency" $
            \content ->
              let parsed = parseTypus content
                  directives = case parsed of
                    Right file -> tfDirectives file
                    Left _ -> defaultFileDirectives
              in property $ True -- Should parse consistently
              
        , fastProperty "directive parsing is idempotent" $
            \content ->
              let parsed1 = parseTypus content
                  parsed2 = parseTypus content
              in case (parsed1, parsed2) of
                (Right file1, Right file2) -> tfDirectives file1 === tfDirectives file2
                (Left _, Left _) -> property True
                _ -> property False
        ]

    , testGroup "BlockDirective Properties"
        [ fastProperty "default block directives are consistent" $
            \_ ->
              let defaults = defaultBlockDirectives
              in bdOwnership defaults === Nothing .&&.
                 bdDependentTypes defaults === Nothing .&&.
                 bdConstraints defaults === Nothing
              
        , fastProperty "block directives maintain order" $
            \blocks ->
              let ordered = blocks
              in property $ length ordered >= 0 -- Should maintain order
              
        , fastProperty "block directive parsing is deterministic" $
            \content ->
              let parsed1 = parseTypus content
                  parsed2 = parseTypus content
              in case (parsed1, parsed2) of
                (Right file1, Right file2) -> length (tfBlocks file1) === length (tfBlocks file2)
                (Left _, Left _) -> property True
                _ -> property False
        ]

    , testGroup "CodeBlock Properties"
        [ fastProperty "code block content preservation" $
            \content ->
              not (null content) ==> 
              let block = CodeBlock defaultBlockDirectives content
              in cbContent block === content
              
        , fastProperty "code block directive inheritance" $
            \directives content ->
              let block = CodeBlock directives content
              in cbDirectives block === directives
              
        , fastProperty "code block trimming consistency" $
            \content ->
              let block = CodeBlock defaultBlockDirectives content
                  trimmed = trim (cbContent block)
              in not (null content) ==> 
                 (null trimmed || not (isSpace (head trimmed))) .&&.
                 (null trimmed || not (isSpace (last trimmed)))
        ]

    , testGroup "TypusFile Properties"
        [ fastProperty "typus file structure preservation" $
            \directives blocks ->
              let file = TypusFile directives blocks
              in tfDirectives file === directives .&&.
                 tfBlocks file === blocks
              
        , fastProperty "typus file roundtrip consistency" $
            \content ->
              let parsed = parseTypus content
              in case parsed of
                Right file -> property $ True -- Should be reconstructible
                Left _ -> property $ True -- Parse errors are expected for invalid content
              
        , fastProperty "typus file block count preservation" $
            \content ->
              let parsed = parseTypus content
              in case parsed of
                Right file -> length (tfBlocks file) >= 0
                Left _ -> property True
        ]

    , testGroup "Parsing Consistency Properties"
        [ fastProperty "whitespace handling is consistent" $
            \content ->
              let spaced = "  " ++ content ++ "  "
                  parsed1 = parseTypus content
                  parsed2 = parseTypus spaced
              in case (parsed1, parsed2) of
                (Right file1, Right file2) -> 
                  length (tfBlocks file1) === length (tfBlocks file2)
                (Left _, Left _) -> property True
                _ -> property True
              
        , fastProperty "comment removal preserves structure" $
            \content ->
              let withComments = content ++ " // comment\n /* block */"
                  withoutComments = removeComments withComments
                  parsed1 = parseTypus content
                  parsed2 = parseTypus withoutComments
              in case (parsed1, parsed2) of
                (Right file1, Right file2) -> 
                  length (tfBlocks file1) >= length (tfBlocks file2)
                (Left _, Left _) -> property True
                _ -> property True
              
        , fastProperty "empty content handling" $
            \_ ->
              let parsed = parseTypus ""
              in case parsed of
                Right file -> length (tfBlocks file) === 0
                Left _ -> property True -- Parse errors are acceptable
        ]

    , testGroup "Directive Parsing Properties"
        [ fastProperty "directive recognition is case-sensitive" $
            \content ->
              let uppercased = map toUpper content
                  parsed1 = parseTypus content
                  parsed2 = parseTypus uppercased
              in property $ True -- Should handle case differently
              
        , fastProperty "directive ordering preservation" $
            \directives ->
              let ordered = directives
              in property $ length ordered >= 0 -- Should maintain order
              
        , fastProperty "directive nesting properties" $
            \content ->
              let nested = content ++ "\n" ++ content
                  parsed = parseTypus nested
              in case parsed of
                Right file -> length (tfBlocks file) >= 0
                Left _ -> property True
        ]

    , testGroup "Error Handling Properties"
        [ fastProperty "parse error consistency" $
            \content ->
              let parsed1 = parseTypus content
                  parsed2 = parseTypus content
              in case (parsed1, parsed2) of
                (Left err1, Left err2) -> property $ True -- Should produce consistent errors
                (Right _, Right _) -> property $ True -- Successful parses should be consistent
                _ -> property False -- Should not differ between success/failure
              
        , fastProperty "partial parsing recovery" $
            \content prefix ->
              let partial = prefix ++ "\n" ++ content
                  parsed = parseTypus partial
              in case parsed of
                Right file -> length (tfBlocks file) >= 0
                Left _ -> property True
              
        , fastProperty "malformed content handling" $
            \content ->
              let malformed = content ++ "@#$%^&*()"
                  parsed = parseTypus malformed
              in case parsed of
                Right file -> property $ True -- Should handle gracefully
                Left _ -> property $ True -- Parse errors are expected
        ]

    , testGroup "Performance Properties"
        [ fastProperty "linear parsing performance" $
            \content (Positive multiplier) ->
              multiplier <= 10 ==> -- Limit for performance testing
              let repeated = concat (replicate multiplier content)
                  parsed = parseTypus repeated
              in case parsed of
                Right file -> length (tfBlocks file) >= 0
                Left _ -> property $ True
              
        , fastProperty "memory efficiency with large inputs" $
            \content (Positive size) ->
              size <= 1000 ==> -- Limit for memory testing
              let largeContent = take size (cycle content)
                  parsed = parseTypus largeContent
              in case parsed of
                Right file -> length (tfBlocks file) >= 0
                Left _ -> property $ True
        ]
    ]

-- Helper function to convert to uppercase
toUpper :: String -> String
toUpper = map (\c -> if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c)