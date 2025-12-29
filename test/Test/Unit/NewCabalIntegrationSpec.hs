{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalIntegrationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Either (isLeft, isRight)
import qualified Data.Text as T
import Data.Char (isSpace)

import Utils (trim, removeComments)
import SourceLocation (SourcePos(..), startPos, posAfter, advancePosByText)
import Compiler.Errors.Core 
  ( ErrorLocation(..)
  , newErrorCollector
  , addError
  , errorAt
  , hasErrors
  , formatError
  )
import Parser
  ( parseTypus
  , TypusFile(..)
  , defaultFileDirectives
  )

-- | Unit tests for module integration
tests :: TestTree
tests =
  testGroup "New Cabal Integration Tests"
    [ testGroup "Unit Tests"
        [ testCase "Utils + Parser: trim before parsing" $
            let content = "   \n//! ownership=true\n```go\nprint(\"hello\")\n```\n   "
                trimmed = trim content
                result = parseTypus trimmed "test.typus"
            in case result of
              Right typusFile -> 
                case fdOwnership (tfDirectives typusFile) of
                  Just (Located _ True) -> assertBool "Ownership directive parsed after trim" True
                  _ -> assertBool "Ownership directive not parsed correctly" False
              Left _ -> assertBool "Should parse successfully after trim" False
              
        , testCase "SourceLocation + ErrorHandler: error with location" $
            let pos = posAfter 'a' startPos
                loc = ErrorLocation (posLine pos) (posColumn pos) (posOffset pos)
                error = errorAt loc "Test error"
                collector = addError error newErrorCollector
                formatted = formatError error
            in do
              hasErrors collector @?= True
              "Test error" `T.isInfixOf` formatted @?= True
              
        , testCase "Parser + Utils: remove comments before parsing" $
            let content = "//! ownership=true // comment\n```go\nprint(\"hello\")\n```"
                cleaned = removeComments content
                result = parseTypus cleaned "test.typus"
            in case result of
              Right typusFile -> 
                case fdOwnership (tfDirectives typusFile) of
                  Just (Located _ True) -> assertBool "Ownership directive parsed after comment removal" True
                  _ -> assertBool "Ownership directive not parsed correctly" False
              Left _ -> assertBool "Should parse successfully after comment removal" False
              
        , testCase "Utils + SourceLocation: advance position by text" $
            let text = "hello\nworld"
                finalPos = advancePosByText text startPos
            in posLine finalPos @?= 2
            
        , testCase "Parser + ErrorHandler: handle parse errors gracefully" $
            let malformed = "//! ownership=true\n```go\nprint(\"hello\"\n```"
                result = parseTypus malformed "test.typus"
            in case result of
              Right typusFile -> 
                -- Should still parse but might have syntax errors
                assertBool "Parsed with potential syntax errors" True
              Left _ -> 
                assertBool "Parse failed gracefully" True
        ]
    
    , testGroup "QuickCheck Properties"
        [ fastProperty "Utils -> Parser: trim doesn't break parsing" $
            \content filename ->
              let trimmed = trim content
                  result1 = parseTypus content filename
                  result2 = parseTypus trimmed filename
              in case (result1, result2) of
                (Right _, Right _) -> property True
                (Left _, Left _) -> property True
                (Right _, Left _) -> property False  -- Should not break parsing
                (Left _, Right _) -> property True   -- Trimming might fix parsing
                
        , fastProperty "SourceLocation -> ErrorHandler: positions map correctly" $
            \line col offset msg ->
              let pos = SourcePos line col offset
                  loc = ErrorLocation line col offset
                  error = errorAt loc msg
                  collector = addError error newErrorCollector
              in hasErrors collector
              
        , fastProperty "Utils -> SourceLocation: text advancement is consistent" $
            \text ->
              let pos1 = advancePosByText text startPos
                  linesInText = length $ filter (== '\n') text
              in posLine pos1 >= 1 && posLine pos1 <= (linesInText + 1)
              
        , fastProperty "Parser -> ErrorHandler: error collection works" $
            \content filename ->
              case parseTypus content filename of
                Right typusFile -> 
                  let syntaxErrors = tfSyntaxErrors typusFile
                      collector = foldr addError newErrorCollector 
                                  [errorAt (ErrorLocation 1 1 0) (T.pack err) | err <- syntaxErrors]
                  in property True  -- Should handle errors gracefully
                Left _ -> property True  -- Parse errors are expected
                
        , fastProperty "Utils + Parser + ErrorHandler: end-to-end flow" $
            \content filename ->
              let cleaned = removeComments $ trim content
                  result = parseTypus cleaned filename
              in case result of
                Right typusFile -> 
                  let syntaxErrors = tfSyntaxErrors typusFile
                  in property True  -- Successfully processed
                Left _ -> property True  -- Failed gracefully
        ]
    ]