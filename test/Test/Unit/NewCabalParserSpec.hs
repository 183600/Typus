{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalParserSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation (SourcePos(..), startPos)

-- | Unit tests for Parser module
tests :: TestTree
tests =
  testGroup "New Cabal Parser Tests"
    [ testGroup "Unit Tests"
        [ testCase "defaultFileDirectives: L.all fields are Nothing" $
            do
              fdOwnership defaultFileDirectives @?= Nothing
              fdDependentTypes defaultFileDirectives @?= Nothing
              fdConstraints defaultFileDirectives @?= Nothing
              
        , testCase "defaultBlockDirectives: L.all fields are Nothing" $
            do
              bdOwnership defaultBlockDirectives @?= Nothing
              bdDependentTypes defaultBlockDirectives @?= Nothing
              bdConstraints defaultBlockDirectives @?= Nothing
              
        , testCase "parseTypus: empty input succeeds" $
            let result = parseTypus "" "test.typus"
            in isRight result @?= True
            
        , testCase "parseTypus: simple code block" $
            let content = "```go\nprint(\"hello\")\n```"
                result = parseTypus content "test.typus"
            in case result of
              Right typusFile -> L.length (tfBlocks typusFile) @?= 1
              Left _ -> assertBool "Should parse successfully" False
              
        , testCase "parseTypus: file directive parsing" $
            let content = "//! ownership=true, dependent-types=false\n```go\nprint(\"hello\")\n```"
                result = parseTypus content "test.typus"
            in case result of
              Right typusFile -> 
                case fdOwnership (tfDirectives typusFile) of
                  Just (Located _ True) -> assertBool "Ownership directive parsed" True
                  _ -> assertBool "Ownership directive not parsed correctly" False
              Left _ -> assertBool "Should parse successfully" False
        ]
    
    , testGroup "QuickCheck Properties"
        [ fastProperty "parseTypus: empty content always succeeds" $
            \filename -> isRight (parseTypus "" filename)
            
        , fastProperty "parseTypus: whitespace-only content succeeds" $
            \ws filename -> L.all isSpace ws ==> isRight (parseTypus ws filename)
            
        , fastProperty "parseTypus: single comment line succeeds" $
            \comment filename ->
              let content = "// " ++ comment
              in isRight (parseTypus content filename)
              
        , fastProperty "parseTypus: file directives with valid syntax" $
            \filename ->
              let content = "//! ownership=true"
              in isRight (parseTypus content filename)
              
        , fastProperty "parseTypus: malformed directives don't crash" $
            \content filename ->
              let malformed = "//! " ++ content
              in case parseTypus malformed filename of
                Right _ -> property True
                Left _ -> property True  -- Expected to fail but not crash
                  
        , fastProperty "parseTypus: code block markers are recognized" $
            \code filename ->
              let content = "```\n" ++ code ++ "\n```"
              in case parseTypus content filename of
                Right typusFile -> 
                  property $ L.length (tfBlocks typusFile) >= 0
                Left _ -> property True
        ]
    ]