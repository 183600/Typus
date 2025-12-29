{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalSourceLocationSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace)

import SourceLocation
  ( SourcePos(..)
  , SourceSpan(..)
  , Located(..)
  , startPos
  , posAfter
  , posAt
  , posAtLineCol
  , emptySpan
  , spanFrom
  , spanTo
  , mergeSpans
  , isValidSpan
  , locatedAt
  , locatedWithSpan
  , locatedValue
  , locatedSpan
  , advancePos
  , advancePosByText
  )

-- | Unit tests for SourceLocation module
tests :: TestTree
tests =
  testGroup "New Cabal SourceLocation Tests"
    [ testGroup "Unit Tests"
        [ testCase "startPos has correct initial values" $
            do
              posLine startPos @?= 1
              posColumn startPos @?= 1
              posOffset startPos @?= 0
              
        , testCase "posAfter: newline increments line, resets column" $
            let pos = posAfter '\n' startPos
            in do
              posLine pos @?= 2
              posColumn pos @?= 1
              posOffset pos @?= 1
              
        , testCase "posAfter: regular character increments column" $
            let pos = posAfter 'a' startPos
            in do
              posLine pos @?= 1
              posColumn pos @?= 2
              posOffset pos @?= 1
              
        , testCase "posAt: creates position at specific line and column" $
            let pos = posAt 5 10
            in do
              posLine pos @?= 5
              posColumn pos @?= 10
              
        , testCase "emptySpan: creates a valid empty span" $
            let span = emptySpan startPos
            in isValidSpan span @?= True
            
        , testCase "locatedAt: creates located value at position" $
            let located = locatedAt startPos "test"
            in do
              locatedValue located @?= "test"
              posLine (spanStart $ locatedSpan located) @?= 1
        ]
    
    , testGroup "QuickCheck Properties"
        [ fastProperty "posAfter: newline always resets column to 1" $
            \pos -> let newPos = posAfter '\n' pos
                    in posColumn newPos === 1
                    
        , fastProperty "posAfter: newline always increments line" $
            \pos -> posLine (posAfter '\n' pos) === posLine pos + 1
            
        , fastProperty "posAfter: regular character increments column by 1" $
            \pos c -> c /= '\n' && c /= '\t' ==> 
                      posColumn (posAfter c pos) === posColumn pos + 1
                      
        , fastProperty "posAfter: offset always increments by 1" $
            \pos c -> posOffset (posAfter c pos) === posOffset pos + 1
            
        , fastProperty "posAtLineCol: creates position with given values" $
            \line col offset -> 
              let pos = posAtLineCol line col offset
              in posLine pos === line && 
                 posColumn pos === col && 
                 posOffset pos === offset
                 
        , fastProperty "mergeSpans: start of merged span equals start of first" $
            \span1 span2 -> 
              let merged = mergeSpans span1 span2
              in spanStart merged === spanStart span1
              
        , fastProperty "locatedValue: returns the original value" $
            \pos v -> locatedValue (locatedAt pos v) === v
            
        , fastProperty "advancePosByText: empty text leaves position unchanged" $
            \pos -> advancePosByText "" pos === pos
        ]
    ]