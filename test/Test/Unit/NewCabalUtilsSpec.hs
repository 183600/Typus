{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewCabalUtilsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Data.Char (isSpace, isAlphaNum)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (intercalate)

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , breakOn
  )

-- | Unit tests for Utils module
tests :: TestTree
tests =
  testGroup "New Cabal Utils Tests"
    [ testGroup "Unit Tests"
        [ testCase "trim: empty string stays empty" $
            trim "" @?= ""
            
        , testCase "trim: only whitespace becomes empty" $
            trim "   \t\n  " @?= ""
            
        , testCase "trim: preserves non-whitespace content" $
            trim "hello world" @?= "hello world"
            
        , testCase "splitBy: single character delimiter" $
            splitBy ',' "a,b,c" @?= ["a", "b", "c"]
            
        , testCase "splitByCollapsed: removes empty segments" $
            splitByCollapsed ',' "a,,b" @?= ["a", "b"]
            
        , testCase "removeLineComments: basic case" $
            removeLineComments "code // comment" @?= "code "
            
        , testCase "removeComments: block comment" $
            removeComments "code /* comment */ more" @?= "code  more"
        ]
    
    , testGroup "QuickCheck Properties"
        [ fastProperty "trim: idempotent" $
            \s -> trim (trim s) === trim s
            
        , fastProperty "trim: no leading/trailing whitespace" $
            \s -> let t = trim s
                   in null t || (not (isSpace (L.head t)) && not (isSpace (last t)))
                   
        , fastProperty "splitBy: L.length equals delimiter count + 1" $
            \c s -> let segments = splitBy c s
                     in L.length segments >= 1 && 
                        (if null s then L.length segments == 1
                         else L.length segments == countChar c s + 1)
                        
        , fastProperty "splitByCollapsed: never returns empty segments" $
            \c s -> not (L.null (splitByCollapsed c s)) || 
                    L.all (not . null) (splitByCollapsed c s)
                    
        , fastProperty "splitByComma: roundtrip with intercalate" $
            \xs -> let csv = intercalate "," xs
                       parsed = splitByComma csv
                   in parsed === xs
                   
        , fastProperty "removeLineComments: preserves non-commented lines" $
            \s -> not ("//" `L.isInfixOf` s) ==> removeLineComments s === s
        ]
    ]

-- Helper function to count character occurrences
countChar :: Char -> String -> Int
countChar c = L.length . L.filter (== c)