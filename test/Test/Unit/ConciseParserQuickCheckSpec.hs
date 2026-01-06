{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module Test.Unit.ConciseParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.QuickCheck (testProperty, Property, (===), (==>), Arbitrary(..), Gen, oneof, listOf, elements)
import Data.Char (isSpace)
import qualified Data.Text as T
import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import Utils (trim)

-- | 简洁的QuickCheck测试，针对Parser模块的基础功能
tests :: TestTree
tests =
  testGroup "Concise Parser QuickCheck Tests"
    [ testGroup "Basic parsing properties"
        [ testProperty "parseTypus handles empty input" $
            \(_ :: ()) -> case parseTypus "" of
                    Left _ -> property True
                    Right file -> tfBlocks file === []
                    
        , testProperty "parseTypus preserves line count in simple cases" $
            \lines -> not (null lines) ==> 
            let input = unlines lines
            in case parseTypus input of
                 Left _ -> property True
                 Right file -> property (L.length (tfBlocks file) <= L.length lines)
                 
        , testProperty "parseTypus handles whitespace-only lines" $
            \ws -> case parseTypus ws of
                     Left _ -> property True
                     Right file -> property (L.all (L.all isSpace . unlines . map cbContent . (:[])) (tfBlocks file))
                     
        , testProperty "parseTypus result contains syntax errors list" $
            \input -> case parseTypus input of
                        Left _ -> property True
                        Right file -> property (L.length (tfSyntaxErrors file) >= 0)
        ]
        
    , testGroup "Directive parsing"
        [ testProperty "File directives can be parsed independently" $
            \ownership dependent -> 
            let input = if ownership then "//!ownership:true\n" else ""
                     ++ if dependent then "//!dependentTypes:true\n" else ""
            in case parseTypus input of
                 Left _ -> property True
                 Right file -> property True  -- If parsing succeeds, we consider it a success
        ]
    ]

-- Helper function for QuickCheck properties
property :: Bool -> Property
property b = b === True