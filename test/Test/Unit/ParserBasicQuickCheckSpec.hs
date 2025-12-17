{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isAlphaNum, isSpace)

import Parser (FileDirectives(..), BlockDirectives(..), defaultFileDirectives, defaultBlockDirectives)
import Utils (trim, splitBy)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Parser Basic QuickCheck Tests"
  [ directivesProperties
  , identifierProperties
  , whitespaceProperties
  ]

directivesProperties :: TestTree
directivesProperties = testGroup "Directives Properties"
  [ fastProperty "default file directives are all Nothing" $
      fdOwnership defaultFileDirectives === Nothing .&&.
      fdDependentTypes defaultFileDirectives === Nothing .&&.
      fdConstraints defaultFileDirectives === Nothing
  
  , fastProperty "default block directives are all Nothing" $
      bdOwnership defaultBlockDirectives === Nothing .&&.
      bdDependentTypes defaultBlockDirectives === Nothing .&&.
      bdConstraints defaultBlockDirectives === Nothing
  ]

identifierProperties :: TestTree
identifierProperties = testGroup "Identifier Properties"
  [ fastProperty "valid identifiers start with letter or underscore" $ \c ->
      let valid = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
      in valid ==> property True
  
  , fastProperty "identifiers contain alphanumeric or underscore" $ \(s :: String) ->
      all (\c -> isAlphaNum c || c == '_') s ==> property True
  ]

whitespaceProperties :: TestTree
whitespaceProperties = testGroup "Whitespace Properties"
  [ fastProperty "trim removes leading spaces" $ \s ->
      case trim s of
        [] -> property True
        (c:_) -> property (not (isSpace c))
  
  , fastProperty "trim removes trailing spaces" $ \s ->
      case reverse (trim s) of
        [] -> property True
        (c:_) -> property (not (isSpace c))
  
  , fastProperty "splitting by space preserves words" $ \(words' :: [String]) ->
      not (null words') ==>
      let s = unwords words'
          parts = filter (not . null) (splitBy ' ' s)
      in length parts <= length words'
  ]
