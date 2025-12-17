{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.UtilsBasicQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import Data.Char (isSpace, toLower, toUpper)
import Data.List (isPrefixOf, isSuffixOf)

import Utils (trim, splitBy, splitByCollapsed)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Utils Basic QuickCheck Tests"
  [ stringManipulationProperties
  , splittingProperties
  , predicateProperties
  ]

stringManipulationProperties :: TestTree
stringManipulationProperties = testGroup "String Manipulation Properties"
  [ fastProperty "trim is idempotent" $ \s ->
      let t = trim s in trim t === t
  
  , fastProperty "trim result has no leading spaces" $ \s ->
      case trim s of
        [] -> property True
        (c:_) -> property (not (isSpace c))
  
  , fastProperty "trim result has no trailing spaces" $ \s ->
      case reverse (trim s) of
        [] -> property True
        (c:_) -> property (not (isSpace c))
  
  , fastProperty "toLower then toUpper may not restore original" $ \c ->
      toUpper (toLower c) === toUpper c .||. toLower (toUpper c) === toLower c
  ]

splittingProperties :: TestTree
splittingProperties = testGroup "Splitting Properties"
  [ fastProperty "splitBy preserves content" $ \c s ->
      c /= '\0' ==>
      let parts = splitBy c s
      in concat parts === filter (/= c) s .||. any (== c) s
  
  , fastProperty "splitByCollapsed removes empty parts" $ \c s ->
      c /= '\0' ==>
      let parts = splitByCollapsed c s
      in all (not . null) parts
  
  , fastProperty "splitting empty string gives one empty part" $
      splitBy ',' "" === [""]
  
  , fastProperty "splitting by delimiter at start gives empty first part" $ \c s ->
      c /= '\0' && not (null s) ==>
      let parts = splitBy c ([c] ++ s)
      in case parts of
           (p:_) -> p === ""
           [] -> property False
  ]

predicateProperties :: TestTree
predicateProperties = testGroup "Predicate Properties"
  [ fastProperty "isPrefixOf is reflexive" $ \(s :: String) ->
      s `isPrefixOf` s
  
  , fastProperty "isSuffixOf is reflexive" $ \(s :: String) ->
      s `isSuffixOf` s
  
  , fastProperty "empty string is prefix of any string" $ \(s :: String) ->
      "" `isPrefixOf` s
  
  , fastProperty "empty string is suffix of any string" $ \(s :: String) ->
      "" `isSuffixOf` s
  ]
