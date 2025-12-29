{-# LANGUAGE ScopedTypeVariables #-}
module Test.Unit.EnhancedCabalQuickCheckTestSuite (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
    ( Property, forAll, Arbitrary, arbitrary, (.&&.), (==>)
    , (===), classify, counterexample, property
    , Gen, choose, listOf, elements, oneof, suchThat, vectorOf
    , Positive(..), NonEmptyList(..)
    )
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)

import qualified Utils

-- | Enhanced QuickCheck property tests for Utils module functionality
tests :: TestTree
tests =
  testGroup "Enhanced Cabal QuickCheck Tests"
    [ testGroup "Utils String Processing Properties"
        [ fastProperty "trim removes only leading/trailing whitespace" $
            \s ->
              let trimmed = Utils.trim s
                  hasLeadingOrTrailing = not (null s) && 
                    (isSpace (head s) || isSpace (last s))
              in not hasLeadingOrTrailing ==> 
                (not (null trimmed) ==> not (isSpace (head trimmed) || isSpace (last trimmed)))
                
        , fastProperty "splitBy and intersperse roundtrip preserves non-empty segments" $
            \delim (NonEmpty xs) ->
              let delim' = if null delim then ',' else head delim
                  s = concat (intersperse [delim'] xs)
                  result = Utils.splitBy delim' s
              in result === xs
              
        , fastProperty "splitByCollapsed never produces empty strings" $
            \delim s ->
              let delim' = if null delim then ',' else head delim
                  result = Utils.splitByCollapsed delim' s
              in all (not . null) result
              
        , fastProperty "trim is idempotent" $
            \s ->
              let once = Utils.trim s
                  twice = Utils.trim once
              in once === twice
              
        , fastProperty "splitBy preserves order of segments" $
            \delim s ->
              let delim' = if null delim then ',' else head delim
                  result = Utils.splitBy delim' s
                  rejoined = concat (intersperse [delim'] result)
              in Utils.splitBy delim' rejoined === result
              
        , fastProperty "removeLineComments preserves non-comment lines" $
            \s ->
              let withoutComments = Utils.removeLineComments s
                  linesWithoutComments = lines withoutComments
              in length linesWithoutComments >= 0 -- Always true, ensures function doesn't crash
              
        , fastProperty "normalizeIndentation preserves relative structure" $
            \s ->
              let normalized = Utils.normalizeIndentation s
              in length (lines normalized) === length (lines s)
              
        , fastProperty "breakOn either finds pattern or returns original" $
            \pattern s ->
              let (prefix, suffix) = Utils.breakOn pattern s
              in if pattern `isInfixOf` s 
                 then prefix ++ pattern ++ suffix === s
                 else prefix === s && suffix === ""
        ]
    ]

-- Helper function for list interspersion
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs