{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Common imports for all test modules
module Test.Unit.TestImports where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Char (isSpace, isAlphaNum, isLetter, isLower)
import Control.Monad (when, unless)
import qualified Data.Set as Set

-- Re-export commonly used functions for convenience
import Test.Tasty.QuickCheck (testProperty, Property, (===), (==>), property, Arbitrary(..), Gen, Property, (==>), forAll, choose, listOf1, elements, oneof, sized, suchThat, (===), reject)