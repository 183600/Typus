{-# LANGUAGE CPP #-}

module Test.Unit.CoreDataStructuresQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)

import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Core DataStructures QuickCheck Properties"
  [ sourceLocationTests
  ]

sourceLocationTests :: TestTree
sourceLocationTests = testGroup "SourceLocation Properties"
  [ fastProperty "source span ordering is consistent" prop_sourcespan_ordering
  , fastProperty "source position arithmetic works" prop_sourcepos_arithmetic
  ]

-- SourceLocation properties
prop_sourcespan_ordering :: SourceSpan -> SourceSpan -> Property
prop_sourcespan_ordering span1 span2 =
  let start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in property $ posOffset start1 <= posOffset end1 && posOffset start2 <= posOffset end2

prop_sourcepos_arithmetic :: Positive Int -> Positive Int -> Property
prop_sourcepos_arithmetic (Positive line) (Positive col) =
  let pos1 = SourcePos line col 0
      pos2 = SourcePos line (col + 1) 1
  in property $ posOffset pos1 < posOffset pos2