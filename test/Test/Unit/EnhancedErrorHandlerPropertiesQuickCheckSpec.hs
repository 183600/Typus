{-# LANGUAGE CPP #-}

module Test.Unit.EnhancedErrorHandlerPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck

import EnhancedErrorHandler
import SourceLocation (SourcePos(..), SourceSpan(..))

prop_defaultSpan_wellformed :: Property
prop_defaultSpan_wellformed =
  let span = defaultSpan
      start = spanStart span
      end = spanEnd span
  in conjoin
    [ posLine start > 0
    , posColumn start > 0
    , posOffset start >= 0
    , posLine end > 0
    , posColumn end > 0
    , posOffset end >= 0
    ]

prop_formatCompilerErrors_empty_list :: Property
prop_formatCompilerErrors_empty_list =
  formatCompilerErrors [] === ""

prop_sourcePos_ordering :: Property
prop_sourcePos_ordering =
  let pos1 = SourcePos 1 1 0
      pos2 = SourcePos 2 1 10
  in (posOffset pos1 < posOffset pos2) === True

prop_sourceSpan_consistency :: Property
prop_sourceSpan_consistency =
  let start = SourcePos 1 1 0
      end = SourcePos 2 1 10
      span = SourceSpan start end
  in conjoin
    [ spanStart span === start
    , spanEnd span === end
    ]

tests :: TestTree
tests = testGroup "EnhancedErrorHandler Properties QuickCheck Tests"
  [ fastProperty "defaultSpan is well-formed" prop_defaultSpan_wellformed
  , fastProperty "formatCompilerErrors handles empty list" prop_formatCompilerErrors_empty_list
  , fastProperty "SourcePos ordering works" prop_sourcePos_ordering
  , fastProperty "SourceSpan consistency" prop_sourceSpan_consistency
  ]
