{-# LANGUAGE TemplateHaskell #-}
module Test.Unit.NewCabalQuickCheckPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>), choose)
import TestSupport.QuickCheck (fastProperty)
import Data.List (isInfixOf)

import qualified Utils
import qualified SourceLocation
import qualified Parser

-- ============================================================================
-- QuickCheck属性测试用例
-- ============================================================================

-- | 新的Cabal QuickCheck属性测试
tests :: TestTree
tests =
  testGroup "New Cabal QuickCheck Properties"
    [ utilsProperties
    , sourceLocationProperties
    ]

-- ============================================================================
-- Utils模块属性测试
-- ============================================================================
utilsProperties :: TestTree
utilsProperties =
  testGroup "Utils Properties"
    [ testProperty "trim . trim = trim (idempotent)" $
        fastProperty $ \s -> Utils.trim (Utils.trim s) === Utils.trim (s :: String)

    , testProperty "splitBy followed by join should reconstruct original" $
        fastProperty $ \c s -> 
          let delim = toEnum $ (fromEnum c) `mod` 128
              parts = Utils.splitBy delim s
              reconstructed = concatMap (++ [delim]) (init parts) ++ last parts
          in length s <= 100 ==> reconstructed === s

    , testProperty "splitByCollapsed removes empty segments" $
        fastProperty $ \c s ->
          let delim = toEnum $ (fromEnum c) `mod` 128
              parts = Utils.splitByCollapsed delim s
          in all (not . null) parts

    , testProperty "removeLineComments preserves string literals" $
        fastProperty $ \s ->
          let input = "x := \"// not a comment\"\n" ++ s ++ " // real comment"
              result = Utils.removeLineComments input
          in "// not a comment" `isInfixOf` result

    , testProperty "trim length is never greater than original" $
        fastProperty $ \s -> length (Utils.trim s) <= length (s :: String)

    , testProperty "splitBy on empty string returns singleton" $
        fastProperty $ \c ->
          let delim = toEnum $ (fromEnum c) `mod` 128
          in Utils.splitBy delim "" === ["" :: String]

    , testProperty "splitByCollapsed on empty string returns empty" $
        fastProperty $ \c ->
          let delim = toEnum $ (fromEnum c) `mod` 128
          in null (Utils.splitByCollapsed delim "")
    ]

-- ============================================================================
-- SourceLocation模块属性测试
-- ============================================================================
sourceLocationProperties :: TestTree
sourceLocationProperties =
  testGroup "SourceLocation Properties"
    [ testProperty "posAfter advances column by 1" $
        fastProperty $ \line col ->
          let pos = SourceLocation.posAt line col
              after = SourceLocation.posAfter pos
          in SourceLocation.posLine after === SourceLocation.posLine pos &&
             SourceLocation.posColumn after === SourceLocation.posColumn pos + 1

    , testProperty "spanFrom and spanTo create valid span" $
        fastProperty $ \line1 col1 line2 col2 ->
          let start = SourceLocation.posAt line1 col1
              end = SourceLocation.posAt (max line1 line2) (max col1 col2)
              span = SourceLocation.spanBetween start end
          in SourceLocation.isValidSpan span

    , testProperty "mergeSpans is commutative" $
        fastProperty $ \line1 col1 line2 col2 ->
          let span1 = SourceLocation.spanFrom (SourceLocation.posAt line1 col1)
              span2 = SourceLocation.spanFrom (SourceLocation.posAt line2 col2)
              merged1 = SourceLocation.mergeSpans span1 span2
              merged2 = SourceLocation.mergeSpans span2 span1
          in merged1 === merged2

    , testProperty "advancePos by newline increments line" $
        fastProperty $ \line col ->
          let pos = SourceLocation.posAt line col
              after = SourceLocation.advancePos '\n' pos
          in SourceLocation.posLine after === SourceLocation.posLine pos + 1 &&
             SourceLocation.posColumn after === 1

    , testProperty "advancePos by other char increments column" $
        fastProperty $ \line col c ->
          let pos = SourceLocation.posAt line col
              after = SourceLocation.advancePos c pos
          in c /= '\n' ==> 
             SourceLocation.posLine after === SourceLocation.posLine pos &&
             SourceLocation.posColumn after === SourceLocation.posColumn pos + 1
    ]

-- ============================================================================
-- 实例定义
-- ============================================================================

-- 为String和Char定义Arbitrary实例以支持QuickCheck测试
instance Arbitrary String where
  arbitrary = do
    n <- choose (0, 20)
    sequence [choose (' ', '~') | _ <- [1..n]]