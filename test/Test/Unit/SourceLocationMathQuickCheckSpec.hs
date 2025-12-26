{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationMathQuickCheckSpec (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty, QuickCheckTests(..))
import Test.Tasty.HUnit (testCase, assert)
import SourceLocation
  ( SourcePos(..), SourceSpan(..), Located(..)
  , startPos, posAfter, advancePos, advancePosBy
  , emptySpan, spanFrom, spanTo, mergeSpans, isValidSpan
  , locatedAt, locatedWithSpan
  )
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (choose, listOf1)
import Data.Word (Word32)

-- | Generate arbitrary positive line and column numbers
newtype PositiveInt = PositiveInt Int
  deriving (Show)

instance Arbitrary PositiveInt where
  arbitrary = PositiveInt <$> choose (1, 1000)

-- | Generate arbitrary source positions
instance Arbitrary SourcePos where
  arbitrary = do
    PositiveInt line <- arbitrary
    PositiveInt col <- arbitrary
    return $ SourcePos line col

-- | Generate arbitrary source spans
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    PositiveInt lineOffset <- arbitrary
    PositiveInt colOffset <- arbitrary
    let end = SourcePos 
          (sourceLine start + lineOffset)
          (sourceColumn start + colOffset)
    return $ SourceSpan start end

-- | Generate arbitrary located values
instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = do
    value <- arbitrary
    span <- arbitrary
    return $ Located span value

tests :: TestTree
tests = testGroup "SourceLocation Math Tests"
  [ testProperty "advancePos increments column by 1" $ \pos ->
      let advanced = advancePos pos
      in sourceColumn advanced == sourceColumn pos + 1 &&
         sourceLine advanced == sourceLine pos
  
  , testProperty "advancePosBy increments column by specified amount" $ \pos ->
      \n -> let n' = getPositiveInt n
                advanced = advancePosBy pos n'
            in sourceColumn advanced == sourceColumn pos + n' &&
               sourceLine advanced == sourceLine pos
  
  , testProperty "advancePosBy handles line wrapping when n is 0" $ \pos ->
      let advanced = advancePosBy pos 0
      in advanced == pos
  
  , testProperty "posAfter creates position after character" $ \pos ->
      let after = posAfter pos
      in sourceColumn after == sourceColumn pos + 1 &&
         sourceLine after == sourceLine pos
  
  , testProperty "emptySpan has same start and end" $ \pos ->
      let span = emptySpan pos
      in spanStart span == spanEnd span &&
         spanStart span == pos
  
  , testProperty "spanFrom creates span from position" $ \pos ->
      \len -> let len' = getPositiveInt len
                  span = spanFrom pos len'
                  expectedEnd = advancePosBy pos len'
              in spanStart span == pos &&
                 spanEnd span == expectedEnd
  
  , testProperty "spanTo creates span between positions" $ \start ->
      \end -> let span = spanTo start end
              in spanStart span == start &&
                 spanEnd span == end
  
  , testProperty "mergeSpans combines spans correctly" $ \span1 ->
      \span2 -> let merged = mergeSpans span1 span2
                    minStart = min (spanStart span1) (spanStart span2)
                    maxEnd = max (spanEnd span1) (spanEnd span2)
                in spanStart merged == minStart &&
                   spanEnd merged == maxEnd
  
  , testProperty "isValidSpan checks span validity" $ \start ->
      \end -> let span = spanTo start end
                  valid = sourceLine start <= sourceLine end ||
                         (sourceLine start == sourceLine end && 
                          sourceColumn start <= sourceColumn end)
              in isValidSpan span == valid
  
  , testCase "startPos creates position (1,1)" $
      startPos @?= SourcePos 1 1
  
  , testCase "locatedAt creates located value at position" $ do
      let pos = SourcePos 5 10
          value = "test"
          located = locatedAt pos value
      locatedSpan located @?= emptySpan pos
      locatedValue located @?= value
  
  , testProperty "locatedWithSpan creates located value with span" $ \span ->
      \value -> let located = locatedWithSpan span value
                in locatedSpan located == span &&
                   locatedValue located == value
  
  , testProperty "advancePos maintains line number for column changes" $ \pos ->
      \n -> let n' = getPositiveInt n
                advanced = advancePosBy pos n'
            in sourceLine advanced == sourceLine pos
  ]
  where
    getPositiveInt (PositiveInt n) = n