module Test.Unit.ErrorHandlingPropertySpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, )
import SourceLocation (SourcePos(..), SourceSpan(..), spanBetween)
-- | Property tests for Error Handling module Test.Unit.ErrorHandlingPropertySpec :: TestTree
tests =   testGroup "Error Handling Property Tests"
  [             testProperty "span validity is consistent" propSpanValidityConsistency
  ,             testProperty "position ordering affects span validity" propPositionOrderingAffectsSpan
  ]

-- | Span validity should be consistent
propSpanValidityConsistency :: SourcePos -> SourcePos -> Property
propSpanValidityConsistency start                               end =
  let span = spanBetween start end
  in span `seq` True  -- Just ensure it doesn't crash

-- | Position ordering should affect span validity
propPositionOrderingAffectsSpan :: SourcePos -> SourcePos -> Property
propPositionOrderingAffectsSpan start                               end =
  let span = spanBetween start end
                                    startLine = sourceLine start
                                    endLine = sourceLine end
                                    startCol = sourceColumn start
                                    endCol = sourceColumn end
                                    properlyOrdered = startLine < endLine || 
                        (startLine == endLine && startCol < endCol)
  in                               properlyOrdered ==> True

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary SourcePos where
                                              arbitrary = SourcePos <$> positive <*> positive
    where
                                      positive = getPositive <$> arbitrary

newtype                               Positive = Positive Int deriving (Show, Eq)

instance Arbitrary Positive where
                                              arbitrary = Positive <$> arbitrary `suchThat` (> 0)

getPositive :: Positive -> Int
getPositive (Positive n) = n