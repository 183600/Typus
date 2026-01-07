module Test.Unit.SimpleCabalTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, )
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Utils 
propTrimIdempotent                               s = trim (trim s) === trim s
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- | Property: splitBy preserves segments
propSplitByPreservesSegments :: Char -> String -> Property
propSplitByPreservesSegments delim                               s = 
  let segments = splitBy delim s
                                    rejoined = L.intercalate [delim] segments
  in property $ L.length rejoined >= L.length s

-- | Unit test for trim basic functionality
testTrimBasic :: IO ()
                              testTrimBasic = do
                          assertEqual "trim empty string" "" (trim "")
              assertEqual "trim whitespace" "" (trim "   ")
              assertEqual "trim preserves content" "abc" (trim "  abc  ")

-- | Unit test for splitBy basic functionality
testSplitByBasic :: IO ()
                              testSplitByBasic = do
                          assertEqual "splitBy single char" ["a", "b"] (splitBy ',' "a,b")
              assertEqual "splitBy with empty segments" ["a", "", "b"] (splitBy ',' "a,,b")

-- Helper imports
import qualified Data.List as L

-- Helper function for property testing
property :: Bool -> Property
                              property = property' where
    property' :: Bool -> Property
  property' = id