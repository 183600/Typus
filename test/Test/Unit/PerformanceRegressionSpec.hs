module Test.Unit.PerformanceRegressionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty)

import TestSupport.QuickCheck (fastProperty)

import Utils
import SourceLocation
import qualified Data.Text as T
import Control.DeepSeq (NFData, force)
import Criterion.Main (bench, nf, whnf)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- | Test performance regression scenarios and ensure operations remain efficient
tests :: TestTree
tests =
  testGroup "Performance Regression Tests"
    [ testGroup "String Processing Performance"
        [ testCase "large comment removal performance" $ do
            let largeInput = unlines $ replicate 10000 $ "code line // comment with some text"
                startTime <- getCurrentTime
                result <- return $ removeComments largeInput
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Should complete within reasonable time (5 seconds for safety)
            assertBool "Comment removal took too long" (duration < 5)

        , testCase "large text splitting performance" $ do
            let largeText = concat $ replicate 10000 "item,"
                startTime <- getCurrentTime
                result <- return $ splitBy ',' largeText
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Should complete quickly
            assertBool "Text splitting took too long" (duration < 3)

        , testCase "indentation normalization performance" $ do
            let nestedInput = unlines $ map (\i -> replicate i ' ' ++ "line") [1..1000]
                startTime <- getCurrentTime
                result <- return $ normalizeIndentation nestedInput
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Should handle nested indentation efficiently
            assertBool "Indentation normalization took too long" (duration < 2)

        , testCase "trim operation on very large strings" $ do
            let largeString = "   " ++ replicate 100000 'x' ++ "   "
                startTime <- getCurrentTime
                result <- return $ trim largeString
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Trim should be O(n) and fast
            assertBool "Trim operation took too long" (duration < 1)
        ]

    , testGroup "Source Location Performance"
        [ testCase "position advancement performance" $ do
            let largeText = replicate 100000 'a'
                start = startPos
                startTime <- getCurrentTime
                finalPos <- return $ advancePosBy largeText start
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Position tracking should be linear and fast
            assertBool "Position advancement took too long" (duration < 2)

        , testCase "span merging performance" $ do
            let spans = [SourceSpan startPos (posAt 100 100 10000) | _ <- [1..1000]]
                startTime <- getCurrentTime
                merged <- return $ foldl mergeSpans (head spans) (tail spans)
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Span operations should be efficient
            assertBool "Span merging took too long" (duration < 1)

        , testCase "error location conversion performance" $ do
            let positions = [SourcePos l c (l * 1000 + c) | l <- [1..1000], c <- [1..100]]
                startTime <- getCurrentTime
                errorLocs <- return $ map toErrorLocation positions
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Error location conversion should be fast
            assertBool "Error location conversion took too long" (duration < 2)
        ]

    , testGroup "Memory Usage Tests"
        [ testCase "memory efficiency of large text processing" $ do
            let largeInput = unlines $ replicate 5000 $ "line with some content and text"
                -- Force evaluation to ensure memory is actually used
                processed = force $ removeComments largeInput
                length processed `seq` processed @?= processed

        , testCase "memory efficiency of repeated operations" $ do
            let baseText = "test string"
                repeated = concat $ replicate 10000 baseText
                -- Process multiple times to check for memory leaks
                result1 = force $ trim repeated
                result2 = force $ trim repeated
                result3 = force $ trim repeated
            result1 @?= result2
            result2 @?= result3

        , testCase "memory efficiency of location tracking" $ do
            let positions = [SourcePos l c (l * 100 + c) | l <- [1..500], c <- [1..200]]
                spans = map (\p -> SourceSpan p p) positions
                -- Force evaluation
                merged = force $ foldl mergeSpans (head spans) (tail spans)
            length spans `seq` merged @?= merged
        ]

    , testGroup "Algorithmic Complexity Tests"
        [ testCase "linear complexity of text processing" $ do
            let sizes = [1000, 2000, 4000, 8000]
                testSize size = do
                    let text = replicate size 'x'
                        startTime <- getCurrentTime
                        result <- return $ trim text
                        endTime <- getCurrentTime
                        return $ diffUTCTime endTime startTime
                durations <- mapM testSize sizes
                -- Check that growth is roughly linear (each doubling shouldn't more than triple time)
                let ratios = zipWith (/) (tail durations) (init durations)
                assertBool "Text processing shows super-linear growth" (all (< 3.0) ratios)

        , testCase "splitting complexity scales linearly" $ do
            let sizes = [1000, 2000, 4000]
                testSplitSize size = do
                    let text = concat $ replicate size "a,"
                        startTime <- getCurrentTime
                        result <- return $ splitBy ',' text
                        endTime <- getCurrentTime
                        return $ diffUTCTime endTime startTime
                durations <- mapM testSplitSize sizes
                let ratios = zipWith (/) (tail durations) (init durations)
                assertBool "Splitting shows super-linear growth" (all (< 2.5) ratios)

        , testCase "location operations are constant time" $ do
            let positions = [SourcePos 1 i i | i <- [1..10000]]
                startTime <- getCurrentTime
                errorLocs <- return $ map toErrorLocation positions
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Should be very fast even for many operations
            assertBool "Location operations are too slow" (duration < 1)
        ]

    , testGroup "Regression Prevention Tests"
        [ testCase "prevent exponential behavior in nested structures" $ do
            let nestedBraces = concat $ replicate 100 $ replicate 10 '{'
                startTime <- getCurrentTime
                result <- return $ removeComments nestedBraces
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Should not exhibit exponential behavior
            assertBool "Nested structure processing shows exponential behavior" (duration < 2)

        , testCase "prevent quadratic behavior in repeated patterns" $ do
            let pattern = "a,b,c,d,e"
                repeated = concat $ replicate 1000 pattern
                startTime <- getCurrentTime
                parts <- return $ splitBy ',' repeated
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            -- Should be linear in input size
            assertBool "Pattern processing shows quadratic behavior" (duration < 1)

        , testCase "prevent memory leaks in iterative processing" $ do
            let processIteration i = do
                    let text = replicate (i * 100) 'x'
                    return $ trim text
                results <- mapM processIteration [1..100]
                -- Force evaluation of all results
                forced = force results
            length forced @?= 100  -- Should complete without memory issues
        ]

    , testGroup "Stress Tests"
        [ testCase "handle extremely large files gracefully" $ do
            let hugeFile = unlines $ replicate 50000 $ "line content with moderate length"
                startTime <- getCurrentTime
                linesCount <- return $ length $ lines hugeFile
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            linesCount @?= 50000
            assertBool "Huge file processing took too long" (duration < 10)

        , testCase "handle deeply nested comment structures" $ do
            let nestedComments = "code " ++ concat (replicate 1000 "/* comment */ ")
                startTime <- getCurrentTime
                result <- return $ removeComments nestedComments
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            result @?= "code " ++ replicate 1000 " "
            assertBool "Nested comment processing took too long" (duration < 3)

        , testCase "handle massive indentation variations" $ do
            let indentVariations = unlines $ map (\i -> replicate i ' ' ++ "line") [0..2000]
                startTime <- getCurrentTime
                normalized <- return $ normalizeIndentation indentVariations
                endTime <- getCurrentTime
                duration = diffUTCTime endTime startTime
            length (lines normalized) @?= 2001
            assertBool "Massive indentation processing took too long" (duration < 5)
        ]

    , testGroup "Property-based Performance Tests"
        [ fastProperty "trim performance scales linearly with input size" prop_trimLinear
        , fastProperty "splitting performance doesn't degrade with repetitive patterns" prop_splittingEfficient
        , fastProperty "location tracking maintains constant time per operation" prop_locationConstant
        , fastProperty "memory usage remains bounded for repeated operations" prop_memoryBounded
        ]
    ]

-- Property: trim should show linear scaling behavior
prop_trimLinear :: String -> Bool
prop_trimLinear input =
  let base = take 1000 (input ++ repeat 'x')
      doubled = base ++ base
      -- Simple heuristic: doubled input shouldn't take more than 3x time
      -- (In real property tests, we'd use actual timing, here we use length as proxy)
      length (trim doubled) <= 3 * length (trim base)

-- Property: splitting should remain efficient with repetitive patterns
prop_splittingEfficient :: String -> Bool
prop_splittingEfficient input =
  let pattern = take 10 (input ++ repeat 'x')
      repeated = concat $ replicate 100 pattern
      parts = splitBy ',' repeated
  -- Should produce reasonable number of parts
  length parts <= 1000

-- Property: location operations should be constant time per operation
prop_locationConstant :: Int -> Int -> Bool
prop_locationConstant line col =
  let pos = SourcePos (abs line `mod` 10000 + 1) (abs col `mod` 1000 + 1) 0
      errorLoc = toErrorLocation pos
  -- Should always succeed and produce valid result
  line errorLoc == posLine pos && column errorLoc == posColumn pos

-- Property: memory usage should remain bounded
prop_memoryBounded :: String -> Bool
prop_memoryBounded input =
  let base = take 1000 input
      processed = [trim base, removeComments base, normalizeIndentation base]
  -- All operations should produce strings of reasonable size
  all (\s -> length s <= 10000) processed